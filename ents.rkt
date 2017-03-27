; Similar to "#lang racket"
(module ents racket

(require racket/gui/base)
(require racket/set)
; for list-index
(require srfi/1)

(require "misc.rkt")
(require "db.rkt")
(require "ui.rkt")

(provide zinal:ent:manager%)

; TERMINOLOGY AND CONCEPTS

; The db is a rather simple and minimalistic representation of the logic. Displaying it literally
; as a tree would be verbose and ugly. We want a middle layer that parses this "low-level" tree into
; a more concise, sophisticated, "high-level" tree. To do so, we will partition the db into a set of
; "cones". A "cone" is a partial subtree. It is any tree, whose nodes (and edges) are a subset of the
; "embedding" tree, but whose leaves need not be leaves of the embedding tree. Each cone of the
; partitioned db will be implicitly represented by an "ent". Besides storing the root and leaves
; of the corresponding cone, an ent is responsible for building and maintaining a ui cone that
; represents the corresponding db cone. UI cones sum up to form a full ui tree which is used by a
; front-end to display a gui. The displayed gui does not handle events - instead, events are sent to
; the ent-manager (the only part of the ent system that the front-end knows about), which then sends
; the event to the selected ui item, which uses an event handler provided to it by the ent and which
; can be changed or updated by the ent. So ui items store event handlers, but the assignment and
; implementation of those handlers are the responsibility of the ent.

; HELPER FUNCTIONS

(define (handles-equal? handle1 handle2)
  (or
    (and (not handle1) (not handle2))
    (and handle1 handle2 (send handle1 equals? handle2))
  )
)

(define (standard*? db-legacy-link-handle)
  (not (send db-legacy-link-handle get-library))
)

(define (standard-with-name*? db-legacy-link-handle name)
  (and (standard*? db-legacy-link-handle) (equal? name (send db-legacy-link-handle get-name)))
)

(define (get-short-desc-or* db-describable-handle alt)
  (or (send db-describable-handle get-short-desc) alt)
)

(define (can-be-public*? define-handle)
  (and
    (is-one-of? define-handle (list zinal:db:def%% zinal:db:define-class%%))
    (is-a? (send define-handle get-parent) zinal:db:module%%)
  )
)

(define (public*? define-handle)
  (assert "cannot assess \"publicity\" of non-def or non-module-child" (can-be-public*? define-handle))
  (findf (curry handles-equal? define-handle) (send (send define-handle get-parent) get-public-defs))
)

(define (get-containing-class* node)
  (and node (if (is-a? node zinal:db:class%%) node (get-containing-class* (send node get-parent))))
)

(define (issue-warning title message)
  (message-box title message #f '(ok caution))
)

(define (get-zinal-super-class* subclass)
  (define super-class-ref (send subclass get-super-class))
  (and (is-a? super-class-ref zinal:db:class-ref%%) (send super-class-ref get-define-class))
)

(define NO-STYLE (make-object style-delta%))

; EVENT HANDLER

(define event-handler%% (interface ()

  handle-event!! ; (event)
))

(define fallback-event-handler%% (interface ()

  handle-child-event!! ; (event)
))

; Everything is terrible
(define handle-event-info% (class object%

  (define was-handled*? #t)
  (define was-db-affected*? #t)

  (define/public (was-handled?)
    was-handled*?
  )

  (define/public (was-db-affected?)
    (and was-handled*? was-db-affected*?)
  )

  (define/public (set-wasnt-handled!)
    (set! was-handled*? #f)
    ; for convenience; also terrible
    #t
  )

  (define/public (set-db-wasnt-affected!)
    (set! was-db-affected*? #f)
    ; for convenience; also terrible
    #t
  )

  (super-make-object)
))

(define keyname-event-handler% (class* object% (event-handler%%)

  ; handler-function&keynames=pairs is a list of pairs, first item is an event handler function
  ; and second is a list of key names that invoke the function
  (init handler-function&keynames=pairs)

  (define handler-function&keynames=pairs* handler-function&keynames=pairs)

  (define keymap* (make-object keymap%))
  (begin
    (define seen-keynames (mutable-set))
    (for-each
      (lambda (handler-function&keynames)
        (define handler-function (first handler-function&keynames))
        (define keynames (second handler-function&keynames))
        (define function-name (string-join (sort keynames string<?) " "))
        (send keymap* add-function function-name handler-function)
        (for-each
          (lambda (keyname)
            (assert
              (format "keyname ~a already exists" keyname)
              (not (set-member? seen-keynames keyname))
            )
            (set-add! seen-keynames keyname)
            (send keymap* map-function keyname function-name)
          )
          keynames
        )
      )
      handler-function&keynames=pairs
    )
  )

  (define/public (handle-event!! event)
    (define info (make-object handle-event-info%))
    (define was-handler-found?
      (if (is-a? event key-event%)
        (send keymap* handle-key-event info event)
        (send keymap* handle-mouse-event info event)
      )
    )
    (unless was-handler-found? (send info set-wasnt-handled!))
    info
  )

  (define/public (get-handler-function&keynames=pairs*)
    handler-function&keynames=pairs*
  )

  (super-make-object)
))

(define NOOP (make-object keyname-event-handler% '()))

(define NOOP-FALLBACK-EVENT-HANDLER (make-object (class* object% (fallback-event-handler%%)
  (define/public (handle-child-event!! event) (send NOOP handle-event!! event))
  (super-make-object)
)))

(define THING->NOOP (const NOOP))

(define (combine-keyname-event-handlers event-handlers)
  (make-object keyname-event-handler%
    (append* (map (lambda (h) (send h get-handler-function&keynames=pairs*)) event-handlers))
  )
)

; SLOTS

(define slot% (class* object% (fallback-event-handler%%)

  (init slot->event-handler fallback-event-handler)

  (define ent* #f)
  (define event-handler* (slot->event-handler this))
  (define fallback-event-handler* fallback-event-handler)

  ; Must be called at least once immediately after creation
  (define/public (set-ent! new-ent)
    (set! ent* new-ent)
  )

  (define/public (get-ent)
    (assert-valid*)
    ent*
  )

  (define/public (handle-child-event!! event)
    (assert-valid*)
    (define event-info (send event-handler* handle-event!! event))
    (or
      (and (send event-info was-handled?) event-info)
      (send fallback-event-handler* handle-child-event!! event)
    )
  )

  (define (assert-valid*)
    (assert "ent must be initialized before using" ent*)
  )

  (super-make-object)
))

(define (slot/ui-item->ui-item slot/ui-item)
  (if (is-a? slot/ui-item slot%)
    (send (send slot/ui-item get-ent) get-root-ui-item)
    slot/ui-item
  )
)

(define (slot->db-handle slot)
  (send (send slot get-ent) get-cone-root)
)

; INTERACTION GUI

(define choice-dialog%
  (class dialog% ; abstract

    (init label)

    (abstract get-choice-from-ui*)

    (define/override (on-subwindow-char receiver key-event)
      (case (send key-event get-key-code)
        [(#\return)
          (set! has-been-chosen* #t)
          (close*!)
          #t
        ]
        [(#\tab)
          (super on-subwindow-char receiver key-event)
          #f
        ]
        [else (super on-subwindow-char receiver key-event)]
      )
    )

    (define/override (on-activate activated?)
      (super on-activate activated?)
      ; TODO this is a dirty dirty hack to force the choice% to focus against its will
      ; TODO see https://groups.google.com/forum/#!msg/racket-users/ph2nfGslyuA/AjAM6wMMAwAJ
      (send this on-subwindow-char this (make-object key-event% #\tab))
    )

    (define (on-close)
      (set! chosen*
        (if has-been-chosen*
          (get-choice-from-ui*)
          #f
        )
      )
    )
    (augment on-close)

    (super-make-object label)

    (define chosen* #f)
    (define has-been-chosen* #f)

    (define/public (get-choice)
      chosen*
    )

    (define/private (close*!)
      (send this on-close)
      (send this show #f)
    )
  )
)

(define auto-complete-dialog%
  (class choice-dialog%

    ; key-equalifier takes two key args and returns whether they're equivalent.
    ; The first arg will never be #f, but the second one may be
    (init title message keys&choices [key-equalifier equal?])

    (define this* this)

    (define auto-complete-list-box%
      (class list-box%

        (define/override (on-subwindow-char receiver key-event)
          (define cur-selection-cur-index (send this get-selection))
          (define cur-selection-key (and cur-selection-cur-index (send this get-data cur-selection-cur-index)))
          (define num-cur-choices (send this get-number))
          (define key-code (send key-event get-key-code))
          (case key-code
            [(#\tab)
              (send this set-selection
                (if (= cur-selection-cur-index (sub1 num-cur-choices))
                  0
                  (add1 cur-selection-cur-index)
                )
              )
              #t
            ]
            [(#\backspace #\rubout)
              (cond
                [(pair? chars)
                  (set! chars (take chars (sub1 (length chars))))
                  (build-choices*! cur-selection-key)
                  #t
                ]
                [else #f]
              )
            ]
            [else
              (cond
                [(char? key-code)
                  (define char (char-downcase key-code))
                  (set! chars (append chars (list char)))
                  (build-choices*! cur-selection-key)
                  #t
                ]
                [else #f]
              )
            ]
          )
        )

        (super-make-object message '() this* (const #f) '(single vertical-label))

        (define/private (build-choices*! selection-key)
          (send this clear)
          (for-each
            (lambda (pair)
              (define key (car pair))
              (define choice (second pair))
              (when (subsequence? chars (string->list (string-downcase choice)))
                (send this append choice key)
                (define cur-index (sub1 (send this get-number)))
                (when (and selection-key (key-equalifier selection-key key))
                  (send this set-selection cur-index)
                )
              )
            )
            keys&choices*
          )
          (when (zero? (send this get-number)) (reset-choices*!))
          ; TODO we need to laser in on any reference that's an exact match
          ; if we type "n", we don't want it to be underneath "index", because then there's no way to get
          ; to "n" without resorting to the arrow keys or mouse
          (unless (send this get-selection)
            (send this set-selection 0)
          )
          ; TODO very hacky sizing
          (send this min-height (+ 60 (* 25 (send this get-number))))
        )

        (define/private (subsequence? candidate sequence)
          (cond
            [(null? candidate)
              #t
            ]
            [(null? sequence)
              #f
            ]
            [(equal? (car candidate) (car sequence))
              (subsequence? (cdr candidate) (cdr sequence))
            ]
            [else
              (subsequence? candidate (cdr sequence))
            ]
          )
        )

        (define/private (reset-choices*!)
          (set! chars '())
          (build-choices*! #f)
        )

        (define chars '())
        (reset-choices*!)
      )
    )

    (define/override (get-choice-from-ui*)
      (define cur-index (send chooser* get-selection))
      (and cur-index (send chooser* get-data cur-index))
    )

    (super-make-object title)

    (define keys&choices* (sort keys&choices (lambda (a b) (string<? (second a) (second b)))))
    (define chooser* (make-object auto-complete-list-box%))
  )
)

(define (auto-complete* title message keys&choices [key-equalifier equal?])
  (cond
    [(pair? keys&choices)
      (define dialog
        (make-object auto-complete-dialog% title message keys&choices key-equalifier)
      )
      (send dialog show #t)
      ; ugly hack to solve some weird gui focus race condition. See blame for more details.
      (sleep .05)
      (send dialog get-choice)
    ]
    [else
      #f
    ]
  )
)

(define discrete-choice-dialog%
  (class choice-dialog%

    (init title message choices)

    (define this* this)

    (define keyboard-choice%
      (class choice%

        (define/override (on-subwindow-char receiver key-event)
          (define current-selection (send this get-selection))
          (define key-code (send key-event get-key-code))
          (define (reset-chars!) (set! chars ""))
          (case key-code
            [(#\J)
              (reset-chars!)
              (unless (= current-selection (sub1 num-choices*))
                (send this set-selection (add1 current-selection))
              )
              #t
            ]
            [(#\K)
              (reset-chars!)
              (unless (zero? current-selection)
                (send this set-selection (sub1 current-selection))
              )
              #t
            ]
            [else
              (cond
                [(and (char? key-code) (char-alphabetic? key-code))
                  (define char (char-downcase key-code))
                  (set! chars (string-append chars (string char)))
                  (define candidate (findf (curryr string-prefix? chars) choices*))
                  (if candidate
                    (send this set-string-selection candidate)
                    (reset-chars!)
                  )
                  #t
                ]
                [else #f]
              )
            ]
          )
        )

        (super-make-object message choices this* (const #f) '(vertical-label))

        (define chars "")
      )
    )

    (define/override (get-choice-from-ui*)
      (send chooser* get-selection)
    )

    (super-make-object title)

    (define choices* choices)
    (define num-choices* (length choices))
    (define chooser* (make-object keyboard-choice%))
  )
)

(define (get-choice-from-user title message choices)
  (cond
    [(pair? choices)
      (define dialog (make-object discrete-choice-dialog% title message choices))
      (send dialog show #t)
      ; ugly hack to solve some weird gui focus race condition. See blame for more details.
      (sleep .05)
      (send dialog get-choice)
    ]
    [else
      #f
    ]
  )
)

; new-blah-creator is a function of form
; (zinal:db:node%% , list-of zinal:db:referable%%) => (zinal:db:unassigned%% => zinal:db:element%%) OR #f
; parent-handle is the parent of the existing or yet-to-be-created node that will be assigned.
; parent-handle need not be the exact parent, as long as there is no zinal:db:class%% in between parent-handle and the creation location
; visible-referables are handles for all referables that are visible to any newly minted nodes.
; If it returns #f, it means no operation should be performed
; The returned creator is destructive and must succeed

(define (new-list-creator parent-handle visible-referables)
  (lambda (unassigned) (send unassigned assign-list!!))
)

(define (new-assert-creator parent-handle visible-referables)
  (lambda (unassigned)
    (define db-assert-handle (send unassigned assign-assert!!))
    (send (send db-assert-handle get-assertion) assign-bool!! #t)
    (send (send db-assert-handle get-format-string) assign-string!! "Something went wrong ...")
    db-assert-handle
  )
)

(define (use-text-from-user title message resultificator [validator (const #t)])
  (define result
    (get-text-from-user
      title
      message
      #f
      ""
      '(disallow-invalid)
      #:validate validator
    )
  )
  (and result (validator result) (resultificator result))
)

(define (new-number-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a number"
    "Seriously, do it"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-number!! (string->number result)))
    )
    string->number
  )
)

(define (new-character-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a character"
    "Seriously, do it"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-char!! (string-ref result 0)))
    )
    (compose1 (curry = 1) string-length)
  )
)

(define (new-string-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a string"
    "Seriously, do it"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-string!! result))
    )
  )
)

(define (new-symbol-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a symbol as a string"
    "Don't include the leading '"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-symbol!! (string->symbol result)))
    )
  )
)

(define (new-keyword-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a keyword as a string"
    "Don't include the leading #:"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-keyword!! (string->keyword result)))
    )
  )
)

(define (new-boolean-creator parent-handle visible-referables)
  (define choices '("true" "false"))
  (define result (get-choice-from-user "To be or not to be?" "That's the fuckin question" choices))
  (and result
    (lambda (unassigned) (send unassigned assign-bool!! (= result 0)))
  )
)

(define (new-define-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter the new definition's short descriptor"
    "A short descriptor, one or a few words, to identify this variable"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-def!! result))
    )
    non-empty-string?
  )
)

(define (new-lambda-creator parent-handle visible-referables)
  (lambda (unassigned) (send unassigned assign-lambda!!))
)

(define (get-referable-from-user allowed-referables)
  (and (pair? allowed-referables)
    (auto-complete*
      "Select a referable"
      "Start typing bits and pieces of the desired referable's short descriptor"
      (map (lambda (handle) (list handle (get-short-desc-or* handle "<no desc>"))) allowed-referables)
      handles-equal?
    )
  )
)

(define (new-value-read-creator parent-handle visible-referables)
  (define chosen-handle (get-referable-from-user visible-referables))
  (and chosen-handle
    (lambda (unassigned)
      (cond
        [(is-a? chosen-handle zinal:db:param%%) (send unassigned assign-param-ref!! chosen-handle)]
        [(is-a? chosen-handle zinal:db:def%%) (send unassigned assign-def-ref!! chosen-handle)]
        [(is-a? chosen-handle zinal:db:define-class%%) (send unassigned assign-class-ref!! chosen-handle)]
        [(is-a? chosen-handle zinal:db:interface%%) (send unassigned assign-interface-ref!! chosen-handle)]
        [else (error 'new-value-read-creator "Invalid referable")]
      )
    )
  )
)

(define (get-standard-legacy-from-user)
  (use-text-from-user
    "Enter the standard library identifier"
    "It must be from the standard racket library. Use the non-standard option if you want to use an identifier from a different library"
    identity
    ; TODO we need to add a reflective validator
    (conjoin non-empty-string? (negate (curryr member ILLEGAL-STANDARD-LEGACIES)))
  )
)

(define (get-non-standard-legacy-from-user)
  (define library
    (use-text-from-user
      "Enter the library name"
      "Enter exactly what you would enter as the argument to racket 'require - e.g. 'racket/hash' (without quotes)"
      identity
      ; TODO we need to add a reflective validator
      non-empty-string?
    )
  )
  (define name
    (and library
      (use-text-from-user
        "Enter the identifier name"
        "Does this really need explaining?"
        identity
        ; TODO we need to add a reflective validator
        non-empty-string?
      )
    )
  )
  (and name (list library name))
)

(define (new-standard-legacy-creator parent-handle visible-referables)
  (define result (get-standard-legacy-from-user))
  (and result
    (lambda (unassigned) (send unassigned assign-legacy-link!! #f result))
  )
)

(define (new-non-standard-legacy-creator parent-handle visible-referables)
  (define library&name (get-non-standard-legacy-from-user))
  (and library&name
    (lambda (unassigned) (send unassigned assign-legacy-link!! (first library&name) (second library&name)))
  )
)

(define new-unassigned-creator (const identity))

(define (get-method-from-user choosable-referables)
  (define visible-types (filter (curryr is-a? zinal:db:type%%) choosable-referables))
  (define type-with-desired-method
    (or
      (and (= 1 (length visible-types)) (car visible-types))
      (auto-complete*
        "Select the type which possesses the method"
        "Sorry, zinal isn't smart enough to figure out the appropriate type from context, so please enlighten"
        (map (lambda (type-handle) (list type-handle (get-short-desc-or* type-handle "<unnamed type>"))) visible-types)
        handles-equal?
      )
    )
  )
  (and type-with-desired-method
    (auto-complete*
      "Select a method"
      "Start typing bits and pieces of the desired method's short descriptor"
      (map (lambda (method-handle) (list method-handle (get-short-desc-or* method-handle "<unnamed method>"))) (send type-with-desired-method get-all-methods))
      handles-equal?
    )
  )
)

(define (get-super-method-from-user node-handle)
  (define super-class-handle (get-zinal-super-class* (get-containing-class* node-handle)))
  (cond
    [super-class-handle
      (auto-complete*
        "Select a super method"
        "Start typing bits and pieces of the desired method's short descriptor"
        (map
          (lambda (method-handle) (list method-handle (get-short-desc-or* method-handle "<unnamed method>")))
          (filter (lambda (m) (not (send super-class-handle is-method-abstract? m))) (send super-class-handle get-all-methods))
        )
        handles-equal?
      )
    ]
    [else
      (issue-warning "Can't invoke zinal super method" "This class's super class is a legacy, so it can't super invoke any zinal methods")
      #f
    ]
  )
)

(define (get-method-to-define/override-from-user node-handle)
  (define class-handle (get-containing-class* node-handle))
  (cond
    [class-handle
      (auto-complete*
        "Select a method to define or override"
        "Start typing bits and pieces of the desired method's short descriptor"
        (map
          (lambda (method-handle) (list method-handle (get-short-desc-or* method-handle "<unnamed method>")))
          (filter (lambda (m) (not (send class-handle get-direct-definition-of-method m))) (send class-handle get-all-methods))
        )
        handles-equal?
      )
    ]
    [else
      (issue-warning "Can't define/override method" "A method definition or override must be the immediate child of a class, nowhere else")
      #f
    ]
  )
)

(define (check-in-class? node-handle)
  (cond
    [(get-containing-class* node-handle)
      #t
    ]
    [else
      (issue-warning "Cannot create OOP node here" "The type of node you're trying to create can only exist inside a class body")
      #f
    ]
  )
)

(define (check-directly-in-class? parent-handle)
  (cond
    [(is-a? parent-handle zinal:db:class%%)
      #t
    ]
    [else
      (issue-warning "Cannot create OOP node here" "The type of node you're trying to create can only exist as the direct child of a class")
      #f
    ]
  )
)

(define (check-directly-in-define-class? parent-handle)
  (cond
    [(is-a? parent-handle zinal:db:define-class%%)
      #t
    ]
    [else
      (issue-warning
        "Cannot define new method here"
        "A new method can only be defined in a class definition. If you want to override a super class method, choose the 'define/override existing method' option"
      )
      #f
    ]
  )
)

(define (switch-r/z r/z-title r-title r-result-handler z-handler)
  (define zinal/racket '("zinal (non-legacy)" "racket (legacy)"))
  (define choice-index (get-choice-from-user r/z-title "The choice is yours, and yours alone" zinal/racket))
  (and choice-index
    (if (zero? choice-index)
      (z-handler)
      (use-text-from-user
        r-title
        "It's not hard ..."
        r-result-handler
        non-empty-string?
      )
    )
  )
)

(define (new-invoke-method-creator parent-handle visible-referables)
  (switch-r/z
    "Invoke a legacy method or a zinal method?"
    "Enter the name of the racket method to invoke"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-invoke-legacy-method!! result))
    )
    (thunk
      (define method (get-method-from-user visible-referables))
      (and method
        (lambda (unassigned) (send unassigned assign-invoke-method!! method))
      )
    )
  )
)

(define (new-invoke-this-method-creator parent-handle visible-referables)
  (and (check-in-class? parent-handle)
    (switch-r/z
      "Invoke a legacy method or a zinal method?"
      "Enter the name of the racket method to invoke"
      (lambda (result)
        (lambda (unassigned)
          (define result-handle (send unassigned assign-invoke-legacy-method!! result))
          (send (send result-handle get-object) assign-this!!)
          result-handle
        )
      )
      (thunk
        (define method
          (auto-complete*
            "Select a method"
            "Start typing bits and pieces of the desired method's short descriptor"
            (map (lambda (method-handle) (list method-handle (get-short-desc-or* method-handle "<unnamed method>"))) (send (get-containing-class* parent-handle) get-all-methods))
            handles-equal?
          )
        )
        (and method
          (lambda (unassigned)
            (define result-handle (send unassigned assign-invoke-method!! method))
            (send (send result-handle get-object) assign-this!!)
            result-handle
          )
        )
      )
    )
  )
)

(define (new-invoke-super-method-creator parent-handle visible-referables)
  (and (check-in-class? parent-handle)
    (switch-r/z
      "Invoke a legacy super method or a zinal super method?"
      "Enter the name of the racket method to invoke"
      (lambda (result)
        (lambda (unassigned) (send unassigned assign-invoke-legacy-super-method!! result))
      )
      (thunk
        (define method (get-super-method-from-user parent-handle))
        (and method
          (lambda (unassigned) (send unassigned assign-invoke-super-method!! method))
        )
      )
    )
  )
)

(define (new-define-new-method-creator parent-handle visible-referables)
  (and (check-directly-in-define-class? parent-handle)
    (use-text-from-user
      "Enter a short description of the new method"
      "A short descriptor, one or a few words, to identify the new method"
      (lambda (result)
        (lambda (unassigned)
          (define new-method-handle (send parent-handle add-direct-method!! result))
          (send unassigned assign-define-method!! new-method-handle)
        )
      )
      non-empty-string?
    )
  )
)

(define (new-define-existing-method-creator parent-handle visible-referables)
  (and (check-directly-in-class? parent-handle)
    (switch-r/z
      "Override a legacy super method, or define/override a zinal method?"
      "Enter the name of the racket method to override"
      (lambda (result)
        (lambda (unassigned) (send unassigned assign-override-legacy-method!! result))
      )
      (thunk
        (define method (get-method-to-define/override-from-user parent-handle))
        (and method
          (lambda (unassigned) (send unassigned assign-define-method!! method))
        )
      )
    )
  )
)

(define (new-super-init-creator parent-handle visible-referables)
  (and (check-directly-in-class? parent-handle) (not (findf (curryr is-a? zinal:db:super-init%%) (send parent-handle get-body)))
    (lambda (unassigned) (send unassigned assign-super-init!!))
  )
)

(define (new-create-object-creator parent-handle visible-referables)
  (lambda (unassigned) (send unassigned assign-create-object!!))
)

(define (new-this-creator parent-handle visible-referables)
  (and (check-in-class? parent-handle)
    (lambda (unassigned) (send unassigned assign-this!!))
  )
)

(define (new-define-class-creator parent-handle visible-referables)
  (use-text-from-user
    "Enter a name for the new class definition"
    "A short descriptor, one or a few words, to identify this class"
    (lambda (result)
      (lambda (unassigned) (send unassigned assign-define-class!! result))
    )
    non-empty-string?
  )
)

(define (new-class-instance-creator parent-handle visible-referables)
  (lambda (unassigned) (send unassigned assign-class-instance!!))
)

; GUI CONSTANTS

(define FRIENDLY-TYPE->CREATOR (hash
  "number" new-number-creator
  "character" new-character-creator
  "string" new-string-creator
  "boolean" new-boolean-creator
  "symbol" new-symbol-creator
  "keyword" new-keyword-creator

  "list" new-list-creator
  "assertion" new-assert-creator
  "define" new-define-creator
  "lambda" new-lambda-creator
  "reference" new-value-read-creator
  "legacy (standard library)" new-standard-legacy-creator
  "legacy (non-standard library)" new-non-standard-legacy-creator
  "TODO" new-unassigned-creator

  "invoke method" new-invoke-method-creator
  "invoke this (⊙) method" new-invoke-this-method-creator
  "invoke super (🡡) method" new-invoke-super-method-creator
  "create new object (☼)" new-create-object-creator
  "initialize super class (🡡☼)" new-super-init-creator
  "define new method" new-define-new-method-creator
  "define/override existing method" new-define-existing-method-creator
  "this (⊙)" new-this-creator
  "class (definition)" new-define-class-creator
  "class (anonymous instantiation)" new-class-instance-creator
))

; Returns #f to signify no action is to be taken (i.e. the user cancels the dialog)
; Returns a function that takes an unassigned db handle and will assign it to something else, returning the new handle
; The returned creator is not allowed to fail. A failure of this function should return #f instead of a creator
(define (request-new-item-creator parent-handle visible-referables [allowed-types (sort (hash-keys FRIENDLY-TYPE->CREATOR) string<?)])
  (define choice (get-choice-from-user "Choose the new node's type:" "Choose the node's type:" allowed-types))
  (if choice
    ((hash-ref FRIENDLY-TYPE->CREATOR (list-ref allowed-types choice)) parent-handle visible-referables)
    #f
  )
)

; ENT MANAGER

(define zinal:ent:manager% (class object%

  (init db)

  (super-make-object)

  (define db* db)
  (define ent-manager* this)
  (define selected* #f)

  ; TODO We want ui:scalar% to only refresh its text if the db has actually changed somehow, because the db calls required to update the text
  ; are expensive. There are several major changes that could be made to the framework that might facilitate this, like making db handles
  ; hashable, or giving every ent% a refresh! method that is invoked when that ent is first created and when it's reparsed. These are good
  ; options to consider for later, but for now they require large reworkings and rethinkings of things, and aren't worth the time required to
  ; do them properly. So for now it's sufficient just to keep a count of the parses that have happened so ui:var-scalar% can compare to the
  ; global count and refresh if necessary
  (define parse-count* 0)

  (define/public (get-initial-ui!)
    (unless (navigate-to-fresh-module*!)
      (get-initial-ui!)
    )
    (send selected* get-root)
  )

  (define/public (handle-event!! event)
    (assert "Something must always be selected" selected*)
    (define global-event-info (handle-global-event*!! event))
    (assert "global events currently cannot require reparse" (not (send global-event-info was-db-affected?)))
    (unless (send global-event-info was-handled?)
      (define event-info (send selected* handle-event!! event))
      (when (send event-info was-db-affected?)
        (set! parse-count* (add1 parse-count*))
        (maybe-reparse*! (send (send (send selected* get-root) get-parent-ent) get-slot) (reverse (get-backwards-selection-path* selected*)))
      )
    )
    (send selected* get-root)
  )

  (define (select! slot/item)
    (set! selected* (slot/ui-item->ui-item slot/item))
  )

  (define (get-backwards-selection-path* ui-item)
    (cond
      [ui-item
        (define slot (send (send ui-item get-parent-ent) get-slot))
        (define sub-chain (get-backwards-selection-path* (send ui-item get-parent)))
        (if (and (pair? sub-chain) (eq? (car sub-chain) slot))
          sub-chain
          (cons slot sub-chain)
        )
      ]
      [else
        '()
      ]
    )
  )

  (define (maybe-reparse*! slot selection-path)
    (define current-ent (send slot get-ent))
    (define db-handle (send current-ent get-cone-root))
    (define ui-parent (send (send current-ent get-root-ui-item) get-parent))
    (define cone-leaves (send current-ent get-cone-leaves))
    (define was-this-slot-respawned? #f)
    (unless (implies ui-parent (is-a? current-ent (parse-entity*! db-handle)))
      (spawn-entity*! slot db-handle ui-parent (curryr spawn-or-reassign-entity*! cone-leaves))
      ; Imperative style, but the functional alternatives are just so damn ugly
      (set! was-this-slot-respawned? #t)
    )
    ; TODO This is hacky, but that's ok, cuz it gives us an idea of how we might have a more general purpose reactive/notification/spooky-action-at-a-distance system in the future
    (when (and (not was-this-slot-respawned?) (is-a? current-ent ent:define-class%))
      (send current-ent reset-abstracts)
    )
    (define is-current-slot-part-of-selection-path? (and (pair? selection-path) (eq? (car selection-path) slot)))
    (define next-selection-path (and is-current-slot-part-of-selection-path? (cdr selection-path)))
    (define is-some-child-part-of-selection-path?
      (pair? (filter-map (curryr maybe-reparse*! next-selection-path) (send (send slot get-ent) get-cone-leaves)))
    )
    (when (and was-this-slot-respawned? is-current-slot-part-of-selection-path? (not is-some-child-part-of-selection-path?))
      (select! slot)
    )
    is-current-slot-part-of-selection-path?
  )

  (define (change-module*! handle-event-info event)
    (define module-to-go-to (get-module-from-user))
    (when module-to-go-to (spawn-root-entity*! module-to-go-to))
    (send handle-event-info set-db-wasnt-affected!)
  )

  (define (create-new-module*!! handle-event-info event)
    (define module-to-go-to (create-module*!!))
    (when module-to-go-to (spawn-root-entity*! module-to-go-to))
    (send handle-event-info set-db-wasnt-affected!)
  )

  (define (change-interface*! handle-event-info event)
    (define interface-to-go-to (get-interface-from-user (send db* get-all-interfaces)))
    (when interface-to-go-to (spawn-root-entity*! interface-to-go-to))
    (send handle-event-info set-db-wasnt-affected!)
  )

  (define (create-new-interface*!! handle-event-info event)
    (define interface-to-go-to (create-interface*!!))
    (when interface-to-go-to (spawn-root-entity*! interface-to-go-to))
    (send handle-event-info set-db-wasnt-affected!)
  )

  (define global-event-handler*
    (make-object keyname-event-handler% (list
      (list change-module*! '("e"))
      (list create-new-module*!! '("E"))
      (list change-interface*! '("c:e"))
      (list create-new-interface*!! '("c:E"))
    ))
  )

  (define (handle-global-event*!! event)
    (send global-event-handler* handle-event!! event)
  )

  ; ENTS

  ; Some style and text-getter consts that are used by some ents

  (define CONST-VALUE-STYLE (send (make-object style-delta%) set-delta-foreground "Pink"))

  (define REF-STYLE (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Cyan"))

  (define (get-ref-text ref-handle)
    (get-short-desc-or* (send ref-handle get-referable) "<nameless ref>")
  )

  (define ASSERT-STYLE (send (make-object style-delta%) set-delta-foreground "Lime"))

  (define ATOM-STYLE (send (make-object style-delta%) set-delta-foreground "Orchid"))

  (define (get-atom-text atom-handle)
    (define raw-value (send atom-handle get-val))
    (cond
      [(is-a? atom-handle zinal:db:symbol%%)
        (format "'~a" raw-value)
      ]
      [(is-a? atom-handle zinal:db:char%%)
        (~s raw-value)
      ]
      [else
        (~a raw-value)
      ]
    )
  )

  (define STRING-STYLE (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Orchid"))

  (define DEF-STYLE (send (make-object style-delta%) set-delta-foreground "Yellow"))

  (define LEGACY-STYLE (send (make-object style-delta%) set-delta-foreground "Cyan"))

  (define UNASSIGNED-STYLE (send (make-object style-delta% 'change-bold) set-delta-foreground "Chocolate"))

  (define (get-unassigned-text unassigned-handle)
    (get-short-desc-or* unassigned-handle "<?>")
  )

  (define ent% (class* object% (fallback-event-handler%%) ; abstract

    ; Every ent must have an init of the form (init cone-root-handle child-spawner!). This
    ; base class doesn't actually need the child-spawner! , so its init only takes the root handle.
    ; But all subclasses should accept the child-spawner! and use it for spawning all initial child
    ; ents.
    (init cone-root-handle)

    (abstract get-root-ui-item)

    (define cone-root* cone-root-handle)
    (define slot* #f)

    (define/public (get-cone-root)
      cone-root*
    )

    (define/public (handle-child-event!! event)
      (assert "You must call assign-to-slot! before sending any events" slot*)
      (send slot* handle-child-event!! event)
    )

    ; Returns a list of slots corresponding to the leaves of this cone and the roots of the child cones
    (define/public (get-cone-leaves)
      (define (get-cone-leaves* ui-item)
        (if (is-a? ui-item zinal:ui:list%%)
          (append* (map get-cone-leaves* (send ui-item get-children-with-header-internal)))
          (if (is-a? ui-item slot%) (list ui-item) '())
        )
      )
      (get-cone-leaves* (get-root-ui-item))
    )

    ; This must be called before the ent can handle events
    (define/public (assign-to-slot! slot ui-parent)
      (send slot set-ent! this)
      (set! slot* slot)
      (send (get-root-ui-item) set-parent! ui-parent)
    )

    (define/public (get-slot)
      slot*
    )

    (super-make-object)
  ))

  (define ent:short-definition% (class ent% ; abstract

    (init cone-root-handle child-spawner!)

    (define/public (get-prefix-text)
      #f
    )

    (define/public (get-synopsis-text)
      "..."
    )

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define this-ent* this)

    (define ui-item* (make-object (class ui:possibly-public-def-list%

      (define/override (get-prefix-text)
        (send this-ent* get-prefix-text)
      )

      (define/override (get-default-name-text)
        "<nameless definition>"
      )

      (define/override (get-bridge-text)
        (format "= ~a" (get-synopsis-text))
      )

      (define/override (get-event-handler)
        (combine-keyname-event-handlers (list
          (super get-event-handler)
          (create-navigate-to-root-handler* (send this-ent* get-cone-root))
        ))
      )

      (super-make-object this-ent* this-ent*)
    )))

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  (define ent:singleton% (class ent% ; abstract

    (init cone-root-handle child-spawner! [bookends #f])

    (abstract db-get-single-item)
    (abstract get-header)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define this-ent* this)

    (define ui-item* (make-object (class ui:slotted-list%

      (define/override (get-visible-referables-for-slot slot)
        (send (send this-ent* get-cone-root) get-visible-referables-underneath)
      )

      (super-make-object this-ent* this-ent* (get-header) bookends)

      (define item-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
      (child-spawner! item-slot* (db-get-single-item) this)
      (send this insert! 0 item-slot*)
    )))

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  (define ent:typical-list% (class ent%

    (init cone-root-handle child-spawner!)

    (define/public (db-insert!! index)
      (send (db-get-list-handle) insert!! index)
    )

    (define/public (db-can-remove? index)
      (is-a? (list-ref (send (db-get-list-handle) get-items) index) zinal:db:unassigned%%)
    )

    (define/public (db-remove!! index)
      (send (db-get-list-handle) remove!! index)
    )

    (define/public (db-get-items)
      (send (db-get-list-handle) get-items)
    )

    (define/public (db-get-list-handle)
      (send this get-cone-root)
    )

    (define/public (get-pseudo-headers)
      '()
    )

    (define/public (get-header)
      #f
    )

    (define/public (get-separator)
      #f
    )

    (define/public (get-bookends)
      (list
        (make-object ui:const% this NO-STYLE "(")
        (make-object ui:const% this NO-STYLE ")")
      )
    )

    (define/public (horizontal-by-default?)
      #f
    )

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define this-ent* this)
    (define ui-list* (make-object (class ui:dynamic-slotted-list%

      (define/override (db-insert!! index)
        (send this-ent* db-insert!! index)
      )

      (define/override (db-can-remove? index)
        (send this-ent* db-can-remove? index)
      )

      (define/override (db-remove!! index)
        (send this-ent* db-remove!! index)
      )

      (define/override (db-get-items)
        (send this-ent* db-get-items)
      )

      (define/override (db-get-list-handle)
        (send this-ent* db-get-list-handle)
      )

      (define/override (get-pseudo-headers)
        (send this-ent* get-pseudo-headers)
      )

      (super-make-object this-ent* this-ent* child-spawner! (get-header) (get-separator) (get-bookends))
    )))

    (when (horizontal-by-default?) (send ui-list* set-horizontal! #t))

    (define/override (get-root-ui-item)
      ui-list*
    )
  ))

  (define ent:typical-has-body% (class ent:typical-list% ; abstract

    (init cone-root-handle child-spawner!)

    (define/public (get-has-body-handle)
      (send this get-cone-root)
    )

    (define/override (db-insert!! index)
      (send (get-has-body-handle) insert-into-body!! index)
    )

    (define/override (db-can-remove? index)
      (is-a? (list-ref (db-get-items) index) zinal:db:unassigned%%)
    )

    (define/override (db-remove!! index)
      (send (get-has-body-handle) remove-from-body!! index)
    )

    (define/override (db-get-items)
      (send (get-has-body-handle) get-body)
    )

    (define/override (db-get-list-handle)
      (get-has-body-handle)
    )

    (define/override (get-separator)
      (make-object ui:const% this NO-STYLE "; ")
    )

    (define/override (get-bookends)
      (list
        (make-object ui:const% this NO-STYLE "{")
        (make-object ui:const% this NO-STYLE "}")
      )
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:typical-has-args% (class ent:typical-list% ; abstract

    (init cone-root-handle child-spawner!)

    (define/public (get-has-args-handle)
      (send this get-cone-root)
    )

    (define/override (db-insert!! index)
      (send (get-has-args-handle) insert-arg!! index)
    )

    (define/override (db-can-remove? index)
      (is-a? (list-ref (db-get-items) index) zinal:db:unassigned%%)
    )

    (define/override (db-remove!! index)
      (send (get-has-args-handle) remove-arg!! index)
    )

    (define/override (db-get-items)
      (send (get-has-args-handle) get-args)
    )

    (define/override (db-get-list-handle)
      (get-has-args-handle)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:module% (class ent:typical-has-body%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    (define event-handler*
      (combine-keyname-event-handlers (list
        (create-name-change-handler (thunk (send this get-cone-root)))
        (create-simple-event-handler "m"
          (lambda (handle-event-info event)
            (define module-handle (send this get-cone-root))
            (cond
              [(send module-handle is-main-module?)
                (send module-handle set-main-module!! #f)
              ]
              [(send db* get-main-module)
                (issue-warning "Cannot make main" "Only one module can be the main module")
              ]
              [else
                (send module-handle set-main-module!! #t)
              ]
            )
            #t
          )
        )
        (create-simple-event-handler "d"
          (lambda (handle-event-info event)
            (define module-handle (send this get-cone-root))
            (if (send module-handle can-delete?)
              (when (navigate-to-fresh-module*! (get-all-modules* module-handle)) (send module-handle delete!!))
              (issue-warning "Cannot delete module" "Either other modules require this module or contain references to defs in this module")
            )
            #t
          )
        )
      ))
    )

    (define (get-module-text*)
      (define prefix
        (if (send (send this get-cone-root) is-main-module?)
          "Main module"
          "Module"
        )
      )
      (format "~a: ~a requires:" prefix (get-short-desc-or* (send this get-cone-root) "<nameless module>"))
    )

    (define/override (get-header)
      (make-object (class ui:set-list%

        (define/override (db-get-items)
          (send (send this-ent* get-cone-root) get-required-modules)
        )

        (define/override (db-add-item!!)
          (define this-module (send this-ent* get-cone-root))
          (define module-to-require (get-module-from-user (filter (lambda (m) (send this-module can-require? m)) (get-all-modules*))))
          (cond
            [module-to-require
              (send this-module require!! module-to-require)
              module-to-require
            ]
            [else
              #f
            ]
          )
        )

        (define/override (db-can-remove-item? to-remove)
          #t
        )

        (define/override (db-remove-item!! to-remove)
          (send (send this-ent* get-cone-root) unrequire!! to-remove)
        )

        (define/override (get-item-ui-style)
          REF-STYLE
        )

        (define header-header*
          (make-object ui:var-scalar% this-ent* (send (make-object style-delta% 'change-bold) set-delta-foreground "Lime") get-module-text* (const event-handler*) NOOP-FALLBACK-EVENT-HANDLER)
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER header-header*)
      ))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:invokation% (class ent:typical-list% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract get-header-style)

    (define/override (db-insert!! index)
      (super db-insert!! (add1 index))
    )

    (define/override (db-can-remove? index)
      (super db-can-remove? (add1 index))
    )

    (define/override (db-remove!! index)
      (super db-remove!! (add1 index))
    )

    (define/override (db-get-items)
      (cdr (super db-get-items))
    )

    (define/override (get-header)
      (make-object ui:var-scalar% this (get-header-style) get-header-text* header->event-handler* NOOP-FALLBACK-EVENT-HANDLER)
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (define (get-header-text*)
      (define func (get-func-handle*))
      (cond
        [(is-a? func zinal:db:legacy-link%%)
          (legacy-handle->string func)
        ]
        [(is-a? func zinal:db:reference%%)
          (get-short-desc-or* (send func get-referable) "<nameless ref>")
        ]
        [else
          (error 'get-header-text* "invalid type")
        ]
      )
    )

    (define (header->event-handler* header)
      (define (interaction-function)
        (define list-handle (send this db-get-list-handle))
        (request-new-item-creator list-handle (send list-handle get-visible-referables-underneath) '("legacy (standard library)" "legacy (non-standard library)" "reference"))
      )
      (define (result-handler new-handle-initializer!!)
        (new-handle-initializer!! (send (get-func-handle*) unassign!!))
      )
      (create-interaction-dependent-event-handler interaction-function result-handler "s")
    )

    (define (get-func-handle*)
      (car (super db-get-items))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:legacy-invokation% (class ent:invokation%

    (init cone-root-handle child-spawner!)

    (define/override (get-header-style)
      LEGACY-STYLE
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:reference-invokation% (class ent:invokation%

    (init cone-root-handle child-spawner!)

    (define/override (get-header-style)
      REF-STYLE
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:list-list% (class ent:typical-list%

    (init cone-root-handle child-spawner!)

    (define/override (db-insert!! index)
      (super db-insert!! (add1 index))
    )

    (define/override (db-can-remove? index)
      (super db-can-remove? (add1 index))
    )

    (define/override (db-remove!! index)
      (super db-remove!! (add1 index))
    )

    (define/override (db-get-items)
      (cdr (super db-get-items))
    )

    (define/override (get-bookends)
      (list
        (make-object ui:const% this (make-object style-delta% 'change-bold) "[")
        (make-object ui:const% this (make-object style-delta% 'change-bold) "]")
      )
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:quoted-list% (class ent:typical-list%

    (init cone-root-handle child-spawner!)

    (define/override (db-get-list-handle)
      (second (send (send this get-cone-root) get-items))
    )

    (define/override (get-bookends)
      (list
        (make-object ui:const% this (make-object style-delta% 'change-bold) "'(")
        (make-object ui:const% this (make-object style-delta% 'change-bold) ")")
      )
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:lambda-like% (class ent:typical-has-body% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract get-params-header)
    (abstract get-lambda-handle)

    (define/override (get-has-body-handle)
      (get-lambda-handle)
    )

    (define child-spawner*! child-spawner!)

    (define/override (get-header)
      (make-object ui:params-list% this this child-spawner*! (get-lambda-handle) (get-params-header))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:lambda% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define/override (get-params-header)
      (make-object ui:const% this NO-STYLE "λ:")
    )

    (define/override (get-lambda-handle)
      (send this get-cone-root)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:func-def% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    (define/override (get-params-header)
      (make-object (class ui:possibly-public-def-list%

        (define/override (get-default-name-text)
          "<nameless def>"
        )

        (define/override (get-bridge-text)
          "= λ:"
        )

        (super-make-object this-ent*)
      ))
    )

    (define/override (get-lambda-handle)
      (send (send this get-cone-root) get-expr)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:def% (class ent:singleton%

    (init cone-root-handle child-spawner!)

    (define/override (db-get-single-item)
      (send (send this get-cone-root) get-expr)
    )

    (define this-ent* this)

    (define/override (get-header)
      (make-object (class ui:possibly-public-def-list%

        (define/override (get-default-name-text)
          "<nameless def>"
        )

        (super-make-object this-ent*)
      ))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-def% (class ent:short-definition%

    (init cone-root-handle child-spawner!)

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-func-def% (class ent:short-definition%

    (init cone-root-handle child-spawner!)

    (define/override (get-synopsis-text)
      (has-params->short-params-string (send (send this get-cone-root) get-expr))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-method-definition% (class ent% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract get-prefix-string)
    (abstract get-method-name)
    (abstract get-name-change-handler)

    (define this-ent* this)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define ui-item* (make-object (class ui:list%

      (define (get-params-text*)
        (format "= ~a" (has-params->short-params-string (send (send this-ent* get-cone-root) get-lambda)))
      )

      (define/override (get-event-handler)
        (combine-keyname-event-handlers (list
          (super get-event-handler)
          (create-navigate-to-root-handler* (send this-ent* get-cone-root))
        ))
      )

      (super-make-object this-ent* this-ent*)

      (send this insert! 0 (make-object ui:const% this-ent* NO-STYLE (get-prefix-string)))
      (send this insert! 1 (make-object ui:var-scalar% this-ent* DEF-STYLE (thunk (get-method-name)) (thunk* (get-name-change-handler)) NOOP-FALLBACK-EVENT-HANDLER))
      ; TODO - this and other places will need to change to var-scalar if we ever get to a point of saving ui trees when switching roots
      (send this insert! 2 (make-object ui:const% this-ent* NO-STYLE (get-params-text*)))
      (send this set-horizontal! #t)
    )))

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  (define ent:short-define-method% (class ent:short-method-definition%

    (init cone-root-handle child-spawner!)

    (define/override (get-prefix-string)
      "method"
    )

    (define/override (get-method-name)
      (get-short-desc-or* (send (send this get-cone-root) get-method) "<nameless method>")
    )

    (define/override (get-name-change-handler)
      (create-name-change-handler (thunk (send (send this get-cone-root) get-method)))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-override-legacy-method% (class ent:short-method-definition%

    (init cone-root-handle child-spawner!)

    (define/override (get-prefix-string)
      "override legacy method"
    )

    (define/override (get-method-name)
      (send (send this get-cone-root) get-legacy-method-name)
    )

    (define/override (get-name-change-handler)
      (create-legacy-method-name-change-handler (thunk (send this get-cone-root)))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-augment-legacy-method% (class ent:short-override-legacy-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-prefix-string)
      "augment legacy method"
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:short-class-instance% (class ent%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define ui-item* (make-object (class ui:list%

      (define (get-super-text*)
        (define super-class-handle (send (send this-ent* get-cone-root) get-super-class))
        (if (is-a? super-class-handle zinal:db:legacy-link%%)
          (legacy-handle->string super-class-handle)
          (get-short-desc-or* (send super-class-handle get-referable) "<some class>")
        )
      )

      (define/override (get-event-handler)
        (combine-keyname-event-handlers (list
          (super get-event-handler)
          (create-navigate-to-root-handler* (send this-ent* get-cone-root))
        ))
      )

      (super-make-object this-ent* this-ent*)

      (send this insert! 0 (make-object ui:const% this-ent* NO-STYLE "new anonymous"))
      (send this insert! 1 (make-object ui:var-scalar% this-ent* REF-STYLE get-super-text* THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))
      (send this set-horizontal! #t)
    )))

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  (define ent:short-define-class% (class ent:short-definition%

    (init cone-root-handle child-spawner!)

    (define/override (get-prefix-text)
      "class"
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:assert% (class ent%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define header* (make-object (class ui:slotted-list%

      (define/override (get-visible-referables-for-slot slot)
        (send (send this-ent* get-cone-root) get-visible-referables-underneath)
      )

      (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

      (define assertion-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
      (child-spawner! assertion-slot* (send (send this-ent* get-cone-root) get-assertion) this)
      (define format-string-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
      (child-spawner! format-string-slot* (send (send this-ent* get-cone-root) get-format-string) this)

      (send this insert! 0 (make-object ui:const% this-ent* ASSERT-STYLE "assert"))
      (send this insert! 1 assertion-slot*)
      (send this insert! 2 (make-object ui:const% this-ent* ASSERT-STYLE ":"))
      (send this insert! 3 format-string-slot*)
    )))

    (define ui-assert* (make-object (class ui:dynamic-slotted-list%

      (define/override (db-insert!! index)
        (send (db-get-list-handle) insert-format-arg!! index)
      )

      (define/override (db-can-remove? index)
        (is-a? (list-ref (db-get-items) index) zinal:db:unassigned%%)
      )

      (define/override (db-remove!! index)
        (send (db-get-list-handle) remove-format-arg!! index)
      )

      (define/override (db-get-items)
        (send (db-get-list-handle) get-format-args)
      )

      (define/override (db-get-list-handle)
        (send this-ent* get-cone-root)
      )

      (super-make-object this-ent* this-ent* child-spawner! header*)
    )))

    (send ui-assert* set-horizontal! #t)

    (define/override (get-root-ui-item)
      ui-assert*
    )
  ))

  (define ent:atom% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-scalar*
      (make-object ui:var-scalar% this ATOM-STYLE (thunk (get-atom-text (send this get-cone-root))) THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:string% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-scalar*
      (make-object ui:var-scalar% this STRING-STYLE (thunk (send (send this get-cone-root) get-val)) THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:empty-string% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-scalar*
      (make-object ui:var-scalar% this CONST-VALUE-STYLE (const "ε") THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:ref% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-scalar*
      (make-object ui:var-scalar% this REF-STYLE (thunk (get-ref-text (send this get-cone-root))) THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:optional-param% (class ent:singleton%

    (init cone-root-handle child-spawner!)

    (define/override (db-get-single-item)
      (define default (send (send this get-cone-root) get-default))
      (assert "optional param has no default" default)
      default
    )

    (define this-ent* this)

    (define/override (get-header)
      (make-object (class ui:def-list%

        (define/override (get-default-name-text)
          "<nameless param>"
        )

        (super-make-object this-ent*)
      ))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:required-param% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (get-short-desc-or* (send this get-cone-root) "<nameless param>")
    )

    (define (name-ui->event-handler* name-ui)
      (create-name-change-handler (thunk (send this get-cone-root)))
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this DEF-STYLE get-text name-ui->event-handler* this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:legacy% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (legacy-handle->string (send this get-cone-root))
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this LEGACY-STYLE get-text THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:unassigned% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-scalar*
      (make-object ui:var-scalar% this UNASSIGNED-STYLE (thunk (get-unassigned-text (send this get-cone-root))) THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:class% (class ent:typical-has-body% ; abstract

    (init cone-root-handle child-spawner!)

    ; a list of ui:item% that should be prepended to the header. Other than this list, the header starts with the superclass slot
    (abstract get-header-prefix-list)

    (define this-ent* this)
    (define child-spawner*! child-spawner!)

    (define/override (get-header)
      (define header-header (make-object (class ui:list%

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (define super-class-slot* #f)

        (define (super-class-slot->event-handler slot)
          (define class-handle (send this-ent* get-cone-root))
          (define (interaction-function)
            (cond
              [(send class-handle can-set-super-class?)
                (define choice-index
                  (get-choice-from-user
                    "Is the super class zinal or racket?"
                    "Choose whether to refer to a zinal class, standard lib legacy class, or non-standard lib legacy class"
                    '("zinal class" "racket class (standard library)" "racket class (non-standard library)")
                  )
                )
                (case choice-index
                  [(0)
                    (get-referable-from-user
                      (filter
                        (conjoin (negate (curry handles-equal? class-handle)) (curryr is-a? zinal:db:define-class%%))
                        (send class-handle get-visible-referables-underneath)
                      )
                    )
                  ]
                  [(1)
                    (get-standard-legacy-from-user)
                  ]
                  [(2)
                    (get-non-standard-legacy-from-user)
                  ]
                  [else
                    #f
                  ]
                )
              ]
              [else
                (issue-warning "Can't change super class" "Changing the super class would orphan a method definition or super invokation, you hapless fool!")
                #f
              ]
            )
          )
          (define (result-handler result)
            (define new-handle
              (cond
                [(is-a? result zinal:db:define-class%%)
                  (send class-handle set-super-class!! result)
                ]
                [(string? result)
                  (send class-handle set-legacy-super-class!! #f result)
                ]
                [(and (pair? result) (= 2 (length result)) (andmap string? result))
                  (send class-handle set-legacy-super-class!! (first result) (second result))
                ]
                [else
                  (error 'super-class-slot->event-handler "Bad super-class choice result:" result)
                ]
              )
            )
            (spawn-entity*! super-class-slot* new-handle this)
            (select! super-class-slot*)
          )
          (create-interaction-dependent-event-handler interaction-function result-handler "s")
        )

        (define header-prefix-list* (get-header-prefix-list))
        (define prefix-size* (length header-prefix-list*))
        (map-by-index
          (lambda (index item) (send this insert! index item))
          header-prefix-list*
        )
        (set! super-class-slot* (make-object slot% super-class-slot->event-handler NOOP-FALLBACK-EVENT-HANDLER))
        (child-spawner*! super-class-slot* (send (send this-ent* get-cone-root) get-super-class) this)
        (send this insert! prefix-size* super-class-slot*)
        (send this insert! (add1 prefix-size*) (make-object ui:const% this-ent* NO-STYLE "implementing:"))
      )))

      (make-object ui:interface-set-list% this-ent* NOOP-FALLBACK-EVENT-HANDLER header-header)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:class-instance% (class ent:class%

    (init cone-root-handle child-spawner!)

    (define/override (get-header-prefix-list)
      (list
        (make-object ui:const% this NO-STYLE "create anonymous instance of")
      )
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:define-class% (class ent:class%

    (init cone-root-handle child-spawner!)

    (define child-spawner*! child-spawner!)
    (define this-ent* this)
    (define params-list* #f)
    (define abstracts* #f)

    (define (name-ui->event-handler* name-ui)
      (create-name-change-handler (thunk (send this-ent* get-cone-root)))
    )

    (define (get-name-text*)
      (get-short-desc-or* (send this-ent* get-cone-root) "<unnamed class>")
    )

    (define/override (get-header-prefix-list)
      (list
        (make-object ui:const% this-ent* NO-STYLE "class")
        (make-object ui:var-scalar% this-ent* DEF-STYLE get-name-text* name-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER)
        (make-object ui:const% this-ent* NO-STYLE "subclass of")
      )
    )

    (define/public (reset-abstracts)
      (when abstracts* (send abstracts* reset!))
    )

    (define (get-abstracts*)
      (make-object (class ui:set-list%

        (define/override (db-get-items)
          (define class-handle (send this-ent* get-cone-root))
          (filter (lambda (m) (send class-handle is-method-abstract? m)) (send class-handle get-direct-methods))
        )

        (define/override (db-add-item!!)
          (use-text-from-user
            "Enter short descriptor for the new method"
            "A short descriptor, one or a few words, to identify this method"
            (lambda (result)
              (send (send this-ent* get-cone-root) add-direct-method!! result)
            )
            non-empty-string?
          )
        )

        (define/override (db-can-remove-item? to-remove)
          (send (send this-ent* get-cone-root) can-remove-direct-method? to-remove)
        )

        (define/override (db-remove-item!! to-remove)
          (send (send this-ent* get-cone-root) remove-direct-method!! to-remove)
        )

        (define/override (get-item-ui-style)
          DEF-STYLE
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER (make-object ui:const% this NO-STYLE "abstract methods:"))
      ))
    )

    (define/override (get-pseudo-headers)
      (unless params-list*
        (set! params-list*
          (make-object ui:params-list% this NOOP-FALLBACK-EVENT-HANDLER child-spawner*! (send this get-cone-root) (make-object ui:const% this NO-STYLE "init params:"))
        )
        (set! abstracts*
          (get-abstracts*)
        )
      )
      (list
        params-list*
        abstracts*
      )
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:define-method% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    (define/override (get-params-header)
      (make-object (class ui:list%

        (define (name-ui->event-handler* name-ui)
          (create-name-change-handler get-method*)
        )

        (define (get-name-text*)
          (get-short-desc-or* (get-method*) "<unnamed method>")
        )

        (define (get-method*)
          (send (send this-ent* get-cone-root) get-method)
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (send this insert! 0 (make-object ui:const% this-ent* NO-STYLE "method"))
        (send this insert! 1 (make-object ui:var-scalar% this-ent* DEF-STYLE get-name-text* name-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
        (send this insert! 2 (make-object ui:const% this-ent* NO-STYLE "= λ:"))
        (send this set-horizontal! #t)
      ))
    )

    (define/override (get-lambda-handle)
      (send (send this get-cone-root) get-lambda)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:override-legacy-method% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    (define/override (get-params-header)
      (make-object (class ui:list%

        (define (name-ui->event-handler* name-ui)
          (create-legacy-method-name-change-handler (thunk (send this-ent* get-cone-root)))
        )

        (define (get-name-text*)
          (send (send this-ent* get-cone-root) get-legacy-method-name)
        )

        (define (get-prefix-text*)
          (define prefix (if (send (send this-ent* get-cone-root) is-augment?) "augment" "override"))
          (format "~a legacy" prefix)
        )

        (define (prefix-ui->event-handler* prefix-ui)
          (create-simple-event-handler "a"
            (lambda (handle-event-info event)
              (define override-handle (send this-ent* get-cone-root))
              (send override-handle set-is-augment!! (not (send override-handle is-augment?)))
              #t
            )
          )
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (send this insert! 0 (make-object ui:var-scalar% this-ent* NO-STYLE get-prefix-text* prefix-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
        (send this insert! 1 (make-object ui:var-scalar% this-ent* DEF-STYLE get-name-text* name-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
        (send this insert! 2 (make-object ui:const% this-ent* NO-STYLE "= λ:"))
        (send this set-horizontal! #t)
      ))
    )

    (define/override (get-lambda-handle)
      (send (send this get-cone-root) get-lambda)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:invoke-method% (class ent:typical-has-args% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract get-method-name)
    (abstract get-method-style)
    (abstract get-new-method-from-user)
    (abstract set-method!!)

    (define/public (get-alternative-object-ui)
      #f
    )

    (define this-ent* this)
    (define child-spawner*! child-spawner!)

    (define/override (get-header)
      (make-object (class ui:slotted-list%

        (define/override (get-visible-referables-for-slot slot)
          (send (send this-ent* get-cone-root) get-visible-referables-after)
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (define (method-ui->event-handler* method-ui)
          (define (interaction-function)
            (get-new-method-from-user)
          )
          (define (result-handler result-from-user)
            (set-method!! result-from-user)
          )
          (create-interaction-dependent-event-handler interaction-function result-handler "s")
        )

        (define method-ui* (make-object ui:var-scalar% this-ent* (get-method-style) (thunk (get-method-name)) method-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
        (define alternative-object-ui* (get-alternative-object-ui))
        (cond
          [alternative-object-ui*
            (send this insert! 0 alternative-object-ui*)
            (send this insert! 1 method-ui*)
          ]
          [else
            (define object-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
            (child-spawner*! object-slot* (send (send this-ent* get-cone-root) get-object) this)
            (send this insert! 0 object-slot*)
            (send this insert! 1 (make-object ui:const% this-ent* NO-STYLE "⇒"))
            (send this insert! 2 method-ui*)
          ]
        )
        (send this set-horizontal! #t)
      ))
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:invoke-zinal-method% (class ent:invoke-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-method-name)
      (get-short-desc-or* (send (send this get-cone-root) get-method) "<unnamed method>")
    )

    (define/override (get-method-style)
      REF-STYLE
    )

    (define/override (get-new-method-from-user)
      (get-method-from-user (send (send this get-cone-root) get-visible-referables-after))
    )

    (define/override (set-method!! method-handle)
      (send (send this get-cone-root) set-method!! method-handle)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:invoke-legacy-method% (class ent:invoke-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-method-name)
      (send (send this get-cone-root) get-legacy-method-name)
    )

    (define/override (get-method-style)
      LEGACY-STYLE
    )

    (define/override (get-new-method-from-user)
      (use-text-from-user
        "Enter the name of the racket method"
        "It's not hard ..."
        identity
        non-empty-string?
      )
    )

    (define/override (set-method!! name)
      (send (send this get-cone-root) set-legacy-method-name!! name)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:this-invoke-zinal-method% (class ent:invoke-zinal-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-alternative-object-ui)
      (make-object ui:const% this CONST-VALUE-STYLE "⊙")
    )

    (define/override (get-new-method-from-user)
      (define class-handle (get-containing-class* (send this get-cone-root)))
      (auto-complete*
        "Select a method"
        "Start typing bits and pieces of the desired method's short descriptor"
        (map (lambda (method-handle) (list method-handle (get-short-desc-or* method-handle "<unnamed method>"))) (send class-handle get-all-methods))
        handles-equal?
      )
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:this-invoke-legacy-method% (class ent:invoke-legacy-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-alternative-object-ui)
      (make-object ui:const% this CONST-VALUE-STYLE "⊙")
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:super-invoke-zinal-method% (class ent:invoke-zinal-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-alternative-object-ui)
      (make-object ui:const% this CONST-VALUE-STYLE "🡡")
    )

    (define/override (get-new-method-from-user)
      (get-super-method-from-user (send this get-cone-root))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:super-invoke-legacy-method% (class ent:invoke-legacy-method%

    (init cone-root-handle child-spawner!)

    (define/override (get-alternative-object-ui)
      (make-object ui:const% this CONST-VALUE-STYLE "🡡")
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:this% (class ent%

    (init cone-root-handle child-spawner!)

    (define ui-item*
      (make-object ui:var-scalar% this CONST-VALUE-STYLE (const "⊙") THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-item*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:create-object% (class ent:typical-has-args%

    (init cone-root-handle child-spawner!)

    (define/override (horizontal-by-default?)
      #t
    )

    (define this-ent* this)
    (define child-spawner*! child-spawner!)

    (define/override (get-header)
      (make-object (class ui:slotted-list%

        (define/override (get-visible-referables-for-slot slot)
          (send (send this-ent* get-cone-root) get-visible-referables-after)
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (send this insert! 0 (make-object ui:const% this-ent* (send (make-object style-delta%) set-delta-foreground "Lime") "☼"))
        (define class-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
        (child-spawner*! class-slot* (send (send this-ent* get-cone-root) get-class-node) this)
        (send this insert! 1 class-slot*)
        (send this set-horizontal! #t)
      ))
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:super-init% (class ent:typical-has-args%

    (init cone-root-handle child-spawner!)

    (define/override (horizontal-by-default?)
      #t
    )

    (define this-ent* this)

    (define/override (get-header)
      (make-object ui:const% this-ent* (send (make-object style-delta%) set-delta-foreground "Lime") "🡡☼")
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:interface% (class ent%

    (init cone-root-handle child-spawner!)

    (define this-ent* this)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define header-header*
      (make-object (class ui:list%

        (define (name-ui->event-handler* name-ui)
          (create-name-change-handler (thunk (send this-ent* get-cone-root)))
        )

        (define (get-name-text*)
          (get-short-desc-or* (send this-ent* get-cone-root) "<unnamed interface>")
        )

        (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER)

        (send this insert! 0 (make-object ui:const% this-ent* NO-STYLE "interface"))
        (send this insert! 1 (make-object ui:var-scalar% this-ent* DEF-STYLE get-name-text* name-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
        (send this set-horizontal! #t)
      ))
    )

    (define header* (make-object ui:interface-set-list% this NOOP-FALLBACK-EVENT-HANDLER header-header*))

    (define ui-item*
      (make-object (class ui:set-list%

        (define/override (get-event-handler)
          (combine-keyname-event-handlers (list
            (super get-event-handler)
            (create-simple-event-handler "d"
              (lambda (handle-event-info event)
                (define interface-handle (send this-ent* get-cone-root))
                (if (send interface-handle can-delete?)
                  (when (navigate-to-fresh-module*! (get-all-modules*)) (send interface-handle delete!!))
                  (issue-warning "Cannot delete interface" "Either this interface is a supertype of something or a reference to it exists")
                )
                #t
              )
            )
          ))
        )

        (define/override (db-get-items)
          (send (send this-ent* get-cone-root) get-direct-methods)
        )

        (define/override (db-add-item!!)
          (use-text-from-user
            "Enter a short description of the new method"
            "A short descriptor, one or a few words, to identify the new method"
            (lambda (result)
              (send (send this-ent* get-cone-root) add-direct-method!! result)
            )
            non-empty-string?
          )
        )

        (define/override (db-can-remove-item? to-remove)
          (send (send this-ent* get-cone-root) can-remove-direct-method? to-remove)
        )

        (define/override (db-remove-item!! to-remove)
          (send (send this-ent* get-cone-root) remove-direct-method!! to-remove)
        )

        (define/override (get-item-ui-style)
          DEF-STYLE
        )

        (super-make-object this-ent* this-ent* header*)

        (send this set-horizontal! #f)
      ))
    )

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  ; UI IMPL

  (define ui:item% (class* object% (zinal:ui:item%% event-handler%% fallback-event-handler%%) ; abstract

    (init parent-ent fallback-event-handler)

    (define parent-ent* parent-ent)
    (define parent* #f)

    (define (select-nearby-item-or*! items index increment alt)
      (define num-items (length items))
      (define (get-candidate) (list-ref items index))
      (if (or (negative? index) (>= index num-items))
        (alt)
        (if (is-a? (get-candidate) zinal:ui:const%%)
          (select-nearby-item-or*! items (+ index increment) increment alt)
          (select! (get-candidate))
        )
      )
    )

    (define (select-neighbor-or*! item before? alt)
      (define parent (send item get-parent))
      (define all-siblings (send parent get-children-with-header-internal))
      (define inc (if before? -1 1))
      (define this-index
        (list-index (compose1 (curry eq? item) slot/ui-item->ui-item) all-siblings)
      )
      (define neighbor-index (+ this-index inc))
      (select-nearby-item-or*! all-siblings neighbor-index inc alt)
    )

    (define (select-next-sibling*! item)
      (define parent (send item get-parent))
      (when parent
        (select-neighbor-or*! item #f (thunk
          (select-next-sibling*! parent)
        ))
      )
    )

    (define (handle-left*! handle-event-info event)
      (cond
        [(not parent*)
          ; TODO this comment is outdated
          ; This is a special case - normally we return #t from every event handler to prevent a fallback handler from getting to handle the event,
          ; but in the case of trying to go "out" when we're at the top level, we allow the fallback to handle it in case we want to go from a root
          ; child back to the containing root
          (send handle-event-info set-wasnt-handled!)
        ]
        [(send parent* horizontal?)
          (select-neighbor-or*! this #t (thunk
            (select! parent*)
          ))
        ]
        [else
          (select! parent*)
        ]
      )
      (send handle-event-info set-db-wasnt-affected!)
    )

    (define (handle-right*! handle-event-info event)
      (define (get-all-children)
        (send this get-children-with-header-internal)
      )
      (if (and (is-a? this zinal:ui:list%%) (pair? (get-all-children)))
        (select-nearby-item-or*! (get-all-children) 0 +1 (thunk (select-next-sibling*! this)))
        (select-next-sibling*! this)
      )
      (send handle-event-info set-db-wasnt-affected!)
    )

    (define (get-child-of-first-vertical-ancestor* item)
      (define parent (send item get-parent))
      (if (and parent (send parent horizontal?))
        (get-child-of-first-vertical-ancestor* parent)
        item
      )
    )

    (define (handle-down*! handle-event-info event)
      (select-next-sibling*! (get-child-of-first-vertical-ancestor* this))
      (send handle-event-info set-db-wasnt-affected!)
    )

    (define (handle-up*! handle-event-info event)
      (define vert-child (get-child-of-first-vertical-ancestor* this))
      (define vert-parent (send vert-child get-parent))
      (if vert-parent
        (select-neighbor-or*! vert-child #t (thunk
          (select! vert-parent)
        ))
        vert-child
      )
      (send handle-event-info set-db-wasnt-affected!)
    )

    (define/public (selected?)
      (eq? this selected*)
    )

    (define/public (highlighted?)
      ; TODO NYI
      (error 'highlighted? "highlighted? NYI")
    )

    (define/public (accept visitor [data #f])
      (send visitor visit-item this data)
    )

    (define/public (get-root)
      (if parent*
        (send parent* get-root)
        this
      )
    )

    (define/public (get-parent-ent)
      parent-ent*
    )

    (define/public (get-parent)
      parent*
    )

    (define/public (set-parent! new-parent)
      (set! parent* new-parent)
    )

    (define/public (get-event-handler)
      (create-movement-handler)
    )

    (define event-handler* (get-event-handler))
    (define fallback-event-handler* fallback-event-handler)

    (define/public (handle-event!! event)
      (define event-info (send event-handler* handle-event!! event))
      (or
        (and (send event-info was-handled?) event-info)
        (send fallback-event-handler* handle-child-event!! event)
      )
    )

    (define/public (handle-child-event!! event)
      (send event-handler* handle-event!! event)
    )

    (define/public (create-movement-handler)
      (combine-keyname-event-handlers (list
        (create-left-handler)
        (create-right-handler)
        (create-up-handler)
        (create-down-handler)
      ))
    )

    (define/public (create-left-handler)
      (make-object keyname-event-handler% (list (list handle-left*! '("left" "h"))))
    )

    (define/public (create-right-handler)
      (make-object keyname-event-handler% (list (list handle-right*! '("right" "l"))))
    )

    (define/public (create-up-handler)
      (make-object keyname-event-handler% (list (list handle-up*! '("up" "k"))))
    )

    (define/public (create-down-handler)
      (make-object keyname-event-handler% (list (list handle-down*! '("down" "j"))))
    )

    (super-make-object)
  ))

  (define ui:scalar% (class* ui:item% (zinal:ui:scalar%%) ; abstract

    (init parent-ent style-delta item->event-handler fallback-event-handler)

    (abstract get-text)

    (define style-delta* style-delta)
    (define event-handler* (item->event-handler this))

    (define/override (accept visitor [data #f])
      (send visitor visit-scalar this data)
    )

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list
        (super get-event-handler)
        event-handler*
      ))
    )

    (define/public (get-style-delta)
      style-delta*
    )

    (super-make-object parent-ent fallback-event-handler)
  ))

  (define ui:const% (class* ui:scalar% (zinal:ui:const%%)

    (init parent-ent style-delta text)

    (define text* text)

    (define/override (accept visitor [data #f])
      (send visitor visit-const this data)
    )

    (define/override (get-text)
      text*
    )

    (super-make-object parent-ent style-delta THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
  ))

  (define ui:var-scalar% (class* ui:scalar% (zinal:ui:var-scalar%%)

    (init parent-ent style-delta text-getter item->event-handler fallback-event-handler)

    (define text-getter* text-getter)
    (define current-text* #f)
    (define last-seen-parse-count* 0)

    (define/override (accept visitor [data #f])
      (send visitor visit-var-scalar this data)
    )

    (define/override (get-text)
      (unless (and current-text* (= last-seen-parse-count* parse-count*))
        (set! current-text* (text-getter*))
        (set! last-seen-parse-count* parse-count*)
      )
      current-text*
    )

    (super-make-object parent-ent style-delta item->event-handler fallback-event-handler)
  ))

  (define ui:list% (class* ui:item% (zinal:ui:list%%)

    (init parent-ent fallback-event-handler [header #f] [separator #f] [bookends #f])
    (assert "header must be an item or #f" (implies header (is-a? header zinal:ui:item%%)))
    (assert "separator must be a const or #f" (implies separator (is-a? separator zinal:ui:const%%)))
    (assert
      "bookends must be a pair of consts or #f"
      (implies bookends (and (pair? bookends) (= 2 (length bookends)) (andmap (curryr is-a? zinal:ui:const%%) bookends)))
    )

    (define header* header)
    (when header* (send header* set-parent! this))
    (define separator* (or separator (make-object ui:const% parent-ent NO-STYLE " ")))
    (send separator* set-parent! this)
    (define bookends* bookends)
    (when bookends* (for-each (lambda (b) (send b set-parent! this)) bookends*))
    (define children* '())
    (define horizontal*? #f)

    (define/override (accept visitor [data #f])
      (send visitor visit-list this data)
    )

    (define/public (get-children)
      (map slot/ui-item->ui-item children*)
    )

    (define/public (get-header)
      header*
    )

    (define/public (horizontal?)
      (define parent (send this get-parent))
      (or
        horizontal*?
        (and parent (send parent horizontal?))
      )
    )

    (define/public (get-horizontal-separator)
      separator*
    )

    (define/public (get-bookends)
      bookends*
    )

    (define/public (create-expand-and-collapse-handler)
      (make-object keyname-event-handler% (list
        (list collapse*! '("s:left" "H"))
        (list expand*! '("s:right" "L"))
      ))
    )

    (define/public (set-horizontal! new-value)
      (set! horizontal*? new-value)
    )

    (define/public (insert-but-dont-select! index new-child)
      (define before (take children* index))
      (define after (drop children* index))
      (set! children* (append before (cons new-child after)))
      (when (is-a? new-child zinal:ui:item%%) (send new-child set-parent! this))
    )

    (define/public (insert! index new-child)
      (insert-but-dont-select! index new-child)
      (select! new-child)
    )

    (define/public (remove! child/index)
      (define is-index? (number? child/index))
      (define index (if is-index? child/index (get-child-index child/index)))
      (define child (if is-index? (list-ref children* child/index) child/index))
      (set! children* (remq child children*))
      (when (eq? selected* (slot/ui-item->ui-item child))
        (define num-children (length children*))
        (define selection
          (cond
            [(< index num-children)
              (list-ref children* index)
            ]
            [(> num-children 0)
              (last children*)
            ]
            [else
              this
            ]
          )
        )
        (select! selection)
      )
    )

    (define/public (clear!)
      (set! children* '())
    )

    (define/public (get-children-internal)
      children*
    )

    (define/public (get-children-with-header-internal)
      (if header* (cons header* children*) children*)
    )

    (define/public (get-child-index child)
      (define index (list-index (curry eq? child) children*))
      (assert "no such child" index)
      index
    )

    (define (collapse*! handle-event-info event)
      (set-horizontal! #t)
      #t
    )

    (define (expand*! handle-event-info event)
      (define (expand ui-list)
        (send ui-list set-horizontal! #f)
        (define parent (send ui-list get-parent))
        (when parent (expand parent))
      )
      (expand this)
      #t
    )

    (super-make-object parent-ent fallback-event-handler)
  ))

  (define ui:def-list% (class ui:list% ; abstract

    (init parent-ent [fallback-event-handler NOOP-FALLBACK-EVENT-HANDLER])

    (abstract get-default-name-text)

    (define parent-ent* parent-ent)

    (define/public (db-get-def-handle)
      (send parent-ent* get-cone-root)
    )

    (define/public (get-bridge-text)
      "="
    )

    (define (name-ui->event-handler* name-ui)
      (create-name-change-handler (thunk (db-get-def-handle)))
    )

    (define (get-name-text*)
      (get-short-desc-or* (db-get-def-handle) (get-default-name-text))
    )

    (super-make-object parent-ent fallback-event-handler)

    (send this set-horizontal! #t)
    (send this insert! 0 (make-object ui:var-scalar% parent-ent DEF-STYLE get-name-text* name-ui->event-handler* NOOP-FALLBACK-EVENT-HANDLER))
    (send this insert! 1 (make-object ui:const% parent-ent NO-STYLE (get-bridge-text)))
  ))

  (define ui:possibly-public-def-list% (class ui:def-list% ; abstract

    (init parent-ent [fallback-event-handler NOOP-FALLBACK-EVENT-HANDLER])

    (define/public (get-prefix-text)
      #f
    )

    (define public* (make-object ui:const% parent-ent (send (make-object style-delta%) set-delta-foreground "Lime") "public"))

    (define (update-publicity-ui*)
      (define db-def-handle (send this db-get-def-handle))
      (when (can-be-public*? db-def-handle)
        (if (public*? db-def-handle)
          (send this insert-but-dont-select! 0 public*)
          (when (eq? public* (first (send this get-children))) (send this remove! 0))
        )
      )
    )

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list
        (super get-event-handler)
        (create-modify-publicity-handler* (thunk (send this db-get-def-handle)) update-publicity-ui*)
      ))
    )

    (super-make-object parent-ent fallback-event-handler)

    (when (get-prefix-text)
      (send this insert! 0 (make-object ui:const% parent-ent NO-STYLE (get-prefix-text)))
    )

    (update-publicity-ui*)
  ))

  (define ui:slotted-list% (class ui:list% ; abstract

    (init parent-ent fallback-event-handler [header #f] [separator #f] [bookends #f])

    (abstract get-visible-referables-for-slot)

    (define/public (child-slot->event-handler slot)
      (combine-keyname-event-handlers (list
        (create-replace-handler slot)
        (create-unassign-handler* slot this)
      ))
    )

    (define/public (create-replace-handler slot)
      (create-replace-handler* slot this (lambda (s) (get-visible-referables-for-slot s)))
    )

    (super-make-object parent-ent fallback-event-handler header separator bookends)
  ))

  (define ui:dynamic-slotted-list% (class ui:slotted-list% ; abstract

    (init parent-ent fallback-event-handler child-spawner! [header #f] [separator #f] [bookends #f])

    (abstract db-insert!!)
    (abstract db-can-remove?)
    (abstract db-remove!!)
    (abstract db-get-items)
    (abstract db-get-list-handle)

    ; items that are not part of the header, but which will be prepended to the list
    (define/public (get-pseudo-headers)
      '()
    )

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list
        (super get-event-handler)
        (send this create-expand-and-collapse-handler)
        (create-insert-start-handler)
        (create-insert-end-handler)
      ))
    )

    (define/override (child-slot->event-handler slot)
      (combine-keyname-event-handlers (list
        (create-insert-before-handler slot)
        (create-insert-after-handler slot)
        (create-insert-todo-handler slot)
        (send this create-replace-handler slot)
        (create-unassign-or-remove-handler slot)
      ))
    )

    (define/override (get-visible-referables-for-slot slot)
      (get-visible-referables-for-hypothetical-index (get-ui-index* slot))
    )

    (define/public (remove-slot!! slot)
      (db-remove!! (get-db-index* slot))
      (send this remove! slot)
    )

    (define/public (create-insert-start-handler [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler get-offset* "I" new-item-creator)
    )

    (define/public (create-insert-end-handler [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler (thunk (length (send this get-children-internal))) "A" new-item-creator)
    )

    (define/public (create-insert-before-handler slot [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler (thunk (get-ui-index* slot)) "i" new-item-creator)
    )

    (define/public (create-insert-after-handler slot [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler (thunk (add1 (get-ui-index* slot))) "a" new-item-creator)
    )

    (define/public (create-insert-todo-handler slot)
      (create-typical-insert-slot-handler (thunk (add1 (get-ui-index* slot))) "o" new-unassigned-creator)
    )

    (define/public (create-remove-handler slot)
      (create-simple-event-handler "d"
        (lambda (handle-event-info event)
          (when (db-can-remove? (get-db-index* slot)) (remove-slot!! slot))
          #t
        )
      )
    )

    (define/public (create-unassign-or-remove-handler slot)
      (create-simple-event-handler "d"
        (lambda (handle-event-info event)
          (cond
            [(db-can-remove? (get-db-index* slot)) (remove-slot!! slot)]
            [(unassignable? (slot->db-handle slot)) (reassign-slot*!! slot this)]
          )
          #t
        )
      )
    )

    (define (get-offset*)
      (length (get-pseudo-headers))
    )

    (define (db-index->ui-index* db-index)
      (+ db-index (get-offset*))
    )

    (define (ui-index->db-index* ui-index)
      (- ui-index (get-offset*))
    )

    (define (get-ui-index* slot)
      (send this get-child-index slot)
    )

    (define (get-db-index* slot)
      (ui-index->db-index* (get-ui-index* slot))
    )

    (define (get-visible-referables-for-hypothetical-index ui-index)
      (get-visible-referables-for-hypothetical-index* (db-get-list-handle) (db-get-items) (ui-index->db-index* ui-index))
    )

    (define (create-insert-slot-handler get-ui-index interaction->new-handle-initializer!! keyname)
      (define (result-handler new-handle-initializer!!)
        (define ui-index (get-ui-index))
        (define intermediate-handle (db-insert!! (ui-index->db-index* ui-index)))
        (define new-handle (new-handle-initializer!! intermediate-handle))
        (insert-new-slot! ui-index new-handle)
      )
      (create-interaction-dependent-event-handler
        interaction->new-handle-initializer!!
        result-handler
        keyname
      )
    )

    (define (create-typical-insert-slot-handler get-ui-index keyname [new-item-creator request-new-item-creator])
      (define interaction->new-handle-initializer!!
        (thunk (new-item-creator (db-get-list-handle) (get-visible-referables-for-hypothetical-index (get-ui-index))))
      )
      (create-insert-slot-handler get-ui-index interaction->new-handle-initializer!! keyname)
    )

    (define (insert-new-slot! ui-index slot-handle [child-spawner*! spawn-entity*!])
      (define new-slot (make-object slot% child-slot->event-handler* this))
      (child-spawner*! new-slot slot-handle this)
      (send this insert! ui-index new-slot)
    )

    (define (child-slot->event-handler* slot)
      (child-slot->event-handler slot)
    )

    (super-make-object parent-ent fallback-event-handler header separator bookends)

    (map-by-index
      (lambda (ui-index item) (send this insert! ui-index item))
      (get-pseudo-headers)
    )
    (map-by-index
      (lambda (db-index db-item) (insert-new-slot! (db-index->ui-index* db-index) db-item child-spawner!))
      (db-get-items)
    )
  ))

  (define ui:params-list% (class ui:dynamic-slotted-list%

    (init parent-ent fallback-event-handler child-spawner! has-params-handle [header #f])

    (define has-params-handle* has-params-handle)

    (define/override (db-insert!! index)
      (define first-opt-index (get-first-opt-index*))
      (if (<= index first-opt-index)
        (send (db-get-list-handle) insert-required-param!! index)
        (send (db-get-list-handle) insert-optional-param!! (- index first-opt-index))
      )
    )

    (define/override (db-can-remove? index)
      (define first-opt-index (get-first-opt-index*))
      (if (< index first-opt-index)
        (send (db-get-list-handle) can-remove-required-param? index)
        (send (db-get-list-handle) can-remove-optional-param? (- index first-opt-index))
      )
    )

    (define/override (db-remove!! index)
      (define first-opt-index (get-first-opt-index*))
      (if (< index first-opt-index)
        (send (db-get-list-handle) remove-required-param!! index)
        (send (db-get-list-handle) remove-optional-param!! (- index first-opt-index))
      )
    )

    (define/override (db-get-items)
      (send (db-get-list-handle) get-all-params)
    )

    (define/override (db-get-list-handle)
      has-params-handle*
    )

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list
        (send this create-movement-handler)
        (send this create-insert-start-handler new-param-creator)
        (send this create-insert-end-handler new-param-creator)
      ))
    )

    (define/override (child-slot->event-handler slot)
      (combine-keyname-event-handlers (list
        (send this create-insert-before-handler slot new-param-creator)
        (send this create-insert-after-handler slot new-param-creator)
        (send this create-remove-handler slot)
        (create-simple-event-handler "r"
          (lambda (handle-event-info event)
            (define first-opt-index (get-first-opt-index*))
            (define slot-index (send this get-child-index slot))
            (when (= slot-index first-opt-index)
              (send (db-get-list-handle) make-last-optional-param-required!!)
              (select! slot)
            )
            #t
          )
        )
        (create-simple-event-handler "o"
          (lambda (handle-event-info event)
            (define first-opt-index (get-first-opt-index*))
            (define slot-index (send this get-child-index slot))
            (when (= slot-index (sub1 first-opt-index))
              (send (db-get-list-handle) make-last-required-param-optional!!)
              (select! slot)
            )
            #t
          )
        )
      ))
    )

    (define (get-first-opt-index*)
      (length (send (db-get-list-handle) get-required-params))
    )

    (define (new-param-creator parent-handle visible-referables)
      (use-text-from-user
        "Enter short descriptor for param"
        "A short descriptor, one or a few words, to identify this param"
        (lambda (result)
          (lambda (param) (send param set-short-desc!! result) param)
        )
        non-empty-string?
      )
    )

    (super-make-object parent-ent fallback-event-handler child-spawner! header (make-object ui:const% parent-ent NO-STYLE ", "))

    (send this set-horizontal! #t)
  ))

  (define ui:set-list% (class ui:list%

    (init parent-ent fallback-event-handler [header #f])

    ; all items must be zinal:db:describable%% , and they should not be part of the tree
    (abstract db-get-items)
    (abstract db-add-item!!)
    (abstract db-can-remove-item?)
    (abstract db-remove-item!!)
    (abstract get-item-ui-style)

    (define parent-ent* parent-ent)

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list
        (super get-event-handler)
        (add-item-event-handler* #f)
      ))
    )

    (define/public (reset! [handle-to-select #f])
      (define selected-index (list-index (curry eq? selected*) (send this get-children)))
      (send this clear!)
      (map-by-index
        (lambda (i h)
          (define to-insert
            (make-object ui:var-scalar%
              parent-ent*
              (get-item-ui-style)
              (thunk (get-short-desc-or* h "<unnamed>"))
              (curry item-handle->event-handler* h)
              NOOP-FALLBACK-EVENT-HANDLER
            )
          )
          (if (and handle-to-select (handles-equal? handle-to-select h))
            (send this insert! i to-insert)
            (send this insert-but-dont-select! i to-insert)
          )
        )
        (sort (db-get-items) item-handle<?)
      )
      (when (and (not handle-to-select) selected-index)
        (define children (send this get-children))
        (if (null? children)
          (select! this)
          (select! (list-ref children (min selected-index (sub1 (length children)))))
        )
      )
    )

    (define (item-handle->event-handler* item-handle item-ui)
      (combine-keyname-event-handlers (list
        (create-name-change-handler (const item-handle))
        (add-item-event-handler* #t)
        (create-simple-event-handler "d"
          (lambda (handle-event-info event)
            (cond
              [(db-can-remove-item? item-handle)
                (db-remove-item!! item-handle)
                (reset!)
              ]
              [else
                (issue-warning "Cannot remove item from set" "I don't really know why you can't, you just can't. Sucks to be you")
              ]
            )
            #t
          )
        )
      ))
    )

    (define (add-item-event-handler* augmented?)
      (make-object keyname-event-handler% (list (list
        (lambda (handle-event-info event)
          (define added-handle (db-add-item!!))
          (when added-handle (reset! added-handle))
          #t
        )
        (append '("I" "A") (if augmented? '("i" "a" "o") '()))
      )))
    )

    (define (item-handle<? item-handle-1 item-handle-2)
      (define (desc ih) (get-short-desc-or* ih ""))
      (string<? (desc item-handle-1) (desc item-handle-2))
    )

    (super-make-object parent-ent fallback-event-handler header (make-object ui:const% parent-ent NO-STYLE ", "))

    (send this set-horizontal! #t)
    (reset!)
  ))

  (define ui:interface-set-list% (class ui:set-list%

    (init parent-ent fallback-event-handler [header #f])

    (define parent-ent* parent-ent)

    (define/override (db-get-items)
      (send (send parent-ent* get-cone-root) get-direct-super-interfaces)
    )

    (define/override (db-add-item!!)
      (define type-handle (send parent-ent* get-cone-root))
      (define interface-to-implement (get-interface-from-user (filter (lambda (i) (send type-handle can-add-direct-super-interface? i)) (send db* get-all-interfaces))))
      (cond
        [interface-to-implement
          (send type-handle add-direct-super-interface!! interface-to-implement)
          interface-to-implement
        ]
        [else
          #f
        ]
      )
    )

    (define/override (db-can-remove-item? to-remove)
      (send (send parent-ent* get-cone-root) can-remove-direct-super-interface? to-remove)
    )

    (define/override (db-remove-item!! to-remove)
      (send (send parent-ent* get-cone-root) remove-direct-super-interface!! to-remove)
    )

    (define/override (get-item-ui-style)
      REF-STYLE
    )

    (super-make-object parent-ent fallback-event-handler header)
  ))

  ; HELPER FUNCTIONS

  (define (legacy-handle->string legacy-handle)
    (define library (send legacy-handle get-library))
    (define name (send legacy-handle get-name))
    (if library (format "~a→~a" library name) name)
  )

  (define (has-params->short-params-string has-params-handle)
    (define param-names
      (map
        (lambda (p)
          (format (if (send p get-default) "[~a]" "~a") (get-short-desc-or* p "<nameless param>"))
        )
        (send has-params-handle get-all-params)
      )
    )
    (format "λ: ~a" (string-join param-names ", "))
  )

  (define (create-module*!!)
    (use-text-from-user
      "Enter the new module's name"
      "A short descriptor, one or a few words, to name this module"
      (lambda (result)
        (send db* create-module!! result)
      )
      non-empty-string?
    )
  )

  (define (create-interface*!!)
    (use-text-from-user
      "Enter the new interface's name"
      "A short descriptor, one or a few words, to name this interface"
      (lambda (result)
        (send db* create-interface!! result)
      )
      non-empty-string?
    )
  )

  (define (get-module-from-user [selectable-modules (get-all-modules*)])
    (define handles&choices
      (map
        (lambda (module)
          (define name (get-short-desc-or* module "<unnamed module>"))
          (list module (if (send module is-main-module?) (format "~a (Main)" name) name))
        )
        selectable-modules
      )
    )
    (auto-complete* "Choose a module" "Start typing bits and pieces of the desired module's name" handles&choices handles-equal?)
  )

  (define (navigate-to-fresh-module*! [selectable-modules (get-all-modules*)] [default-to-main? #t])
    (define main-module (send db* get-main-module))
    (define next-module
      (or
        (and default-to-main? (member main-module selectable-modules handles-equal?) main-module)
        (if (pair? selectable-modules)
          (get-module-from-user selectable-modules)
          (create-module*!!)
        )
      )
    )
    (when next-module (spawn-root-entity*! next-module))
    next-module
  )

  (define (get-all-modules* [module-to-exclude #f])
    ; UGH - srfi/1 redefines remove, so this uses the filter-like definition it provides rather than the standard lib definition
    (remove (curry handles-equal? module-to-exclude) (send db* get-all-modules))
  )

  (define (get-interface-from-user selectable-interfaces)
    (define handles&choices
      (map
        (lambda (interface)
          (list interface (get-short-desc-or* interface "<unnamed interface>"))
        )
        selectable-interfaces
      )
    )
    (auto-complete* "Choose an interface" "Start typing bits and pieces of the desired interface's name" handles&choices handles-equal?)
  )

  (define (unassignable? handle)
    (cond
      [(send handle can-unassign?)
        #t
      ]
      [(send handle get-parent)
        (assert "can't unassign a non-referable, non-root handle" (is-a? handle zinal:db:referable%%))
        (issue-warning
          "Cannot unassign"
          (format
            "Referable ~a has at least one reference that is not a descendant, so it can't be unassigned"
            (send handle get-short-desc)
          )
        )
        #f
      ]
      [else
        (issue-warning "Cannot unassign" "You cannot unassign the root node")
        #f
      ]
    )
  )

  ; result-handler accepts a result from a user interaction and does something with it. Its return
  ; value is ignored.
  ; interaction-function should return a result from the user interaction to be passed to,
  ; result-handler. To indicate that no action should be taken, interaction-function should return #f.
  ; Regardless of what happens, the handler always returns #t, indicating that the action has been
  ; handled
  (define (create-interaction-dependent-event-handler interaction-function result-handler keyname)
    (define (handler-function handle-event-info event)
      (define interaction-result (interaction-function))
      (when interaction-result (result-handler interaction-result))
      #t
    )
    (make-object keyname-event-handler% (list (list handler-function (list keyname))))
  )

  (define (create-simple-event-handler keyname handler-function)
    (make-object keyname-event-handler% (list (list handler-function (list keyname))))
  )

  (define (create-legacy-method-name-change-handler get-legacy-method-handle)
    (define (interaction-function)
      (use-text-from-user
        "Enter the new legacy method name"
        "The exact name of the racket method you want to change this to"
        identity
        non-empty-string?
      )
    )
    (define (result-handler new-name)
      (send (get-legacy-method-handle) set-legacy-method-name!! new-name)
    )
    (create-interaction-dependent-event-handler interaction-function result-handler "s")
  )

  (define (create-name-change-handler get-describable-handle)
    (define (interaction-function)
      (use-text-from-user
        "Enter the new name"
        "A short descriptor, one or a few words, to name this thing"
        identity
        non-empty-string?
      )
    )
    (define (result-handler new-name)
      (send (get-describable-handle) set-short-desc!! new-name)
    )
    (create-interaction-dependent-event-handler interaction-function result-handler "s")
  )

  (define (create-replace-handler* slot ui-parent get-visible-referables-for-slot)
    (define (interaction-function)
      (define handle (slot->db-handle slot))
      (and
        (unassignable? handle)
        (request-new-item-creator (send handle get-parent) (get-visible-referables-for-slot slot))
      )
    )
    (create-interaction-dependent-event-handler interaction-function (lambda (nhi) (reassign-slot*!! slot ui-parent nhi)) "s")
  )

  (define (create-unassign-handler* slot ui-parent)
    (create-simple-event-handler "d"
      (lambda (handle-event-info event)
        (when (unassignable? (slot->db-handle slot)) (reassign-slot*!! slot ui-parent))
        #t
      )
    )
  )

  (define (create-modify-publicity-handler* get-define-handle ui-callback)
    (create-simple-event-handler "m"
      (lambda (handle-event-info event)
        (define define-handle (get-define-handle))
        (when (can-be-public*? define-handle)
          (send (send define-handle get-parent) set-public!! define-handle (not (public*? define-handle)))
          (ui-callback)
        )
        #t
      )
    )
  )

  (define (create-navigate-to-root-handler* root-handle)
    (make-object keyname-event-handler% (list (list
      (lambda (handle-event-info event)
        (spawn-root-entity*! root-handle)
        (send handle-event-info set-db-wasnt-affected!)
      )
      '("space" "enter")
    )))
  )

  (define (reassign-slot*!! slot ui-parent [new-handle-initializer!! identity])
    (define intermediate-handle (send (slot->db-handle slot) unassign!!))
    (define new-handle (new-handle-initializer!! intermediate-handle))
    (spawn-entity*! slot new-handle ui-parent)
    (select! slot)
  )

  (define (get-visible-referables-for-hypothetical-index* list-handle child-handles index)
    (if (zero? index)
      (send list-handle get-visible-referables-underneath)
      (send (list-ref child-handles (sub1 index)) get-visible-referables-after)
    )
  )

  (define (spawn-or-reassign-entity*! slot cone-root-handle ui-parent existing-slots)
    (define existing-slot
      (findf (compose1 (curry handles-equal? cone-root-handle) slot->db-handle) existing-slots)
    )
    (define new-ent
      (if existing-slot
        (send existing-slot get-ent)
        (make-object (parse-entity*! cone-root-handle) cone-root-handle spawn-entity*!)
      )
    )
    (send new-ent assign-to-slot! slot ui-parent)
  )

  (define (spawn-entity*! slot cone-root-handle ui-parent [child-spawner! spawn-entity*!])
    (define new-ent (make-object (parse-entity*! cone-root-handle) cone-root-handle child-spawner!))
    (send new-ent assign-to-slot! slot ui-parent)
  )

  (define (root-slot->root-event-handler* root-slot)
    (make-object keyname-event-handler% (list (list
      (lambda (handle-event-info event)
        (define root-handle (slot->db-handle root-slot))
        (cond
          [(is-one-of? root-handle (list zinal:db:module%% zinal:db:interface%%))
            (navigate-to-fresh-module*! (get-all-modules* root-handle) #f)
          ]
          [else
            (define (find-first-root-ancestor h)
              (define p (send h get-parent))
              (assert "could not find root ancestor of non-module, non-interface root handle" p)
              (if (can-be-root*? p) p (find-first-root-ancestor p))
            )
            (define (find-root-handle-slot s)
              (define s-children (send (send s get-ent) get-cone-leaves))
              (or
                (findf (compose1 (curry handles-equal? root-handle) slot->db-handle) s-children)
                (ormap find-root-handle-slot s-children)
              )
            )
            (define new-root-slot (spawn-root-entity*! (find-first-root-ancestor root-handle)))
            (define new-root-handle-slot (find-root-handle-slot new-root-slot))
            (assert "could not find the previous root handle underneath the new root handle" new-root-handle-slot)
            (select! new-root-handle-slot)
          ]
        )
        (send handle-event-info set-db-wasnt-affected!)
      )
      '("space" "enter" "backspace")
    )))
  )

  (define (spawn-root-entity*! root-handle)
    (assert "attempt to spawn a root for a non-root type" (can-be-root*? root-handle))
    (define root-slot (make-object slot% root-slot->root-event-handler* NOOP-FALLBACK-EVENT-HANDLER))
    (define new-ent (make-object (parse-root-entity*! root-handle) root-handle spawn-entity*!))
    (send new-ent assign-to-slot! root-slot #f)
    (select! root-slot)
    root-slot
  )

  (define (can-be-root*? handle)
    ; TODO maybe make some definitions present inline
    (is-one-of? handle (list zinal:db:def%% zinal:db:define-method%% zinal:db:override-legacy-method%% zinal:db:subtype%% zinal:db:module%%))
  )

  (define (function-definition*? handle)
    (and (is-a? handle zinal:db:def%%) (is-a? (send handle get-expr) zinal:db:lambda%%))
  )

  (define (parse-root-entity*! root-handle)
    (send root-handle accept (make-object (class zinal:db:element-visitor% (super-make-object)

      (define/override (visit-element e meh)
        (error 'parse-root-entity*! "Cannot parse entity for non-root type")
      )

      (define/override (visit-define-method root-define-method-handle meh)
        ent:define-method%
      )

      (define/override (visit-override-legacy-method root-override-legacy-handle meh)
        ent:override-legacy-method%
      )

      (define/override (visit-interface root-interface-handle meh)
        ent:interface%
      )

      (define/override (visit-define-class root-define-class-handle meh)
        ent:define-class%
      )

      (define/override (visit-class-instance root-class-instance-handle meh)
        ent:class-instance%
      )

      (define/override (visit-module root-module-handle meh)
        ent:module%
      )

      (define/override (visit-def root-def-handle meh)
        (if (function-definition*? root-def-handle)
          ent:func-def%
          ent:def%
        )
      )
    )))
  )

  (define (parse-non-nil-list-entity*! db-list-handle)
    (define items (send db-list-handle get-items))
    (define first-item (first items))
    (cond
      [(is-a? first-item zinal:db:legacy-link%%)
        (cond
          [(and (= 2 (length items)) (standard-with-name*? first-item "quote") (is-a? (second items) zinal:db:list%%))
            ent:quoted-list%
          ]
          [(standard-with-name*? first-item "list")
            ent:list-list%
          ]
          [else
            ; TODO we should probably have some sort of quote-context to make this an ordinary
            ; list when underneath something quoted ... or something
            ent:legacy-invokation%
          ]
        )
      ]
      [(is-a? first-item zinal:db:reference%%)
        ent:reference-invokation%
      ]
      [else ent:typical-list%]
    )
  )

  (define (parse-entity*! db-handle)
    (send db-handle accept (make-object (class zinal:db:element-visitor% (super-make-object)

      (define/override (visit-element e meh)
        (error 'parse-entity*! "Cannot parse entity for mysterious db handle")
      )

      (define/override (visit-invoke-method db-invoke-method-handle meh)
        (if (is-a? (send db-invoke-method-handle get-object) zinal:db:this%%)
          ent:this-invoke-zinal-method%
          ent:invoke-zinal-method%
        )
      )

      (define/override (visit-invoke-legacy-method db-invoke-legacy-method-handle meh)
        (if (is-a? (send db-invoke-legacy-method-handle get-object) zinal:db:this%%)
          ent:this-invoke-legacy-method%
          ent:invoke-legacy-method%
        )
      )

      (define/override (visit-create-object db-create-object-handle meh)
        ent:create-object%
      )

      (define/override (visit-super-init db-super-init-handle meh)
        ent:super-init%
      )

      (define/override (visit-invoke-super-method db-invoke-super-method-handle meh)
        ent:super-invoke-zinal-method%
      )

      (define/override (visit-invoke-legacy-super-method db-invoke-legacy-super-method-handle meh)
        ent:super-invoke-legacy-method%
      )

      (define/override (visit-define-method db-define-method-handle meh)
        ent:short-define-method%
      )

      (define/override (visit-override-legacy-method db-override-legacy-handle meh)
        (if (send db-override-legacy-handle is-augment?)
          ent:short-augment-legacy-method%
          ent:short-override-legacy-method%
        )
      )

      (define/override (visit-this db-this-handle meh)
        ent:this%
      )

      (define/override (visit-define-class db-define-class-handle meh)
        ent:short-define-class%
      )

      (define/override (visit-class-instance db-class-instance-handle meh)
        ent:short-class-instance%
      )

      (define/override (visit-reference db-ref-handle meh)
        ent:ref%
      )

      (define/override (visit-assert db-assert-handle meh)
        ent:assert%
      )

      (define/override (visit-atom db-atom-handle meh)
        (if (is-a? db-atom-handle zinal:db:string%%)
          (if (non-empty-string? (send db-atom-handle get-val))
            ent:string%
            ent:empty-string%
          )
          ent:atom%
        )
      )

      (define/override (visit-lambda db-lambda-handle meh)
        ent:lambda%
      )

      (define/override (visit-param db-param-handle meh)
        (if (send db-param-handle get-default)
          ent:optional-param%
          ent:required-param%
        )
      )

      (define/override (visit-list db-list-handle meh)
        (if (pair? (send db-list-handle get-items))
          (parse-non-nil-list-entity*! db-list-handle)
          ent:typical-list%
        )
      )

      (define/override (visit-module db-module-handle meh)
        (error 'parse-entity*! "Modules should never be parsed in the ordinary way - all parsing should parse the root with spawn-root-entity*!")
      )

      (define/override (visit-def db-def-handle meh)
        (if (function-definition*? db-def-handle)
          ent:short-func-def%
          ent:short-def%
        )
      )

      (define/override (visit-legacy-link db-legacy-link-handle meh)
        ent:legacy%
      )

      (define/override (visit-unassigned db-unassigned-handle meh)
        ent:unassigned%
      )
    )))
  )
))
)
