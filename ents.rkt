#lang racket

(require racket/gui/base)
; for hash-union
(require racket/hash)
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

(define (standard*? db-legacy-link-handle)
  (not (send db-legacy-link-handle get-library))
)

(define (standard-with-name*? db-legacy-link-handle name)
  (and (standard*? db-legacy-link-handle) (equal? name (send db-legacy-link-handle get-name)))
)

(define (get-short-desc-or* db-describable-handle alt)
  (or (send db-describable-handle get-short-desc) alt)
)

(define NO-STYLE (make-object style-delta%))

; EVENT HANDLER

(define event-handler%% (interface ()

  handle-event!! ; (event)
))

(define fallback-event-handler%% (interface ()

  handle-child-event!! ; (event)
))

(define keyname-event-handler% (class* object% (event-handler%%)

  ; handler-function&keynames=pairs is a list of pairs, first item is an event handler function
  ; and second is a list of key names that invoke the function
  (init handler-function&keynames=pairs)

  (define handler-function&keynames=pairs* handler-function&keynames=pairs)

  (define keymap* (new keymap%))
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
    (if (is-a? event key-event%)
      (send keymap* handle-key-event #f event)
      (send keymap* handle-mouse-event #f event)
    )
  )

  (define/public (get-handler-function&keynames=pairs*)
    handler-function&keynames=pairs*
  )

  (super-make-object)
))

(define NOOP-FALLBACK-EVENT-HANDLER (make-object (class* object% (fallback-event-handler%%)
  (define/public (handle-child-event!! event) #f)
  (super-make-object)
)))

(define NOOP (make-object keyname-event-handler% '()))

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
    (unless (send event-handler* handle-event!! event)
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

    (define/override (on-subwindow-char receiver key-event)
      (case (send key-event get-key-code)
        [(#\return)
          (set! has-been-chosen* #t)
          (close*!)
          #t
        ]
        [('escape)
          (set! has-been-chosen* #f)
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
      (send this on-subwindow-char this (new key-event% [key-code #\tab]))
    )

    (abstract get-choice-from-ui*)

    (define (on-close)
      (set! chosen*
        (if has-been-chosen*
          (get-choice-from-ui*)
          #f
        )
      )
    )
    (augment on-close)

    (super-new)

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
                [(cons? chars)
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

        (super-new)

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

    ; key-equalifier takes two key args and returns whether they're equivalent.
    ; The first arg will never be #f, but the second one may be
    (init title message keys&choices [key-equalifier equal?])

    (super-new [label title])

    (define keys&choices* (sort keys&choices (lambda (a b) (string<? (second a) (second b)))))
    (define num-all-choices* (length keys&choices))
    (define chooser*
      (new auto-complete-list-box%
        [label message]
        [choices '()]
        [parent this]
        [style '(single vertical-label)]
      )
    )
  )
)

(define discrete-choice-dialog%
  (class choice-dialog%

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

        (super-new)

        (define chars "")
      )
    )

    (define/override (get-choice-from-ui*)
      (send chooser* get-selection)
    )

    (init title message choices)

    (super-new [label title])

    (define choices* choices)
    (define num-choices* (length choices))
    (define chooser*
      (new keyboard-choice%
        [label message]
        [choices choices]
        [parent this]
        [style '(vertical-label)]
      )
    )
  )
)

(define (get-choice-from-user title message choices)
  (define dialog (new discrete-choice-dialog% [title title] [message message] [choices choices]))
  (send dialog show #t)
  (send dialog get-choice)
)

; new-blah-creator is a function of form
; (list-of zinal:db:referable%%) => (zinal:db:unassigned%% => zinal:db:element%%) OR #f
; visible-referables are handles for all referables that are visible to any newly minted nodes.
; If it returns #f, it means no operation should be performed
; The returned creator is destructive and must succeed

(define (new-list-creator visible-referables)
  (lambda (unassigned) (send unassigned assign-list!!))
)

(define (new-number-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter a number"
      "Seriously, do it"
      #f
      ""
      '(disallow-invalid)
      #:validate string->number
    )
  )
  (if (and result (string->number result))
    (lambda (unassigned) (send unassigned assign-number!! (string->number result)))
    #f
  )
)

(define (new-character-creator visible-referables)
  (define validate-char (compose1 (curry = 1) string-length))
  (define result
    (get-text-from-user
      "Enter a character"
      "Seriously, do it"
      #f
      ""
      '(disallow-invalid)
      #:validate validate-char
    )
  )
  (if (and result (validate-char result))
    (lambda (unassigned) (send unassigned assign-char!! (string-ref result 0)))
    #f
  )
)

(define (new-string-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter a string"
      "Seriously, do it"
      #:validate (const #t)
    )
  )
  (if result
    (lambda (unassigned) (send unassigned assign-string!! result))
    #f
  )
)

; TODO let's factor the boilerplate out of these things
(define (new-symbol-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter a symbol as a string"
      "Don't include the leading '"
      #:validate (const #t)
    )
  )
  (if result
    (lambda (unassigned) (send unassigned assign-symbol!! (string->symbol result)))
    #f
  )
)

(define (new-keyword-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter a keyword as a string"
      "Don't include the leading #:"
      #:validate (const #t)
    )
  )
  (if result
    (lambda (unassigned) (send unassigned assign-keyword!! (string->keyword result)))
    #f
  )
)

(define (new-boolean-creator visible-referables)
  (define choices '("true" "false"))
  (define result (get-choice-from-user "To be or not to be?" "That's the fuckin question" choices))
  (if result
    (lambda (unassigned) (send unassigned assign-bool!! (= result 0)))
    #f
  )
)

(define (new-define-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter the new definition's short descriptor"
      "A short descriptor, one or a few words, to identify this variable"
      #:validate non-empty-string?
    )
  )
  (if (and result (non-empty-string? result))
    (lambda (unassigned) (send unassigned assign-def!! result))
    #f
  )
)

(define (new-lambda-creator visible-referables)
  (lambda (unassigned) (send unassigned assign-lambda!!))
)

; TODO fucking terrible inconsistent focus issues. AFAICT they started happening after "upgrading"
; to 16.04.1
(define (new-value-read-creator visible-referables)
  (cond
    [(cons? visible-referables)
      (define handles&choices
        (map
          (lambda (handle) (list handle (get-short-desc-or* handle "<no desc>")))
          visible-referables
        )
      )
      (define dialog
        (new auto-complete-dialog%
          [title "What definition or parameter do you want to read?"]
          [message "Start typing bits and pieces of the desired referable's short descriptor"]
          [keys&choices handles&choices]
          [key-equalifier (lambda (a b) (send a equals? b))]
        )
      )
      (send dialog show #t)
      (define chosen-handle (send dialog get-choice))
      (if chosen-handle
        (lambda (unassigned)
          (if (is-a? chosen-handle zinal:db:param%%)
            (send unassigned assign-param-ref!! chosen-handle)
            (send unassigned assign-def-ref!! chosen-handle)
          )
        )
        #f
      )
    ]
    [else #f]
  )
)

(define (new-legacy-creator visible-referables)
  (define result
    (get-text-from-user
      "Enter the standard library identifier"
      "Enter the standard library identifier"
      ; TODO we need to add a reflective validator
      #:validate non-empty-string?
    )
  )
  (if (and result (non-empty-string? result))
    (lambda (unassigned) (send unassigned assign-legacy-link!! #f result))
    #f
  )
)

(define new-unassigned-creator (const identity))

; GUI CONSTANTS

(define FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "list" new-list-creator
  "number" new-number-creator
  "character" new-character-creator
  "string" new-string-creator
  "boolean" new-boolean-creator
  "symbol" new-symbol-creator
  "keyword" new-keyword-creator
))

(define FRIENDLY-TYPE->CREATOR (hash-union FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "define" new-define-creator
  "lambda" new-lambda-creator
  "reference" new-value-read-creator
  "legacy" new-legacy-creator
  "TODO" new-unassigned-creator
)))

; Returns #f to signify no action is to be taken (i.e. the user cancels the dialog)
; Returns a function that takes an unassigned db handle and will assign it to something else, returning the new handle
; The returned creator is not allowed to fail. A failure of this function should return #f instead of a creator
(define (request-new-item-creator visible-referables [allowed-types (hash-keys FRIENDLY-TYPE->CREATOR)])
  (define choice (get-choice-from-user "Choose the new node's type:" "Choose the node's type:" allowed-types))
  (if choice
    ((hash-ref FRIENDLY-TYPE->CREATOR (list-ref allowed-types choice)) visible-referables)
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

  (define/public (get-initial-ui!)
    (define root-slot (make-object slot% THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))
    (spawn-entity*! root-slot (send db* get-root) #f)
    (select! root-slot)
    (send selected* get-root)
  )

  (define/public (handle-event!! event)
    (assert "Something must always be selected" selected*)
    ; TODO if we implement db transactions, then end-transaction could return "has changed",
    ; in which case we can avoid reparsing unless a change has actually occurred. If we do that,
    ; we should have reparse traverse the whole damn tree
    (send selected* handle-event!! event)
    (maybe-reparse*! (send (send (send selected* get-root) get-parent-ent) get-slot) (reverse (get-backwards-selection-path selected*)))
    (send selected* get-root)
  )

  (define (select! slot/item)
    (set! selected* (slot/ui-item->ui-item slot/item))
  )

  (define (get-backwards-selection-path ui-item)
    (cond
      [ui-item
        (define slot (send (send ui-item get-parent-ent) get-slot))
        (define sub-chain (get-backwards-selection-path (send ui-item get-parent)))
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
    (unless (is-a? current-ent (parse-entity*! db-handle))
      (spawn-entity*! slot db-handle ui-parent (curryr spawn-or-reassign-entity*! cone-leaves))
      ; Imperative style, but the functional alternatives are just so damn ugly
      (set! was-this-slot-respawned? #t)
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

  ; ENTS

  (define ent% (class* object% (fallback-event-handler%%) ; abstract

    ; Every ent must have an init of the form (init cone-root-handle child-spawner!). This
    ; base class doesn't actually need the child-spawner! , so its init only takes the root handle.
    ; But all subclasses should accept the child-spawner! and use it for spawning all initial child
    ; ents.
    (init cone-root-handle)

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

    (abstract get-root-ui-item)

    (super-make-object)
  ))

  (define ent:singleton% (class ent% ; abstract

    (init cone-root-handle child-spawner! header)

    (abstract db-get-single-item)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define this-ent* this)

    (define ui-item* (make-object (class ui:slotted-list%

      (define/override (get-visible-referables-for-slot slot)
        (send (send this-ent* get-cone-root) get-visible-referables-underneath)
      )

      (super-make-object this-ent* this-ent* header)

      (define item-slot* (make-object slot% (lambda (s) (send this child-slot->event-handler s)) NOOP-FALLBACK-EVENT-HANDLER))
      (child-spawner! item-slot* (db-get-single-item) this)
      (send this insert! 0 item-slot*)
    )))

    (define/override (get-root-ui-item)
      ui-item*
    )
  ))

  (define ent:list% (class ent%

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

    (define/public (get-header)
      #f
    )

    (define/public (get-separator)
      (make-object ui:const% this NO-STYLE " " THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
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

      (super-make-object this-ent* this-ent* child-spawner! (get-header) (get-separator))
    )))

    (when (horizontal-by-default?) (send ui-list* set-horizontal! #t))

    (define/override (get-root-ui-item)
      ui-list*
    )
  ))

  (define ent:invokation% (class ent:list% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract should-underline?)

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
      (make-object ui:var-scalar% this (send (make-object style-delta% (get-style-change-command*)) set-delta-foreground "Cyan") get-header-text* header->event-handler* NOOP-FALLBACK-EVENT-HANDLER)
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (define (get-header-text*)
      (define func (get-func-handle*))
      (cond
        [(is-a? func zinal:db:legacy-link%%)
          (send func get-name)
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
        (request-new-item-creator (send (send this db-get-list-handle) get-visible-referables-underneath) '("legacy" "reference"))
      )
      (define (result-handler new-handle-initializer!!)
        (new-handle-initializer!! (send (get-func-handle*) unassign!!))
      )
      (create-interaction-dependent-event-handler interaction-function result-handler "s")
    )

    (define (get-func-handle*)
      (car (super db-get-items))
    )

    (define (get-style-change-command*)
      (if (should-underline?) 'change-toggle-underline 'change-nothing)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:legacy-invokation% (class ent:invokation% 

    (init cone-root-handle child-spawner!)

    (define/override (should-underline?)
      #f
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:reference-invokation% (class ent:invokation% 

    (init cone-root-handle child-spawner!)

    (define/override (should-underline?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:list-list% (class ent:list%

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

    (define/override (get-header)
      (make-object ui:const% this (make-object style-delta% 'change-bold) "[" THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:quoted-list% (class ent:list%

    (init cone-root-handle child-spawner!)

    (define/override (db-get-list-handle)
      (second (send (send this get-cone-root) get-items))
    )

    (define/override (get-header)
      (make-object ui:const% this NO-STYLE "`" THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
    )

    (define/override (horizontal-by-default?)
      #t
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:lambda-like% (class ent% ; abstract

    (init cone-root-handle child-spawner!)

    (abstract get-params-header)
    (abstract get-lambda-handle)

    ; Gross. We happen to know that the superclass does not actually need to call get-root-ui-item during
    ; initialization, so we can resolve a cyclic dependency by calling super-make-object before overriding
    ; get-root-ui-item
    (super-make-object cone-root-handle)

    (define this-ent* this)

    (define params-separator* (make-object ui:const% this NO-STYLE ", " THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))

    (define ui-params* (make-object (class ui:dynamic-slotted-list%

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
        (get-lambda-handle)
      )

      (define/override (get-event-handler)
        (combine-keyname-event-handlers (list
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
            (lambda (data event)
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
            (lambda (data event)
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

      (define (new-param-creator visible-referables)
        (define short-desc
          (get-text-from-user
            ; TODO we used to say "for ~ath (required|optional) param" but now we can't. Really we ought to be able to do this
            "Enter short descriptor for param"
            "A short descriptor, one or a few words, to identify this param"
            #:validate non-empty-string?
          )
        )
        (and
          (and short-desc (non-empty-string? short-desc))
          (lambda (param) (send param set-short-desc!! short-desc) param)
        )
      )

      (super-make-object this-ent* NOOP-FALLBACK-EVENT-HANDLER child-spawner! (get-params-header) params-separator*)
    )))

    (send ui-params* set-horizontal! #t)

    (define body-separator* (make-object ui:const% this NO-STYLE "; " THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))

    (define ui-body* (make-object (class ui:dynamic-slotted-list%

      (define/override (db-insert!! index)
        (send (db-get-list-handle) insert-into-body!! index)
      )

      (define/override (db-can-remove? index)
        (is-a? (list-ref (db-get-items) index) zinal:db:unassigned%%)
      )

      (define/override (db-remove!! index)
        (send (db-get-list-handle) remove-from-body!! index)
      )

      (define/override (db-get-items)
        (send (db-get-list-handle) get-body)
      )

      (define/override (db-get-list-handle)
        (get-lambda-handle)
      )

      (super-make-object this-ent* this-ent* child-spawner! ui-params* body-separator*)
    )))

    (define/override (get-root-ui-item)
      ui-body*
    )
  ))

  (define ent:lambda% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define/override (get-params-header)
      (make-object ui:const% this NO-STYLE "λ" THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
    )

    (define/override (get-lambda-handle)
      (send this get-cone-root)
    )

    (super-make-object cone-root-handle child-spawner!)
  ))

  (define ent:func-def% (class ent:lambda-like%

    (init cone-root-handle child-spawner!)

    (define/override (get-params-header)
      (define (get-header-text*)
        (format "~a = λ" (get-short-desc-or* (send this get-cone-root) "<nameless def>"))
      )
      (make-object ui:var-scalar% this (send (make-object style-delta%) set-delta-foreground "Yellow") get-header-text* THING->NOOP NOOP-FALLBACK-EVENT-HANDLER)
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

    (define (get-header-text*)
      (format "~a =" (get-short-desc-or* (send this get-cone-root) "<nameless def>"))
    )

    (define header* (make-object ui:var-scalar% this (send (make-object style-delta%) set-delta-foreground "Yellow") get-header-text* THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))

    (super-make-object cone-root-handle child-spawner! header*)
  ))

  (define ent:atom% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (define db-atom (send this get-cone-root))
      (define prepend
        (cond
          [(is-a? db-atom zinal:db:symbol%%)
            "'"
          ]
          [(is-a? db-atom zinal:db:char%%)
            "#\\"
          ]
          [else
            ""
          ]
        )
      )
      (format "~a~a" prepend (send db-atom get-val))
    )

    (define ui-scalar*
      ; TODO make strings underlined
      (make-object ui:var-scalar% this (send (make-object style-delta%) set-delta-foreground "Orchid") get-text THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:ref% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (get-short-desc-or* (send (send this get-cone-root) get-referable) "<nameless ref>")
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Cyan") get-text THING->NOOP this)
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

    (define (get-header-text*)
      (format "~a =" (get-short-desc-or* (send this get-cone-root) "<?>"))
    )

    (define header* (make-object ui:var-scalar% this (send (make-object style-delta%) set-delta-foreground "Yellow") get-header-text* THING->NOOP NOOP-FALLBACK-EVENT-HANDLER))

    (super-make-object cone-root-handle child-spawner! header*)
  ))

  (define ent:required-param% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (get-short-desc-or* (send this get-cone-root) "<?>")
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Yellow") get-text THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:legacy% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (send (send this get-cone-root) get-name)
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this (send (make-object style-delta%) set-delta-foreground "Cyan") get-text THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  (define ent:unassigned% (class ent%

    (init cone-root-handle child-spawner!)

    (define (get-text)
      (get-short-desc-or* (send this get-cone-root) "<?>")
    )

    (define ui-scalar*
      (make-object ui:var-scalar% this (send (make-object style-delta% 'change-bold) set-delta-foreground "Chocolate") get-text THING->NOOP this)
    )

    (define/override (get-root-ui-item)
      ui-scalar*
    )

    (super-make-object cone-root-handle)
  ))

  ; UI IMPL

  (define ui:item% (class* object% (zinal:ui:item%% event-handler%% fallback-event-handler%%) ; abstract

    (init parent-ent fallback-event-handler)

    (abstract get-event-handler)

    (define parent-ent* parent-ent)
    (define parent* #f)

    (define (select-neighbor-or*! item before? alt)
      (define parent (send item get-parent))
      (define all-siblings (send parent get-children-with-header-internal))
      (define inc (if before? -1 1))
      (define boundary (if before? -1 (length all-siblings)))
      (define this-index
        (list-index (compose1 (curry eq? item) slot/ui-item->ui-item) all-siblings)
      )
      (define neighbor-index (+ this-index inc))
      (if (= neighbor-index boundary)
        (alt)
        (select! (list-ref all-siblings neighbor-index))
      )
    )

    (define (select-next-sibling item)
      (define parent (send item get-parent))
      (when parent
        (select-neighbor-or*! item #f (thunk
          (select-next-sibling parent)
        ))
      )
    )

    (define (handle-left*! thing event)
      (cond
        [(not parent*)
          (void)
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
      #t
    )

    (define (handle-right*! thing event)
      (define (get-all-children)
        (send this get-children-with-header-internal)
      )
      (if (and (is-a? this zinal:ui:list%%) (pair? (get-all-children)))
        (select! (car (get-all-children)))
        (select-next-sibling this)
      )
      #t
    )

    (define (get-child-of-first-vertical-ancestor* item)
      (define parent (send item get-parent))
      (if (and parent (send parent horizontal?))
        (get-child-of-first-vertical-ancestor* parent)
        item
      )
    )

    (define (handle-down*! thing event)
      (select-next-sibling (get-child-of-first-vertical-ancestor* this))
      #t
    )

    (define (handle-up*! thing event)
      (define vert-child (get-child-of-first-vertical-ancestor* this))
      (define vert-parent (send vert-child get-parent))
      (if vert-parent
        (select-neighbor-or*! vert-child #t (thunk
          (select! vert-parent)
        ))
        vert-child
      )
      #t
    )

    (define event-handler*
      (combine-keyname-event-handlers (list
        ; This is a weird pattern. Shouldn't we just use inheritance? The biggest problem
        ; is that constructor params would wind up getting unmanageable. This solution feels kind of
        ; silly and has several questionable aspects but i feel it has less boilerplate and does not
        ; have any flaw as egregious as parameter blowup
        (get-event-handler)
        (make-object keyname-event-handler% (list
          (list handle-left*! '("left" "h"))
          (list handle-right*! '("right" "l"))
          (list handle-up*! '("up" "k"))
          (list handle-down*! '("down" "j"))
        ))
      ))
    )
    (define fallback-event-handler* fallback-event-handler)

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

    (define/public (handle-event!! event)
      (unless (send event-handler* handle-event!! event)
        (send fallback-event-handler* handle-child-event!! event)
      )
    )

    (define/public (handle-child-event!! event)
      (send event-handler* handle-event!! event)
    )

    (super-make-object)
  ))

  (define ui:scalar% (class* ui:item% (zinal:ui:scalar%%) ; abstract

    (init parent-ent style-delta item->event-handler fallback-event-handler)

    (define style-delta* style-delta)
    (define event-handler* (item->event-handler this))

    (define/override (accept visitor [data #f])
      (send visitor visit-scalar this data)
    )

    (define/override (get-event-handler)
      event-handler*
    )

    (define/public (get-style-delta)
      style-delta*
    )

    (abstract get-text)

    (super-make-object parent-ent fallback-event-handler)
  ))

  (define ui:const% (class* ui:scalar% (zinal:ui:const%%)

    (init parent-ent style-delta text item->event-handler fallback-event-handler)

    (define text* text)

    (define/override (accept visitor [data #f])
      (send visitor visit-const this data)
    )

    (define/override (get-text)
      text*
    )

    (super-make-object parent-ent style-delta item->event-handler fallback-event-handler)
  ))

  (define ui:var-scalar% (class* ui:scalar% (zinal:ui:var-scalar%%)

    (init parent-ent style-delta text-getter item->event-handler fallback-event-handler)

    (define text-getter* text-getter)

    (define/override (accept visitor [data #f])
      (send visitor visit-var-scalar this data)
    )

    (define/override (get-text)
      (text-getter*)
    )

    (super-make-object parent-ent style-delta item->event-handler fallback-event-handler)
  ))

  (define ui:list% (class* ui:item% (zinal:ui:list%%)

    (init parent-ent fallback-event-handler [header #f] [separator #f])
    (assert "Header must be an item or #f" (implies header (is-a? header zinal:ui:item%%)))
    (assert "Separator must be a const or #f" (implies separator (is-a? separator zinal:ui:const%%)))

    (define header* header)
    (when header* (send header* set-parent! this))
    (define separator* separator)
    (when separator* (send separator* set-parent! this))
    (define children* '())
    (define horizontal*? #f)

    (define/override (accept visitor [data #f])
      (send visitor visit-list this data)
    )

    (define/override (get-event-handler)
      NOOP
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

    (define/public (set-horizontal! new-value)
      (set! horizontal*? new-value)
    )

    (define/public (insert! index new-child)
      (define before (take children* index))
      (define after (drop children* index))
      (set! children* (append before (cons new-child after)))
      (when (is-a? new-child zinal:ui:item%%) (send new-child set-parent! this))
      (select! new-child)
    )

    (define/public (remove! child)
      (define index (get-child-index child))
      (assert "cannot remove child that's not even in the list" index)
      (set! children* (remq child children*))
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

    (super-make-object parent-ent fallback-event-handler)
  ))

  (define ui:slotted-list% (class ui:list% ; abstract

    (init parent-ent fallback-event-handler [header #f] [separator #f])

    (abstract get-visible-referables-for-slot)

    (define/public (child-slot->event-handler slot)
      (combine-keyname-event-handlers (list
        (create-replace-handler slot)
        (create-unassign-handler slot)
      ))
    )

    (define/public (reassign-slot!! slot [new-handle-initializer!! identity])
      (define intermediate-handle (send (slot->db-handle slot) unassign!!))
      (define new-handle (new-handle-initializer!! intermediate-handle))
      (spawn-entity*! slot new-handle this)
      (select! slot)
    )

    (define/public (create-unassign-handler slot)
      (create-simple-event-handler "d"
        (lambda (data event)
          (when (unassignable? (slot->db-handle slot)) (reassign-slot!! slot))
          #t
        )
      )
    )

    (define/public (create-replace-handler slot)
      (define (interaction-function)
        (and
          (unassignable? (slot->db-handle slot))
          (request-new-item-creator (get-visible-referables-for-slot slot))
        )
      )
      (create-interaction-dependent-event-handler interaction-function (lambda (nhi) (reassign-slot!! slot nhi)) "s")
    )

    (super-make-object parent-ent fallback-event-handler header separator)
  ))

  (define ui:dynamic-slotted-list% (class ui:slotted-list% ; abstract

    (init parent-ent fallback-event-handler child-spawner! [header #f] [separator #f])

    (abstract db-insert!!)
    (abstract db-can-remove?)
    (abstract db-remove!!)
    (abstract db-get-items)
    (abstract db-get-list-handle)

    (define/override (get-event-handler)
      (combine-keyname-event-handlers (list (create-insert-start-handler) (create-insert-end-handler)))
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
      (get-visible-referables-for-hypothetical-index (send this get-child-index slot))
    )

    (define/public (remove-slot!! slot)
      (db-remove!! (send this get-child-index slot))
      (send this remove! slot)
    )

    (define/public (create-insert-start-handler [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler (const 0) "I" new-item-creator)
    )

    (define/public (create-insert-end-handler [new-item-creator request-new-item-creator])
      (create-typical-insert-slot-handler (thunk (length (send this get-children-internal))) "A" new-item-creator)
    )

    (define/public (create-insert-before-handler slot [new-item-creator request-new-item-creator])
      (define (get-index) (send this get-child-index slot))
      (create-typical-insert-slot-handler get-index "i" new-item-creator)
    )

    (define/public (create-insert-after-handler slot [new-item-creator request-new-item-creator])
      (define (get-index) (add1 (send this get-child-index slot)))
      (create-typical-insert-slot-handler get-index "a" new-item-creator)
    )

    (define/public (create-insert-todo-handler slot)
      (define (get-index) (add1 (send this get-child-index slot)))
      (create-typical-insert-slot-handler get-index "o" new-unassigned-creator)
    )

    (define/public (create-remove-handler slot)
      (create-simple-event-handler "d"
        (lambda (data event)
          (when (db-can-remove? (send this get-child-index slot)) (remove-slot!! slot))
          #t
        )
      )
    )

    (define/public (create-unassign-or-remove-handler slot)
      (create-simple-event-handler "d"
        (lambda (data event)
          (cond
            [(db-can-remove? (send this get-child-index slot)) (remove-slot!! slot)]
            [(unassignable? (slot->db-handle slot)) (send this reassign-slot!! slot)]
          )
          #t
        )
      )
    )

    (define (get-visible-referables-for-hypothetical-index index)
      (if (zero? index)
        (send (db-get-list-handle) get-visible-referables-underneath)
        (send (list-ref (db-get-items) (sub1 index)) get-visible-referables-after)
      )
    )

    (define (create-insert-slot-handler get-index interaction->new-handle-initializer!! keyname)
      (define (result-handler new-handle-initializer!!)
        (define index (get-index))
        (define intermediate-handle (db-insert!! index))
        (define new-handle (new-handle-initializer!! intermediate-handle))
        (insert-new-slot index new-handle)
      )
      (create-interaction-dependent-event-handler
        interaction->new-handle-initializer!!
        result-handler
        keyname
      )
    )

    (define (create-typical-insert-slot-handler get-index keyname [new-item-creator request-new-item-creator])
      (define interaction->new-handle-initializer!!
        (thunk (new-item-creator (get-visible-referables-for-hypothetical-index (get-index))))
      )
      (create-insert-slot-handler get-index interaction->new-handle-initializer!! keyname)
    )

    (define (insert-new-slot index slot-handle [child-spawner*! spawn-entity*!])
      (define new-slot (make-object slot% child-slot->event-handler* this))
      (child-spawner*! new-slot slot-handle this)
      (send this insert! index new-slot)
    )

    (define (child-slot->event-handler* slot)
      (child-slot->event-handler slot)
    )

    (super-make-object parent-ent fallback-event-handler header separator)

    (begin
      (define handles (list->vector (db-get-items)))
      (build-list
        (vector-length handles)
        (lambda (i)
          (insert-new-slot i (vector-ref handles i) child-spawner!)
        )
      )
    )
  ))

  ; HELPER FUNCTIONS

  (define (issue-warning title message)
    (message-box title message #f '(ok caution))
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
    (define (handler-function data event)
      (define interaction-result (interaction-function))
      (when interaction-result (result-handler interaction-result))
      #t
    )
    (make-object keyname-event-handler% (list (list handler-function (list keyname))))
  )

  (define (create-simple-event-handler keyname handler-function)
    (make-object keyname-event-handler% (list (list handler-function (list keyname))))
  )

  (define (spawn-or-reassign-entity*! slot cone-root-handle ui-parent existing-slots)
    (define existing-slot
      (findf (lambda (s) (send cone-root-handle equals? (slot->db-handle s))) existing-slots)
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
      [else ent:list%]
    )
  )

  (define (parse-entity*! db-handle)
    (send db-handle accept (make-object (class zinal:db:element-visitor% (super-make-object)

      (define/override (visit-element e meh)
        (error 'parse-entity*! "Cannot parse entity for mysterious db handle")
      )

      (define/override (visit-reference db-ref-handle meh)
        ent:ref%
      )

      (define/override (visit-atom db-atom-handle meh)
        ent:atom%
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
        (if (cons? (send db-list-handle get-items))
          (parse-non-nil-list-entity*! db-list-handle)
          ent:list%
        )
      )

      (define/override (visit-def db-def-handle meh)
        (define def-expr (send db-def-handle get-expr))
        (if (is-a? def-expr zinal:db:lambda%%)
          ent:func-def%
          ent:def%
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
