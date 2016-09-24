#lang racket

(require racket/gui/base)
; for hash-union
(require racket/hash)

(require "misc.rkt")
(require "db.rkt")
(require "sql-db.rkt")
(require "hier-gui.rkt")
(require "transpile.rkt")

; GENERIC HELPERS

(define (get-short-desc-or* db-describable-handle alt)
  (or (send db-describable-handle get-short-desc) alt)
)

; ENTS

(define ent:manager%
  (class object%

    (init db gui-model-manager)

    (define (spawn-entity*! slot db-handle gui-model-parent index)
      (define new-ent
        (new (parse-entity*! db-handle)
          [slot slot]
          [subroot-handle db-handle]
          [subroot-model-parent gui-model-parent]
          [subroot-model-index index]
        )
      )
      (send slot set-ent! new-ent)
      new-ent
    )

    (define (parse-entity*! db-handle)
      (send db-handle accept (new (class veme:db:element-visitor% (super-new)

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

        (define/override (visit-list db-list-handle meh)
          (if (cons? (send db-list-handle get-items))
            (parse-non-nil-list-entity*! db-list-handle)
            ent:list%
          )
        )

        (define/override (visit-def db-def-handle meh)
          (define def-expr (send db-def-handle get-expr))
          (if (is-a? def-expr veme:db:lambda%%)
            ent:func-def%
            ent:def%
          )
        )

        (define/override (visit-legacy-link db-legacy-link-handle meh)
          ent:legacy-link%
        )

        (define/override (visit-unassigned db-unassigned-handle meh)
          ent:unassigned%
        )
      )))
    )

    (define (parse-non-nil-list-entity*! db-list-handle)
      (define items (send db-list-handle get-items))
      (define first-item (first items))
      (cond
        [(is-a? first-item veme:db:legacy-link%%)
          (if
            (and
              (= 2 (length items))
              (standard-with-name*? first-item "quote")
              (is-a? (second items) veme:db:list%%)
            )
            ent:quoted-list%
            ; TODO we should probably have some sort of quote-context to make this an ordinary
            ; list when underneath something quoted ... or something
            ent:invokation%
          )
        ]
        [else ent:list%]
      )
    )

    ; ENTITY CLASSES

    ; TODO - probably, each slot should hold the gui model parent and index, so that they can easily
    ; perform operations on the model
    (define slot%
      (class object%

        (init event-handler!!)

        (define/public (get-event-handler)
          (assert-valid*)
          event-handler*!!
        )

        (define/public (get-ent)
          (assert-valid*)
          ent*
        )

        (define/public (set-ent! new-ent)
          (set! ent* new-ent)
        )

        (define/public (handle-event!! key-event)
          (assert-valid*)
          (event-handler*!! key-event)
        )

        (define (assert-valid*)
          (assert "ent must be initialized before using" ent*)
        )

        (define ent* #f)
        (define event-handler*!! event-handler!!)

        (super-new)
      )
    )

    (define indexed-slot%
      (class slot%

        (init index)

        (define/override (handle-event!! key-event)
          ((send this get-event-handler) index* key-event)
        )

        (define/public (set-index! new-index)
          (set! index* new-index)
        )

        (define/public (get-index)
          index*
        )

        (define index* index)

        (super-new)
      )
    )

    (define ent%
      (class object% ; abstract

        (init slot subroot-handle subroot-model-parent subroot-model-index)

        (define/public (get-slot)
          slot*
        )

        (define/public (get-subroot-handle)
          subroot-handle*
        )

        ; Overrides of this method should call super for events they don't want to handle themselves
        ; Any events unhandled by subclasses, will be handled by the slot.
        (define/public (handle-event!! key-event)
          (send (get-slot) handle-event!! key-event)
        )

        (define/public (get-event-handler)
          (lambda (key-event)
            (send this handle-event!! key-event)
          )
        )

        (abstract init!)
        (abstract select!)
        ; TODO this doesn't really belong in the interface. it's just some convenience bullshit to
        ; make it as easy as possible to refresh short text in the current fairly flawed framework
        (abstract refresh-text!)

        (define slot* slot)
        (define subroot-handle* subroot-handle)

        (super-new)

        (init! subroot-model-parent subroot-model-index)
      )
    )

    (define scalar-ent%
      (class ent% ; abstract

        (define/override (init! subroot-model-parent subroot-model-index)
          (set! subroot-model* (send subroot-model-parent insert-item! subroot-model-index (send this get-event-handler)))
          (refresh-text!)
        )

        (define/override (select!)
          (send subroot-model* select!)
        )

        (define/override (refresh-text!)
          (send subroot-model* set-short-text! (get-short-text))
        )

        (define/public (get-subroot-model)
          subroot-model*
        )

        (define/public (get-short-text)
          (get-short-text* (send this get-subroot-handle))
        )

        (define subroot-model* #f)

        (super-new)
      )
    )

    (define compound-ent%
      (class ent% ; abstract

        (define/override (init! subroot-model-parent subroot-model-index)
          (set! subroot-model*
            (send subroot-model-parent insert-list! subroot-model-index (send this get-event-handler))
          )
          (refresh-text!)
          (send subroot-model* open!)
        )

        (define/override (select!)
          (send subroot-model* select!)
        )

        (define/override (refresh-text!)
          (send subroot-model* set-open-text! (get-open-text))
          (send subroot-model* set-closed-text! (get-closed-text))
        )

        (define/public (get-subroot-model)
          subroot-model*
        )

        (abstract get-open-text)
        (abstract get-closed-text)

        (define subroot-model* #f)

        (super-new)
      )
    )

    (define ent:basic-list%
      (class compound-ent% ; abstract

        (define/override (handle-event!! key-event)
          (case (send key-event get-key-code)
            [(#\I) (maybe-add-item-to-list*!! 0)]
            [(#\A) (maybe-add-item-to-list*!!)]
            ; TODO non-default list event handling
            [else (super handle-event!! key-event)]
          )
        )

        (define/override (refresh-text!)
          (super refresh-text!)
          (for-each
            (lambda (s)
              (send (send s get-ent) refresh-text!)
            )
            slots*
          )
        )

        ; TODO currently we only need this to support the questionable refresh-text! method for root,
        ; so we may be able to delete this in the future after this stuff is better designed
        (define/public (get-child-ents)
          (map (lambda (s) (send s get-ent)) slots*)
        )

        (define/public (get-basic-list-handle)
          (send this get-subroot-handle)
        )

        (define/public (get-item-handles)
          (send (get-basic-list-handle) get-node-children)
        )

        (abstract db-insert!!)

        (define (handle-slot-event*!! index key-event)
          (case (send key-event get-key-code)
            [(#\o) (maybe-add-item-to-list*!! (add1 index) new-unassigned-creator)]
            [(#\i) (maybe-add-item-to-list*!! index)]
            [(#\I) (maybe-add-item-to-list*!! 0)]
            [(#\a) (maybe-add-item-to-list*!! (add1 index))]
            [(#\A) (maybe-add-item-to-list*!!)]
            [(#\() (maybe-add-item-to-list*!! (add1 index) new-list-creator)]
            [(#\s) (maybe-replace-item*!! index)]
          )
        )

        (define (maybe-add-item-to-list*!! [index (length (get-item-handles))] [creator*!! #f])
          ; TODO current doesn't work for func-def
          (define visible-referables (get-visible-referables-for-hypothetical-index* index))
          (define creator!!
            (if creator*!!
              (creator*!! visible-referables)
              (request-new-item-creator visible-referables)
            )
          )
          (when creator!!
            (define new-unassigned-handle (db-insert!! index))
            (define new-db-handle (creator!! new-unassigned-handle))
            (select-and-refresh*! (insert-slot*! new-db-handle index))
          )
        )

        (define (maybe-replace-item*!! index)
          ; TODO kinda silly to use the slot to get the index, then use the index to get the slot.
          ; Need to rethink and refactor slot system
          (define slot (list-ref slots* index))
          (define handle (send (send slot get-ent) get-subroot-handle))
          ; TODO we shouldn't really be checking the type of the handle like this
          (when (is-a? handle veme:db:unassigned%%)
            (define visible-referables (get-visible-referables-for-hypothetical-index* index))
            (define creator!! (request-new-item-creator visible-referables))
            (when creator!!
              (define new-handle (creator!! handle))
              (respawn-slot*! slot new-handle)
              (select-and-refresh*! slot)
            )
          )
        )

        (define (get-visible-referables-for-hypothetical-index* index)
          (if (zero? index)
            (send (get-basic-list-handle) get-visible-referables-underneath)
            (send (send (send (list-ref slots* (sub1 index)) get-ent) get-subroot-handle) get-visible-referables-after)
          )
        )

        (define (select-and-refresh*! slot)
          (send root* refresh-text!)
          (send (send slot get-ent) select!)
        )

        (define (insert-slot*! new-handle ind)
          (define before (take slots* ind))
          (define after (drop slots* ind))
          (define slot (new indexed-slot% [event-handler!! handle-slot-event*!!] [index ind]))
          (spawn-entity*! slot new-handle (send this get-subroot-model) ind)
          (set! slots* (append before (cons slot after)))
          (when (cons? after)
            (build-list (length slots*) (lambda (i) (send (list-ref slots* i) set-index! i)))
          )
          slot
        )

        ; Do not call this unless the db data for the old handle has already been deleted
        (define (respawn-slot*! slot new-handle)
          (define ind (send slot get-index))
          (send (send this get-subroot-model) remove! ind)
          (spawn-entity*! slot new-handle (send this get-subroot-model) ind)
        )

        ; Do not call this unless the db data has already been deleted
        ; TODO this should probably just issue a delete to the child, which would then delete its own db data
        (define (delete-slot*! slot)
          (define ind (send slot get-index))
          (define before (take slots* ind))
          (define after (drop slots* (add1 ind)))
          (set! slots* (append before after))
          (send (send this get-subroot-model) remove! ind)
        )

        (define slots* '())

        (super-new)

        (begin
          (define handles (list->vector (get-item-handles)))
          (build-list
            (vector-length handles)
            (lambda (i)
              (insert-slot*! (vector-ref handles i) i)
            )
          )
        )
      )
    )

    (define ent:ref%
      (class scalar-ent%
        (super-new)
      )
    )

    (define ent:atom%
      (class scalar-ent%
        (super-new)
      )
    )

    (define ent:lambda%
      (class ent:basic-list%

        (define/override (get-open-text)
          (get-open-lambda-text* (send this get-subroot-handle))
        )

        (define/override (get-closed-text)
          (get-closed-lambda-text* (send this get-subroot-handle))
        )

        (define/override (db-insert!! index)
          (send (send this get-subroot-handle) insert-into-body!! index)
        )

        (super-new)
      )
    )

    (define ent:list%
      (class ent:basic-list%

        (define/override (get-open-text)
          (get-short-desc-or** "(")
        )

        (define/override (get-closed-text)
          (get-short-text* (send this get-subroot-handle))
        )

        (define/override (db-insert!! index)
          (send (send this get-subroot-handle) insert!! index)
        )

        (define (get-short-desc-or** alt)
          (get-short-desc-or*
            (send this get-subroot-handle)
            (if (cons? (send this get-item-handles))
              alt
              "()"
            )
          )
        )

        (super-new)
      )
    )

    (define ent:invokation% ent:list%)

    (define ent:def%
      (class compound-ent%

        (define/override (get-open-text)
          (get-short-text* (send this get-subroot-handle))
        )

        (define/override (get-closed-text)
          (format "~a ~a" (get-open-text) (get-short-text* (get-expr-handle*)))
        )

        (define/override (refresh-text!)
          (super refresh-text!)
          (when expr-slot*
            (send (send expr-slot* get-ent) refresh-text!)
          )
        )

        (define (handle-expr-event*!! key-event)
          (case (send key-event get-key-code)
            [(#\s) (maybe-replace-item*!!)]
          )
        )

        (define (maybe-replace-item*!!)
          ; TODO this looks a whole lot like the one for lists -_-
          (define handle (send (send expr-slot* get-ent) get-subroot-handle))
          ; TODO we shouldn't really be checking the type of the handle like this
          (when (is-a? handle veme:db:unassigned%%)
            (define visible-referables (send (send this get-subroot-handle) get-visible-referables-underneath))
            (define creator!! (request-new-item-creator visible-referables))
            (when creator!!
              (creator!! handle)
              (respawn-expr*!)
              (send root* refresh-text!)
              (send (send expr-slot* get-ent) select!)
            )
          )
        )

        (define (respawn-expr*!)
          (send (send this get-subroot-model) remove! 0)
          (spawn-expr*!)
        )

        (define (spawn-expr*!)
          (spawn-entity*! expr-slot* (get-expr-handle*) (send this get-subroot-model) 0)
        )

        (define (get-expr-handle*)
          (send (send this get-subroot-handle) get-expr)
        )

        ; TODO super hacky
        (define expr-slot* #f)

        (super-new)

        (set! expr-slot* (new slot% [event-handler!! handle-expr-event*!!]))

        (spawn-expr*!)
      )
    )

    (define ent:legacy-link%
      (class scalar-ent%
        (super-new)
      )
    )

    (define ent:unassigned%
      (class scalar-ent%
        (super-new)
      )
    )

    (define ent:quoted-list%
      (class ent:basic-list%

        (define/override (get-open-text)
          (get-short-desc-or** "'(")
        )

        (define/override (get-closed-text)
          (format "'~a" (get-short-text* (get-basic-list-handle)))
        )

        (define/override (get-basic-list-handle)
          (second (send (send this get-subroot-handle) get-items))
        )

        (define/override (db-insert!! index)
          (send (get-basic-list-handle) insert!! index)
        )

        (define (get-short-desc-or** alt)
          (get-short-desc-or*
            ; TODO which short desc should we use in cases like this?
            (get-basic-list-handle)
            (if (cons? (send this get-item-handles))
              alt
              "'()"
            )
          )
        )

        (super-new)
      )
    )

    (define ent:func-def%
      (class ent:basic-list%

        (define/override (get-open-text)
          (get-text* get-open-lambda-text*)
        )

        (define/override (get-closed-text)
          (get-text* get-closed-lambda-text*)
        )

        (define/override (get-basic-list-handle)
          (send (send this get-subroot-handle) get-expr)
        )

        (define/override (db-insert!! index)
          (send (get-basic-list-handle) insert-into-body!! index)
        )

        (define (get-text* lambda-text-getter)
          (format "~a ~a"
            (get-short-text* (send this get-subroot-handle))
            (lambda-text-getter (get-basic-list-handle))
          )
        )

        (super-new)
      )
    )

    (define ent:root%
      (class ent:list%

        (init root-handle)

        ; We really shouldn't ever be overriding init! like this, but i don't see a less awkward solution
        (define/override (init! subroot-model-parent subroot-model-index)
          (assert "Don't create an ent:root% with a real gui model parent" (not subroot-model-parent))
          (set! root-model*
            (send gui-model-manager* create-root-list-model! (send this get-event-handler))
          )
          (refresh-text!)
          (send root-model* open!)
        )

        (define/override (refresh-text!)
          (send root-model* set-open-text! (send this get-open-text))
          (send root-model* set-closed-text! (send this get-closed-text))
          (for-each
            (lambda (e)
              (send e refresh-text!)
            )
            (send this get-child-ents)
          )
        )

        (define/override (get-subroot-model)
          root-model*
        )

        (define root-model* #f)
        (define dummy-slot* (new slot% [event-handler!! NOOP]))
        (send dummy-slot* set-ent! this)

        (super-new
          [slot dummy-slot*]
          [subroot-handle root-handle]
          [subroot-model-parent #f]
          [subroot-model-index 0]
        )
      )
    )

    ; HELPER FUNCTIONS

    (define (get-params-text* db-lambda-handle)
      (string-join (map (curryr get-short-desc-or* "¯\\_(ツ)_/¯") (send db-lambda-handle get-params)) ", ")
    )

    (define (get-open-lambda-text* lambda-handle)
      (format "~a:" (get-short-text* lambda-handle))
    )

    (define (get-closed-lambda-text* lambda-handle)
      (format "~a -> ..." (get-short-text* lambda-handle))
    )

    (define (get-short-text* db-handle [in-list? #f])
      ; TODO doing this recursively via handles is probably wrong. The recursion aspect should recurse on ents.
      ; But for now it's simple and it works, so we're going to keep, until it starts sucking, or until we
      ; refactor ents in a big way, whichever comes first
      (send db-handle accept (new (class veme:db:element-visitor% (super-new)
        (define/override (visit-element e meh)
          (error 'get-short-text* "Unhandled element")
        )

        (define/override (visit-reference r meh)
          (get-short-desc-or* (send r get-referable) "<nameless ref>")
        )

        (define/override (visit-atom a meh)
          (~a (send a get-val))
        )

        (define/override (visit-lambda l meh)
          (if in-list?
            (format "λ~a" (get-short-desc-or* l "..."))
            (format "λ ~a (~a)" (get-short-desc-or* l "") (get-params-text* l))
          )
        )

        (define/override (visit-list l meh)
          (get-short-desc-or* l
            (if in-list?
              (if (cons? (send l get-items)) "(...)" "()")
              (format "(~a)" (string-join (map (curryr get-short-text* #t) (send l get-items)) " "))
            )
          )
        )

        (define/override (visit-def d meh)
          (define short-desc (get-short-desc-or* d "<nameless def>"))
          (if in-list?
            (format "{def ~a}" short-desc)
            (format "~a =" short-desc)
          )
        )

        (define/override (visit-legacy-link l meh)
          (send l get-name)
        )

        (define/override (visit-unassigned u meh)
          (get-short-desc-or* u "<?>")
        )
      )))
    )

    (define (standard*? db-legacy-link-handle)
      (not (send db-legacy-link-handle get-library))
    )

    (define (standard-with-name*? db-legacy-link-handle name)
      (and (standard*? db-legacy-link-handle) (equal? name (send db-legacy-link-handle get-name)))
    )

    (define db* db)
    (define gui-model-manager* gui-model-manager)
    (define NOOP (const #f))
    (define root* (new ent:root% [root-handle (send db* get-root)]))

    (super-new)
  )
)

; GUI

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
            [(#\j)
              (reset-chars!)
              (unless (= current-selection (sub1 num-choices*))
                (send this set-selection (add1 current-selection))
              )
              #t
            ]
            [(#\k)
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
; (list-of veme:db:referable%%) => (veme:db:unassigned%% => veme:db:element%%) OR #f
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
      #:validate (const #t)
    )
  )
  (if result
    (lambda (unassigned) (send unassigned assign-def!! result))
    #f
  )
)

(define (new-lambda-creator visible-referables)
  (define validate-natural (compose1 (conjoin integer? non-negative?) string->number))
  (define arity-string
    (get-text-from-user
      "Enter the new function's arity"
      "How many params does it have? Seriously."
      #f
      ""
      '(disallow-invalid)
      #:validate validate-natural
    )
  )
  (cond
    [(and arity-string (validate-natural arity-string))
      (define arity (string->number arity-string))
      (define param-short-names
        (build-list arity (lambda (p)
          (get-text-from-user
            (format "Enter the short descriptor of the ~ath parameter" p)
            (format "Param ~a is:" p)
            #:validate (const #t)
          )
        ))
      )
      (lambda (unassigned)
        (define lambda-handle (send unassigned assign-lambda!! arity))
        (define params (send lambda-handle get-params))
        (map (lambda (p n) (send p set-short-desc!! n)) params param-short-names)
        lambda-handle
      )
    ]
    [else #f]
  )
)

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
          (if (is-a? chosen-handle veme:db:param%%)
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

; Returns #f to signify no action is to be taken (i.e. the user cancels the dialog)
; Returns a function that takes an unassigned db handle and will assign it to something else, returning the new handle
; The returned creator is not allowed to fail. A failure of this function should return #f instead of a creator
(define (request-new-item-creator visible-referables)
  (define friendly-types (hash-keys FRIENDLY-TYPE->CREATOR))
  (define choice (get-choice-from-user "Choose the new node's type:" "Choose the node's type:" friendly-types))
  (if choice
    ((hash-ref FRIENDLY-TYPE->CREATOR (list-ref friendly-types choice)) visible-referables)
    #f
  )
)

; GUI CONSTANTS

(define FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "list" new-list-creator
  "number" new-number-creator
  "character" new-character-creator
  "string" new-string-creator
  "boolean" new-boolean-creator
))

(define FRIENDLY-TYPE->CREATOR (hash-union FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "define" new-define-creator
  "lambda" new-lambda-creator
  "reference" new-value-read-creator
  "legacy" new-legacy-creator
  "TODO" new-unassigned-creator
)))

; PROGRAM

(define main-window (new frame% [label "Veme"]))
(define main-gui-manager (new veme:gui-manager% [parent main-window]))
(define main-db (new veme:sql-db% [filename "junk.db"]))
(define main-ent-manager (new ent:manager% [db main-db] [gui-model-manager main-gui-manager]))
(send main-window show #t)
(send main-window maximize #t)
(send main-gui-manager focus)

; (transpile main-db)
