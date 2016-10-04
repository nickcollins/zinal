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

(define (issue-warning title message)
  (message-box title message #f '(ok caution))
)

(define (if-unassignable handle action)
  (cond
    [(send handle can-unassign?)
      (action)
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

; ENTS

(define ent:manager%
  (class object%

    (init db gui-model-manager)

    (define (spawn-entity*! slot db-handle gui-model-parent index)
      (create-entity*! (parse-entity*! db-handle) slot db-handle gui-model-parent index)
    )

    (define (create-entity*! ent-class slot db-handle gui-model-parent index)
      (define new-ent
        (new ent-class
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
      (send db-handle accept (new (class zinal:db:element-visitor% (super-new)

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
        [(is-a? first-item zinal:db:legacy-link%%)
          (if
            (and
              (= 2 (length items))
              (standard-with-name*? first-item "quote")
              (is-a? (second items) zinal:db:list%%)
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

        (define/public (get-subroot-handle)
          subroot-handle*
        )

        ; Overrides of this method should call super for events they don't want to handle themselves
        ; Any events unhandled by subclasses, will be handled by the slot.
        (define/public (handle-event!! key-event)
          (send slot* handle-event!! key-event)
        )

        (define/public (get-event-handler)
          (lambda (key-event)
            (send this handle-event!! key-event)
          )
        )

        (define/public (select!)
          (send (get-subroot-model) select!)
        )

        (abstract init!)
        (abstract get-subroot-model)
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

        (define/override (refresh-text!)
          (send subroot-model* set-short-text! (get-short-text))
        )

        (define/override (get-subroot-model)
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

        (define/override (refresh-text!)
          (send subroot-model* set-open-text! (get-open-text))
          (send subroot-model* set-closed-text! (get-closed-text))
        )

        (define/override (get-subroot-model)
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
            [(#\A) (maybe-add-item*!!)]
            [(#\I) (maybe-add-item*!! 0)]
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
          (send (get-basic-list-handle) get-children)
        )

        (define/public (handle-child-event!! index key-event)
          (case (send key-event get-key-code)
            [(#\a) (maybe-add-item*!! (add1 index))]
            [(#\A) (maybe-add-item*!!)]
            [(#\d) (maybe-unassign-or-remove-item*!! index)]
            [(#\i) (maybe-add-item*!! index)]
            [(#\I) (maybe-add-item*!! 0)]
            [(#\o) (maybe-add-item*!! (add1 index) new-unassigned-creator)]
            [(#\s) (maybe-replace-item*!! index)]
            [(#\() (maybe-add-item*!! (add1 index) new-list-creator)]
          )
        )

        (define/public (insert-at-index! new-handle ind)
          (define before (take slots* ind))
          (define after (drop slots* ind))
          (define slot (new indexed-slot% [event-handler!! (lambda (i k) (handle-child-event!! i k))] [index ind]))
          (spawn-entity*! slot new-handle (send this get-subroot-model) ind)
          (set! slots* (append before (cons slot after)))
          (when (cons? after) (reset-slot-indices*!))
          slot
        )

        (define/public (delete-index! ind)
          (define slot (get-slot* ind))
          ; TODO just make sure this whole ent system doesn't last very long
          ; (assert "Can only delete unassigned slots" (is-a? (send slot get-ent) ent:unassigned%))
          (db-remove!! ind)
          (define before (take slots* ind))
          (define after (drop slots* (add1 ind)))
          (set! slots* (append before after))
          (when (cons? after) (reset-slot-indices*!))
          (send (send this get-subroot-model) remove! ind)
        )

        (define/public (select-index! ind)
          (select*! (get-slot* ind))
        )

        (abstract db-insert!!)
        (abstract db-remove!!)

        (define (maybe-add-item*!! [index (length (get-item-handles))] [creator-getter #f])
          (maybe-create*!!
            (get-visible-referables-for-hypothetical-index* index)
            (thunk (db-insert!! index))
            (lambda (new-handle)
              (select*! (insert-at-index! new-handle index))
            )
            creator-getter
          )
        )

        (define (maybe-unassign-or-remove-item*!! index)
          (define slot (get-slot* index))
          (define ent (send slot get-ent))
          (cond
            [(is-a? ent ent:unassigned%)
              (delete-index! index)
              (define num-slots (length slots*))
              (cond
                [(< index num-slots)
                  (send (send (get-slot* index) get-ent) select!)
                ]
                [(> num-slots 0)
                  (send (send (get-slot* (sub1 num-slots)) get-ent) select!)
                ]
                [else
                  (send this select!)
                ]
              )
              (refresh-all-text*!)
            ]
            [else
              (define handle (send ent get-subroot-handle))
              (if-unassignable handle (thunk
                (respawn-slot*! slot (send handle unassign!!))
                (select*! slot)
                (refresh-all-text*!)
              ))
            ]
          )
        )

        (define (maybe-replace-item*!! index)
          ; TODO kinda silly to use the slot to get the index, then use the index to get the slot.
          ; Need to rethink and refactor slot system
          (define slot (get-slot* index))
          (define handle (send (send slot get-ent) get-subroot-handle))
          (maybe-replace*!!
            (get-visible-referables-for-hypothetical-index* index)
            handle
            (lambda (new-handle)
              (respawn-slot*! slot new-handle)
              (select*! slot)
            )
          )
        )

        (define (get-visible-referables-for-hypothetical-index* index)
          (if (zero? index)
            (send (get-basic-list-handle) get-visible-referables-underneath)
            (send (send (send (get-slot* (sub1 index)) get-ent) get-subroot-handle) get-visible-referables-after)
          )
        )

        (define (select*! slot)
          (send (send slot get-ent) select!)
        )

        ; Do not call this unless the db data for the old handle has already been deleted
        (define (respawn-slot*! slot new-handle)
          (define ind (send slot get-index))
          (send (send this get-subroot-model) remove! ind)
          (spawn-entity*! slot new-handle (send this get-subroot-model) ind)
        )

        (define (get-slot* index)
          (list-ref slots* index)
        )

        (define (reset-slot-indices*!)
          (build-list (length slots*) (lambda (i) (send (get-slot* i) set-index! i)))
        )

        (define slots* '())

        (super-new)

        (begin
          (define handles (list->vector (get-item-handles)))
          (build-list
            (vector-length handles)
            (lambda (i)
              (insert-at-index! (vector-ref handles i) i)
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
      (class compound-ent%

        (define body*%
          (class ent:basic-list%

            (define/override (get-open-text)
              "("
            )

            (define/override (get-closed-text)
              "(...)"
            )

            (define/override (db-insert!! index)
              (send (send this get-subroot-handle) insert-into-body!! index)
            )

            (define/override (db-remove!! index)
              (send (send this get-subroot-handle) remove-from-body!! index)
            )

            (define/override (get-item-handles)
              (send (send this get-subroot-handle) get-body)
            )

            (super-new)
          )
        )

        (define params*%
          (class ent:basic-list% ; abstract

            (define/override (get-open-text)
              "("
            )

            (define/override (get-closed-text)
              (get-params-text* (send this get-item-handles))
            )

            (define/override (handle-event!! key-event)
              (case (send key-event get-key-code)
                [(#\A) (maybe-add-param*!!)]
                [(#\I) (maybe-add-param*!! 0)]
              )
            )

            (define/override (handle-child-event!! index key-event)
              (case (send key-event get-key-code)
                [(#\a) (maybe-add-param*!! (add1 index))]
                [(#\A) (maybe-add-param*!!)]
                [(#\d) (maybe-remove-param*!! index)]
                [(#\i) (maybe-add-param*!! index)]
                [(#\I) (maybe-add-param*!! 0)]
              )
            )

            (abstract get-required/optional-string)
            (abstract can-remove-param?)

            (define (maybe-add-param*!! [index (length (send this get-item-handles))])
              (define short-desc
                (get-text-from-user
                  (format "Enter short descriptor for ~ath ~a param" index (get-required/optional-string))
                  "A short descriptor, one or a few words, to identify this param"
                  #:validate non-empty-string?
                )
              )
              (when (and short-desc (non-empty-string? short-desc))
                (define new-handle (send this db-insert!! index short-desc))
                (send this insert-at-index! new-handle index)
                (send this select-index! index)
                (refresh-all-text*!)
              )
            )

            (define (maybe-remove-param*!! index)
              (when (can-remove-param? index)
                (send this delete-index! index)
                (define params (send this get-item-handles))
                (define num-params (length params))
                (cond
                  [(< index num-params)
                    (send this select-index! index)
                  ]
                  [(> num-params 0)
                    (send this select-index! (sub1 num-params))
                  ]
                  [else
                    (send this select!)
                  ]
                )
                (refresh-all-text*!)
              )
            )

            (super-new)
          )
        )

        (define req-params*%
          (class params*%

            (define/override (db-insert!! index [short-desc #f])
              (send (send this get-subroot-handle) insert-required-param!! index short-desc)
            )

            (define/override (db-remove!! index)
              (send (send this get-subroot-handle) remove-required-param!! index)
            )

            (define/override (get-item-handles)
              (send (send this get-subroot-handle) get-required-params)
            )

            (define/override (get-required/optional-string)
              "required"
            )

            (define/override (can-remove-param? index)
              (send (send this get-subroot-handle) can-remove-required-param? index)
            )

            (super-new)
          )
        )

        (define opt-params*%
          (class params*%

            (define/override (db-insert!! index [short-desc #f])
              (send (send this get-subroot-handle) insert-optional-param!! index short-desc)
            )

            (define/override (db-remove!! index)
              (send (send this get-subroot-handle) remove-optional-param!! index)
            )

            (define/override (get-item-handles)
              (send (send this get-subroot-handle) get-optional-params)
            )

            (define/override (get-required/optional-string)
              "optional"
            )

            (define/override (can-remove-param? index)
              (send (send this get-subroot-handle) can-remove-optional-param? index)
            )

            (super-new)
          )
        )

        (define/override (get-open-text)
          (get-open-lambda-text* (send this get-subroot-handle))
        )

        (define/override (get-closed-text)
          (get-closed-lambda-text* (send this get-subroot-handle))
        )

        (define/override (refresh-text!)
          (super refresh-text!)
          (when req-params*
            (send req-params* refresh-text!)
          )
          (when opt-params*
            (send opt-params* refresh-text!)
          )
          (when body*
            (send body* refresh-text!)
          )
        )

        ; TODO super hacky
        (define req-params* #f)
        (define opt-params* #f)
        (define body* #f)

        (super-new)

        ; It's a bit hacky to have the component lists be their own ents, because subroot-handle is supposed
        ; to be the root of the corresponding subtree, but the db api does not expose a root for the params
        ; or body sublists. So we just give it the lambda handle, which is a bit of a lie.
        (set! req-params*
          (create-entity*! req-params*% (new slot% [event-handler!! NOOP]) (send this get-subroot-handle) (send this get-subroot-model) 0)
        )
        (set! opt-params*
          (create-entity*! opt-params*% (new slot% [event-handler!! NOOP]) (send this get-subroot-handle) (send this get-subroot-model) 1)
        )
        (set! body*
          (create-entity*! body*% (new slot% [event-handler!! NOOP]) (send this get-subroot-handle) (send this get-subroot-model) 2)
        )
      )
    )

    (define ent:required-param%
      (class scalar-ent%
        (super-new)
      )
    )

    ; TODO this should obviously be deduped with ent:def%, but not going to now cuz the whole front end is going to rebuilt probably very
    ; soon
    (define ent:optional-param%
      (class compound-ent%

        (define/override (get-open-text)
          (get-short-text* (send this get-subroot-handle))
        )

        (define/override (get-closed-text)
          (get-open-text)
        )

        (define/override (refresh-text!)
          (super refresh-text!)
          (when default-slot*
            (send (send default-slot* get-ent) refresh-text!)
          )
        )

        (define (handle-default-event*!! key-event)
          (case (send key-event get-key-code)
            [(#\d) (unassign-default*!!)]
            [(#\s) (maybe-replace-default*!!)]
          )
        )

        (define (unassign-default*!!)
          (define handle (get-default-handle*))
          (if-unassignable handle (thunk
            (send handle unassign!!)
            (respawn-default*!)
            (send (send default-slot* get-ent) select!)
            (refresh-all-text*!)
          ))
        )

        (define (maybe-replace-default*!!)
          (maybe-replace*!!
            (send (send this get-subroot-handle) get-visible-referables-underneath)
            (send (send default-slot* get-ent) get-subroot-handle)
            (lambda (new-handle)
              (respawn-default*!)
              (send (send default-slot* get-ent) select!)
            )
          )
        )

        (define (respawn-default*!)
          (send (send this get-subroot-model) remove! 0)
          (spawn-default*!)
        )

        (define (spawn-default*!)
          (spawn-entity*! default-slot* (get-default-handle*) (send this get-subroot-model) 0)
        )

        (define (get-default-handle*)
          (send (send this get-subroot-handle) get-default)
        )

        ; TODO super hacky
        (define default-slot* #f)

        (super-new)

        (set! default-slot* (new slot% [event-handler!! handle-default-event*!!]))

        (spawn-default*!)
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

        (define/override (db-remove!! index)
          (send (send this get-subroot-handle) remove!! index)
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
            [(#\d) (unassign-expr*!!)]
            [(#\s) (maybe-replace-expr*!!)]
          )
        )

        (define (unassign-expr*!!)
          (define handle (get-expr-handle*))
          (if-unassignable handle (thunk
            (send handle unassign!!)
            (respawn-expr*!)
            (send (send expr-slot* get-ent) select!)
            (refresh-all-text*!)
          ))
        )

        (define (maybe-replace-expr*!!)
          (maybe-replace*!!
            (send (send this get-subroot-handle) get-visible-referables-underneath)
            (send (send expr-slot* get-ent) get-subroot-handle)
            (lambda (new-handle)
              (respawn-expr*!)
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

        (define/override (db-remove!! index)
          (send (get-basic-list-handle) remove!! index)
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

    ; TODO we proved the point we can do this, no real point in making effort to support it until
    ; we overhaul the gui and ent frameworks
    (define ent:func-def% ent:def%)

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

    (define (get-params-text* params)
      (string-join
        (map
          (lambda (p)
            (define desc (get-short-desc-or* p "¯\\_(ツ)_/¯"))
            (if (send p get-default)
              (format "[~a]" desc)
              desc
            )
          )
          params
        )
        ", "
      )
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
      (send db-handle accept (new (class zinal:db:element-visitor% (super-new)
        (define/override (visit-element e meh)
          (error 'get-short-text* "Unhandled element")
        )

        (define/override (visit-reference r meh)
          (get-short-desc-or* (send r get-referable) "<nameless ref>")
        )

        (define/override (visit-atom a meh)
          (~a (send a get-val))
        )

        (define/override (visit-symbol s meh)
          (format "'~a" (send s get-val))
        )

        (define/override (visit-char c meh)
          (format "#\\~a" (send c get-val))
        )

        (define/override (visit-lambda l meh)
          (if in-list?
            (format "λ~a" (get-short-desc-or* l "..."))
            (format "λ ~a (~a)" (get-short-desc-or* l "") (get-params-text* (send l get-all-params)))
          )
        )

        (define/override (visit-param p meh)
          (define basic (get-short-desc-or* p "¯\\_(ツ)_/¯"))
          (if (send p get-default)
            (format "[~a]" basic)
            basic
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

    (define (maybe-create*!! visible-referables generate-replaceable-handle follow-up [get-creator #f])
      (define creator!!
        (if get-creator
          (get-creator visible-referables)
          (request-new-item-creator visible-referables)
        )
      )
      (when creator!!
        (define new-handle (creator!! (send (generate-replaceable-handle) unassign!!)))
        (follow-up new-handle)
        (refresh-all-text*!)
      )
    )

    (define (maybe-replace*!! visible-referables handle-to-replace follow-up)
      (if-unassignable handle-to-replace (thunk
        (maybe-create*!!
          visible-referables
          (const handle-to-replace)
          follow-up
        )
      ))
    )

    (define (refresh-all-text*!)
      (send root* refresh-text!)
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

; PROGRAM

(define main-window (new frame% [label "zinal"]))
(define main-gui-manager (new zinal:gui-manager% [parent main-window]))
(define main-db (new zinal:sql-db% [filename "junk.db"]))
(define main-ent-manager (new ent:manager% [db main-db] [gui-model-manager main-gui-manager]))
(send main-window show #t)
(send main-window maximize #t)
(send main-gui-manager focus)

; (transpile main-db)
