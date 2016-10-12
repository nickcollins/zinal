#lang racket

; for list-index
(require srfi/1)

(require "misc.rkt")
(require "db.rkt")

(provide zinal:ent:manager%)

; TERMINOLOGY AND CONCEPTS

; TODO current make sure docs discuss events and keymaps properly
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

; Any operation which only affects the internals of a cone is the sole responsibility of the
; corresponding ent. It's fairly straightforward for the ent to make db writes or change its ui cone
; as necessary. But an operation which affects the root of a cone may involve two cones, and thus
; two ents. For example, deletion of a cone root generally falls under the responsibility of the
; parent ent. Such a deletion means cleanly deleting db, ent, and ui subtrees.
; TODO current blah blah blah something about slots or something

; HELPERS

(define NOOP (const #f))

(define event-handler%% (interface ()

  handle-event!! ; (event)
))

; TODO current i have no idea what i'm doing
; We wouldn't need this if keymaps could be copied, inspected, or functionally extended! ugh
(define keymap-event-handler% (class* object% (event-handler%%)

  (init keymap/function)

  (define keymap/function* keymap/function)

  (define/public (compose fallback-event-handler)
    (make-object keymap-event-handler% 
      (lambda (event)
        (unless (handle-event!! event) (send fallback-event-handler handle-event!! event))
      )
    )
  )

  (define/public (handle-event!! event)
    (if (keymap-based*?)
      (if (is-a? event key-event%)
        (send keymap/function* handle-key-event #f event)
        (send keymap/function* handle-mouse-event #f event)
      )
      (keymap/function* event)
    )
  )

  (define (keymap-based*?)
    (is-a? keymap/function* keymap%)
  )

  (super-make-object)
))

(define NOOP-EVENT-HANDLER (make-object keymap-event-handler% (new keymap%)))

(define (get-basic-nav-event-handler ui-context)
  (define keymap (new keymap%))

  (send keymap add-function "left" (lambda (meh event)
    (if (send ui-context horizontal?)
      .
    )
  ))

  (send keymap add-function "down" (lambda (meh event)
    (if (send ui-context horizontal?)
      .
    )
  ))

  (send keymap map-function "left" "left")
  (send keymap map-function "h" "left")
  (send keymap map-function "right" "right")
  (send keymap map-function "l" "right")
  (send keymap map-function "up" "up")
  (send keymap map-function "k" "up")
  (send keymap map-function "down" "down")
  (send keymap map-function "j" "down")

  (make-object keymap-event-handler% keymap)
)

; SLOTS

(define slot% (class* object% (event-handler%%)

  (init keymap fallback-event-handler)

  (define ent* #f)
  (define keymap* keymap)
  (define fallback-event-handler* fallback-event-handler)

  ; Must be called at least once immediately after construction
  (define/public (set-ent! new-ent)
    (set! ent* new-ent)
  )

  (define/public (get-ent)
    (assert-valid*)
    ent*
  )

  (define/public (handle-event!! event)
    (assert-valid*)
    (unless (handle-event keymap* event) (send fallback-event-handler* handle-event!! event))
  )

  (define (assert-valid*)
    (assert "ent must be initialized before using" ent*)
  )

  (super-make-object)
))

; ENTS

(define zinal:ent:manager% (class object%

  (init db)

  (define db* db)
  (define selected* #f)

  (define/public (get-initial-ui)
    ; TODO current return a zinal:ui:item%% to be initially displayed
    (send db* get-root)
  )

  (define/public (handle-event!! event)
    (send (send selected* get-item) handle-event!! event)
    (send selected* get-root)
  )

  (define/public (get-selected-ui-context)
    selected*
  )

  (define/public (select! slot/context)
    (set! selected*
      (if (is-a? slot/context slot%)
        (send (send slot/context get-ent) get-root-ui-context)
        slot/context
      )
    )
  )

  (super-make-object)
))

(define ent% (class* object% (event-handler%%) ; abstract

  (init slot cone-root-handle)

  (define slot* slot)
  (define cone-root* cone-root-handle)

  (define/public (get-cone-root)
    cone-root*
  )

  (define/public (handle-event!! event)
    (send slot* handle-event!! event)
  )

  (abstract get-root-ui-context)
  ; Returns a list of slots corresponding to the leaves of this cone and the roots of the child cones
  (abstract get-cone-leaves)

  (super-make-object)
))

(define ent:basic-list%)

; UI IMPL

(define ui:context% (class* object (zinal:ui:context%%)

  (init ent-manager horiz-item vert-item is-horizontal?)

  (define ent-manager* ent-manager)
  (define horiz-item* horiz-item)
  (define vert-item* vert-item)
  (define horizontal*? is-horizontal?)
  (define parent-context* #f)

  (define/public (selected?)
    (eq? this (send ent-manager* get-selected-ui-context))
  )

  (define/public (highlighted?)
    ; TODO NYI
    (error 'highlighted? "highlighted? NYI")
  )

  (define/public (horizontal?)
    (or
      horizontal*?
      (and parent-context* (send parent-context* horizontal?))
    )
  )

  (define/public (get-item)
    (if (horizontal?) horiz-item vert-item)
  )

  (define/public (set-horizontal! new-value)
    (set! horizontal*? new-value)
  )

  (define/public (get-root)
    (if parent-context*
      (send parent-context* get-root)
      this
    )
  )

  (define/public (get-ent-manager)
    ent-manager*
  )

  (define/public (set-parent-context! new-parent)
    (set! parent-context* new-parent)
  )

  (super-make-object)
))

(define ui:item% (class* object% (zinal:ui:item%% event-handler%%) ; abstract

  (init context keymap fallback-event-handler)

  (define context* context)
  (define keymap* keymap)
  (define fallback-event-handler* fallback-event-handler)

  (define (get-context)
    context*
  )

  (define/public (handle-event!! event)
    (unless (handle-event keymap* event) (send fallback-event-handler* handle-event!! event))
  )

  (define/public (get-ent-manager)
    (send context* get-ent-manager)
  )

  (super-make-object)
))

(define ui:scalar% (class* ui:item% (zinal:ui:scalar%%) ; abstract

  (init context keymap fallback-event-handler style-delta)

  (define style-delta* style-delta)

  (define/public (get-style-delta)
    style-delta*
  )

  (super-make-object context keymap fallback-event-handler)
))

(define ui:const% (class* ui:scalar% (zinal:ui:const%%)

  (init context keymap fallback-event-handler style-delta text)

  (define text* text)

  (define/public (get-text)
    text*
  )

  (super-make-object context keymap fallback-event-handler style-delta)
))

(define ui:var-scalar% (class* ui:scalar% (zinal:ui:var-scalar%%)

  (init context keymap fallback-event-handler style-delta text-getter)

  (define text-getter* text-getter)

  (define/public (get-text)
    (text-getter*)
  )

  (super-make-object context keymap fallback-event-handler style-delta)
))

(define ui:list% (class* ui:item% (zinal:ui:list%%)

  (init context keymap fallback-event-handler [header #f] [separator #f])
  (assert "Header must be a context or #f" (implies header (is-a? header zinal:ui:context%%)))
  (assert "Separator must be a const or #f" (implies separator (is-a? separator zinal:ui:const%%)))

  (define header* header)
  (define separator* separator)
  (define children* '())

  (define/public (get-children)
    (map
      (lambda (c)
        (if (is-a? c slot%)
          (send (send c get-ent) get-root-ui-context)
          c
        )
      )
      children*
    )
  )

  (define/public (get-header)
    header*
  )

  (define/public (get-horizontal-separator)
    separator*
  )

  ; TODO current probably delete all these
  (define/public (insert-before! anchor-child new-child)
    (insert! (list-index (curry eq? anchor-child) children*) new-child)
  )

  (define/public (insert-after! anchor-child new-child)
    (insert! (add1 (list-index (curry eq? anchor-child) children*)) new-child)
  )

  (define/public (insert-first! new-child)
    (set! children* (cons new-child children*))
    (send (send this get-ent-manager) select! new-child)
  )

  (define/public (insert-last! new-child)
    (set! children* (append children* (list new-child)))
    (send (send this get-ent-manager) select! new-child)
  )

  ; TODO current depublicize if we go with the insert-before variety
  (define/public (insert! index new-child)
    ; TODO current delete or change assertion if this method stays public
    (assert "Invalid insertion index" index)
    (define before (take children* index))
    (define after (drop children* index))
    (set! children* (append before (cons new-child after)))
    (send (send this get-ent-manager) select! new-child)
  )

  ; TODO current probably make an indexed version of this
  (define/public (remove! child)
    (define index (list-index (curry eq? child) children*))
    (assert "cannot remove child that's not even in the list" index)
    (set! children* (remq child children*))
    (define num-children (length children*))
    (define selection
      (cond
        [(< index num-children)
          (list-ref children* index)
        ]
        [(> num-children 0)
          (list-ref children* (sub1 num-children))
        ]
        [else
          this
        ]
      )
    )
    (send (send this get-ent-manager) select! selection)
  )

  (define/public (get-children-internal)
    children*
  )

  (super-make-object context keymap fallback-event-handler)
))

(define ui:basic-list% (class ui:list% ; abstract

  (init context fallback-event-handler!!)

  (define (handle-event!! event)
    ; TODO current
  )

  (define (handle-child-event!! event)
    ; TODO current convert to keymap
    (case (send key-event get-key-code)
      [(#\h) (send this select-out)]
      [(#\j) (move-down*! selected-model*)]
      [(#\k) (move-up*!)]
      [(#\l) (send this select-in)]
      [else (fallback-event-handler!! key-event)]
    )
  )

  (super-make-object context handle-event!!)
))

(define ui:typical-dynamic-list% (class ui:list% ; abstract

  (init context)

  (define (handle-event!! event)
    ; TODO current convert to keymap
    (case (send key-event get-key-code)
      [(#\A) (maybe-add-item*!!)]
      [(#\I) (maybe-add-item*!! 0)]
      [else (super handle-event!! key-event)]
    )
  )

  (define (get-child-index child)
    (list-index (curry eq? child) (send this get-children-internal))
  )

  ; TODO current

  (super-make-object context handle-event!!)
))

; TODO current
(define (parse-entity*! db-handle)
  (send db-handle accept (new (class zinal:db:element-visitor% (super-make-object)

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
