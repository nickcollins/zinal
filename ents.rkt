#lang racket

(require "misc.rkt")
(require "db.rkt")

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

; Any operation which only affects the internals of a cone is the sole responsibility of the
; corresponding ent. It's fairly straightforward for the ent to make db writes or change its ui cone
; as necessary. But an operation which affects the root of a cone may involve two cones, and thus
; two ents. For example, deletion of a cone root generally falls under the responsibility of the
; parent ent. Such a deletion means cleanly deleting db, ent, and ui subtrees. To make this happen
; smoothly, and to help us connect ui cones, we have "ligaments". A ligament is a simple struct
; containing an ent, a ui list, and an index into the ui list. Ligaments are used as the cone leaves
; of the ent and the ui. The ent stores the roots of its ui and db cones, so a single ligament is
; effectively a connection between a db cone root, the corresponding ui cone root, and the location
; of the ui cone root in its parent cone (the location of the db handle is not necessary cuz the db
; knows how to handle that sort of thing internally). In the case of deletion, it is rather easy to
; swap out a ligament's ent with another one. For this and other operations, the ligament also holds
; an event handler that handle events (like deletion) that the child escalates to the parent. In this
; case the ligament's location information is very useful for making necessary modifications to the
; parent structures.

; TODO probably delete, but not yet, let's at least commit for posterity
; We can imagine that a db cone is a bone, and a
; ui cone is a muscle, and the ent is just a struct storing the ligaments (well, ok, the ent is also
; responsible for "editing" the bone and the muscle, but no analogy is perfect). The bone is
; connected to the muscle by a ligament at each end, but otherwise the bone and muscle diverge and
; look very different. Another visualization that better respects the tree structure is tent poles
; that are attached to the rain cover by stakes.

(define zinal:ent:manager% (class object%

  (init db)

  (define db* db)
  (define selected* #f)

  (define/public (get-initial-ui)
    ; TODO current return a zinal:ui:item%% to be initially displayed
    (send db* get-root)
  )

  (define/public (handle-event!! key-event)
    ; TODO current dispatch event to the currently selected zinal:ui:item%%,
    ; and return the zinal:ui:item%% to display
  )

  (define/public (get-selected-ui)
    selected*
  )

  (super-make-object)
))

; LIGAMENTS

(define ligament% (class object%

  (init ui-parent ui-parent-index event-handler-getter)

  (define ent* #f)
  (define ui-parent* ui-parent)
  (define index* ui-parent-index)
  (define event-handler*!! (event-handler-getter this))

  ; Must be called at least once immediately after construction
  (define/public (set-ent! new-ent)
    (set! ent* new-ent)
  )

  (define/public (get-ent)
    (assert-valid*)
    ent*
  )

  (define/public (handle-event!! key-event)
    (assert-valid*)
    (event-handler*!! key-event)
  )

  (define/public (get-index)
    (assert-valid*)
    index*
  )

  (define/public (set-index! new-index)
    (assert-valid*)
    (set! index* new-index)
  )

  (define (assert-valid*)
    (assert "ent must be initialized before using" ent*)
  )

  (super-make-object)
))

; ENTS

(define ent% (class object% ; abstract

  (init cone-root-handle)

  (define cone-root* cone-root-handle)

  (define/public (get-cone-root)
    cone-root* 
  )

  (abstract get-root-ui-item)
  ; Returns a list of pairs, the first of each pair is a db handle, and the second is the entity
  ; rooted at that handle.
  (abstract get-cone-leaves)

  (super-make-object)
))

(define ent:basic-list%)

; UI IMPL

(define ui:item% (class* object% (zinal:ui:item%%) ; abstract

  (init ent-manager parent event-handler)

  (define ent-manager* ent-manager)
  (define parent* parent)
  (define event-handler* event-handler)

  (define/public (selected?)
    (eq? this (send ent-manager* get-selected-ui))
  )

  (define/public (highlighted?)
    ; TODO NYI
    (error 'highlighted? "highlighted? NYI")
  )

  (define/public (get-parent)
    parent*
  )

  (define/public (get-event-handler)
    event-handler*
  )

  (define/public (set-event-handler new-event-handler)
    (set! event-handler* new-event-handler)
  )

  (super-make-object)
))

(define ui:scalar% (class* ui:item% (zinal:ui:scalar%%) ; abstract

  (init ent-manager parent event-handler style)

  ; TODO current what type is style?
  (define style* style)

  (define/public (get-style)
    style*
  )

  (super-make-object ent-manager parent event-handler)
))

(define ui:const% (class* ui:scalar% (zinal:ui:const%%)

  (init ent-manager parent event-handler style text)

  (define text* text)

  (define/public (get-text)
    text*
  )

  (super-make-object ent-manager parent event-handler style)
))

(define ui:var-scalar% (class* ui:scalar% (zinal:ui:var-scalar%%)

  (init ent-manager parent event-handler style text-getter)

  (define text-getter* text-getter)

  (define/public (get-text)
    (text-getter*)
  )

  (super-make-object ent-manager parent event-handler style)
))

(define ui:vector% (class* ui:item% (zinal:ui:vector%%) ; abstract

  (init ent-manager parent event-handler children)

  (define children* children)

  (define/public (get-children)
    children*
  )

  (define/public (insert! index ui-item)
    (define before (take children* index))
    (define after (drop children* index))
    (set! children* (append before (cons ui-item after)))
  )

  (define/public (remove! ui-item)
    (set! children* (remq ui-item children*))
  )

  (super-make-object ent-manager parent event-handler)
))

(define ui:hlist% (class* ui:vector% (zinal:ui:hlist%%)

  (init ent-manager parent event-handler [separator #f] [children '()])
  (assert
    "separator must be a const type or #f"
    (implies separator (is-a? separator zinal:ui:const%%))
  )

  (define separator* separator)

  (define/public (get-separator)
    separator*
  )

  (super-make-object ent-manager parent event-handler children)
))

(define ui:vlist% (class* ui:vector% (zinal:ui:vlist%%)

  (init ent-manager parent event-handler header [children '()])

  (define header* header)

  (define/public (get-header)
    header*
  )

  (super-make-object ent-manager parent event-handler children)
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
