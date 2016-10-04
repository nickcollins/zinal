#lang racket

(require mrlib/hierlist)

(require "misc.rkt")

(provide zinal:gui-model-item% zinal:gui-model-list-item% zinal:gui-manager%)

; GUI MODEL

; The gui is built with hier-lists, but they do not provide functionality to modify lists fluently. As a result,
; the actual gui objects must be wholly or partially rebuilt after most changes. But we want to rebuild partially
; if we can, and when we rebuild, we need to keep track of what item was selected, which lists were open, etc. So
; we need an in-memory model of exactly what the gui _should_ look like, that is preserved during gui rebuilds,
; and from which the gui is built. This model is equivalent in structure to the actual gui object and should not
; attempt to bridge the gap between the gui structure and the db structure. That job will be taken care of by a
; middle layer.
(define zinal:gui-model-item%
  (class object%

    (define/public (get-manager)
      manager*
    )

    (define/public (get-parent)
      parent*
    )

    (define/public (handle-key-event!! key-event)
      (event-handler* key-event)
    )

    (define/public (set-short-text! new-text)
      (assert (format "short text must be a string: ~a" new-text) (string? new-text))
      (unless (equal? short-text* new-text)
        (set! short-text* new-text)
        (update-text!)
      )
    )

    (define/public (selected?)
      (eq? this (send manager* get-selected-model))
    )

    (define/public (select!)
      (send manager* select gui-item*)
    )

    (define/public (get-gui-item)
      gui-item*
    )

    (define/public (create-gui!)
      (assert (format "You must delete-gui before using create-gui") (not gui-item*))
      (define parent-gui-item (get-parent-gui-item*))
      (set! gui-item* (new-gui-item parent-gui-item))
      (send gui-item* user-data this)
      (update-text!)
      (when (selected?) (select!))
    )

    (define/public (delete-gui!)
      (when gui-item*
        (define parent-gui-item (get-parent-gui-item*))
        (send parent-gui-item delete-item gui-item*)
        (set! gui-item* #f)
      )
    )

    ; The racket-y way to do this seems to be using augment, but let's not mess with a lot of weird stuff
    ; that'll make it harder to do bootstrapping
    ; TODO this should be protected. After bootstrapping make it so
    (define/public (new-gui-item parent-gui-item)
      (send parent-gui-item new-item)
    )

    (define/public (is-root?)
      (not parent*)
    )

    (define/public (get-current-text)
      short-text*
    )

    (define/public (update-text!)
      ; lots of code liberally stolen from mred-designer
      (assert "deletion and creation of gui-item* should be atomic" gui-item*)
      (define ed (send gui-item* get-editor))
      (send ed erase)
      (send ed insert (get-current-text))
    )

    (define (get-parent-gui-item*)
      (if (is-root?)
        manager*
        (send parent* get-gui-item)
      )
    )

    (super-new)

    (init manager parent event-handler)
    (define manager* manager)
    (define parent* parent)
    (define event-handler* event-handler)
    (define short-text* "")
    (define gui-item* #f)

    (create-gui!)
  )
)

(define zinal:gui-model-list-item%
  (class zinal:gui-model-item%

    (define/override (set-short-text! new-text)
      (error 'set-short-text!  "for lists, use set-open-text! or set-closed-text! instead")
    )

    (define/override (new-gui-item parent-gui-item)
      (send parent-gui-item new-list)
    )

    (define/override (get-current-text)
      (if open*? open-text* closed-text*)
    )

    (define/override (create-gui!)
      (super create-gui!)
      (create-gui-items*!)
      (define gui-item (send this get-gui-item))
      (if open*?
        (send gui-item open)
        (send gui-item close)
      )
    )

    (define/override (delete-gui!)
      (delete-gui-items*!)
      (super delete-gui!)
    )

    (define/public (get-items)
      items*
    )

    (define/public (open?)
      open*?
    )

    (define/public (open!)
      (set! open*? #t)
      (define gui-item (send this get-gui-item))
      (send gui-item open)
      (send this update-text!)
    )

    (define/public (close!)
      (set! open*? #f)
      (define gui-item (send this get-gui-item))
      (send gui-item close)
      (send this update-text!)
    )

    (define/public (set-open-text! new-text)
      (assert (format "open text must be a string: ~a" new-text) (string? new-text))
      (unless (equal? open-text* new-text)
        (set! open-text* new-text)
        (send this update-text!)
      )
    )

    (define/public (set-closed-text! new-text)
      (assert (format "closed text must be a string: ~a" new-text) (string? new-text))
      (unless (equal? closed-text* new-text)
        (set! closed-text* new-text)
        (send this update-text!)
      )
    )

    (define/public (insert-list! index event-handler)
      (insert*! zinal:gui-model-list-item% index event-handler)
    )

    (define/public (insert-item! index event-handler)
      (insert*! zinal:gui-model-item% index event-handler)
    )

    (define/public (remove! index)
      (define doomed-item (list-ref items* index))
      (define before (take items* index))
      (define after (drop items* (add1 index)))
      (set! items* (append before after))
      (send doomed-item delete-gui!)
    )

    (define/public (maybe-refresh-gui!)
      (cond
        [needs-refresh*?
          (delete-gui-items*!)
          (create-gui-items*!)
        ]
        [else
          (for-each
            (lambda (item)
              (when (is-a? item zinal:gui-model-list-item%)
                (send item maybe-refresh-gui!)
              )
            )
            items*
          )
        ]
      )
    )

    (define (insert*! class index event-handler)
      (define before (take items* index))
      (define after (drop items* index))
      (define new-model-item
        (new class
          [manager (send this get-manager)]
          [parent this]
          [event-handler event-handler]
        )
      )
      (set! items* (append before (cons new-model-item after)))
      (when (cons? after) (needs-refresh*!))
      new-model-item
    )

    (define (needs-refresh*!)
      (set! needs-refresh*? #t)
    )

    (define (create-gui-items*!)
      (for-each (lambda (item) (send item create-gui!)) items*)
      (set! needs-refresh*? #f)
    )

    (define (delete-gui-items*!)
      (for-each (lambda (item) (send item delete-gui!)) items*)
    )

    (define open*? #t)
    (define open-text* "")
    (define closed-text* "")
    (define items* '())
    (define needs-refresh*? #f)

    (super-new)
  )
)

(define zinal:gui-manager%
  (class hierarchical-list%

    (define/override (on-select gui-item)
      (super on-select gui-item)
      (set! selected-model* (if gui-item (get-model gui-item) #f))
    )

    (define/override (on-item-opened gui-item)
      (super on-item-opened gui-item)
      (define model (get-model gui-item))
      (unless (send model open?) (send model open!))
    )

    (define/override (on-item-closed gui-item)
      (super on-item-closed gui-item)
      (define model (get-model gui-item))
      (when (send model open?) (send model close!))
    )

    (define/override (on-char key-event)
      (when selected-model*
        (case (send key-event get-key-code)
          [(#\h) (send this select-out)]
          [(#\H) (close-current-list*!)]
          [(#\j) (move-down*! selected-model*)]
          [(#\k) (move-up*!)]
          [(#\l) (send this select-in)]
          [else (send selected-model* handle-key-event!! key-event)]
        )
      )
      (send root-list-model* maybe-refresh-gui!)
      (super on-char key-event)
    )

    (define/public (create-root-list-model! event-handler)
      (assert "root-list-model* is already set" (not root-list-model*))
      (set! root-list-model*
        (new zinal:gui-model-list-item%
          [manager this]
          [parent #f]
          [event-handler event-handler]
        )
      )
      (send root-list-model* select!)
      root-list-model*
    )

    (define/public (get-selected-model)
      selected-model*
    )

    (define (close-current-list*!)
      (when (implies (is-a? selected-model* zinal:gui-model-list-item%) (not (send selected-model* open?)))
        (send this select-out)
      )
      (send selected-model* close!)
    )

    (define (move-up*!)
      ; TODO move to last item, once we have a G command
      (unless (send selected-model* is-root?)
        (define parent-model (send selected-model* get-parent))
        (if (eq? selected-model* (car (send parent-model get-items)))
          (send this select-out)
          (send this select-prev)
        )
      )
    )

    (define (is-last-in-list* model-item parent-list)
      (eq? model-item (last (send parent-list get-items)))
    )

    (define (move-down*! model-item [can-go-in #t])
      (define parent (send model-item get-parent))
      (cond
        [(send model-item is-root?) (send this select-in)]
        [(is-last-in-list* model-item parent)
          (if (and can-go-in (is-a? model-item zinal:gui-model-list-item%) (cons? (send model-item get-items)))
            (send this select-in)
            (begin (send parent select!) (move-down*! parent #f))
          )
        ]
        [else (send this select-next)]
      )
    )

    (super-new)

    (define root-list-model* #f)
    (define selected-model* #f)
  )
)

(define (get-model gui-item)
  (send gui-item user-data)
)
