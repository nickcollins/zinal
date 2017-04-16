; Similar to "#lang racket"
(module db-util racket

(require "misc.rkt")
(require "db.rkt")

(provide db-search-prev db-search-next handles-equal? get-define-methods assign-reference!! get-short-desc-or)

(define (handles-equal? handle1 handle2)
  (or
    (and (not handle1) (not handle2))
    (and handle1 handle2 (send handle1 equals? handle2))
  )
)

(define (get-short-desc-or describable-handle alt)
  (or (send describable-handle get-short-desc) alt)
)

(define (assign-reference!! unassigned-handle referable-handle)
  (cond
    [(is-a? referable-handle zinal:db:param%%) (send unassigned-handle assign-param-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:def%%) (send unassigned-handle assign-def-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:define-class%%) (send unassigned-handle assign-class-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:interface%%) (send unassigned-handle assign-interface-ref!! referable-handle)]
    [else (error 'assign-reference!! "Invalid referable")]
  )
)

(define (get-define-methods class)
  (filter (curryr is-a? zinal:db:define-method%%) (send class get-body))
)

(define (db-search-prev criterion start-node)
  (db-search* tree-search-prev criterion start-node)
)

(define (db-search-next criterion start-node)
  (db-search* tree-search-next criterion start-node)
)

(define (db-search* searcher criterion start-node)
  (searcher
    (lambda (n) (send n get-parent))
    (lambda (n) (if (is-a? n zinal:db:parent-node%%) (send n get-children) #f))
    criterion
    start-node
  )
)

)
