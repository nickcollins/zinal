; Similar to "#lang racket"
(module db-util racket

(require "misc.rkt")
(require "db.rkt")

(provide db-search-prev db-search-next handles-equal?)

(define (handles-equal? handle1 handle2)
  (or
    (and (not handle1) (not handle2))
    (and handle1 handle2 (send handle1 equals? handle2))
  )
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
