; Similar to "#lang racket"
(module misc racket

(require "misc.rkt")
(require "db.rkt")

(provide handles-equal?)

(define (handles-equal? handle1 handle2)
  (or
    (and (not handle1) (not handle2))
    (and handle1 handle2 (send handle1 equals? handle2))
  )
)

)
