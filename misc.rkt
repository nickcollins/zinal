; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module misc racket

(provide (all-defined-out))

; MACROS

(define-syntax-rule (assert msg bool)
  (unless bool (error msg))
)

; FUNCTIONS

; ugh - in srfi/13, but that seems to redefine string-join
(define (string-suffix? suf s)
  (let (
    [s-len (string-length s)]
    [suf-len (string-length suf)])
    (equal? (substring s (- s-len suf-len) s-len) suf)
  )
)

(define non-negative? (negate negative?))

; proc index lst[index] => any
(define (map-by-index proc lst)
  (define vec (list->vector lst))
  (build-list (vector-length vec) (lambda (i) (proc i (vector-ref vec i))))
)
)
