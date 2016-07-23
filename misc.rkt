#lang racket

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
