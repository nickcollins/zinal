; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module translate racket

(require "misc.rkt")
(require "db.rkt")

(provide translate)

; Reads racket-file , which must only use features supported by zinal, and creates a new module in
; db whose contents will correspond to the scheme logic in racket-file . Translation does not claim
; to be be perfect, and may make some mistakes or its results may require some manual fixes, and
; the set of programs it works on at all (i.e., that are "conforming") may be somewhat arbitrary or
; ill-defined. Some references and identifiers may be translated into TODOs, and other stuff may be
; left unfinished.
; No meaningful return value.
(define (translate racket-file db)
  (define in (open-input-file racket-file #:mode 'text))
  (define file-data (read in))
  (close-input-port in)
  (assert
    (format "translate can only accept files which start with \"(module <MODULE-NAME> racket\": ~a" file-data)
    (and (list? file-data) (> (length file-data) 3) (equal? (first file-data) 'module) (equal? (third file-data) 'racket))
  )
  (define module-children (drop file-data 3))
  (define module-name (second file-data))
  (define module (send db create-module!! (symbol->string module-name)))
  (translate-list module-children module)
  (void)
)

(define (translate-list list-data db-list)
  (define vec (list->vector list-data))
  (build-list
    (vector-length vec)
    (lambda (i)
      (translate-datum (vector-ref vec i) (send db-list insert!! i))
    )
  )
)

(define (translate-lambda params body db-unassigned)
  (assert (format "lambda params must be a list: ~a" params) (list? params))
  (assert (format "lambda must have non-empty body: ~a" body) (pair? body))

  (define db-lambda (send db-unassigned assign-lambda!!))

  (define required-params (list->vector (takef params (negate list?))))
  (build-list
    (vector-length required-params)
    (lambda (i)
      (define param (vector-ref required-params i))
      (assert (format "required param must be a symbol: ~a" param) (symbol? param))
      (send db-lambda insert-required-param!! i (symbol->string param))
    )
  )

  (define optional-params (list->vector (dropf params (negate list?))))
  (build-list
    (vector-length optional-params)
    (lambda (i)
      (define param (vector-ref optional-params i))
      (assert (format "optional param must be of form [name value]: ~a" param) (and (list? param) (= 2 (length param))))
      (define name (first param))
      (define value (second param))
      (assert (format "name must be a symbol: ~a" name) (symbol? name))
      (define db-param (send db-lambda insert-optional-param!! i (symbol->string name)))
      (translate-datum value (send db-param get-default))
    )
  )

  (define body-vec (list->vector body))
  (build-list
    (vector-length body-vec)
    (lambda (i)
      (translate-datum (vector-ref body-vec i) (send db-lambda insert-into-body!! i))
    )
  )
)

(define (translate-datum datum db-unassigned)
  (cond
    [(list? datum)
      (define (first-item) (first datum))
      (cond
        [(null? datum)
          (send db-unassigned assign-list!!)
        ]
        [(and (equal? 'quote (first-item)) (= 2 (length datum)) (symbol? (second datum)))
          (send db-unassigned assign-symbol!! (second datum))
        ]
        [(equal? 'lambda (first-item))
          (define params (second datum))
          (define body (drop datum 2))
          (translate-lambda params body db-unassigned)
        ]
        [(equal? 'define (first-item))
          (define second* (second datum))
          (cond
            [(pair? second*)
              (define name (car second*))
              (define params (cdr second*))
              (define body (drop datum 2))
              (translate-lambda params body (send (send db-unassigned assign-def!! (symbol->string name)) get-expr))
            ]
            [(symbol? second*)
              (assert (format "non-function define must be of form (define a b): ~a" datum) (= 3 (length datum)))
              (translate-datum (third datum) (send (send db-unassigned assign-def!! (symbol->string second*)) get-expr))
            ]
            [else
              (error 'translate-datum "the second part of a define must be a list or symbol: ~a" datum)
            ]
          )
        ]
        [else
          (translate-list datum (send db-unassigned assign-list!!))
        ]
      )
    ]
    [(number? datum)
      (send db-unassigned assign-number!! datum)
    ]
    [(char? datum)
      (send db-unassigned assign-char!! datum)
    ]
    [(string? datum)
      (send db-unassigned assign-string!! datum)
    ]
    [(boolean? datum)
      (send db-unassigned assign-bool!! datum)
    ]
    [(keyword? datum)
      (send db-unassigned assign-keyword!! datum)
    ]
    [(symbol? datum)
      ; TODO Later we'll translate at least some of these to legacies or refs
      (send db-unassigned set-short-desc!! (~a datum))
    ]
    [else
      (error 'translate-datum "type for datum ~a is indeterminable" datum)
    ]
  )
)

; Example use:
; (require "sql-db.rkt")
; (define main-db (make-object zinal:sql-db% "junk.db"))
; (translate "translate.rkt" main-db)
)
