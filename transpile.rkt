; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module transpile racket

; for list-index
(require srfi/1)

(require "misc.rkt")
(require "db.rkt")

(provide transpile)

; reads the db, transpiles its contents into scheme, and returns a scheme list of all expressions required by the main
; module in an appropriate order
(define (transpile db)
  (define main-module (send db get-main-module))
  (assert "Currently, we can only transpile programs with a main module, libraries will be supported later" main-module)
  (define referables (send db get-all-referables))
  (define included-modules '())
  (define transpilation '())
  (define (include-module module)
    (for-each include-module (send module get-required-modules))
    (unless (findf (lambda (im) (send im equals? module)) included-modules)
      (set! transpilation (append transpilation (db-elem->scheme module referables)))
      (set! included-modules (cons module included-modules))
    )
  )
  (include-module main-module)
  transpilation
)

(define (db-elem->scheme elem referables)
  (send elem accept transpiler referables)
)

(define (db-elems->scheme elems referables)
  (map (curryr db-elem->scheme referables) elems)
)

(define transpiler (new (class zinal:db:element-visitor%
  (super-new)

  (define/override (visit-element e referables)
    (error 'visit-element "Missing visitor method for some element")
  )

  (define/override (visit-reference r referables)
    (define referable (send r get-referable))
    (assert
      (format "Can't compile reference to ~a because it's not visible" (send referable get-short-desc))
      (send r is-referable-visible?)
    )
    (get-unique-id referable referables)
  )

  (define/override (visit-lambda l referables)
    (append
      (list 'lambda (db-elems->scheme (send l get-all-params) referables))
      (db-elems->scheme (send l get-body) referables)
    )
  )

  (define/override (visit-assert a referables)
    (list 'unless (db-elem->scheme (send a get-assertion) referables)
      (append
        (list 'error ''zinal (db-elem->scheme (send a get-format-string) referables))
        (db-elems->scheme (send a get-format-args) referables)
      )
    )
  )

  (define/override (visit-atom a referables)
    (send a get-val)
  )

  (define/override (visit-symbol s referables)
    (list 'quote (send s get-val))
  )

  (define/override (visit-list l referables)
    (db-elems->scheme (send l get-items) referables)
  )

  (define/override (visit-def d referables)
    (list 'define (get-unique-id d referables) (db-elem->scheme (send d get-expr) referables))
  )

  (define/override (visit-param p referables)
    (define default (send p get-default))
    (define param-identifier (get-unique-id p referables))
    (if default
      (list param-identifier (db-elem->scheme default referables))
      param-identifier
    )
  )

  (define/override (visit-legacy-link l referables)
    (define library (send l get-library))
    (define name (send l get-name))
    (if library
      ; TODO NYI
      (error 'visit-legacy-link "Support for non-standard libraries not yet implemented: ~a :: ~a" library name)
      (string->symbol name)
    )
  )

  (define/override (visit-unassigned u referables)
    (error 'visit-unassigned "Ya can't build scheme code if ya got unassigned shit!")
  )

  ; TODO This implementaiton is rather slow and a bit goofy.
  ; I've come up with 2 alternatives so far:
  ;
  ; a) Add a 'get-unique-id' method to zinal:db:referable%% .
  ;    This option makes the caller code trivial and very performant, and it can be trivially
  ;    implemented by the sql db. However, it adds an awkward burden to the db api, that may
  ;    be difficult or awkward to implement if we switch to a graph database. Also, it is
  ;    awkward to try and explain in the interface what constraints the method would have and
  ;    why it should basically only be used for (and only exists for) one purpose.
  ;
  ; b) Make zinal:db:element%% hashable.
  ;    This seems like a cleaner option than a), while being pretty easy (tho non-trivial)
  ;    to implement on both sides of the interface, and being essentially as efficient as a) .
  ;    This option has no real downsides except that it's harder to implement than the current
  ;    goofy solution, so I'm going to just do the goofy solution for now, until there's
  ;    evidence of perf issues, which is the only thing that b) majorly improves on over the
  ;    current solution
  (define (get-unique-id referable referables)
    (define num-id (list-index (lambda (other) (send referable equals? other)) referables))
    (assert (format "Could not find referable ~a in referables" (send referable get-short-desc)) num-id)
    (string->symbol (format "zinal-id:_~a" (add1 num-id)))
  )
)))
)
