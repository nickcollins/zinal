#lang racket

; for list-index
(require srfi/1)

(require "misc.rkt")
(require "db.rkt")

(provide transpile)

(define (transpile db)
  (define referables (send db get-referables))
  (define root-elem (send db get-root-list))
  (db-elem->scheme root-elem referables)
)

(define (db-elem->scheme elem referables)
  (send elem accept transpiler referables)
)

(define (db-elems->scheme elems referables)
  (map (curryr db-elem->scheme referables) elems)
)

(define transpiler (new (class veme:db-element-visitor%
  (super-new)

  (define/override (visit-element e referables)
    (error 'visit-element "Missing visitor method for some element")
  )

  (define/override (visit-reference r referables)
    (get-unique-id (send r get-referable))
  )

  (define/override (visit-lambda l referables)
    (append
      (list 'lambda (db-elems->scheme (send l get-params) referables))
      (db-elem->scheme (send l get-body-list))
    )
  )

  (define/override (visit-atom a referables)
    (send a get-val)
  )

  (define/override (visit-list l referables)
    (db-elems->scheme (send l get-items) referables)
  )

  (define/override (visit-def d referables)
    (list 'define (get-unique-id d referables) (db-elem->scheme (send d get-expr) referables))
  )

  (define/override (visit-param p referables)
    (get-unique-id p referables)
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

  ; TODO This implementaiton is rather slow and a bit goofy.
  ; I've come up with 2 alternatives so far:
  ;
  ; a) Add a 'get-unique-id' method to veme:db-referable%% .
  ;    This option makes the caller code trivial and very performant, and it can be trivially
  ;    implemented by the sql db. However, it adds an awkward burden to the db api, that may
  ;    be difficult or awkward to implement if we switch to a graph database. Also, it is
  ;    awkward to try and explain in the interface what constraints the method would have and
  ;    why it should basically only be used for (and only exists for) one purpose.
  ;
  ; b) Make veme:db-element%% hashable.
  ;    This seems like a cleaner option than a), while being pretty easy (tho non-trivial)
  ;    to implement on both sides of the interface, and being essentially as efficient as a) .
  ;    This option has no real downsides except that it's harder to implement than the current
  ;    goofy solution, so I'm going to just do the goofy solution for now, until there's
  ;    evidence of perf issues, which is the only thing that b) majorly improves on over the
  ;    current solution
  (define (get-unique-id referable referables)
    (define num-id (list-index (lambda (other) (send referable equals? other)) referables))
    (string->symbol (format "veme-id:_~a" (add1 num-id)))
  )
)))
