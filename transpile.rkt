; Similar to "#lang racket"
(module transpile racket

(require racket/set)
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
  (define identifiables (send db get-all-referables))
  (define methods (append-map (lambda (i) (if (is-a? i zinal:db:type%%) (send i get-direct-methods) '())) identifiables))
  (set! identifiables (append identifiables methods))
  (define included-modules '())
  (define transpilation (append (transpile-requires (send db get-all-legacies)) (transpile-all-interfaces (send db get-all-interfaces) identifiables)))
  (define (include-module module)
    (for-each include-module (send module get-required-modules))
    (unless (findf (curry equals*? module) included-modules)
      (set! transpilation (append transpilation (db-elems->scheme (send module get-body) identifiables)))
      (set! included-modules (cons module included-modules))
    )
  )
  (include-module main-module)
  transpilation
)

(define (db-elem->scheme elem identifiables)
  (send elem accept transpiler identifiables)
)

(define (db-elems->scheme elems identifiables)
  (map (curryr db-elem->scheme identifiables) elems)
)

(define (transpile-all-interfaces all-interfaces identifiables)
  (map
    (lambda (i)
      (define methods (send i get-direct-methods))
      (define supers (send i get-direct-super-interfaces))
      (list
        'define
        (get-unique-id i identifiables)
        (list* 'interface (db-elems->scheme supers identifiables) (db-elems->scheme methods identifiables))
      )
    )
    all-interfaces
  )
)

(define (transpile-requires all-legacies)
  (define requires-data (make-hash))
  (for-each
    (lambda (l)
      (define library (send l get-library))
      (when library
        (hash-update! requires-data library (curryr set-add (send l get-name)) set)
      )
    )
    all-legacies
  )
  (define requires
    (hash-map
      requires-data
      (lambda (library id-set)
        (define name-pairs
          (set-map
            id-set
            (lambda (name) (list (string->symbol name) (get-non-standard-legacy-id library name)))
          )
        )
        (append (list 'only-in (string->symbol library)) name-pairs)
      )
    )
  )
  (if (pair? requires)
    (list (cons 'require requires))
    '()
  )
)

; TODO This implementation is rather slow and a bit goofy.
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
(define (get-unique-id identifiable identifiables)
  (define num-id (list-index (curry equals*? identifiable) identifiables))
  (assert (format "Could not find identifiable ~a in identifiables" (send identifiable get-short-desc)) num-id)
  (string->symbol (format "zinal_id:_~a" (add1 num-id)))
)

(define (get-non-standard-legacy-id library name)
  (string->symbol (format "zinal_legacy:~a::~a" library name))
)

(define (equals*? elem1 elem2)
  (send elem1 equals? elem2)
)

(define transpiler (make-object (class zinal:db:element-visitor%
  (super-make-object)

  (define/override (visit-element e identifiables)
    (error 'visit-element "Missing visitor method for some element")
  )

  (define/override (visit-reference r identifiables)
    (define referable (send r get-referable))
    (assert
      (format "Can't compile reference to ~a because it's not visible" (send referable get-short-desc))
      (send r is-referable-visible?)
    )
    (get-unique-id referable identifiables)
  )

  (define/override (visit-lambda l identifiables)
    (visit-lambda-like 'lambda #f l identifiables)
  )

  (define/override (visit-assert a identifiables)
    (list 'unless (db-elem->scheme (send a get-assertion) identifiables)
      (list*
        'error
        ''zinal
        (db-elem->scheme (send a get-format-string) identifiables)
        (db-elems->scheme (send a get-format-args) identifiables)
      )
    )
  )

  (define/override (visit-atom a identifiables)
    (send a get-val)
  )

  (define/override (visit-symbol s identifiables)
    (list 'quote (send s get-val))
  )

  (define/override (visit-list l identifiables)
    (db-elems->scheme (send l get-items) identifiables)
  )

  (define/override (visit-def d identifiables)
    (list 'define (get-unique-id d identifiables) (db-elem->scheme (send d get-expr) identifiables))
  )

  (define/override (visit-param p identifiables)
    (define default (send p get-default))
    (define param-identifier (get-unique-id p identifiables))
    (if default
      (list param-identifier (db-elem->scheme default identifiables))
      param-identifier
    )
  )

  (define/override (visit-legacy-link l identifiables)
    (define library (send l get-library))
    (define name (send l get-name))
    (if library (get-non-standard-legacy-id library name) (string->symbol name))
  )

  (define/override (visit-invoke-method im identifiables)
    (visit-has-args
      (list 'send (db-elem->scheme (send im get-object) identifiables) (db-elem->scheme (send im get-method) identifiables))
      im
      identifiables
    )
  )

  (define/override (visit-invoke-legacy-method ilm identifiables)
    (visit-has-args
      (list 'send (db-elem->scheme (send ilm get-object) identifiables) (string->symbol (send ilm get-legacy-method-name)))
      ilm
      identifiables
    )
  )

  (define/override (visit-create-object co identifiables)
    (visit-has-args
      (list 'make-object (db-elem->scheme (send co get-class-node) identifiables))
      co
      identifiables
    )
  )

  (define/override (visit-super-init si identifiables)
    (visit-has-args '(super-make-object) si identifiables)
  )

  (define/override (visit-invoke-super-method ism identifiables)
    (visit-has-args
      (list 'super (db-elem->scheme (send ism get-method) identifiables))
      ism
      identifiables
    )
  )

  (define/override (visit-invoke-legacy-super-method ilsm identifiables)
    (visit-has-args
      (list 'super (string->symbol (send ilsm get-legacy-method-name)))
      ilsm
      identifiables
    )
  )

  (define/override (visit-define-method dm identifiables)
    (define method (send dm get-method))
    (define super-class-ref (send (send dm get-parent) get-super-class))
    (define super-class (and (is-a? super-class-ref zinal:db:class-ref%%) (send super-class-ref get-define-class)))
    (visit-lambda-like
      (if (and super-class (findf (curry equals*? method) (send super-class get-all-methods)))
        'define/override
        'define/public
      )
      (db-elem->scheme method identifiables)
      (send dm get-lambda)
      identifiables
    )
  )

  (define/override (visit-override-legacy-method olm identifiables)
    (define name (string->symbol (send olm get-legacy-method-name)))
    (define lambda* (send olm get-lambda))
    (if (send olm is-augment?)
      (list 'begin
        (visit-lambda-like
          'define
          name
          lambda*
          identifiables
        )
        (list 'augment name)
      )
      (visit-lambda-like
        'define/override
        name
        lambda*
        identifiables
      )
    )
  )

  (define/override (visit-this t identifiables)
    'this
  )

  (define/override (visit-define-class c identifiables)
    (define superclass (send c get-super-class))
    (define super-methods (if (is-a? superclass zinal:db:class-ref%%) (send (send superclass get-define-class) get-all-methods) '()))
    (define init (cons 'init (db-elems->scheme (send c get-all-params) identifiables)))
    (define abstracts
      (filter-map
        (lambda (m) (and (send c is-method-abstract? m) (list 'abstract (db-elem->scheme m identifiables))))
        (remove* super-methods (send c get-all-methods) equals*?)
      )
    )
    (list
      'define
      (get-unique-id c identifiables)
      (visit-class* c (list* init abstracts) identifiables)
    )
  )

  (define/override (visit-class-instance c identifiables)
    (assert
      "zinal:db:class-instance%% has abstract method"
      (andmap (lambda (m) (not (send c is-method-abstract? m))) (send c get-all-methods))
    )
    (list
      'make-object
      (visit-class* c #f identifiables)
    )
  )

  (define/override (visit-interface i identifiables)
    (get-unique-id i identifiables)
  )

  (define/override (visit-method m identifiables)
    (get-unique-id m identifiables)
  )

  (define/override (visit-unassigned u identifiables)
    (error 'visit-unassigned "Ya can't build scheme code if ya got unassigned shit!")
  )

  (define (visit-has-args before-args has-args identifiables)
    (append
      before-args
      (db-elems->scheme (send has-args get-args) identifiables)
    )
  )

  (define (visit-lambda-like key-symbol before-params lambda-handle identifiables)
    (define params (send lambda-handle get-all-params))
    (define body (send lambda-handle get-body))
    (assert "All lambdas and method definitions must have non-empty bodies" (pair? body))
    (list*
      key-symbol
      (append (if before-params (list before-params) '()) (db-elems->scheme params identifiables))
      (db-elems->scheme body identifiables)
    )
  )

  (define (visit-class* class before-body identifiables)
    (define body (send class get-body))
    (assert "A class can't be compiled because it doesn't contain a zinal:db:super-init%%" (findf (curryr is-a? zinal:db:super-init%%) body))
    (list*
      'class*
      (db-elem->scheme (send class get-super-class) identifiables)
      (db-elems->scheme (send class get-direct-super-interfaces) identifiables)
      (append (or before-body '()) (db-elems->scheme (send class get-body) identifiables))
    )
  )
)))

; Example usage
; (require "sql-db.rkt")
; (define main-db (make-object zinal:sql-db% "junk.db"))
; (transpile main-db)
)
