; Copyright 2017 Nick Collins
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

; Similar to "#lang racket"
(module transpile racket

(require racket/set)
; for list-index
(require srfi/1)

(require "misc.rkt")
(require "db.rkt")
(require "db-util.rkt")
(require "sql-db.rkt")

(provide transpile)

(define ID-PREFIX "zinal_:id:_")

(define transpiler #f)

; reads the db, transpiles its contents into scheme, and returns a scheme list of all expressions required by the main
; module in an appropriate order
(define (transpile db)
  (define main-module (send db get-main-module))
  (assert "Currently, we can only transpile programs with a main module, libraries will be supported later" main-module)
  (define identifiables (send db get-all-referables))
  (define methods (append-map (lambda (i) (if (is-a? i zinal:db:type%%) (send i get-direct-methods) '())) identifiables))
  (set! identifiables (append identifiables methods))
  (define included-modules '())
  (define transpilation (append
    (error-setup identifiables)
    (transpile-requires (send db get-all-rackets))
    (transpile-all-interfaces (send db get-all-interfaces) identifiables)
  ))
  (define (include-module db-module)
    (for-each include-module (send db-module get-required-modules))
    (unless (findf (curry equals*? db-module) included-modules)
      (set! transpilation (append transpilation (db-elems->scheme (send db-module get-body) identifiables)))
      (set! included-modules (cons db-module included-modules))
    )
  )
  (include-module main-module)
  transpilation
)

(define (error-setup identifiables)
  ; TODO this (and other things) should use quasiquote, but i haven't figure out how to make that work with translation
  (list
    (list 'define 'zinal:_ordered-identifiable-strings_ (cons 'list (map (curryr get-short-desc-or "<unnamed>") identifiables)))
    (list 'define 'zinal:_orig-error-display-handler_ (list 'error-display-handler))
    (list 'error-display-handler (list 'lambda (list 'msg 'ex)
      (list 'define 'orig-error-port (list 'current-error-port))
      (list 'define 'error-port (list 'open-output-string))
      (list 'current-error-port 'error-port)
      (list 'zinal:_orig-error-display-handler_ 'msg 'ex)
      (list 'current-error-port 'orig-error-port)
      (list 'define 'display-result (list 'regexp-replace* (list 'pregexp (format "~a\\d+\\b" ID-PREFIX)) (list 'get-output-string 'error-port) (list 'lambda (list 'id)
        ; Note - this algo must invert identifiable->unique-id
        (list 'list-ref 'zinal:_ordered-identifiable-strings_ (list 'sub1 (list 'string->number (list 'substring 'id (string-length ID-PREFIX)))))
      )))
      (list 'display 'display-result 'orig-error-port)
      (list 'close-output-port 'error-port)
    ))
  )
)

(define (db-elem->scheme elem identifiables)
  (send elem accept transpiler identifiables)
)

(define (db-elems->scheme elems identifiables)
  (map (curryr db-elem->scheme identifiables) elems)
)

(define (transpile-all-interfaces all-interfaces identifiables)
  (define seen-ifaces (mutable-set))
  (define ordered-ifaces '())
  (define (push-iface iface)
    (unless (set-member? seen-ifaces iface)
      (for-each push-iface (send iface get-direct-sub-interfaces))
      (set-add! seen-ifaces iface)
      (set! ordered-ifaces (cons iface ordered-ifaces))
    )
  )
  (for-each push-iface (filter (lambda (i) (null? (send i get-direct-super-interfaces))) all-interfaces))
  (map
    (lambda (i)
      (define methods (send i get-direct-methods))
      (define supers (send i get-direct-super-interfaces))
      (list
        'define
        (identifiable->unique-id i identifiables)
        (list* 'interface (db-elems->scheme supers identifiables) (db-elems->scheme methods identifiables))
      )
    )
    ordered-ifaces
  )
)

(define (transpile-requires all-rackets)
  (define requires-data (make-hash))
  (for-each
    (lambda (l)
      (define library (send l get-library))
      (when library
        (hash-update! requires-data library (curryr set-add (send l get-name)) set)
      )
    )
    all-rackets
  )
  (define requires
    (hash-map
      requires-data
      (lambda (library id-set)
        (define name-pairs
          (set-map
            id-set
            (lambda (name) (list (string->symbol name) (get-non-standard-racket-id library name)))
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
; a) Add a 'identifiable->unique-id' method to zinal:db:referable%% .
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
(define (identifiable->unique-id identifiable identifiables)
  (define num-id (list-index (curry equals*? identifiable) identifiables))
  (assert (format "Could not find identifiable ~a in identifiables" (send identifiable get-short-desc)) num-id)
  ; Note - this algo must invert the one that is output for error handling setup
  (string->symbol (format "~a~a" ID-PREFIX (add1 num-id)))
)

(define (get-non-standard-racket-id library name)
  (string->symbol (format "zinal_:racket_id:~a::~a" library name))
)

(define (equals*? elem1 elem2)
  (send elem1 equals? elem2)
)

(set! transpiler (make-object (class zinal:db:element-visitor%
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
    (identifiable->unique-id referable identifiables)
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
    (list 'define (identifiable->unique-id d identifiables) (db-elem->scheme (send d get-expr) identifiables))
  )

  (define/override (visit-param p identifiables)
    (define default (send p get-default))
    (define param-identifier (identifiable->unique-id p identifiables))
    (if default
      (list param-identifier (db-elem->scheme default identifiables))
      param-identifier
    )
  )

  (define/override (visit-racket l identifiables)
    (define library (send l get-library))
    (define name (send l get-name))
    (if library (get-non-standard-racket-id library name) (string->symbol name))
  )

  (define/override (visit-invoke-method im identifiables)
    (visit-has-args
      (list 'send (db-elem->scheme (send im get-object) identifiables) (db-elem->scheme (send im get-method) identifiables))
      im
      identifiables
    )
  )

  (define/override (visit-invoke-racket-method ilm identifiables)
    (visit-has-args
      (list 'send (db-elem->scheme (send ilm get-object) identifiables) (string->symbol (send ilm get-racket-method-name)))
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
    (visit-has-args (list 'super-make-object) si identifiables)
  )

  (define/override (visit-invoke-super-method ism identifiables)
    (visit-has-args
      (list 'super (db-elem->scheme (send ism get-method) identifiables))
      ism
      identifiables
    )
  )

  (define/override (visit-invoke-racket-super-method ilsm identifiables)
    (visit-has-args
      (list 'super (string->symbol (send ilsm get-racket-method-name)))
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

  (define/override (visit-override-racket-method olm identifiables)
    (define name (string->symbol (send olm get-racket-method-name)))
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
      (identifiable->unique-id c identifiables)
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
    (identifiable->unique-id i identifiables)
  )

  (define/override (visit-method m identifiables)
    (identifiable->unique-id m identifiables)
  )

  (define/override (visit-unassigned u identifiables)
    (error 'visit-unassigned "The program cannot be compiled if it contains any unassigned nodes")
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
)
