; Similar to "#lang racket"
(module translate racket

(require racket/set)

(require "misc.rkt")
(require "db.rkt")
(require "db-util.rkt")

(provide translate)

(define BNS (make-base-namespace))
(define BASE-LEGACIES (set-subtract (list->set (namespace-mapped-symbols BNS)) (list->set (map string->symbol ILLEGAL-STANDARD-LEGACIES))))
(define SEMI-STANDARD-LEGACIES (list->set '(
  is-a?
  first
  second
  third
  curry
  curryr
  negate
  conjoin
  disjoin
  identity
  const
  thunk
  thunk*
  append*
  append-map
  filter-map
  string-join
  last
  dropf
  takef
  non-empty-string?
  implies
  xor
)))
(define SENDTINEL "{zinal sendtinel}: ")

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
  (define new-module (send db create-module!! (symbol->string module-name)))
  (translate-body module-children new-module)
  (second-pass new-module)
  (void)
)

(define (second-pass module-handle)
  (audit-unassigned* module-handle module-handle)
)

(define (audit-unassigned* module-handle current-handle)
  (define next-handle (db-search-next (disjoin (curry handles-equal? module-handle) (curryr is-a? zinal:db:unassigned%%)) current-handle))
  (when (is-a? next-handle zinal:db:unassigned%%)
    (define todo-desc (send next-handle get-short-desc))
    (define next-handle-parent (send next-handle get-parent))
    (define real-next-handle
      (cond
        [(not todo-desc)
          next-handle
        ]
        [(string-prefix? todo-desc SENDTINEL)
          (translate-send (read (open-input-string (substring todo-desc (string-length SENDTINEL)))) next-handle)
        ]
        [(translate-reference todo-desc next-handle) =>
          identity
        ]
        [(and (is-a? next-handle-parent zinal:db:list%%) (= 1 (length (send next-handle-parent get-items))) (local-method-call->method (string->symbol todo-desc) next-handle)) =>
          (lambda (m)
            (define db-invoke (send (send next-handle-parent unassign!!) assign-invoke-method!! m))
            (send (send db-invoke get-object) assign-this!!)
            db-invoke
          )
        ]
        [else
          next-handle
        ]
      )
    )
    (audit-unassigned* module-handle real-next-handle)
  )
)

(define (translate-list-like inserter list-data)
  (map-by-index
    (lambda (i v) (translate-datum v (inserter i)))
    list-data
  )
)

(define (translate-list list-data db-list)
  (translate-list-like (lambda (i) (send db-list insert!! i)) list-data)
)

(define (translate-args args-data db-has-args)
  (translate-list-like (lambda (i) (send db-has-args insert-arg!! i)) args-data)
)

(define (translate-body body-data db-has-body)
  (translate-list-like (lambda (i) (send db-has-body insert-into-body!! i)) body-data)
)

; must return the generated handle
(define (translate-send datum db-unassigned)
  (define method-name (~a (third datum)))
  (define visible-types (filter (curryr is-a? zinal:db:type%%) (send db-unassigned get-visible-referables-underneath)))
  (define visible-methods (append-map (lambda (t) (send t get-direct-methods)) visible-types))
  (define zinal-method (get-unique-describable method-name visible-methods))
  (define db-invoke (if zinal-method
    (send db-unassigned assign-invoke-method!! zinal-method)
    (send db-unassigned assign-invoke-legacy-method!! method-name)
  ))
  (translate-datum (second datum) (send db-invoke get-object))
  (translate-args (drop datum 3) db-invoke)
  db-invoke
)

(define (translate-params params-data db-has-params)

  (define required-params (takef params-data (negate list?)))
  (map-by-index
    (lambda (i param)
      (assert (format "required param must be a symbol: ~a" param) (symbol? param))
      (send db-has-params insert-required-param!! i (symbol->string param))
    )
    required-params
  )

  (define optional-params (dropf params-data (negate list?)))
  (map-by-index
    (lambda (i param)
      (assert (format "optional param must be of form [name value]: ~a" param) (and (list? param) (= 2 (length param))))
      (define name (first param))
      (define value (second param))
      (assert (format "name must be a symbol: ~a" name) (symbol? name))
      (define db-param (send db-has-params insert-optional-param!! i (symbol->string name)))
      (translate-datum value (send db-param get-default))
    )
    optional-params
  )
)

(define (translate-lambda params body db-unassigned)
  (assert (format "lambda params must be a list: ~a" params) (list? params))
  (assert (format "lambda must have non-empty body: ~a" body) (pair? body))
  (define db-lambda (send db-unassigned assign-lambda!!))
  (translate-params params db-lambda)
  (translate-body body db-lambda)
)

(define (translate-assert assertion format-string format-args db-unassigned)
  (define db-assert (send db-unassigned assign-assert!!))

  (translate-datum assertion (send db-assert get-assertion))
  (translate-datum format-string (send db-assert get-format-string))

  (map-by-index
    (lambda (i v) (translate-datum v (send db-assert insert-format-arg!! i)))
    format-args
  )
)

(define (translate-class class-data db-unassigned [definition-name #f])
  (define class-symbol (first class-data))
  (define (super*) (second class-data))
  (assert
    (format "class definition must be of form (class*\\? super (interfaces ...)\\? (init params ...)\\? (abstract m)... body ...): ~a" class-data)
    (and (> (length class-data) 2) (symbol? (super*)))
  )
  (define db-class
    (if definition-name
      (send db-unassigned assign-define-class!! definition-name)
      (send db-unassigned assign-class-instance!!)
    )
  )
  (define super-name (~a (super*)))
  (define super-ref (get-unique-referable super-name db-class))
  (if (and super-ref (is-a? super-ref zinal:db:define-class%%))
    (send db-class set-super-class!! super-ref)
    (send db-class set-legacy-super-class!! #f super-name)
  )
  (define body (drop class-data 2))
  (when (equal? class-symbol 'class*)
    (define interfaces (car body))
    (assert
      (format "class* definition must have interfaces list: ~a" class-data)
      (and (list? interfaces) (andmap symbol? interfaces) (> (length body) 1))
    )
    (add-direct-super-interfaces!! (send db-class get-db) interfaces db-class)
    (set! body (cdr body))
  )
  (define possible-init (first body))
  (when (and (pair? possible-init) (equal? 'init (first possible-init)))
    (assert (format "class-instance can't have init params: ~a" class-data) definition-name)
    (assert (format "class*\\? definition with init has no body: ~a" class-data) (> (length body) 1))
    (translate-params (cdr possible-init) db-class)
    (set! body (cdr body))
  )
  (define super-methods (send db-class get-all-methods))
  (define abstract? (conjoin pair? (compose1 (curry equal? 'abstract) first)))
  (define abstracts (takef body abstract?))
  (for-each
    (lambda (abstract)
      (assert (format "class-instance can't have abstract methods: ~a" class-data) definition-name)
      (assert (format "invalid abstract clause: ~a" abstract) (and (= 2 (length abstract)) (symbol? (second abstract))))
      (define abstract-string (~a (second abstract)))
      (unless (ormap (lambda (m) (equal? abstract-string (send m get-short-desc))) super-methods)
        (send db-class add-direct-method!! abstract-string)
      )
    )
    abstracts
  )
  (translate-body (dropf body abstract?) db-class)
)

(define (translate-define/override-method define-method-data db-unassigned)
  (define (name+params) (second define-method-data))
  (define (name-symbol) (car (name+params)))
  (define (params) (cdr (name+params)))
  (assert
    (format "invalid method definition/override: ~a" define-method-data)
    (and (> (length define-method-data) 2) (pair? (name+params)) (symbol? (name-symbol)))
  )
  (define containing-class (get-class-parent db-unassigned))
  (assert "no class parent found" containing-class)
  (define name (~a (name-symbol)))
  (define existing-method (findf (lambda (m) (equal? name (send m get-short-desc))) (send containing-class get-all-methods)))
  (define db-define/override-method-node
    (if existing-method
      (send db-unassigned assign-define-method!! existing-method)
      (if (equal? (first define-method-data) 'define/public)
        (send db-unassigned assign-define-method!! (send containing-class add-direct-method!! name))
        (send db-unassigned assign-override-legacy-method!! name)
      )
    )
  )
  (define db-lambda (send db-define/override-method-node get-lambda))
  (translate-params (params) db-lambda)
  (translate-body (drop define-method-data 2) db-lambda)
)

(define (translate-define datum db-unassigned)
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
      (define db (send db-unassigned get-db))
      (define defined-name (symbol->string second*))
      (define defined (third datum))
      (define (defined-is? expected-first) (and (pair? defined) (equal? expected-first (first defined))))
      (cond
        [(defined-is? 'interface)
          (define (supers) (second defined))
          (define (methods) (drop defined 2))
          (assert
            (format "interface definition must be of form (interface (supers ...) methods ...): ~a" defined)
            (and (>= (length defined) 2) (list? (supers)) (andmap symbol? (supers)) (andmap symbol? (methods)))
          )
          (define db-iface (send db create-interface!! defined-name))
          (add-direct-super-interfaces!! db (supers) db-iface)
          (for-each (lambda (m) (send db-iface add-direct-method!! m)) (map ~a (methods)))
        ]
        [(ormap defined-is? '(class class*))
          (translate-class defined db-unassigned defined-name)
        ]
        [else
          (translate-datum (third datum) (send (send db-unassigned assign-def!! defined-name) get-expr))
        ]
      )
    ]
    [else
      (error 'translate-define "the second part of a define must be a list or symbol: ~a" datum)
    ]
  )
)

(define (translate-reference ref-name db-unassigned)
  (define ref (get-unique-referable ref-name db-unassigned))
  (and ref (assign-reference!! db-unassigned ref))
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
          (translate-define datum db-unassigned)
        ]
        [(equal? 'assert (first-item))
          (define msg (second datum))
          (define assertion (third datum))
          (if (and (pair? msg) (>= (length msg) 2) (equal? 'format (first msg)))
            (translate-assert assertion (second msg) (drop msg 2) db-unassigned)
            (translate-assert assertion msg '() db-unassigned)
          )
        ]
        [(equal? 'make-object (first-item))
          (define class* (second datum))
          (define (class-symbol) (first class*))
          (cond
            [(and (list? class*) (member (class-symbol) '(class class*)))
              (translate-class class* db-unassigned)
            ]
            [else
              (define db-create-object (send db-unassigned assign-create-object!!))
              (translate-datum class* (send db-create-object get-class-node))
              (translate-args (drop datum 2) db-create-object)
            ]
          )
        ]
        [(member (first-item) '(define/public define/override))
          (translate-define/override-method datum db-unassigned)
        ]
        [(equal? 'send (first-item))
          (define (method-symbol) (third datum))
          (assert (format "invalid method invokation: ~a" datum) (and (> (length datum) 2) (symbol? (method-symbol))))
          (send db-unassigned set-short-desc!! (string-append SENDTINEL (~s datum)))
        ]
        [(equal? 'super-make-object (first-item))
          (translate-args (cdr datum) (send db-unassigned assign-super-init!!))
        ]
        [(equal? 'super (first-item))
          (define (method-symbol) (second datum))
          (assert (format "invalid super invokation: ~a" datum) (and (> (length datum) 1) (symbol? (method-symbol))))
          (define method-name (~a (method-symbol)))
          (define class-parent (get-class-parent db-unassigned))
          (assert "can't invoke a super method in a non-class context" class-parent)
          (define zinal-method (get-unique-describable method-name (send class-parent get-all-methods)))
          (define db-invoke (if zinal-method
            (send db-unassigned assign-invoke-super-method!! zinal-method)
            (send db-unassigned assign-invoke-legacy-super-method!! method-name)
          ))
          (translate-args (drop datum 2) db-invoke)
        ]
        [(local-method-call->method (first-item) db-unassigned) =>
          (lambda (m)
            (define db-invoke (send db-unassigned assign-invoke-method!! m))
            (send (send db-invoke get-object) assign-this!!)
            (translate-args (cdr datum) db-invoke)
          )
        ]
        [else
          (translate-list datum (send db-unassigned assign-list!!))
        ]
      )
    ]
    [(equal? 'this datum)
      (send db-unassigned assign-this!!)
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
      (define datum-as-string (symbol->string datum))
      (if (is-legacy? datum)
        (send db-unassigned assign-legacy-link!! #f datum-as-string)
        (unless (translate-reference datum-as-string db-unassigned)
          (send db-unassigned set-short-desc!! datum-as-string)
        )
      )
    ]
    [else
      (error 'translate-datum "type for datum ~a is indeterminable" datum)
    ]
  )
)

(define (local-method-call->method potential-method-datum db-unassigned)
  (define class-parent (get-class-parent db-unassigned))
  (define (local-methods)
    (remove-duplicates
      (append (send class-parent get-direct-methods) (map (lambda (dm) (send dm get-method)) (get-define-methods class-parent)))
      handles-equal?
    )
  )
  (and (is-a? class-parent zinal:db:define-class%%) (symbol? potential-method-datum)
    (get-unique-describable (~a potential-method-datum) (local-methods))
  )
)

(define (get-unique-referable ref-name location-node)
  (get-unique-describable ref-name (send location-node get-visible-referables-after))
)

(define (get-unique-describable name describables)
  (unique (filter (lambda (d) (equal? name (send d get-short-desc))) describables))
)

(define (is-legacy? datum)
  (or
    (set-member? BASE-LEGACIES datum)
    (set-member? SEMI-STANDARD-LEGACIES datum)
  )
)

(define (add-direct-super-interfaces!! db super-interface-symbols db-sub-type)
  (define all-interfaces (send db get-all-interfaces))
  (for-each
    (lambda (s)
      (define super-name (symbol->string s))
      (define db-super (findf (lambda (i) (equal? super-name (send i get-short-desc))) all-interfaces))
      (when db-super
        (send db-sub-type add-direct-super-interface!! db-super)
      )
    )
    super-interface-symbols
  )
)

(define (get-class-parent db-node)
  (cond
    [(is-a? db-node zinal:db:class%%) db-node]
    [db-node (get-class-parent (send db-node get-parent))]
    [else #f]
  )
)

; Example use:
(require "sql-db.rkt")
(define main-db (make-object zinal:sql-db% "junk.db"))
; (second-pass (findf (lambda (m) (equal? "ents" (send m get-short-desc))) (send main-db get-all-modules)))
; (second-pass (findf (lambda (m) (equal? "main" (send m get-short-desc))) (send main-db get-all-modules)))
(second-pass (findf (lambda (m) (equal? "transpile" (send m get-short-desc))) (send main-db get-all-modules)))
; (translate "misc.rkt" main-db)
; (translate "db.rkt" main-db)
; (translate "db-util.rkt" main-db)
; (translate "ui.rkt" main-db)
; (translate "sql-db.rkt" main-db)
; (translate "ui-styles.rkt" main-db)
; (translate "ents.rkt" main-db)
; (translate "main.rkt" main-db)
; (translate "transpile.rkt" main-db)
)
