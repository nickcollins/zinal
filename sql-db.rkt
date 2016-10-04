#lang racket

(require db)

(require "misc.rkt")
(require "db.rkt")

(provide (prefix-out zinal: sql-db%))

; All indexed db queries must check that id is real first. higher-level stuff need not check directly
(define-syntax-rule (assert-real-id id)
  (assert (format "id ~a was non-positive" id) (and (number? id) (positive? id)))
)

(define (sql:// a b)
  (if (sql-null? a) b a)
)

(define (or-sql-null v)
  (or v sql-null)
)

(define TABLES->NON-ID-COLS (hash
  "list_headers" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "cdr_id INT")
  "lambdas" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "req_params_id INT" "opt_params_id INT" "body_id INT")
  "params" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "default_id INT")
  "defines" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "expr_id INT")
  "param_refs" '("param_id INT UNIQUE")
  "definitions" '("define_id INT UNIQUE")
  "list_nodes" '("owner_id INT" "car_id INT" "cdr_id INT")
  "atoms" '("type TEXT" "value TEXT")
  "legacies" '("ref_count INT" "library TEXT" "name TEXT")
  "unassigned" '("short_desc TEXT" "long_desc TEXT")
))

(define TABLES (list->vector (hash-keys TABLES->NON-ID-COLS)))

(define (get-table-mod* table)
  (define mod (vector-member table TABLES))
  (assert (format "Invalid table ~a" table) mod)
  mod
)

; note PROG-START-ID is not necessarily the very first id, merely the id of the root list
(define PROG-START-ID (+ (vector-length TABLES) (get-table-mod* "list_headers")))

(define DEFAULT-LIBRARY "")

(define BOGUS-ID -1)

(define NIL-ID 0)

(define sql-db%
  (class* object% (zinal:db%%)

    (init filename)

    (define/public (get-root)
      (get-handle! ROOT-LOC)
    )

    (define/public (get-referables)
      (map get-handle! (append (get-referables-of-type* "params") (get-referables-of-type* "defines")))
    )

    (define/public (get-filename)
      filename*
    )

    ; DB ELEMENT IMPLEMENTATION CLASSES

    (define db-element%
      (class* object% (zinal:db:element%%)

        (init id)
        (assert-real-id id)

        (define/public (get-db)
          (assert-valid)
          sql-db*
        )

        (define/public (accept visitor [data #f])
          (assert-valid)
          (send visitor visit-element this data)
        )

        (define/public (equals? other-element)
          (assert-valid)
          (send other-element assert-valid)
          (eq? this other-element)
        )

        ; TODO Ugh - these should be private to the file or to the sql-db% class, but it's unlikely that any way of doing so is going
        ; to happen post-bootstrap

        (define/public (get-id)
          (assert-valid)
          id*
        )

        (define/public (assert-valid)
          (assert (format "This element has already been deleted or unassigned, and can no longer be used: ~a" id*) valid*?)
        )

        ; TODO we should probably force this to call a subclass method for handles* invalidation
        (define/public (invalidate!)
          (set! valid*? #f)
        )

        (abstract delete-and-invalidate*!!)

        (define id* id)
        ; This object will be invalidated if the data it's referring to gets deleted or unassigned. Any attempts to do something with
        ; an invalid handle will cause an exception. Calling code is responsible for never using a stale handle, but this is a sanity
        ; check.
        (define valid*? #t)

        (super-new)
      )
    )

    (define db-node%
      (class* db-element% (zinal:db:node%%) ; abstract

        (init loc)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-node this data)
        )

        (define/override (invalidate!)
          (hash-remove! handles* (send loc* get-id&col))
          (super invalidate!)
        )

        ; Overrides must invoke this as their last action
        (define/override (delete-and-invalidate*!!)
          (set-loc-dangerous*!! loc* BOGUS-ID)
          (send this invalidate!)
        )

        (define/public (get-parent)
          (send this assert-valid)
          (cond
            [(send loc* root?)
              #f
            ]
            [else
              (define loc-id (send loc* get-id))
              (define direct-parent-id
                (if (equal? "list_nodes" (get-table loc-id))
                  (get-cell* loc-id "owner_id")
                  loc-id
                )
              )
              (cond
                [(= direct-parent-id PROG-START-ID)
                  (get-handle! ROOT-LOC)
                ]
                [else
                  ; We want the parent of a lambda body expr to be the lambda, not the list it's hidden in
                  (define direct-grandparent-id (get-cell* direct-parent-id "parent_id"))
                  (define parent-id
                    (if (equal? "lambdas" (get-table direct-grandparent-id))
                      direct-grandparent-id
                      direct-parent-id
                    )
                  )
                  (get-handle! (get-cell* parent-id "parent_id") (get-cell* parent-id "parent_col"))
                ]
              )
            ]
          )
        )

        (define/public (get-visible-referables-underneath)
          (send this assert-valid)
          (get-visible-referables* this)
        )

        (define/public (get-visible-referables-after)
          (send this assert-valid)
          (get-visible-referables* this)
        )

        (define/public (can-unassign?)
          (send this assert-valid)
          (get-parent)
        )

        (define/public (unassign!!)
          (assert
            (format "Cannot unassign node (~a, ~a):~a" (send loc* get-id) (send loc* get-col) (send this get-id))
            (can-unassign?)
          )
          (delete-and-invalidate*!!)
          (create-unassigned!! loc*)
          (get-handle! loc*)
        )

        (define/public (get-loc)
          (send this assert-valid)
          loc*
        )

        (define loc* loc)

        (super-new [id (send loc get-cell)])
      )
    )

    ; TODO we should probably do mixins for this but not sure i want to have much OOP functionality
    ; before boostrapping
    (define db-describable-node%
      (class* db-node% (zinal:db:describable%%)
        (define/public (get-short-desc)
          (send this assert-valid)
          (sql:// (get-cell* (send this get-id) "short_desc") #f)
        )

        (define/public (get-long-desc)
          (send this assert-valid)
          (sql:// (get-cell* (send this get-id) "long_desc") #f)
        )

        (define/public (set-short-desc!! new-desc)
          (send this assert-valid)
          (set-desc*!! 'short (send this get-id) new-desc)
        )

        (define/public (set-long-desc!! new-desc)
          (send this assert-valid)
          (set-desc*!! 'long (send this get-id) new-desc)
        )

        (super-new)
      )
    )

    (define db-lambda%
      (class* db-describable-node% (zinal:db:lambda%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-lambda this data)
        )

        (define/override (get-visible-referables-underneath)
          (send this assert-valid)
          (append
            (get-all-params)
            (super get-visible-referables-underneath)
          )
        )

        (define/override (delete-and-invalidate*!!)
          (send (get-body-list*) delete-and-invalidate*!!)
          (send (get-opt-params-list*) delete-and-invalidate*!!)
          (send (get-reqd-params-list*) delete-and-invalidate*!!)
          (delete-id*!! (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (get-children)
          (send this assert-valid)
          (append (get-all-params) (get-body))
        )

        (define/public (get-all-params)
          (send this assert-valid)
          (append (get-required-params) (get-optional-params))
        )

        (define/public (get-required-params)
          (send this assert-valid)
          (send (get-reqd-params-list*) get-items)
        )

        (define/public (can-remove-required-param? index)
          (send this assert-valid)
          (can-remove-param*? (get-required-params) index)
        )

        (define/public (remove-required-param!! index)
          (send this assert-valid)
          (remove-param*!! (get-reqd-params-list*) index)
        )

        (define/public (insert-required-param!! index [short-desc #f])
          (send this assert-valid)
          (send (get-reqd-params-list*) insert-param*!! index #t short-desc)
        )

        (define/public (get-optional-params)
          (send this assert-valid)
          (send (get-opt-params-list*) get-items)
        )

        (define/public (can-remove-optional-param? index)
          (send this assert-valid)
          (can-remove-param*? (get-optional-params) index)
        )

        (define/public (remove-optional-param!! index)
          (send this assert-valid)
          (remove-param*!! (get-opt-params-list*) index)
        )

        (define/public (insert-optional-param!! index [short-desc #f])
          (send this assert-valid)
          (send (get-opt-params-list*) insert-param*!! index #f short-desc)
        )

        (define/public (get-body)
          (send this assert-valid)
          (send (get-body-list*) get-items)
        )

        (define/public (insert-into-body!! index)
          (send this assert-valid)
          (send (get-body-list*) insert!! index)
        )

        (define/public (remove-from-body!! index)
          (send this assert-valid)
          (send (get-body-list*) remove!! index)
        )

        (define (remove-param*!! param-list index)
          (define params (send param-list get-items))
          (assert
            (format "Cannot delete ~ath required or optional param" index)
            (can-remove-param*? params index)
          )
          (send (list-ref params index) delete-and-invalidate*!!)
          (send param-list remove*!! index #f)
        )

        (define (can-remove-param*? params index)
          (all-references-are-descendants*? (list-ref params index))
        )

        (define (get-reqd-params-list*)
          (get-handle! (send this get-id) "req_params_id")
        )

        (define (get-opt-params-list*)
          (get-handle! (send this get-id) "opt_params_id")
        )

        (define (get-body-list*)
          (get-handle! (send this get-id) "body_id")
        )

        (super-new)
      )
    )

    (define db-def%
      (class* db-describable-node% (zinal:db:def%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-def this data)
        )

        (define/override (can-unassign?)
          (send this assert-valid)
          (and
            (super can-unassign?)
            (all-references-are-descendants*? this)
          )
        )

        (define/override (delete-and-invalidate*!!)
          (define id (send this get-id))
          (send (get-expr) delete-and-invalidate*!!)
          (delete-id*!! (get-definition-id id))
          (delete-id*!! id)
          (super delete-and-invalidate*!!)
        )

        (define/public (get-references)
          (send this assert-valid)
          (get-references* (get-definition-id (send this get-id)))
        )

        (define/public (get-children)
          (send this assert-valid)
          (list (get-expr))
        )

        (define/public (get-expr)
          (send this assert-valid)
          (get-handle! (send this get-id) "expr_id")
        )

        (super-new)
      )
    )

    (define db-list%
      (class* db-describable-node% (zinal:db:list%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-list this data)
        )

        (define/override (delete-and-invalidate*!!)
          (for-each (lambda (h) (send h delete-and-invalidate*!!)) (get-items))
          (delete-list-nodes-and-header* (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (get-children)
          (send this assert-valid)
          (get-items)
        )

        (define/public (get-items)
          (send this assert-valid)
          (map
            (curryr get-handle! "car_id")
            (get-cdrs* (get-cell* (send this get-id) "cdr_id"))
          )
        )

        (define/public (insert!! index)
          (define car-loc (insert*!! index))
          (create-unassigned!! car-loc)
          (get-handle! car-loc)
        )

        (define/public (remove!! index)
          (remove*!! index #t)
        )

        (define/public (insert-param*!! index required? short-desc)
          (define car-loc (insert*!! index))
          (create-param!! car-loc required? short-desc)
          (get-handle! car-loc)
        )

        (define/public (insert*!! index)
          (send this assert-valid)
          (define list-header-id (send this get-id))
          (define insertion-point-id (nth-list-insertion-point* list-header-id index index))
          (define insertion-point-loc (new loc% [id insertion-point-id] [col "cdr_id"]))
          (define old-cdr (send insertion-point-loc get-cell))
          (define new-node-id
            (create-something!! "list_nodes" (list (list "owner_id" list-header-id) (list "car_id" BOGUS-ID) (list "cdr_id" old-cdr)))
          )
          ; We captured the original cdr_id, and moved it to the newly created node, so we can safely replace this node's cdr
          (set-loc-dangerous*!! insertion-point-loc new-node-id)
          ; returns the car_id loc so that the caller can set the node
          (new loc% [id new-node-id] [col "car_id"])
        )

        (define/public (remove*!! index expect-unassigned?)
          (send this assert-valid)
          (define list-header-id (send this get-id))
          (define insertion-point-id (nth-list-insertion-point* list-header-id index index))
          (define id-to-delete (get-cell* insertion-point-id "cdr_id"))
          (assert (format "Index out of bounds: ~a" index) (not (= id-to-delete NIL-ID)))
          (define id-to-contract (get-cell* id-to-delete "cdr_id"))
          (define loc-to-delete (new loc% [id id-to-delete] [col "car_id"]))
          (when expect-unassigned?
            (define unassigned-handle (get-handle! loc-to-delete))
            (assert
              (format "You can only remove!! an unassigned: (~a, ~a):~a" (send loc-to-delete get-id) (send loc-to-delete get-col) (send loc-to-delete get-cell))
              (is-a? unassigned-handle zinal:db:unassigned%%)
            )
            (send unassigned-handle delete-and-invalidate*!!)
          )
          (assert-bogus-id loc-to-delete)
          (delete-id*!! id-to-delete)
          (set-cell-dangerous*!! insertion-point-id "cdr_id" id-to-contract)
          (void)
        )

        (define (nth-list-insertion-point* list-start-id index orig-index)
          (assert
            (format "Index out of bounds: ~a" orig-index)
            (and (non-negative? index) (not (= list-start-id NIL-ID)))
          )
          (if (zero? index)
            list-start-id
            (nth-list-insertion-point* (get-cell* list-start-id "cdr_id") (sub1 index) orig-index)
          )
        )

        (define (get-cdrs* list-node-id)
          (if (= list-node-id NIL-ID)
            '()
            (cons list-node-id (get-cdrs* (get-cell* list-node-id "cdr_id")))
          )
        )

        ; Don't call until cars have been deleted!
        (define (delete-list-nodes-and-header* list-node-id)
          (unless (= list-node-id NIL-ID)
            (delete-list-nodes-and-header* (get-cell* list-node-id "cdr_id"))
            (delete-id*!! list-node-id)
          )
        )

        (super-new)
      )
    )

    (define db-param%
      (class* db-describable-node% (zinal:db:param%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-param this data)
        )

        (define/override (can-unassign?)
          #f
        )

        (define/override (delete-and-invalidate*!!)
          (define default (get-default))
          (when default (send default delete-and-invalidate*!!))
          (define id (send this get-id))
          (delete-id*!! (get-param-ref-id id))
          (delete-id*!! id)
          (super delete-and-invalidate*!!)
        )

        (define/public (get-lambda)
          (send this assert-valid)
          (send this get-parent)
        )

        (define/public (get-default)
          (send this assert-valid)
          (define id (send this get-id))
          (if
            (= NIL-ID (get-cell* id "default_id"))
            #f
            (get-handle! id "default_id")
          )
        )

        (define/public (get-children)
          (send this assert-valid)
          (define default (get-default))
          (if default
            (list default)
            '()
          )
        )

        (define/public (get-references)
          (send this assert-valid)
          (get-references* (get-param-ref-id (send this get-id)))
        )

        (super-new)
      )
    )

    (define db-atom%
      (class* db-node% (zinal:db:atom%%) ; abstract

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-atom this data)
        )

        (define/override (delete-and-invalidate*!!)
          (delete-id*!! (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (assert-correct-type-and-get-stored-value expected-type)
          (send this assert-valid)
          (define actual-type (string->symbol (get-cell* (send this get-id) "type")))
          (assert
            (format "Invalid db type ~a for handle of type ~a" actual-type expected-type)
            (equal? expected-type actual-type)
          )
          (get-cell* (send this get-id) "value")
        )

        (abstract get-val)

        (super-new)
      )
    )

    (define db-number%
      (class* db-atom% (zinal:db:number%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-number this data)
        )

        (define/override (get-val)
          (define stored-value (send this assert-correct-type-and-get-stored-value 'number))
          (or
            (string->number stored-value)
            (error 'get-val "Number ~a cannot be converted to number" stored-value)
          )
        )

        (super-new)
      )
    )

    (define db-char%
      (class* db-atom% (zinal:db:char%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-char this data)
        )

        (define/override (get-val)
          (define stored-value (send this assert-correct-type-and-get-stored-value 'character))
          (define int-value (string->number stored-value))
          (assert
            (format "Character ~a must be the integer value of the desired character" stored-value)
            (and int-value (exact-positive-integer? int-value))
          )
          (integer->char int-value)
        )

        (super-new)
      )
    )

    (define db-string%
      (class* db-atom% (zinal:db:string%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-string this data)
        )

        (define/override (get-val)
          (send this assert-correct-type-and-get-stored-value 'string)
        )

        (super-new)
      )
    )

    (define db-bool%
      (class* db-atom% (zinal:db:bool%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-bool this data)
        )

        (define/override (get-val)
          (define stored-value (send this assert-correct-type-and-get-stored-value 'boolean))
          (case stored-value
            [("f") #f]
            [("t") #t]
            [else (error 'get-val "Boolean ~a is neither 'f' nor 't'" stored-value)]
          )
        )

        (super-new)
      )
    )

    (define db-symbol%
      (class* db-atom% (zinal:db:symbol%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-symbol this data)
        )

        (define/override (get-val)
          (string->symbol (send this assert-correct-type-and-get-stored-value 'symbol))
        )

        (super-new)
      )
    )

    (define db-keyword%
      (class* db-atom% (zinal:db:keyword%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-keyword this data)
        )

        (define/override (get-val)
          (string->keyword (send this assert-correct-type-and-get-stored-value 'keyword))
        )

        (super-new)
      )
    )

    (define db-legacy%
      (class* db-node% (zinal:db:legacy-link%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-legacy-link this data)
        )

        (define/override (delete-and-invalidate*!!)
          (dec-ref-count!! (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (get-library)
          (send this assert-valid)
          (define stored-result (get-cell* (send this get-id) "library"))
          (and (non-empty-string? stored-result) stored-result)
        )

        (define/public (get-name)
          (send this assert-valid)
          (get-cell* (send this get-id) "name")
        )

        (super-new)
      )
    )

    (define db-reference%
      (class* db-node% (zinal:db:reference%%) ; abstract

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-reference this data)
        )

        (define/public (get-referable)
          (send this assert-valid)
          (define referable-id (get-cell* (send this get-id) (get-referable-id-col)))
          (get-handle! (get-cell* referable-id "parent_id") (get-cell* referable-id "parent_col"))
        )

        (define/public (is-referable-visible?)
          (send this assert-valid)
          (define referable (get-referable))
          (findf (lambda (r) (send r equals? referable)) (send this get-visible-referables-after))
        )

        (abstract get-referable-id-col)

        (super-new)
      )
    )

    (define db-param-ref%
      (class* db-reference% (zinal:db:param-ref%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-param-ref this data)
        )

        (define/override (get-referable-id-col)
          "param_id"
        )

        (define/public (get-param)
          (send this get-referable)
        )

        (super-new)
      )
    )

    (define db-def-ref%
      (class* db-reference% (zinal:db:def-ref%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-def-ref this data)
        )

        (define/override (get-referable-id-col)
          "define_id"
        )

        (define/public (get-def)
          (send this get-referable)
        )

        (super-new)
      )
    )

    (define db-unassigned%
      (class* db-describable-node% (zinal:db:unassigned%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-unassigned this data)
        )

        (define/override (unassign!!)
          this
        )

        (define/override (delete-and-invalidate*!!)
          (delete-id*!! (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (assign-lambda!! [short-desc #f] [long-desc #f])
          (assign*!! (lambda (loc)
            (define lambda-id
              (create-parent!!
                "lambdas"
                loc
                short-desc
                long-desc
                (list
                  (list "req_params_id" BOGUS-ID)
                  (list "opt_params_id" BOGUS-ID)
                  (list "body_id" BOGUS-ID)
                )
              )
            )
            (create-list-header!! (new loc% [id lambda-id] [col "req_params_id"]))
            (create-list-header!! (new loc% [id lambda-id] [col "opt_params_id"]))
            (create-list-header!! (new loc% [id lambda-id] [col "body_id"]))
          ))
        )

        (define/public (assign-def!! [short-desc #f] [long-desc #f])
          (assign*!! (lambda (loc)
            (define define-id (create-parent!! "defines" loc short-desc long-desc (list (list "expr_id" BOGUS-ID))))
            (create-unassigned!! (new loc% [id define-id] [col "expr_id"]))
            (create-something!! "definitions" (list (list "define_id" define-id)))
          ))
        )

        (define/public (assign-list!! [short-desc #f] [long-desc #f])
          (assign*!! (curryr create-list-header!! short-desc long-desc))
        )

        (define/public (assign-def-ref!! def-handle)
          (assign-ref*!! (get-definition-id (send def-handle get-id)))
        )

        (define/public (assign-param-ref!! param-handle)
          (assign-ref*!! (get-param-ref-id (send param-handle get-id)))
        )

        (define/public (assign-number!! value)
          (assert (format "~a is not a number bruh" value) (number? value))
          (assign-atom*!! 'number (number->string value))
        )

        (define/public (assign-char!! value)
          (assert (format "~a is not a char, bruh" value) (char? value))
          (assign-atom*!! 'character (number->string (char->integer value)))
        )

        (define/public (assign-string!! value)
          (assert (format "~a is not a string, bruh" value) (string? value))
          (assign-atom*!! 'string value)
        )

        (define/public (assign-bool!! value)
          (assign-atom*!! 'boolean (if value "t" "f"))
        )

        (define/public (assign-symbol!! value)
          (assert (format "~a aint no symbol" value) (symbol? value))
          (assign-atom*!! 'symbol (symbol->string value))
        )

        (define/public (assign-keyword!! value)
          (assert (format "~a aint no keyword" value) (keyword? value))
          (assign-atom*!! 'keyword (keyword->string value))
        )

        (define/public (assign-legacy-link!! library name)
          (assert
            (format "Invalid library or identifier: ~a :: ~a" library name)
            (and (implies library (non-empty-string? library)) (non-empty-string? name))
          )
          ; TODO properly vet the library and name
          (define storage-lib (or library DEFAULT-LIBRARY))
          (assign*!! (lambda (loc)
            (define link-id
              (or
                (query-maybe-value db* "SELECT id FROM legacies WHERE library = ?1 AND name = ?2" storage-lib name)
                (create-something!! "legacies" (list (list "ref_count" 0) (list "library" storage-lib) (list "name" name)))
              )
            )
            (inc-ref-count!! link-id)
            (set-id!! loc link-id)
          ))
        )

        (define/private (assign-atom*!! type storage-value)
          (assign*!! (lambda (loc)
            (create-child!! "atoms" loc (list (list "type" (symbol->string type)) (list "value" storage-value)))
          ))
        )

        (define/private (assign-ref*!! ref-id)
          (assign*!! (lambda (loc)
            (set-id!! loc ref-id)
          ))
        )

        (define/private (assign*!! assigner!!)
          (send this assert-valid)
          (define loc (send this get-loc))
          (delete-and-invalidate*!!)
          (assigner!! loc)
          (get-handle! loc)
        )

        (super-new)
      )
    )

    ; HELPER CLASSES

    (define loc%
      (class object%
        (init id col)
        (assert (format "cannot create loc with id ~a which is not a number" id) (number? id))
        (super-new)
        (define id* id)
        (define col* col)
        (define/public (get-id) id*)
        (define/public (get-col) col*)
        (define/public (root?) (eq? this ROOT-LOC))
        (define/public (get-cell)
          (if (root?)
            PROG-START-ID
            (get-cell* id* col*)
          )
        )
        (define/public (get-id&col)
          (list id* col*)
        )
      )
    )
    (define ROOT-LOC (new loc% [id NIL-ID] [col #f]))

    ; HELPER FUNCTIONS

    ; id&col is a horrible abomination but it seems the least painful way to use locs as hash keys
    ; TODO we may be able to fix this if we wind up supporting init-field after all
    (define (id&col->loc id&col)
      (define id (first id&col))
      (if (= id (send ROOT-LOC get-id))
        ROOT-LOC
        (new loc% [id id] [col (second id&col)])
      )
    )

    (define (get-table id)
      (assert-real-id id)
      (vector-ref TABLES (modulo id (vector-length TABLES)))
    )

    ; query is executed "WHERE id = 'id'". Use ~a for the table, and ?2 ... for other q-parms
    (define (q!! q-proc query id . q-parms)
      (define table (get-table id))
      (apply q-proc db* (format (string-append query " WHERE id = ?1") table) id q-parms)
    )

    ; not purely functional, because of interning
    (define (get-handle! loc/id [col #f])
      (assert
        (format "the first arg of get-handle! must be a loc iff the second arg is #f: ~a ~a" loc/id col)
        (not (xor col (number? loc/id)))
      )
      (get-handle*!
        (if col
          (list loc/id col)
          (send loc/id get-id&col)
        )
      )
    )

    (define (get-handle*! id&col)
      (or
        (hash-ref handles* id&col #f)
        (create-handle*! id&col)
      )
    )

    (define (create-handle*! id&col)
      (define loc (id&col->loc id&col))
      (define id (send loc get-cell))
      (define table (get-table id))
      (define handle
        (case table
          [("lambdas") (new db-lambda% [loc loc])]
          [("defines") (new db-def% [loc loc])]
          [("list_headers") (new db-list% [loc loc])]
          [("params") (new db-param% [loc loc])]
          [("param_refs") (new db-param-ref% [loc loc])]
          [("definitions") (new db-def-ref% [loc loc])]
          [("atoms")
            (define type (string->symbol (get-cell* id "type")))
            (case type
              [(number) (new db-number% [loc loc])]
              [(character) (new db-char% [loc loc])]
              [(string) (new db-string% [loc loc])]
              [(boolean) (new db-bool% [loc loc])]
              [(symbol) (new db-symbol% [loc loc])]
              [(keyword) (new db-keyword% [loc loc])]
              [else (error 'create-handle*! "Invalid atom type ~a for id ~a" type id)]
            )
          ]
          [("legacies") (new db-legacy% [loc loc])]
          [("unassigned") (new db-unassigned% [loc loc])]
          [else (error 'create-handle*! "cannot create a handle for loc ~a of invalid type ~a" loc table)]
        )
      )
      (hash-set! handles* id&col handle)
      handle
    )

    (define (create-something-sql-string* table col-val-assocs)
      (define placeholders (build-list (length col-val-assocs) (compose1 (curry format "?~a") add1)))
      (define cols (map first col-val-assocs))
      (format "INSERT INTO ~a(~a) values(~a)" table (string-join cols ", ") (string-join placeholders ", "))
    )

    (define (create-something!! table col-val-assocs)
      (define id (get-next-id table))
      (define total-assocs (append (list (list "id" id)) col-val-assocs))
      (apply query-exec db* (create-something-sql-string* table total-assocs) id (map second col-val-assocs))
      id
    )

    (define (get-next-id table)
      (+ (vector-length TABLES) (sql:// (query-value db* (format "SELECT MAX(id) FROM ~a" table)) (get-table-mod* table)))
    )

    ; loc must be empty (i.e. BOGUS-ID) before calling this
    (define (create-child!! table loc col-value-assocs)
      (assert-bogus-id loc)
      (define id (create-something!! table col-value-assocs))
      (set-id!! loc id)
      id
    )

    ; loc must be empty (i.e. BOGUS-ID) before calling this
    ; If either desc is #f, it'll be stored as sql-null
    (define (create-describable-child!! table loc short-desc long-desc col-value-assocs)
      (define expanded-assocs
        (append
          (list (list "short_desc" (or-sql-null short-desc)) (list "long_desc" (or-sql-null long-desc)))
          col-value-assocs
        )
      )
      (create-child!! table loc expanded-assocs)
    )

    ; loc must be empty (i.e. BOGUS-ID) before calling this
    ; If either desc is #f, it'll be stored as sql-null
    (define (create-parent!! table loc short-desc long-desc col-value-assocs)
      (define expanded-assocs
        (append
          (list (list "parent_id" (send loc get-id)) (list "parent_col" (send loc get-col)))
          col-value-assocs
        )
      )
      (create-describable-child!! table loc short-desc long-desc expanded-assocs)
    )

    (define (create-unassigned!! loc)
      (create-describable-child!! "unassigned" loc #f #f '())
    )

    (define (create-list-header!! loc [short-desc #f] [long-desc #f])
      (create-parent!! "list_headers" loc short-desc long-desc (list (list "cdr_id" NIL-ID)))
    )

    (define (create-param!! loc required? [short-desc #f] [long-desc #f])
      (define param-id
        (create-parent!! "params" loc short-desc long-desc (list (list "default_id" (if required? NIL-ID BOGUS-ID))))
      )
      (unless required? (create-unassigned!! (new loc% [id param-id] [col "default_id"])))
      (create-something!! "param_refs" (list (list "param_id" param-id)))
      param-id
    )

    (define (assert-bogus-id loc)
      (define id (send loc get-cell))
      (assert
        (format "cell ~a:~a should be ~a but is ~a" (send loc get-id) (send loc get-col) BOGUS-ID id)
        (= BOGUS-ID id)
      )
    )

    (define (get-cell* id col)
      (assert-real-id id)
      (define result (q!! query-maybe-value (format "SELECT ~a FROM ~~a" col) id))
      (assert (format "Could not find cell (~a, ~a)" id col) result)
      result
    )

    ; WARNING: This function can orphan extant nodes.
    (define (set-loc-dangerous*!! loc value)
      (set-cell-dangerous*!! (send loc get-id) (send loc get-col) value)
    )

    ; WARNING: This function can orphan extant nodes.
    (define (set-cell-dangerous*!! id col value)
      (q!! query-exec (format "UPDATE ~~a SET ~a = ?2" col) id value)
    )

    (define (set-id!! loc id)
      (define col (send loc get-col))
      (assert (format "destination column does not end in '_id': ~a" col) (string-suffix? "_id" col))
      (assert-bogus-id loc)
      (set-loc-dangerous*!! loc id)
    )

    (define (dec-ref-count!! id)
      (change-ref-count*!! id (sub1 (get-cell* id "ref_count")))
    )

    (define (inc-ref-count!! id)
      (change-ref-count*!! id (add1 (get-cell* id "ref_count")))
    )

    (define (change-ref-count*!! id new-ref-count)
      (q!! query-exec "UPDATE ~a SET ref_count = ?2" id new-ref-count)
      new-ref-count
    )

    (define (get-param-ref-id param-id)
      (query-value db* "SELECT id FROM param_refs WHERE param_id = ?1" param-id)
    )

    (define (get-definition-id define-id)
      (query-value db* "SELECT id FROM definitions WHERE define_id = ?1" define-id)
    )

    (define (get-references* id)
      (append
        (get-references-of-type* "list_nodes" "car_id" id)
        (get-references-of-type* "defines" "expr_id" id)
        (get-references-of-type* "params" "default_id" id)
      )
    )

    (define (get-referables-of-type* table)
      (map
        (lambda (v) (new loc% [id (vector-ref v 0)] [col (vector-ref v 1)]))
        (query-rows db* (format "SELECT parent_id, parent_col FROM ~a" table))
      )
    )

    (define (get-references-of-type* table col id)
      (map
        (curryr get-handle! col)
        (query-list db* (format "SELECT id FROM ~a WHERE ~a = ?1" table col) id)
      )
    )

    (define (set-desc*!! short/long id new-desc)
      (assert
        (format "short/long must either be 'short or 'long: ~a" short/long)
        (or (equal? short/long 'short) (equal? short/long 'long))
      )
      (assert
        (format "~a is not a string or #f" new-desc)
        (implies new-desc (string? new-desc))
      )
      (define col (format "~a_desc" (symbol->string short/long)))
      (set-cell-dangerous*!! id col (or-sql-null new-desc))
    )

    (define (delete-id*!! id)
      (q!! query-exec "DELETE FROM ~a" id)
    )

    (define (get-visible-referables* location-node [check-younger-siblings? #f])
      (define parent (send location-node get-parent))
      (if parent
        (append
          (get-visible-sibling-referables* location-node (send parent get-children) check-younger-siblings?)
          (get-visible-referables* parent #t)
        )
        ; We assume root is not a referable
        '()
      )
    )

    (define (get-visible-sibling-referables* location-node siblings check-younger-siblings?)
      (define (not-location-node? sib)
        (not (send sib equals? location-node))
      )
      (define older (takef siblings not-location-node?))
      (append
        (filter
          (curryr is-a? zinal:db:referable%%)
          (cons location-node older)
        )
        (cond
          [(and check-younger-siblings? (function-definition? location-node))
            (define younger (cdr (dropf siblings not-location-node?)))
            (takef younger function-definition?)
          ]
          [else
            '()
          ]
        )
      )
    )

    (define (function-definition? handle)
      (and (is-a? handle zinal:db:def%%) (is-a? (send handle get-expr) zinal:db:lambda%%))
    )

    (define (all-references-are-descendants*? referable)
      (andmap
        (curryr descendant? referable)
        (send referable get-references)
      )
    )

    (define (descendant? child subroot)
      (define parent (send child get-parent))
      (or
        (send child equals? subroot)
        (and
          parent
          (descendant? parent subroot)
        )
      )
    )

    (super-new)

    (define filename* filename)
    (unless (file-exists? filename*) (close-output-port (open-output-file filename*)))
    (define db* (sqlite3-connect #:database filename*))
    (define sql-db* this)
    ; This should hold values weakly, but racket seems to only support weak keys.
    (define handles* (make-hash))

    (unless (positive? (file-size filename*))
      (vector-map
        (lambda (t)
          (query-exec db* (format "CREATE TABLE ~a(id INTEGER PRIMARY KEY, ~a)" t (string-join (hash-ref TABLES->NON-ID-COLS t) ", ")))
        )
        TABLES
      )

      (define first-id
        (create-something!! "list_headers"
          (list
            (list "parent_id" NIL-ID)
            (list "parent_col" sql-null)
            (list "short_desc" "Main Program")
            (list "long_desc" "")
            (list "cdr_id" NIL-ID)
          )
        )
      )
      (assert
        (format "first created item should have id ~a but has id ~a" PROG-START-ID)
        (= PROG-START-ID first-id)
      )
    )
  )
)
