; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module sql-db racket

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

; TABLE-INFO hashes map table names to cols.
; "hidden" cols refer to list nodes that are being used by the implementation but are invisible to the interface.
; get-parent invoked on any child of this hidden list will return the first ancestor that is not in one of these
; columns instead of the direct parent
; "can-be-ref" cols are allowed to have a reference as their value. No other col permits references

(define NORMAL-TABLE-INFO (hash
  "defined_classes"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"] ["superclass_id" "INT" 'can-be-ref] ["params_id" "INT" 'hidden] ["body_id" "INT" 'hidden])
  "insta_classes"
    '(["superclass_id" "INT" 'can-be-ref] ["body_id" "INT" 'hidden])
  "method_defines"
    '(["method_id" "INT"] ["lambda_id" "INT"])
  "legacy_overrides"
    '(["legacy_name" "TEXT"] ["lambda_id" "INT"])
  "super_inits"
    '(["args_id" "INT" 'hidden])
  "method_invokations"
    '(["object_id" "INT" 'can-be-ref] ["method_id" "INT"] ["args_id" "INT" 'hidden])
  "legacy_method_invokations"
    '(["object_id" "INT" 'can-be-ref] ["legacy_name" "TEXT"] ["args_id" "INT" 'hidden])
  "super_invokations"
    '(["method_id" "INT"] ["args_id" "INT" 'hidden])
  "legacy_super_invokations"
    '(["legacy_name" "TEXT"] ["args_id" "INT" 'hidden])
  "object_constructions"
    '(["class_id" "INT" 'can-be-ref] ["args_id" "INT" 'hidden])

  "list_headers"
    '(["cdr_id" "INT" 'hidden])
  "lambdas"
    '(["params_id" "INT" 'hidden] ["body_id" "INT" 'hidden])
  "params"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"] ["default_id" "INT" 'can-be-ref])
  "defines"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"] ["expr_id" "INT" 'can-be-ref])
  "asserts"
    '(["assertion_id" "INT" 'can-be-ref] ["format_string_id" "INT" 'can-be-ref] ["format_args_id" "INT" 'hidden])
  "list_nodes"
    '(["owner_id" "INT"] ["car_id" "INT" 'can-be-ref] ["cdr_id" "INT" 'hidden])
  "atoms"
    '(["type" "TEXT"] ["value" "TEXT"])
  "unassigned"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"])
))

; non-nodes (only nodes have "parents") or node data which can be pointed to from more than one place
(define NO-UNIQUE-PARENT-TABLE-INFO (hash
  "interfaces"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"])
  "methods"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"] ["container_id" "INT"])
  ; the body list inside the module is not "hidden" - rather, the module is "hidden", merely acting as a parent for the body
  "modules"
    '(["short_desc" "TEXT"] ["long_desc" "TEXT"] ["body_id" "INT"] ["is_main" "INT"])
  "param_refs"
    '(["param_id" "INT UNIQUE"])
  "define_refs"
    '(["define_id" "INT UNIQUE"])
  "class_refs"
    '(["class_id" "INT UNIQUE"])
  "interface_refs"
    '(["interface_id" "INT UNIQUE"])
  "legacies"
    '(["ref_count" "INT"] ["library" "TEXT"] ["name" "TEXT"])
))

; sets and relations
(define NO-ID-TABLE-INFO (hash
  "public_defs" '(["module_id" "INT"] ["public_def_id" "INT"])
  "requires" '(["requirer_id" "INT"] ["required_id" "INT"])
  "extends" '(["subtype_id" "INT"] ["supertype_id" "INT"])
))

(define ID-TABLES (list->vector (sort (append (hash-keys NORMAL-TABLE-INFO) (hash-keys NO-UNIQUE-PARENT-TABLE-INFO)) string<?)))

(define HIDDEN-NODE-COLS (make-hash))
(define CAN-BE-REF-COLS (make-hash))

(define (get-table-mod* table)
  (define mod (vector-member table ID-TABLES))
  (assert (format "Invalid table ~a" table) mod)
  mod
)

(define INVALID_LEGACIES '(
  "new"
  "super-new"
  "make-object"
  "super-make-object"
  "super"
  "send"
  "this"
  "class"
  "class*"
  "interface"
  "init"
  "abstract"
  "define/public"
  "define/override"
  "augment"
  "define"
  "lambda"
  "assert"
  "module"
  "require"
  "provide"
  "define-syntax-rule"
))

(define DEFAULT-LIBRARY "")

(define BOGUS-ID -1)

(define NIL-ID 0)

; get-next-id always returns a number at least as large as the length of ID-TABLES , so this is
; safe
(define THIS-ID 1)

(define SQL-FALSE 0)

(define SQL-TRUE 1)

(define sql-db%
  (class* object% (zinal:db%%)

    (init filename)

    (super-make-object)

    (define filename* filename)
    (unless (file-exists? filename*) (close-output-port (open-output-file filename*)))
    (define db* (sqlite3-connect #:database filename*))
    (define sql-db* this)
    ; This should hold values weakly, but racket seems to only support weak keys.
    (define handles* (make-hash))

    (define/public (get-all-modules)
      (map module-id->handle! (query-list db* "SELECT id FROM modules"))
    )

    (define/public (get-main-module)
      (define main-module-id (query-maybe-value db* "SELECT id FROM modules WHERE is_main"))
      (and main-module-id (module-id->handle! main-module-id))
    )

    (define/public (create-module!! [short-desc #f] [long-desc #f])
      (define new-module-id
        (create-describable!! "modules" short-desc #f (list
          (list "body_id" BOGUS-ID)
          (list "is_main" SQL-FALSE)
        ))
      )
      (create-list-header!! (make-object loc% new-module-id "body_id"))
      (module-id->handle! new-module-id)
    )

    (define/public (get-all-referables)
      (append* (get-all-interfaces) (map get-node-referables-of-type* '("params" "defines" "defined_classes")))
    )

    (define/public (get-all-interfaces)
      (map id->handle! (query-list db* "SELECT id FROM interfaces"))
    )

    (define/public (create-interface!! [short-desc #f] [long-desc #f])
      (define interface-id (create-describable!! "interfaces" short-desc long-desc '()))
      (create-something!! "interface_refs" (list (list "interface_id" interface-id)))
      (id->handle! interface-id)
    )

    (define/public (get-filename)
      filename*
    )

    ; DB ELEMENT IMPLEMENTATION CLASSES

    (define db-element% ; abstract
      (class* object% (zinal:db:element%%)

        (init id)

        (abstract delete-and-invalidate*!!)

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

        (define id* id)
        ; This object will be invalidated if the data it's referring to gets deleted or unassigned. Any attempts to do something with
        ; an invalid handle will cause an exception. Calling code is responsible for never using a stale handle, but this is a sanity
        ; check.
        (define valid*? #t)

        (super-new)
      )
    )

    (define db-non-node-element% (class db-element% ; abstract

      (init id)

      (define/override (invalidate!)
        (hash-remove! handles* (send this get-id))
        (super invalidate!)
      )

      (define/override (delete-and-invalidate*!!)
        (delete-id*!! (send this get-id))
        (send this invalidate!)
      )

      (super-make-object id)
    ))

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
          (convert-to-valid-parent* (send loc* get-id))
        )

        (define/public (get-module)
          (send this assert-valid)
          (send (get-parent) get-module)
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
          (define col (send loc* get-col))
          (define table (get-table (send loc* get-id)))
          (not (equal? "superclass_id" col))
        )

        (define/public (unassign!!)
          (send this assert-valid)
          (assert
            (format "Cannot unassign node (~a, ~a):~a" (send loc* get-id) (send loc* get-col) (send this get-id))
            (can-unassign?)
          )
          (delete-and-invalidate*!!)
          (create-unassigned!! loc*)
          (get-node-handle! loc*)
        )

        (define/public (get-loc)
          (send this assert-valid)
          loc*
        )

        (define (convert-to-valid-parent* parent-id)
          (define grandparent-id (get-cell* parent-id "parent_id"))
          (cond
            [(equal? "list_nodes" (get-table parent-id))
              ; optimization
              (convert-to-valid-parent* (get-cell* parent-id "owner_id"))
            ]
            [(hidden? (get-table grandparent-id) (get-cell* parent-id "parent_col"))
              (convert-to-valid-parent* grandparent-id)
            ]
            [else
              (id->handle! parent-id)
            ]
          )
        )

        (define loc* loc)

        (super-new [id (send loc get-cell)])
      )
    )

    (define db-describable-node%
      (class* db-node% (zinal:db:describable%%)

        (define/public (get-short-desc)
          (get-short-desc* this)
        )

        (define/public (get-long-desc)
          (get-long-desc* this)
        )

        (define/public (set-short-desc!! new-desc)
          (set-short-desc*!! this new-desc)
        )

        (define/public (set-long-desc!! new-desc)
          (set-long-desc*!! this new-desc)
        )

        (super-new)
      )
    )

    ; OOP

    (define db-interface% (class* db-non-node-element% (zinal:db:interface%%)

      (init id)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-interface this data)
      )

      (define/override (delete-and-invalidate*!!)
        (for-each (lambda (m) (remove-direct-method!! m)) (get-direct-methods))
        (delete-subtype-relations!! this)
        (delete-id*!! (get-reference-id*))
        (super delete-and-invalidate*!!)
      )

      (define/public (get-short-desc)
        (get-short-desc* this)
      )

      (define/public (get-long-desc)
        (get-long-desc* this)
      )

      (define/public (set-short-desc!! new-desc)
        (set-short-desc*!! this new-desc)
      )

      (define/public (set-long-desc!! new-desc)
        (set-long-desc*!! this new-desc)
      )

      (define/public (get-direct-super-interfaces)
        (get-direct-super-interfaces* this)
      )

      (define/public (can-add-direct-super-interface? to-super)
        (can-add-direct-super-interface*? this to-super)
      )

      (define/public (add-direct-super-interface!! to-super)
        (add-direct-super-interface*!! this to-super)
      )

      (define/public (can-remove-direct-super-interface? to-remove)
        (can-remove-direct-super-interface*? this to-remove)
      )

      (define/public (remove-direct-super-interface!! to-remove)
        (remove-direct-super-interface*!! this to-remove)
      )

      (define/public (get-all-methods)
        (get-all-methods* this)
      )

      (define/public (get-direct-methods)
        (get-direct-methods* this)
      )

      (define/public (add-direct-method!! [short-desc #f] [long-desc #f])
        (add-direct-method*!! this short-desc long-desc)
      )

      (define/public (can-remove-direct-method? to-remove)
        (can-remove-direct-method*? this to-remove)
      )

      (define/public (remove-direct-method!! to-remove)
        (remove-direct-method*!! this to-remove)
      )

      (define/public (can-delete?)
        (send this assert-valid)
        (and
          (null? (get-references))
          (andmap (lambda (m) (can-remove-direct-method? m)) (get-direct-methods))
          (not (query-maybe-value db* "SELECT 1 FROM extends WHERE supertype_id = ?1" (send this get-id)))
        )
      )

      (define/public (delete!!)
        (send this assert-valid)
        (assert (format "interface ~a cannot be deleted" (send this get-id)) (can-delete?))
        (delete-and-invalidate*!!)
        (void)
      )

      (define/public (get-references)
        (send this assert-valid)
        (get-references* (get-reference-id*))
      )

      (define/public (get-reference-id*)
        (send this assert-valid)
        (query-value db* "SELECT id FROM interface_refs WHERE interface_id = ?1" (send this get-id))
      )

      (super-make-object id)
    ))

    (define db-class% (class* db-node% (zinal:db:class%%) ; abstract

      (init loc)

      (abstract get-children)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-class this data)
      )

      (define/override (delete-and-invalidate*!!)
        (delete-subtype-relations!! this)
        (send (get-super-class) delete-and-invalidate*!!)
        (delete-and-invalidate-body*!! this)
        (delete-id*!! (send this get-id))
        (super delete-and-invalidate*!!)
      )

      (define/public (get-direct-super-interfaces)
        (get-direct-super-interfaces* this)
      )

      (define/public (can-add-direct-super-interface? to-super)
        (can-add-direct-super-interface*? this to-super)
      )

      (define/public (add-direct-super-interface!! to-super)
        (add-direct-super-interface*!! this to-super)
      )

      (define/public (can-remove-direct-super-interface? to-remove)
        (can-remove-direct-super-interface*? this to-remove)
      )

      (define/public (remove-direct-super-interface!! to-remove)
        (remove-direct-super-interface*!! this to-remove)
      )

      (define/public (get-all-methods)
        (get-all-methods* this)
      )

      (define/public (get-direct-definition-of-method method)
        (send this assert-valid)
        (assert-valid-method* this method)
        (findf (compose1 (curry equals*? method) get-method*) (get-define-methods this))
      )

      (define/public (is-method-abstract? method)
        (send this assert-valid)
        (assert-valid-method* this method)
        (not
          (or (get-direct-definition-of-method method) (does-any-super-define-method? this method))
        )
      )

      (define/public (is-method-overridden? method)
        (send this assert-valid)
        (assert-valid-method* this method)
        (and (get-direct-definition-of-method method) (does-any-super-define-method? this method))
      )

      (define/public (get-super-class)
        (send this assert-valid)
        (get-node-handle! (get-super-class-loc*))
      )

      (define/public (can-set-super-class?)
        (send this assert-valid)
        (define super-class (get-non-legacy-super-class* this))
        (implies super-class (can-remove-direct-super-type*? this super-class))
      )

      (define/public (set-super-class!! to-be-super)
        (send this assert-valid)
        (assert-is* to-be-super zinal:db:define-class%%)
        (assert-visible* (get-super-class) to-be-super)
        (set-super-class*!! (curryr create-reference!! to-be-super))
      )

      (define/public (set-legacy-super-class!! library name)
        (send this assert-valid)
        (set-super-class*!! (curryr create-legacy-node!! library name))
      )

      (define/public (get-body)
        (get-body* this)
      )

      (define/public (insert-into-body!! index)
        (insert-into-body*!! this index)
      )

      (define/public (remove-from-body!! index)
        (remove-from-body*!! this index)
      )

      (define (set-super-class*!! creator)
        (assert
          (format "~a cannot change super class, as doing so would orphan something" (send this get-id))
          (can-set-super-class?)
        )
        (send (get-super-class) delete-and-invalidate*!!)
        (define loc (get-super-class-loc*))
        (creator loc)
        (get-node-handle! loc)
      )

      (define (get-super-class-loc*)
        (make-object loc% (send this get-id) "superclass_id")
      )

      (super-make-object loc)
    ))

    (define db-define-class% (class* db-class% (zinal:db:define-class%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-define-class this data)
      )

      (define/override (can-unassign?)
        (send this assert-valid)
        (and
          (super can-unassign?)
          (andmap (lambda (m) (can-remove-direct-method? m)) (get-direct-methods))
          (all-references-are-descendants*? this)
        )
      )

      (define/override (get-visible-referables-underneath)
        (send this assert-valid)
        (append
          (get-all-params)
          (super get-visible-referables-underneath)
        )
      )

      (define/override (delete-and-invalidate*!!)
        (query-exec db* "DELETE FROM public_defs WHERE public_def_id = ?1" (send this get-id))
        (for-each (lambda (m) (remove-direct-method!! m)) (get-direct-methods))
        (delete-and-invalidate-params*!! this)
        (delete-id*!! (get-reference-id*))
        (super delete-and-invalidate*!!)
      )

      (define/override (get-children)
        (send this assert-valid)
        (append (list (send this get-super-class)) (get-all-params) (send this get-body))
      )

      (define/public (get-short-desc)
        (get-short-desc* this)
      )

      (define/public (get-long-desc)
        (get-long-desc* this)
      )

      (define/public (set-short-desc!! new-desc)
        (set-short-desc*!! this new-desc)
      )

      (define/public (set-long-desc!! new-desc)
        (set-long-desc*!! this new-desc)
      )

      (define/public (get-all-params)
        (get-all-params* this)
      )

      (define/public (get-required-params)
        (get-required-params* this)
      )

      (define/public (can-remove-required-param? index)
        (can-remove-required-param*? this index)
      )

      (define/public (remove-required-param!! index)
        (remove-required-param*!! this index)
      )

      (define/public (insert-required-param!! index [short-desc #f])
        (insert-required-param*!! this index short-desc)
      )

      (define/public (make-last-required-param-optional!!)
        (make-last-required-param-optional*!! this)
      )

      (define/public (get-optional-params)
        (get-optional-params* this)
      )

      (define/public (can-remove-optional-param? index)
        (can-remove-optional-param*? this index)
      )

      (define/public (remove-optional-param!! index)
        (remove-optional-param*!! this index)
      )

      (define/public (insert-optional-param!! index [short-desc #f])
        (insert-optional-param*!! this index short-desc)
      )

      (define/public (make-last-optional-param-required!!)
        (make-last-optional-param-required*!! this)
      )

      (define/public (get-direct-methods)
        (get-direct-methods* this)
      )

      (define/public (add-direct-method!! [short-desc #f] [long-desc #f])
        (add-direct-method*!! this short-desc long-desc)
      )

      (define/public (can-remove-direct-method? to-remove)
        (can-remove-direct-method*? this to-remove)
      )

      (define/public (remove-direct-method!! to-remove)
        (remove-direct-method*!! this to-remove)
      )

      (define/public (get-references)
        (send this assert-valid)
        (get-references* (get-reference-id*))
      )

      (define/public (get-reference-id*)
        (send this assert-valid)
        (query-value db* "SELECT id FROM class_refs WHERE class_id = ?1" (send this get-id))
      )

      (super-make-object loc)
    ))

    (define db-insta-class% (class* db-class% (zinal:db:class-instance%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-class-instance this data)
      )

      (define/override (get-children)
        (send this assert-valid)
        (cons (send this get-super-class) (send this get-body))
      )

      (super-make-object loc)
    ))

    (define db-this% (class* db-node% (zinal:db:this%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-this this data)
      )

      (super-make-object loc)
    ))

    (define db-method% (class* db-non-node-element% (zinal:db:method%%)

      (init id)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-method this data)
      )

      (define/public (get-short-desc)
        (get-short-desc* this)
      )

      (define/public (get-long-desc)
        (get-long-desc* this)
      )

      (define/public (set-short-desc!! new-desc)
        (set-short-desc*!! this new-desc)
      )

      (define/public (set-long-desc!! new-desc)
        (set-long-desc*!! this new-desc)
      )

      (define/public (get-containing-type)
        (id->handle! (get-cell* (send this get-id) "container_id"))
      )

      (super-make-object id)
    ))

    (define db-general-define-method% (class db-node% ; abstract

      (init loc)

      (define/override (delete-and-invalidate*!!)
        (send (get-lambda) delete-and-invalidate*!!)
        (delete-id*!! (send this get-id))
        (super delete-and-invalidate*!!)
      )

      (define/public (get-children)
        (send this assert-valid)
        (list (get-lambda))
      )

      (define/public (get-lambda)
        (send this assert-valid)
        (get-node-handle! (send this get-id) "lambda_id")
      )

      (super-make-object loc)
    ))

    (define db-define-method% (class* db-general-define-method% (zinal:db:define-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-define-method this data)
      )

      (define/override (can-unassign?)
        (send this assert-valid)
        (and (super can-unassign?) (can-undefine-method*? this))
      )

      (define/public (get-method)
        (send this assert-valid)
        (get-method* this)
      )

      (define/public (is-override?)
        (send this assert-valid)
        (does-any-super-define-method? this (get-method))
      )

      (super-make-object loc)
    ))

    (define db-legacy-override% (class* db-general-define-method% (zinal:db:override-legacy-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-override-legacy-method this data)
      )

      (define/public (get-legacy-method-name)
        (send this assert-valid)
        (get-cell* (send this get-id) "legacy_name")
      )

      (define/public (set-legacy-method-name!! name)
        (send this assert-valid)
        (assert-valid-legacy-method-name* name)
        (set-cell-dangerous*!! (send this get-id) "legacy_name" name)
        (void)
      )

      (super-make-object loc)
    ))

    (define db-node-with-args% (class* db-node% (zinal:db:has-args%%) ; abstract

      (init loc)

      (define/override (delete-and-invalidate*!!)
        (delete-and-invalidate-args*!! this)
        (delete-id*!! (send this get-id))
        (super delete-and-invalidate*!!)
      )

      (define/public (get-children)
        (send this assert-valid)
        (get-args)
      )

      (define/public (get-args)
        (get-args* this)
      )

      (define/public (insert-arg!! index)
        (insert-arg*!! this index)
      )

      (define/public (remove-arg!! index)
        (remove-arg!! this index)
      )

      (super-make-object loc)
    ))

    (define db-super-init% (class* db-node-with-args% (zinal:db:super-init%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-super-init this data)
      )

      (super-make-object loc)
    ))

    (define db-general-invoke% (class db-node-with-args% ; abstract

      (init loc)

      (define/override (delete-and-invalidate*!!)
        (send (get-object) delete-and-invalidate*!!)
        (super delete-and-invalidate*!!)
      )

      (define/override (get-children)
        (send this assert-valid)
        (cons (get-object) (super get-children))
      )

      (define/public (get-object)
        (send this assert-valid)
        (get-node-handle! (send this get-id) "object_id")
      )

      (super-make-object loc)
    ))

    (define db-method-invokation% (class* db-general-invoke% (zinal:db:invoke-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-invoke-method this data)
      )

      (define/public (get-method)
        (send this assert-valid)
        (get-method* this)
      )

      (define/public (set-method!! method)
        (send this assert-valid)
        (assert-is* method zinal:db:method%%)
        (assert-method-visible* this method)
        (set-cell-dangerous*!! (send this get-id) "method_id" (send method get-id))
        (void)
      )

      (super-make-object loc)
    ))

    (define db-legacy-invokation% (class* db-general-invoke% (zinal:db:invoke-legacy-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-invoke-legacy-method this data)
      )

      (define/public (get-legacy-method-name)
        (send this assert-valid)
        (get-cell* (send this get-id) "legacy_name")
      )

      (define/public (set-legacy-method-name!! name)
        (send this assert-valid)
        (assert-valid-legacy-method-name* name)
        (set-cell-dangerous*!! (send this get-id) "legacy_name" name)
        (void)
      )

      (super-make-object loc)
    ))

    (define db-super-invokation% (class* db-node-with-args% (zinal:db:invoke-super-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-invoke-super-method this data)
      )

      (define/public (get-method)
        (send this assert-valid)
        (get-method* this)
      )

      (define/public (set-method!! method)
        (send this assert-valid)
        (assert-is* method zinal:db:method%%)
        (assert-can-super-invoke-method* this method)
        (set-cell-dangerous*!! (send this get-id) "method_id" (send method get-id))
        (void)
      )

      (super-make-object loc)
    ))

    (define db-legacy-super-invokation% (class* db-node-with-args% (zinal:db:invoke-legacy-super-method%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-invoke-legacy-super-method this data)
      )

      (define/public (get-legacy-method-name)
        (send this assert-valid)
        (get-cell* (send this get-id) "legacy_name")
      )

      (define/public (set-legacy-method-name!! name)
        (send this assert-valid)
        (assert-valid-legacy-method-name* name)
        (set-cell-dangerous*!! (send this get-id) "legacy_name" name)
        (void)
      )

      (super-make-object loc)
    ))

    (define db-object-construction% (class* db-node-with-args% (zinal:db:create-object%%)

      (init loc)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-create-object this data)
      )

      (define/override (delete-and-invalidate*!!)
        (send (get-class-node) delete-and-invalidate*!!)
        (super delete-and-invalidate*!!)
      )

      (define/override (get-children)
        (send this assert-valid)
        (cons (get-class-node) (super get-children))
      )

      (define/public (get-class-node)
        (send this assert-valid)
        (get-node-handle! (send this get-id) "class_id")
      )

      (super-make-object loc)
    ))

    ; NON OOP

    (define db-lambda%
      (class* db-node% (zinal:db:lambda%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-lambda this data)
        )

        (define/override (can-unassign?)
          (send this assert-valid)
          (define parent (send this get-parent))
          (not (or (is-a? parent zinal:db:define-method%%) (is-a? parent zinal:db:override-legacy-method%%)))
        )

        (define/override (get-visible-referables-underneath)
          (send this assert-valid)
          (append
            (get-all-params)
            (super get-visible-referables-underneath)
          )
        )

        (define/override (delete-and-invalidate*!!)
          (delete-and-invalidate-body*!! this)
          (delete-and-invalidate-params*!! this)
          (delete-id*!! (send this get-id))
          (super delete-and-invalidate*!!)
        )

        (define/public (get-children)
          (send this assert-valid)
          (append (get-all-params) (get-body))
        )

        (define/public (get-all-params)
          (get-all-params* this)
        )

        (define/public (get-required-params)
          (get-required-params* this)
        )

        (define/public (can-remove-required-param? index)
          (can-remove-required-param*? this index)
        )

        (define/public (remove-required-param!! index)
          (remove-required-param*!! this index)
        )

        (define/public (insert-required-param!! index [short-desc #f])
          (insert-required-param*!! this index short-desc)
        )

        (define/public (make-last-required-param-optional!!)
          (make-last-required-param-optional*!! this)
        )

        (define/public (get-optional-params)
          (get-optional-params* this)
        )

        (define/public (can-remove-optional-param? index)
          (can-remove-optional-param*? this index)
        )

        (define/public (remove-optional-param!! index)
          (remove-optional-param*!! this index)
        )

        (define/public (insert-optional-param!! index [short-desc #f])
          (insert-optional-param*!! this index short-desc)
        )

        (define/public (make-last-optional-param-required!!)
          (make-last-optional-param-required*!! this)
        )

        (define/public (get-body)
          (get-body* this)
        )

        (define/public (insert-into-body!! index)
          (insert-into-body*!! this index)
        )

        (define/public (remove-from-body!! index)
          (remove-from-body*!! this index)
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
          (query-exec db* "DELETE FROM public_defs WHERE public_def_id = ?1" id)
          (send (get-expr) delete-and-invalidate*!!)
          (delete-id*!! (get-reference-id*))
          (delete-id*!! id)
          (super delete-and-invalidate*!!)
        )

        (define/public (get-references)
          (send this assert-valid)
          (get-references* (get-reference-id*))
        )

        (define/public (get-children)
          (send this assert-valid)
          (list (get-expr))
        )

        (define/public (get-expr)
          (send this assert-valid)
          (get-node-handle! (send this get-id) "expr_id")
        )

        (define/public (get-reference-id*)
          (send this assert-valid)
          (query-value db* "SELECT id FROM define_refs WHERE define_id = ?1" (send this get-id))
        )

        (super-new)
      )
    )

    (define db-list%
      (class* db-node% (zinal:db:list%%)

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
            (curryr get-node-handle! "car_id")
            (get-cdrs* (get-cell* (send this get-id) "cdr_id"))
          )
        )

        (define/public (insert!! index)
          (define car-loc (insert*!! index))
          (create-unassigned!! car-loc)
          (get-node-handle! car-loc)
        )

        (define/public (remove!! index)
          (remove*!! index #t)
        )

        (define/public (insert-param*!! index required? short-desc)
          (define car-loc (insert*!! index))
          (create-param!! car-loc required? short-desc)
          (get-node-handle! car-loc)
        )

        (define/public (insert*!! index)
          (send this assert-valid)
          (define list-header-id (send this get-id))
          (define insertion-point-id (nth-list-insertion-point* list-header-id index index))
          (define insertion-point-loc (new loc% [id insertion-point-id] [col "cdr_id"]))
          (define old-cdr (send insertion-point-loc get-cell))
          ; We captured the original cdr_id, so we can safely delete this node's cdr
          (set-loc-dangerous*!! insertion-point-loc BOGUS-ID)
          (define new-node-id
            (create-normal!! "list_nodes" insertion-point-loc (list (list "owner_id" list-header-id) (list "car_id" BOGUS-ID) (list "cdr_id" old-cdr)))
          )
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
            (define unassigned-handle (get-node-handle! loc-to-delete))
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

    (define db-module% (class* db-node% (zinal:db:module%%)

      (init loc)

      (define as-list-handle* (make-object db-list% loc))

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-module this data)
      )

      (define/override (invalidate!)
        (send as-list-handle* invalidate!)
        (super invalidate!)
      )

      (define/public (get-children)
        (send this assert-valid)
        (get-body)
      )

      (define/public (get-body)
        (send this assert-valid)
        (send as-list-handle* get-items)
      )

      (define/public (insert-into-body!! index)
        (send this assert-valid)
        (send as-list-handle* insert!! index)
      )

      (define/public (remove-from-body!! index)
        (send this assert-valid)
        (send as-list-handle* remove!! index)
      )

      (define/public (get-short-desc)
        (send this assert-valid)
        (get-short-desc-from-id* (get-module-id*))
      )

      (define/public (get-long-desc)
        (send this assert-valid)
        (get-long-desc-from-id* (get-module-id*))
      )

      (define/public (set-short-desc!! new-desc)
        (send this assert-valid)
        (set-short-desc-from-id*!! (get-module-id*) new-desc)
      )

      (define/public (set-long-desc!! new-desc)
        (send this assert-valid)
        (set-long-desc-from-id*!! (get-module-id*) new-desc)
      )

      (define/override (can-unassign?)
        (send this assert-valid)
        #f
      )

      (define/override (get-parent)
        (send this assert-valid)
        #f
      )

      (define/override (get-module)
        (send this assert-valid)
        this
      )

      (define/override (delete-and-invalidate*!!)
        (assert
          (format "delete-and-invalidate*!! should not have even been called if ~a is required by something" (get-module-id*))
          (null? (get-requiring-modules))
        )
        (define module-id (get-module-id*))
        (query-exec db* "DELETE FROM public_defs WHERE module_id = ?1" module-id)
        (query-exec db* "DELETE FROM requires WHERE requirer_id = ?1" module-id)
        (send as-list-handle* delete-and-invalidate*!!)
        ; Normally, this should be the last action, but in this case we need the module
        ; row to still exist when deleting the loc
        (super delete-and-invalidate*!!)
        (delete-id*!! module-id)
      )

      (define/public (get-public-defs)
        (send this assert-valid)
        (map id->handle! (query-list db* "SELECT public_def_id FROM public_defs WHERE module_id = ?1" (get-module-id*)))
      )

      (define/public (set-public!! index/def-handle new-value)
        (send this assert-valid)
        (define children (send this get-children))
        (define def-handle (if (number? index/def-handle) (list-ref children index/def-handle) index/def-handle))
        (define module-id (get-module-id*))
        (define def-handle-id (send def-handle get-id))
        (assert
          (format "You can only set the publicity of a zinal:db:def%% or zinal:db:define-class%% : ~a, ~a" module-id def-handle-id)
          (or (is-a? def-handle zinal:db:def%%) (is-a? def-handle zinal:db:define-class%%))
        )
        (assert (format "You can only set the publicity of a module's direct child: ~a, ~a" module-id def-handle-id) (find* def-handle children))
        (if new-value
          (unless (query-maybe-value db* "SELECT 1 FROM public_defs WHERE module_id = ?1 AND public_def_id = ?2" module-id def-handle-id)
            ; probably the correct way to do this is to use UNIQUE or IF NOT EXISTS or something, but whatever
            (query-exec db* "INSERT INTO public_defs(module_id, public_def_id) values(?1, ?2)" module-id def-handle-id)
          )
          (query-exec db* "DELETE FROM public_defs WHERE module_id = ?1 AND public_def_id = ?2" module-id def-handle-id)
        )
        (void)
      )

      (define/public (is-main-module?)
        (send this assert-valid)
        (define main-module (get-main-module*))
        (and main-module (equals*? this main-module))
      )

      (define/public (can-be-main-module?)
        (send this assert-valid)
        (and
          (implies (get-main-module*) (is-main-module?))
          (null? (get-requiring-modules))
        )
      )

      (define/public (set-main-module!! new-value)
        (send this assert-valid)
        (define module-id (get-module-id*))
        (assert (format "Module ~a cannot be main module" module-id) (implies new-value (can-be-main-module?)))
        (q!! query-exec "UPDATE ~a SET is_main = ?2" module-id (list (if new-value SQL-TRUE SQL-FALSE)))
        (void)
      )

      (define/public (get-required-modules)
        (send this assert-valid)
        (map module-id->handle! (query-list db* "SELECT required_id FROM requires WHERE requirer_id = ?1" (get-module-id*)))
      )

      (define/public (get-requiring-modules)
        (send this assert-valid)
        (map module-id->handle! (query-list db* "SELECT requirer_id FROM requires WHERE required_id = ?1" (get-module-id*)))
      )

      (define/public (can-require? to-be-required)
        (send this assert-valid)
        (assert-is* to-be-required zinal:db:module%%)
        (and
          (not (send to-be-required is-main-module?))
          (not (path? to-be-required this (lambda (m) (send m get-required-modules))))
        )
      )

      (define/public (require!! to-be-required)
        (send this assert-valid)
        (define module-id (get-module-id*))
        (define to-be-required-id (get-module-id (send to-be-required get-id)))
        (assert-is* to-be-required zinal:db:module%%)
        (assert (format "Module ~a cannot require module ~a" module-id to-be-required-id) (can-require? to-be-required))
        (unless (query-maybe-value db* "SELECT 1 FROM requires WHERE requirer_id = ?1 AND required_id = ?2" module-id to-be-required-id)
          ; probably the correct way to do this is to use UNIQUE or IF NOT EXISTS or something, but whatever
          (query-exec db* "INSERT INTO requires(requirer_id, required_id) values(?1, ?2)" module-id to-be-required-id)
        )
        (void)
      )

      (define/public (unrequire!! to-unrequire)
        (send this assert-valid)
        (assert-is* to-unrequire zinal:db:module%%)
        (query-exec db* "DELETE FROM requires WHERE requirer_id = ?1 AND required_id = ?2" (get-module-id*) (get-module-id (send to-unrequire get-id)))
        (void)
      )

      (define/public (can-delete?)
        (send this assert-valid)
        (and
          (null? (get-requiring-modules))
          (andmap
            (curryr all-references-are-descendants*? this)
            (filter (disjoin (curryr is-a? zinal:db:define-class%%) (curryr is-a? zinal:db:def%%)) (send this get-children))
          )
        )
      )

      (define/public (delete!!)
        (send this assert-valid)
        (assert (format "Cannot delete module: ~a" (get-module-id*)) (can-delete?))
        (delete-and-invalidate*!!)
        (void)
      )

      (define (get-module-id*)
        (get-module-id (send this get-id))
      )

      (define (get-main-module*)
        (send (send this get-db) get-main-module)
      )

      (super-make-object loc)
    ))

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
          (delete-id*!! (get-reference-id*))
          (delete-id*!! (send this get-id))
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
            (get-node-handle! id "default_id")
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
          (get-references* (get-reference-id*))
        )

        (define/public (get-reference-id*)
          (send this assert-valid)
          (query-value db* "SELECT id FROM param_refs WHERE param_id = ?1" (send this get-id))
        )

        (super-new)
      )
    )

    (define db-assert% (class* db-node% (zinal:db:assert%%)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-assert this data)
      )

      (define/override (delete-and-invalidate*!!)
        (send (get-assertion) delete-and-invalidate*!!)
        (send (get-format-string) delete-and-invalidate*!!)
        (send (get-format-args-list*) delete-and-invalidate*!!)
        (delete-id*!! (send this get-id))
        (super delete-and-invalidate*!!)
      )

      (define/public (get-children)
        (send this assert-valid)
        (append (list (get-assertion) (get-format-string)) (get-format-args))
      )

      (define/public (get-assertion)
        (send this assert-valid)
        (get-node-handle! (send this get-id) "assertion_id")
      )

      (define/public (get-format-string)
        (send this assert-valid)
        (get-node-handle! (send this get-id) "format_string_id")
      )

      (define/public (get-format-args)
        (send this assert-valid)
        (send (get-format-args-list*) get-items)
      )

      (define/public (insert-format-arg!! index)
        (send this assert-valid)
        (send (get-format-args-list*) insert!! index)
      )

      (define/public (remove-format-arg!! index)
        (send this assert-valid)
        (send (get-format-args-list*) remove!! index)
      )

      (define (get-format-args-list*)
        (get-node-handle! (send this get-id) "format_args_id")
      )

      (super-make-object)
    ))

    (define db-atom%
      (class* db-node% (zinal:db:atom%%) ; abstract

        (abstract get-val)

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

        (abstract get-referable-id-col)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-reference this data)
        )

        (define/public (get-referable)
          (send this assert-valid)
          (define referable-id (get-cell* (send this get-id) (get-referable-id-col)))
          (id->handle! referable-id)
        )

        (define/public (is-referable-visible?)
          (send this assert-valid)
          (is-referable-visible*? this (get-referable))
        )

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

    (define db-class-ref% (class* db-reference% (zinal:db:class-ref%%)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-define-class-ref this data)
      )

      (define/override (get-referable-id-col)
        "class_id"
      )

      (define/public (get-define-class)
        (send this get-referable)
      )

      (super-make-object)
    ))

    (define db-interface-ref% (class* db-reference% (zinal:db:interface-ref%%)

      (define/override (accept visitor [data #f])
        (send this assert-valid)
        (send visitor visit-define-class-ref this data)
      )

      (define/override (get-referable-id-col)
        "interface_id"
      )

      (define/public (get-interface)
        (send this get-referable)
      )

      (super-make-object)
    ))

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

        (define/public (assign-lambda!!)
          (assign*!! create-lambda!!)
        )

        (define/public (assign-assert!!)
          (assign*!! (lambda (loc)
            (define assert-id
              (create-normal!!
                "asserts"
                loc
                (list
                  (list "assertion_id" BOGUS-ID)
                  (list "format_string_id" BOGUS-ID)
                  (list "format_args_id" BOGUS-ID)
                )
              )
            )
            (create-unassigned!! (new loc% [id assert-id] [col "assertion_id"]))
            (create-unassigned!! (new loc% [id assert-id] [col "format_string_id"]))
            (create-list-header!! (new loc% [id assert-id] [col "format_args_id"]))
          ))
        )

        (define/public (assign-def!! [short-desc #f] [long-desc #f])
          (assign*!! (lambda (loc)
            (define define-id (create-describable-normal!! "defines" loc short-desc long-desc (list (list "expr_id" BOGUS-ID))))
            (create-unassigned!! (new loc% [id define-id] [col "expr_id"]))
            (create-something!! "define_refs" (list (list "define_id" define-id)))
          ))
        )

        (define/public (assign-list!!)
          (assign*!! create-list-header!!)
        )

        (define/public (assign-def-ref!! def-handle)
          (assert-visible* this def-handle)
          (assign-ref*!! def-handle)
        )

        (define/public (assign-param-ref!! param-handle)
          (assert-visible* this param-handle)
          (assign-ref*!! param-handle)
        )

        (define/public (assign-class-ref!! define-class-handle)
          (assert-visible* this define-class-handle)
          (assign-ref*!! define-class-handle)
        )

        (define/public (assign-interface-ref!! interface-handle)
          (assign-ref*!! interface-handle)
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
          (assert-valid-legacy* library name)
          (assign*!! (lambda (loc)
            (create-legacy-node!! loc library name)
          ))
        )

        (define/public (assign-define-class!! [short-desc #f] [long-desc #f])
          (assign*!! (lambda (loc)
            (define class-id
              (create-describable-normal!! "defined_classes" loc short-desc long-desc
                (list
                  (list "superclass_id" BOGUS-ID)
                  (list "params_id" BOGUS-ID)
                  (list "body_id" BOGUS-ID)
                )
              )
            )
            (create-something!! "class_refs" (list (list "class_id" class-id)))
            (create-legacy-node!! (make-object loc% class-id "superclass_id") #f "object%")
            (create-list-header!! (make-object loc% class-id "params_id"))
            (create-list-header!! (make-object loc% class-id "body_id"))
          ))
        )

        (define/public (assign-class-instance!!)
          (assign*!! (lambda (loc)
            (define class-id
              (create-normal!! "insta_classes" loc
                (list
                  (list "superclass_id" BOGUS-ID)
                  (list "body_id" BOGUS-ID)
                )
              )
            )
            (create-legacy-node!! (make-object loc% class-id "superclass_id") #f "object%")
            (create-list-header!! (make-object loc% class-id "body_id"))
          ))
        )

        (define/public (assign-invoke-method!! method)
          (assert-is* method zinal:db:method%%)
          (assert-method-visible* this method)
          (assign*!! (lambda (loc)
            (define method-invoke-id
              (create-normal!! "method_invokations" loc
                (list
                  (list "object_id" BOGUS-ID)
                  (list "method_id" (send method get-id))
                  (list "args_id" BOGUS-ID)
                )
              )
            )
            (create-unassigned!! (make-object loc% method-invoke-id "object_id"))
            (create-list-header!! (make-object loc% method-invoke-id "args_id"))
          ))
        )

        (define/public (assign-invoke-legacy-method!! method-name)
          (assert-valid-legacy-method-name* method-name)
          (assign*!! (lambda (loc)
            (define method-invoke-id
              (create-normal!! "legacy_method_invokations" loc
                (list
                  (list "object_id" BOGUS-ID)
                  (list "legacy_name" method-name)
                  (list "args_id" BOGUS-ID)
                )
              )
            )
            (create-unassigned!! (make-object loc% method-invoke-id "object_id"))
            (create-list-header!! (make-object loc% method-invoke-id "args_id"))
          ))
        )

        (define/public (assign-create-object!!)
          (assign*!! (lambda (loc)
            (define create-id
              (create-normal!! "object_constructions" loc
                (list
                  (list "class_id" BOGUS-ID)
                  (list "args_id" BOGUS-ID)
                )
              )
            )
            (create-unassigned!! (make-object loc% create-id "class_id"))
            (create-list-header!! (make-object loc% create-id "args_id"))
          ))
        )

        (define/public (assign-define-method!! method)
          (assert-is-within-class*)
          (define containing-class (get-containing-class*))
          (assert-valid-method* containing-class method)
          (define method-id (send method get-id))
          (assert
            (format "cannot define method ~a twice in class ~a" method-id (send containing-class get-id))
            (not (send containing-class get-direct-definition-of-method method))
          )
          (assign*!! (lambda (loc)
            (define define-method-id
              (create-normal!! "method_defines" loc (list
                (list "method_id" method-id)
                (list "lambda_id" BOGUS-ID)
              ))
            )
            (create-lambda!! (make-object loc% define-method-id "lambda_id"))
          ))
        )

        (define/public (assign-override-legacy-method!! method-name)
          (assert-valid-legacy-method-name* method-name)
          (assert-is-within-class*)
          (assign*!! (lambda (loc)
            (define override-id
              (create-normal!! "legacy_overrides" loc (list
                (list "legacy_name" method-name)
                (list "lambda_id" BOGUS-ID)
              ))
            )
            (create-lambda!! (make-object loc% override-id "lambda_id"))
          ))
        )

        (define/public (assign-this!!)
          (assert-is-within-class*)
          (assign*!! (curryr set-id!! THIS-ID))
        )

        (define/public (assign-invoke-super-method!! method)
          (assert-is* method zinal:db:method%%)
          (assert-is-within-class*)
          (assert-can-super-invoke-method* (get-containing-class*) method)
          (assign*!! (lambda (loc)
            (define invoke-id
              (create-normal!! "super_invokations" loc (list
                (list "method_id" (send method get-id))
                (list "args_id" BOGUS-ID)
              ))
            )
            (create-list-header!! (make-object loc% invoke-id "args_id"))
          ))
        )

        (define/public (assign-invoke-legacy-super-method!! method-name)
          (assert-valid-legacy-method-name* method-name)
          (assert-is-within-class*)
          (assign*!! (lambda (loc)
            (define invoke-id
              (create-normal!! "legacy_super_invokations" loc (list
                (list "legacy_name" method-name)
                (list "args_id" BOGUS-ID)
              ))
            )
            (create-list-header!! (make-object loc% invoke-id "args_id"))
          ))
        )

        (define/public (assign-super-init!!)
          (define id (send this get-id))
          (define containing-class (send this get-parent))
          (assert (format "~a is not the direct child of a class" id) (is-a? containing-class zinal:db:class%%))
          (assert
            (format "There can be only one ... super init for class ~a" id)
            (not (findf (curryr is-a? zinal:db:super-init%%) (send containing-class get-body)))
          )
          (assign*!! (lambda (loc)
            (define init-id (create-normal!! "super_inits" loc (list (list "args_id" BOGUS-ID))))
            (create-list-header!! (make-object loc% init-id "args_id"))
          ))
        )

        (define/private (assign-atom*!! type storage-value)
          (assign*!! (lambda (loc)
            (create-normal!! "atoms" loc (list (list "type" (symbol->string type)) (list "value" storage-value)))
          ))
        )

        (define/private (assign-ref*!! referable)
          (assign*!! (curryr create-reference!! referable))
        )

        (define/private (assign*!! assigner!!)
          (send this assert-valid)
          (define loc (send this get-loc))
          (delete-and-invalidate*!!)
          (assigner!! loc)
          (get-node-handle! loc)
        )

        (define (assert-is-within-class*)
          (assert (format "~a is not within a class" (send this get-id)) (get-containing-class*))
        )

        (define (get-containing-class* [node this])
          (and node (if (is-a? node zinal:db:class%%) node (get-containing-class* (send node get-parent))))
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
        (define/public (get-cell) (get-cell* id* col*))
        (define/public (get-id&col) (list id* col*))
      )
    )

    ; OOP HELPER FUNCTIONS

    (define (get-direct-super-interfaces* caller)
      (send caller assert-valid)
      (map id->handle! (query-list db* "SELECT supertype_id FROM extends WHERE subtype_id = ?1" (send caller get-id)))
    )

    (define (can-add-direct-super-interface*? caller to-super)
      (send caller assert-valid)
      (assert-is* to-super zinal:db:interface%%)
      (or
        (is-a? caller zinal:db:class%%)
        (not (path? to-super caller get-direct-super-interfaces*))
      )
    )

    (define (add-direct-super-interface*!! caller to-super)
      (send caller assert-valid)
      (define caller-id (send caller get-id))
      (define to-super-id (send to-super get-id))
      (assert
        (format "~a cannot extend ~a, cuz cycles!" caller-id to-super-id)
        (can-add-direct-super-interface*? caller to-super)
      )
      (unless (query-maybe-value db* "SELECT 1 FROM extends WHERE subtype_id = ?1 AND supertype_id = ?2" caller-id to-super-id)
        (query-exec db* "INSERT INTO extends(subtype_id, supertype_id) values(?1, ?2)" caller-id to-super-id)
      )
      (void)
    )

    (define (can-remove-direct-super-interface*? caller to-remove)
      (send caller assert-valid)
      (assert-is* to-remove zinal:db:interface%%)
      (assert
        (format "interface ~a is not a super interface of ~a" (send to-remove get-id) (send caller get-id))
        (find* to-remove (get-direct-super-interfaces* caller))
      )
      (can-remove-direct-super-type*? caller to-remove)
    )

    (define (remove-direct-super-interface*!! caller to-remove)
      (send caller assert-valid)
      (define caller-id (send caller get-id))
      (define to-remove-id (send to-remove get-id))
      (assert
        (format "~a cannot remove ~a, as doing so would orphan a method definition" caller-id to-remove-id)
        (can-remove-direct-super-type*? caller to-remove)
      )
      (query-exec db* "DELETE FROM extends WHERE subtype_id = ?1 AND supertype_id = ?2" caller-id to-remove-id)
      (void)
    )

    (define/public (get-all-methods* caller)
      (send caller assert-valid)
      (define direct-methods (if (is-a? caller zinal:db:type%%) (get-direct-methods* caller) '()))
      (append direct-methods (map get-direct-methods* (get-all-super-types caller)))
    )

    (define (get-direct-methods* caller)
      (send caller assert-valid)
      (map id->handle! (query-list db* "SELECT id FROM methods WHERE container_id = ?1" (send caller get-id)))
    )

    (define (add-direct-method*!! caller short-desc long-desc)
      (send caller assert-valid)
      (id->handle! (create-describable!! "methods" short-desc long-desc (list (list "container_id" (send caller get-id)))))
    )

    (define (can-remove-direct-method*? caller to-remove)
      (send caller assert-valid)
      (assert-is* to-remove zinal:db:method%%)
      (define caller-id (send caller get-id))
      (define to-remove-id (send to-remove-id get-id))
      (assert
        (format "Method ~a doesn't belong to type ~a" to-remove-id caller-id)
        (equals*? caller (send to-remove get-containing-type))
      )
      (define (clause table) (format "SELECT 1 FROM ~a WHERE method_id = ?1" table))
      (not
        (query-maybe-value db* (sql-union (map clause '("method_defines" "method_invokations" "super_invokations"))) to-remove-id)
      )
    )

    (define (remove-direct-method*!! caller to-remove)
      (send caller assert-valid)
      (assert
        (format "Method ~a is still being used somewhere and can't be deleted" (send to-remove get-id))
        (can-remove-direct-method*? caller to-remove)
      )
      (send to-remove delete-and-invalidate*!!)
      (void)
    )

    ; only returns zinal types - does not include the super class if the super class is a legacy
    (define (get-direct-super-types subtype)
      (define (superclass) (get-non-legacy-super-class* subtype))
      (define super-interfaces (get-direct-super-interfaces* subtype))
      (if (and (is-a? subtype zinal:db:class%%) (superclass))
        (cons (superclass) super-interfaces)
        super-interfaces
      )
    )

    (define (get-direct-sub-types-of-interface* interface)
      (map id->handle! (query-list db* "SELECT subtype_id FROM extends WHERE supertype_id = ?1" (send interface get-id)))
    )

    (define (get-direct-sub-classes-of-class* class)
      (define (clause table) (format "SELECT id FROM ~a WHERE superclass_id = ?1" table))
      (map id->handle! (query-list db* (sql-union (map clause '("defined_classes" "insta_classes"))) (send class get-id)))
    )

    (define (get-direct-sub-types* type)
      (if (is-a? type zinal:db:interface%%)
        (get-direct-sub-types-of-interface* type)
        (get-direct-sub-classes-of-class* type)
      )
    )

    (define (get-all-super-types subtype)
      (remove-duplicates (get-all-super-types* subtype) equals*?)
    )

    (define (get-all-super-types* subtype)
      (define super-types (get-direct-super-types subtype))
      (append* super-types (map get-all-super-types* super-types))
    )

    ; horrendously slow
    (define (get-all-subclasses type)
      (remove-duplicates (get-all-subclasses* type) equals*?)
    )

    (define (get-all-subclasses* type)
      (define subtypes (get-direct-sub-types* type))
      (append*
        (filter (curryr is-a? zinal:db:class%%) subtypes)
        (map get-all-subclasses subtypes)
      )
    )

    (define (does-this-or-any-super-declare-method? subtype method [severed-edge #f])
      (path? subtype (send method get-containing-type) get-direct-super-types severed-edge)
    )

    (define (does-any-super-define-method? subclass method [severed-edge #f])
      (define superclass (get-non-legacy-super-class* subclass))
      (and superclass (not (severed? severed-edge subclass superclass))
        (or
          (find* method (map get-method* (get-define-methods superclass)))
          (does-any-super-define-method? superclass method severed-edge)
        )
      )
    )

    (define (get-define-methods class)
      (filter (curryr is-a? zinal:db:define-method%%) (send class get-body))
    )

    (define (get-method* handle)
      (assert-is-one* handle (list zinal:db:define-method%% zinal:db:invoke-method%% zinal:db:invoke-super-method%%))
      (id->handle! (get-cell* (send handle get-id) "method_id"))
    )

    (define (get-super-invokations class/child)
      (define children (send class/child get-children))
      (append*
        (filter (curryr is-a? zinal:db:invoke-super-method%%) children)
        (map get-super-invokations (filter (negate (curryr is-a? zinal:db:class%%)) children))
      )
    )

    ; abysmally slow
    (define (can-remove-direct-super-type*? subtype supertype)
      (define edge-to-sever (list subtype supertype))
      (andmap
        (lambda (class-to-check)
          (define (check-relevant-nodes checker get-nodes)
            (andmap
              (lambda (m) (checker class-to-check m edge-to-sever))
              (map get-method* (get-nodes class-to-check))
            )
          )
          (and
            (check-relevant-nodes does-this-or-any-super-declare-method? get-define-methods)
            (implies (is-a? supertype zinal:db:class%%)
              (check-relevant-nodes does-any-super-define-method? get-super-invokations)
            )
          )
        )
        (cons subtype (get-all-subclasses subtype))
      )
    )

    (define (can-undefine-method*? define-method)
      (define containing-class (send define-method get-parent))
      (define method (get-method* define-method))
      (define (ok*? subclass)
        (and
          (not (findf (compose1 (curry equals*? method) get-method*) (get-super-invokations subclass)))
          (or
            (send subclass get-direct-definition-of-method method)
            (andmap ok*? (get-direct-sub-classes-of-class* subclass))
          )
        )
      )
      (or
        (does-any-super-define-method? containing-class method)
        (andmap ok*? (get-direct-sub-classes-of-class* containing-class))
      )
    )

    (define (delete-subtype-relations!! subtype)
      (query-exec db* "DELETE FROM extends WHERE subtype_id = ?1" (send subtype get-id))
    )

    (define (get-non-legacy-super-class* class)
      (define super-class-ref (send class get-super-class))
      (and (is-a? super-class-ref zinal:db:class-ref%%) (send super-class-ref get-define-class))
    )

    (define (assert-valid-method* caller method)
      (assert-is* method zinal:db:method%%)
      (assert
        (format "Method ~a not declared by ~a or any of its superclasses" (send method get-id) (send caller get-id))
        (does-this-or-any-super-declare-method? caller method)
      )
    )

    (define (assert-can-super-invoke-method* caller method)
      (assert
        (format "You can't super invoke method ~a because no super class of ~a defines it" (send method get-id) (send caller get-id))
        (does-any-super-define-method? caller method)
      )
    )

    ; DESCRIBABLE HELPERS

    (define (get-short-desc* caller)
      (send caller assert-valid)
      (get-short-desc-from-id* (send caller get-id))
    )

    (define (get-short-desc-from-id* caller-id)
      (sql:// (get-cell* caller-id "short_desc") #f)
    )

    (define (get-long-desc* caller)
      (send caller assert-valid)
      (get-long-desc-from-id* (send caller get-id))
    )

    (define (get-long-desc-from-id* caller-id)
      (sql:// (get-cell* caller-id "long_desc") #f)
    )

    (define (set-short-desc*!! caller new-desc)
      (send caller assert-valid)
      (set-short-desc-from-id*!! (send caller get-id) new-desc)
    )

    (define (set-short-desc-from-id*!! caller-id new-desc)
      (set-desc*!! 'short caller-id new-desc)
    )

    (define (set-long-desc*!! caller new-desc)
      (send caller assert-valid)
      (set-long-desc-from-id*!! (send caller get-id) new-desc)
    )

    (define (set-long-desc-from-id*!! caller-id new-desc)
      (set-desc*!! 'long caller-id new-desc)
    )

    ; HAS-PARAMS HELPERS

    (define (get-all-params* caller)
      (send caller assert-valid)
      (send (get-params-list* caller) get-items)
    )

    (define (get-required-params* caller)
      (send caller assert-valid)
      (takef (get-all-params* caller) (lambda (p) (not (send p get-default))))
    )

    (define (can-remove-required-param*? caller index)
      (send caller assert-valid)
      (assert-valid-param-index* caller index #t)
      (can-remove-param*? caller index)
    )

    (define (remove-required-param*!! caller index)
      (send caller assert-valid)
      (assert-valid-param-index* caller index #t)
      (remove-param*!! caller index)
    )

    (define (insert-required-param*!! caller index [short-desc #f])
      (send caller assert-valid)
      (send (get-params-list* caller) insert-param*!! index #t short-desc)
    )

    (define (make-last-required-param-optional*!! caller)
      (send caller assert-valid)
      (define reqd-params (get-required-params* caller))
      (assert "There is no required param to convert into an optional param" (pair? reqd-params))
      (define last-reqd-param-default-loc (new loc% [id (send (last reqd-params) get-id)] [col "default_id"]))
      (assert "attempt to convert optional param to optional" (= NIL-ID (send last-reqd-param-default-loc get-cell)))
      (set-loc-dangerous*!! last-reqd-param-default-loc BOGUS-ID)
      (create-unassigned!! last-reqd-param-default-loc)
      (void)
    )

    (define (get-optional-params* caller)
      (send caller assert-valid)
      (dropf (get-all-params* caller) (lambda (p) (not (send p get-default))))
    )

    (define (can-remove-optional-param*? caller index)
      (send caller assert-valid)
      (assert-valid-param-index* caller index #f)
      (can-remove-param*? caller (get-optional-index* caller index))
    )

    (define (remove-optional-param*!! caller index)
      (send caller assert-valid)
      (assert-valid-param-index* caller index #f)
      (remove-param*!! caller (get-optional-index* caller index))
    )

    (define (insert-optional-param*!! caller index [short-desc #f])
      (send caller assert-valid)
      (send (get-params-list* caller) insert-param*!! (get-optional-index* caller index) #f short-desc)
    )

    (define (make-last-optional-param-required*!! caller)
      (send caller assert-valid)
      (define opt-params (get-optional-params* caller))
      (assert "There is no optional param to convert into a required param" (pair? opt-params))
      (define first-opt-param (car opt-params))
      (send (send first-opt-param get-default) delete-and-invalidate*!!)
      (set-id!! (new loc% [id (send first-opt-param get-id)] [col "default_id"]) NIL-ID)
      (void)
    )

    (define (get-params-list* caller)
      (get-node-handle! (send caller get-id) "params_id")
    )

    (define (remove-param*!! caller index)
      (assert
        (format "Cannot delete ~ath required or optional param" index)
        (can-remove-param*? caller index)
      )
      (send (list-ref (get-all-params* caller) index) delete-and-invalidate*!!)
      (send (get-params-list* caller) remove*!! index #f)
    )

    (define (can-remove-param*? caller index)
      (all-references-are-descendants*? (list-ref (get-all-params* caller) index))
    )

    (define (get-optional-index* caller index)
      (+ index (length (get-required-params* caller)))
    )

    (define (assert-valid-param-index* caller index required?)
      (assert
        (format "index ~a not a valid index for ~a param" index (if required? "required" "optional"))
        (and (>= index 0) (< index (length (if required? (get-required-params* caller) (get-optional-params* caller)))))
      )
    )

    (define (delete-and-invalidate-params*!! caller)
      (send (get-params-list* caller) delete-and-invalidate*!!)
    )

    ; HAS-BODY HELPERS

    (define (get-body* caller)
      (send caller assert-valid)
      (send (get-body-list* caller) get-items)
    )

    (define (insert-into-body*!! caller index)
      (send caller assert-valid)
      (send (get-body-list* caller) insert!! index)
    )

    (define (remove-from-body*!! caller index)
      (send caller assert-valid)
      (send (get-body-list* caller) remove!! index)
    )

    (define (get-body-list* caller)
      (get-node-handle! (send caller get-id) "body_id")
    )

    (define (delete-and-invalidate-body*!! caller)
      (send (get-body-list* caller) delete-and-invalidate*!!)
    )

    ; HAS-ARGS HELPERS

    (define (get-args* caller)
      (send caller assert-valid)
      (send (get-args-list* caller) get-items)
    )

    (define (insert-arg*!! caller index)
      (send caller assert-valid)
      (send (get-args-list* caller) insert!! index)
    )

    (define (remove-arg*!! caller index)
      (send caller assert-valid)
      (send (get-args-list* caller) remove!! index)
    )

    (define (get-args-list* caller)
      (get-node-handle! (send caller get-id) "args_id")
    )

    (define (delete-and-invalidate-args*!! caller)
      (send (get-args-list* caller) delete-and-invalidate*!!)
    )

    ; HELPER FUNCTIONS

    ; NOTE - this can only be used on normal types, methods, and interfaces
    (define (id->handle! id)
      (define table (get-table id))
      (if (hash-has-key? NORMAL-TABLE-INFO table)
        (get-node-handle! (get-cell* id "parent_id") (get-cell* id "parent_col"))
        (case table
          [("interfaces" "methods")
            (get-non-node-handle! id)
          ]
          [else
            (error 'id->handle! "cannot call id->handle! on just any type of id: id ~a in table ~a" id table)
          ]
        )
      )
    )

    ; id&col is a horrible abomination but it seems the least painful way to use locs as hash keys
    (define (id&col->loc id&col)
      (new loc% [id (first id&col)] [col (second id&col)])
    )

    (define (make-id&col id col)
      (list id col)
    )

    (define (get-table id)
      (assert-real-id id)
      (vector-ref ID-TABLES (modulo id (vector-length ID-TABLES)))
    )

    ; query is executed "WHERE id = 'id'". Use ~a for the table, and ?2 ... for other q-parms
    (define (q!! q-proc query id [q-parms '()])
      (define table (get-table id))
      (apply q-proc db* (format (string-append query " WHERE id = ?1") table) id q-parms)
    )

    ; not purely functional, because of interning
    (define (get-node-handle! loc/id [col #f])
      (assert
        (format "the first arg of get-node-handle! must be a loc iff the second arg is #f: ~a ~a" loc/id col)
        (not (xor col (number? loc/id)))
      )
      (get-node-handle*!
        (if col
          (make-id&col loc/id col)
          (send loc/id get-id&col)
        )
      )
    )

    (define (get-node-handle*! id&col)
      (or
        (hash-ref handles* id&col #f)
        (create-node-handle*! id&col)
      )
    )

    (define (create-node-handle*! id&col)
      (define loc (id&col->loc id&col))
      (define id (send loc get-cell))
      (define (table) (get-table id))
      (define handle-class
        (if (= id THIS-ID)
          db-this%
          (case (table)
            [("defined_classes") db-define-class%]
            [("insta_classes") db-insta-class%]
            [("method_defines") db-define-method%]
            [("legacy_overrides") db-legacy-override%]
            [("super_inits") db-super-init%]
            [("method_invokations") db-method-invokation%]
            [("legacy_method_invokations") db-legacy-invokation%]
            [("super_invokations") db-super-invokation%]
            [("legacy_super_invokations") db-legacy-super-invokation%]
            [("object_constructions") db-object-construction%]
            [("lambdas") db-lambda%]
            [("defines") db-def%]
            [("list_headers")
              (if (get-module-id id)
                db-module%
                db-list%
              )
            ]
            [("params") db-param%]
            [("param_refs") db-param-ref%]
            [("define_refs") db-def-ref%]
            [("class_refs") db-class-ref%]
            [("interface_refs") db-interface-ref%]
            [("asserts") db-assert%]
            [("atoms")
              (define type (string->symbol (get-cell* id "type")))
              (case type
                [(number) db-number%]
                [(character) db-char%]
                [(string) db-string%]
                [(boolean) db-bool%]
                [(symbol) db-symbol%]
                [(keyword) db-keyword%]
                [else (error 'create-handle*! "Invalid atom type ~a for id ~a" type id)]
              )
            ]
            [("legacies") db-legacy%]
            [("unassigned") db-unassigned%]
            [else (error 'create-node-handle*! "cannot create a handle for loc ~a of invalid type ~a" loc (table))]
          )
        )
      )
      (define handle (make-object handle-class loc))
      (hash-set! handles* id&col handle)
      handle
    )

    (define (get-non-node-handle! id)
      (or
        (hash-ref handles* id #f)
        (create-non-node-handle*! id)
      )
    )

    (define (create-non-node-handle*! id)
      (define table (get-table id))
      (define handle
        (case table
          [("interfaces") (make-object db-interface% id)]
          [("methods") (make-object db-method% id)]
          [else (error 'create-non-node-handle*! "cannot create a handle for id ~a of invalid type ~a" id table)]
        )
      )
      (hash-set! handles* id handle)
      handle
    )

    (define (get-next-id table)
      (+ (vector-length ID-TABLES) (sql:// (query-value db* (format "SELECT MAX(id) FROM ~a" table)) (get-table-mod* table)))
    )

    (define (create-something-sql-string* table col-val-assocs)
      (define placeholders (build-list (length col-val-assocs) (compose1 (curry format "?~a") add1)))
      (define cols (map first col-val-assocs))
      (format "INSERT INTO ~a(~a) values(~a)" table (string-join cols ", ") (string-join placeholders ", "))
    )

    (define (col-value-assocs+descs* col-val-assocs short-desc long-desc)
      (assert
        (format "invalid short or long desc: ~a, ~a" short-desc long-desc)
        (and (valid-desc? short-desc) (valid-desc? long-desc))
      )
      (append
        (list (list "short_desc" (or-sql-null short-desc)) (list "long_desc" (or-sql-null long-desc)))
        col-val-assocs
      )
    )

    (define (create-something!! table col-val-assocs)
      (define id (get-next-id table))
      (define total-assocs (append (list (list "id" id)) col-val-assocs))
      (apply query-exec db* (create-something-sql-string* table total-assocs) id (map second col-val-assocs))
      id
    )

    ; If either desc is #f, it'll be stored as sql-null
    (define (create-describable!! table short-desc long-desc col-value-assocs)
      (create-something!! table (col-value-assocs+descs* col-value-assocs short-desc long-desc))
    )

    ; loc must be empty (i.e. BOGUS-ID) before calling this
    (define (create-normal!! table loc col-value-assocs)
      (assert-bogus-id loc)
      (define expanded-assocs
        (append
          (list (list "parent_id" (send loc get-id)) (list "parent_col" (send loc get-col)))
          col-value-assocs
        )
      )
      (define id (create-something!! table expanded-assocs))
      (set-id!! loc id)
      id
    )

    ; loc must be empty (i.e. BOGUS-ID) before calling this
    ; If either desc is #f, it'll be stored as sql-null
    (define (create-describable-normal!! table loc short-desc long-desc col-value-assocs)
      (create-normal!! table loc (col-value-assocs+descs* col-value-assocs short-desc long-desc))
    )

    (define (create-unassigned!! loc)
      (create-describable-normal!! "unassigned" loc #f #f '())
    )

    (define (create-list-header!! loc)
      (create-normal!! "list_headers" loc (list (list "cdr_id" NIL-ID)))
    )

    (define (create-lambda!! loc)
      (define lambda-id
        (create-normal!!
          "lambdas"
          loc
          (list
            (list "params_id" BOGUS-ID)
            (list "body_id" BOGUS-ID)
          )
        )
      )
      (create-list-header!! (new loc% [id lambda-id] [col "params_id"]))
      (create-list-header!! (new loc% [id lambda-id] [col "body_id"]))
      lambda-id
    )

    (define (create-param!! loc required? [short-desc #f] [long-desc #f])
      (define param-id
        (create-describable-normal!! "params" loc short-desc long-desc (list (list "default_id" (if required? NIL-ID BOGUS-ID))))
      )
      (unless required? (create-unassigned!! (new loc% [id param-id] [col "default_id"])))
      (create-something!! "param_refs" (list (list "param_id" param-id)))
      param-id
    )

    (define (create-legacy-node!! loc library name)
      (define storage-lib (or library DEFAULT-LIBRARY))
      (define link-id
        (or
          (query-maybe-value db* "SELECT id FROM legacies WHERE library = ?1 AND name = ?2" storage-lib name)
          (create-something!! "legacies" (list (list "ref_count" 0) (list "library" storage-lib) (list "name" name)))
        )
      )
      (inc-ref-count!! link-id)
      (set-id!! loc link-id)
      link-id
    )

    (define (create-reference!! loc referable)
      (set-id!! loc (send referable get-reference-id*))
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
      (q!! query-exec (format "UPDATE ~~a SET ~a = ?2" col) id (list value))
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
      (q!! query-exec "UPDATE ~a SET ref_count = ?2" id (list new-ref-count))
      new-ref-count
    )

    (define (get-module-id body-id)
      (query-maybe-value db* "SELECT id FROM modules WHERE body_id = ?1" body-id)
    )

    (define (module-id->handle! module-id)
      (get-node-handle! module-id "body_id")
    )

    (define (get-references* id)
      ; TODO use UNION to optimize this
      (append* (hash-map CAN-BE-REF-COLS (lambda (table cols) (append-map (lambda (col) (get-references-of-type* table col id)) cols))))
    )

    (define (get-references-of-type* table col id)
      (map
        (curryr get-node-handle! col)
        (query-list db* (format "SELECT id FROM ~a WHERE ~a = ?1" table col) id)
      )
    )

    (define (get-node-referables-of-type* table)
      (map
        (lambda (v) (get-node-handle! (vector-ref v 0) (vector-ref v 1)))
        (query-rows db* (format "SELECT parent_id, parent_col FROM ~a" table))
      )
    )

    (define (set-desc*!! short/long id new-desc)
      (assert
        (format "short/long must either be 'short or 'long: ~a" short/long)
        (or (equal? short/long 'short) (equal? short/long 'long))
      )
      (assert
        (format "~a is not a string or #f" new-desc)
        (valid-desc? new-desc)
      )
      (define col (format "~a_desc" (symbol->string short/long)))
      (set-cell-dangerous*!! id col (or-sql-null new-desc))
    )

    (define (valid-desc? s)
      (implies s (string? s))
    )

    (define (delete-id*!! id)
      (q!! query-exec "DELETE FROM ~a" id)
    )

    (define (is-referable-visible*? location-node referable)
      (or
        (is-a? referable zinal:db:interface%%)
        (find* referable (send location-node get-visible-referables-after))
      )
    )

    (define (get-visible-referables* location-node)
      (append*
        (get-visible-referables-recursive* location-node #f)
        (get-all-interfaces)
        (map (lambda (m) (send m get-public-defs)) (send (send location-node get-module) get-required-modules))
      )
    )

    (define (get-visible-referables-recursive* location-node check-younger-siblings?)
      (define parent (send location-node get-parent))
      (if parent
        (append
          (get-visible-sibling-referables* location-node (send parent get-children) check-younger-siblings?)
          (get-visible-referables-recursive* parent #t)
        )
        '()
      )
    )

    (define (get-visible-sibling-referables* location-node siblings check-younger-siblings?)
      (define not-location-node? (negate (curry equals*? location-node)))
      (define older (takef siblings not-location-node?))
      (append
        (filter
          (curryr is-a? zinal:db:referable%%)
          (cons location-node older)
        )
        (cond
          [(and check-younger-siblings? (visible-to-elders? location-node))
            (define younger (cdr (dropf siblings not-location-node?)))
            (takef younger visible-to-elders?)
          ]
          [else
            '()
          ]
        )
      )
    )

    (define (visible-to-elders? handle)
      (or (function-definition? handle) (ormap (curry is-a? handle) (list zinal:db:define-method%% zinal:db:override-legacy-method%% zinal:db:define-class%%)))
    )

    (define (function-definition? handle)
      (and (is-a? handle zinal:db:def%%) (is-a? (send handle get-expr) zinal:db:lambda%%))
    )

    (define (descendant? child subroot)
      (define parent (send child get-parent))
      (or
        (equals*? child subroot)
        (and
          parent
          (descendant? parent subroot)
        )
      )
    )

    (define (all-references-are-descendants*? referable [ancestor referable])
      (andmap
        (curryr descendant? ancestor)
        (send referable get-references)
      )
    )

    (define (assert-is* handle type)
      (assert (format "~a is not a ~a:" (send handle get-id) type) (is-a? handle type))
    )

    (define (assert-is-one* handle types)
      (assert (format "~a is not one of ~a:" (send handle get-id) types) (ormap (curry is-a? handle) types))
    )

    (define (assert-valid-legacy* library name)
      ; TODO properly vet the library and name
      (assert
        (format "Invalid library or identifier: ~a :: ~a" library name)
        (and (implies library (non-empty-string? library)) (non-empty-string? name))
      )
      (assert
        (format "You can't use a legacy for which there's a corresponding primitive: ~a" name)
        (or library (not (member name INVALID_LEGACIES)))
      )
    )

    (define (assert-valid-legacy-method-name* name)
      (assert (format "Legacy method name '~a' is not a non-empty string" name) (non-empty-string? name))
    )

    (define (assert-visible* location-node ref-handle)
      (assert
        (format
          "You cannot create a reference (at ~a) to point to a referable (~a) that's not visible to it"
          (send location-node get-id)
          (send ref-handle get-id)
        )
        (is-referable-visible*? location-node ref-handle)
      )
    )

    (define (assert-method-visible* location-node method)
      (define container (send method get-containing-type))
      (unless (is-a? container zinal:db:interface%%) (assert-visible* location-node container))
    )

    (define (path? a b get-children* [severed-edge #f])
      (or
        (equals*? a b)
        (ormap
          (curryr path? b get-children* severed-edge)
          (filter (negate (curry severed? severed-edge a)) (get-children* a))
        )
      )
    )

    (define (severed? severed-edge from to)
      (and severed-edge (equals*? from (first severed-edge)) (equals*? to (second severed-edge)))
    )

    (define (sql-union clauses)
      (string-join clauses " UNION ")
    )

    (define (find* v lst)
      (findf (curry equals*? v) lst)
    )

    (define (equals*? elem1 elem2)
      (send elem1 equals? elem2)
    )

    (define (hidden? table col)
      (member col (hash-ref HIDDEN-NODE-COLS table))
    )

    (define (can-be-ref? table col)
      (member col (hash-ref CAN-BE-REF-COLS table))
    )

    ; INIT

    (define (hidden*? col-info)
      (and (> (length col-info) 2) (equal? ''hidden (third col-info)))
    )

    (define (can-be-ref*? col-info)
      (and (> (length col-info) 2) (equal? ''can-be-ref (third col-info)))
    )

    (define (col-info->col* col-info)
      (first col-info)
    )

    (define create-db*? (zero? (file-size filename*)))

    (define (create-tables tables->cols [extra-cols '()])
      (hash-map
        tables->cols
        (lambda (table col-infos)
          (when create-db*?
            (define col-strings (map (lambda (x) (format "~a ~a" (first x) (second x))) (append extra-cols col-infos)))
            (query-exec db* (format "CREATE TABLE ~a(~a)" table (string-join col-strings ", ")))
          )

          (define hidden-node-cols (filter-map (lambda (c) (and (hidden*? c) (col-info->col* c))) col-infos))
          (hash-update! HIDDEN-NODE-COLS table (curry append hidden-node-cols) '())

          (define can-be-ref-cols (filter-map (lambda (c) (and (can-be-ref*? c) (col-info->col* c))) col-infos))
          (hash-update! CAN-BE-REF-COLS table (curry append can-be-ref-cols) '())
        )
      )
    )

    (create-tables NORMAL-TABLE-INFO '(["id" "INTEGER PRIMARY KEY"] ["parent_id" "INT"] ["parent_col" "TEXT"]))
    (create-tables NO-UNIQUE-PARENT-TABLE-INFO '(["id" "INTEGER PRIMARY KEY"]))
    (create-tables NO-ID-TABLE-INFO)
  )
)
)
