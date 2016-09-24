#lang racket

(require db)

(require "misc.rkt")
(require "db.rkt")

(provide (prefix-out veme: sql-db%))

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
  "lambdas" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "arity INT" "body_id INT")
  "params" '("short_desc TEXT" "long_desc TEXT" "lambda_id INT" "position INT")
  "defines" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT" "expr_id INT")
  "definitions" '("define_id INT UNIQUE")
  "list_nodes" '("owner_id INT" "car_id INT" "cdr_id INT")
  "atoms" '("parent_id INT" "parent_col TEXT" "type TEXT" "value TEXT")
  "legacies" '("ref_count INT" "library TEXT" "name TEXT")
  "unassigned" '("parent_id INT" "parent_col TEXT" "short_desc TEXT" "long_desc TEXT")
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
  (class* object% (veme:db%%)

    (init filename)

    (define/public (get-root)
      (get-handle! ROOT-LOC)
    )

    (define/public (get-referables)
      (map get-handle!
        (append
          (map
            (lambda (v) (new loc% [id (vector-ref v 0)] [col (vector-ref v 1)]))
            (query-rows db* "SELECT parent_id, parent_col FROM defines")
          )
          (query-list db* "SELECT id FROM params")
        )
      )
    )

    (define/public (get-filename)
      filename*
    )

    ; DB ELEMENT IMPLEMENTATION CLASSES

    (define db-element%
      (class* object% (veme:db:element%%)

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

        (define id* id)
        ; This object will be invalidated if the data it's referring to gets deleted or unassigned. Any attempts to do something with
        ; an invalid handle will cause an exception. Calling code is responsible for never using a stale handle, but this is a sanity
        ; check.
        (define valid*? #t)

        (super-new)
      )
    )

    (define db-node%
      (class* db-element% (veme:db:node%%)

        (init loc)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-node this data)
        )

        (define/override (invalidate!)
          (hash-remove! handles* (send loc* get-id&col))
          (super invalidate!)
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
              ; We want the parent of a lambda body expr to be the lambda, not the list it's hidden in
              (define parent-id
                (if (equal? "body_id" (get-cell* direct-parent-id "parent_col"))
                  (get-cell* direct-parent-id "parent_id")
                  direct-parent-id
                )
              )
              (if (= parent-id PROG-START-ID)
                (get-handle! ROOT-LOC)
                (get-handle! (get-cell* parent-id "parent_id") (get-cell* parent-id "parent_col"))
              )
            ]
          )
        )

        (define/public (get-visible-referables-underneath)
          (get-visible-referables*)
        )

        (define/public (get-visible-referables-after)
          (get-visible-referables*)
        )

        (define/public (unassign!!)
          (send this assert-valid)
          ; TODO NYI
          ; TODO make sure you can't unassign (or do any other weird stuff) the root list
        )

        (define/public (get-loc)
          (send this assert-valid)
          loc*
        )

        (define (get-visible-referables*)
          (define parent (get-parent))
          (if parent
            (append
              (get-visible-sibling-referables* (send parent get-node-children))
              (send parent get-visible-referables-underneath)
            )
            ; We assume root is not a referable
            '()
          )
        )

        (define (get-visible-sibling-referables* siblings)
          (define preceding (takef siblings (lambda (s) (not (send s equals? this)))))
          (filter
            (curryr is-a? veme:db:referable%%)
            (cons this preceding)
          )
        )

        (define loc* loc)

        (super-new [id (send loc get-cell)])
      )
    )

    ; TODO we should probably do mixins for this but not sure i want to have much OOP functionality
    ; before boostrapping
    (define db-describable-node%
      (class* db-node% (veme:db:describable%%)
        (define/public (get-short-desc)
          (send this assert-valid)
          (sql:// (get-cell* (send this get-id) "short_desc") #f)
        )

        (define/public (get-long-desc)
          (send this assert-valid)
          (sql:// (get-cell* (send this get-id) "long_desc") #f)
        )

        ; TODO we could probably factor this a bit
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
      (class* db-describable-node% (veme:db:lambda%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-lambda this data)
        )

        (define/override (get-visible-referables-underneath)
          (append
            (get-params)
            (super get-visible-referables-underneath)
          )
        )

        (define/public (get-params)
          (send this assert-valid)
          (define id (send this get-id))
          (build-list (get-cell* id "arity") (compose1 get-handle! (curry get-param-id id)))
        )

        (define/public (get-node-children)
          (get-body)
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

        (define (get-body-list*)
          (get-handle! (send this get-id) "body_id")
        )

        (super-new)
      )
    )

    (define db-def%
      (class* db-describable-node% (veme:db:def%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-def this data)
        )

        (define/public (get-references)
          (send this assert-valid)
          (get-references* (get-definition-id (send this get-id)))
        )

        (define/public (get-node-children)
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
      (class* db-describable-node% (veme:db:list%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-list this data)
        )

        (define/public (get-node-children)
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
          ; TODO add bounds checking!! probably need to add bounds checking elsewhere in the code too
          (send this assert-valid)
          (define list-header-id (send this get-id))
          (define insertion-point-id (nth-list-insertion-point* list-header-id index))
          (define insertion-point-loc (new loc% [id insertion-point-id] [col "cdr_id"]))
          (define old-cdr (send insertion-point-loc get-cell))
          (define new-node-id
            (create-something!! "list_nodes" (list (list "owner_id" list-header-id) (list "car_id" BOGUS-ID) (list "cdr_id" old-cdr)))
          )
          (define car-loc (new loc% [id new-node-id] [col "car_id"]))
          (create-unassigned!! car-loc)
          ; We captured the original cdr_id, and moved it to the newly created node, so we can safely replace this node's cdr
          (set-loc-dangerous*!! insertion-point-loc new-node-id)
          (get-handle! car-loc)
        )

        (define/public (remove!! index)
          (send this assert-valid)
          ; TODO NYI
        )

        (super-new)
      )
    )

    (define db-param%
      (class* db-element% (veme:db:param%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-param this data)
        )

        (define/override (invalidate!)
          (hash-remove! handles* (send this get-id))
          (super invalidate!)
        )

        (define/public (get-pos)
          (send this assert-valid)
          (get-cell* (send this get-id) "position")
        )

        (define/public (get-lambda)
          (send this assert-valid)
          (get-handle! (send this get-id) "lambda_id")
        )

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

        (define/public (get-references)
          (send this assert-valid)
          (get-references* (send this get-id))
        )

        (super-new)
      )
    )

    (define db-atom%
      (class* db-node% (veme:db:atom%%) ; abstract

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-atom this data)
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
      (class* db-atom% (veme:db:number%%)

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
      (class* db-atom% (veme:db:char%%)

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
      (class* db-atom% (veme:db:string%%)

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
      (class* db-atom% (veme:db:bool%%)

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
      (class* db-atom% (veme:db:symbol%%)

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

    (define db-legacy%
      (class* db-node% (veme:db:legacy-link%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-legacy-link this data)
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
      (class* db-node% (veme:db:reference%%) ; abstract

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-reference this data)
        )

        (super-new)

        (abstract get-referable)
      )
    )

    (define db-param-ref%
      (class* db-reference% (veme:db:param-ref%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-param-ref this data)
        )

        (define/override (get-referable)
          (get-param)
        )

        (define/public (get-param)
          (send this assert-valid)
          (get-handle! (send this get-id))
        )

        (super-new)
      )
    )

    (define db-def-ref%
      (class* db-reference% (veme:db:def-ref%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-def-ref this data)
        )

        (define/override (get-referable)
          (get-def)
        )

        (define/public (get-def)
          (send this assert-valid)
          (define define-id (get-cell* (send this get-id) "define_id"))
          (get-handle! (get-cell* define-id "parent_id") (get-cell* define-id "parent_col"))
        )

        (super-new)
      )
    )

    (define db-unassigned%
      (class* db-describable-node% (veme:db:unassigned%%)

        (define/override (accept visitor [data #f])
          (send this assert-valid)
          (send visitor visit-unassigned this data)
        )

        (define/public (assign-lambda!! arity [short-desc #f] [long-desc #f])
          (assert (format "negative arity: ~a" arity) (non-negative? arity))
          (assign*!! (lambda (loc)
            (define lambda-id
              (create-describable-child!! "lambdas" loc short-desc long-desc (list (list "arity" arity) (list "body_id" BOGUS-ID)))
            )
            (create-list-header!! (new loc% [id lambda-id] [col "body_id"]))
            (build-list arity (curry create-param!! lambda-id))
          ))
        )

        (define/public (assign-def!! [short-desc #f] [long-desc #f])
          (assign*!! (lambda (loc)
            (define define-id (create-describable-child!! "defines" loc short-desc long-desc (list (list "expr_id" BOGUS-ID))))
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
          (assign-ref*!! (send param-handle get-id))
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
          (send this invalidate!)
          (delete-unreferenceable-leaf*!! loc)
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
    (define ROOT-LOC (new loc% [id BOGUS-ID] [col #f]))

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
        (format "get-handle! cannot be called with a col if loc/id is a loc: ~a" loc/id)
        (implies col (number? loc/id))
      )
      (get-handle*!
        (cond
          [col (list loc/id col)]
          [(is-a? loc/id loc%) (send loc/id get-id&col)]
          [else loc/id]
        )
      )
    )

    (define (get-handle*! id&col/id)
      (or
        (hash-ref handles* id&col/id #f)
        (create-handle*! (if (cons? id&col/id) get-node-handle* get-param-handle*) id&col/id)
      )
    )

    (define (create-handle*! handle-getter id&col/id)
      (define handle (handle-getter id&col/id))
      (hash-set! handles* id&col/id handle)
      handle
    )

    (define (get-param-handle* id)
      (new db-param% [id id])
    )

    (define (get-node-handle* id&col)
      (define loc (id&col->loc id&col))
      (define id (send loc get-cell))
      (define table (get-table id))
      (case table
        [("lambdas") (new db-lambda% [loc loc])]
        [("defines") (new db-def% [loc loc])]
        [("list_headers") (new db-list% [loc loc])]
        [("params") (new db-param-ref% [loc loc])]
        [("definitions") (new db-def-ref% [loc loc])]
        [("atoms")
          (define type (string->symbol (get-cell* id "type")))
          (case type
            [(number) (new db-number% [loc loc])]
            [(character) (new db-char% [loc loc])]
            [(string) (new db-string% [loc loc])]
            [(boolean) (new db-bool% [loc loc])]
            [(symbol) (new db-symbol% [loc loc])]
            [else (error 'get-node-handle* "Invalid atom type ~a for id ~a" type id)]
          )
        ]
        [("legacies") (new db-legacy% [loc loc])]
        [("unassigned") (new db-unassigned% [loc loc])]
        [else (error 'create-node-handle*! "cannot create a handle for loc ~a of invalid type ~a" loc table)]
      )
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
      (define id
        (create-something!! table
          (append
            (list (list "parent_id" (send loc get-id)) (list "parent_col" (send loc get-col)))
            col-value-assocs
          )
        )
      )
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

    (define (create-unassigned!! loc)
      (create-describable-child!! "unassigned" loc #f #f '())
    )

    (define (create-list-header!! loc [short-desc #f] [long-desc #f])
      (create-describable-child!! "list_headers" loc short-desc long-desc (list (list "cdr_id" NIL-ID)))
    )

    (define (create-param!! lambda-id position)
      (define arity (get-cell* lambda-id "arity"))
      (assert
        (format "attempt to make param with position ~a for lambda ~a with arity ~a" position lambda-id arity)
        (and (non-negative? position) (< position arity))
      )
      (create-something!! "params" (list
        (list "short_desc" sql-null)
        (list "long_desc" sql-null)
        (list "lambda_id" lambda-id)
        (list "position" position)
      ))
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

    (define (inc-ref-count!! id)
      (define new-ref-count (add1 (get-cell* id "ref_count")))
      (q!! query-exec "UPDATE ~a SET ref_count = ?2" id new-ref-count)
      new-ref-count
    )

    (define (get-definition-id define-id)
      (query-value db* "SELECT id FROM definitions WHERE define_id = ?1" define-id)
    )

    (define (get-references* id)
      (append (get-references-of-type* "list_nodes" "car_id" id) (get-references-of-type* "defines" "expr_id" id))
    )

    (define (get-references-of-type* table col id)
      (map
        (curryr get-handle! col)
        (query-list db* (format "SELECT id FROM ~a WHERE ~a = ?1" table col) id)
      )
    )

    (define (get-param-id lambda-id position)
      (define arity (get-cell* lambda-id "arity"))
      (assert
        (format "attempt to get param with position ~a for lambda ~a with arity ~a" position lambda-id arity)
        (and (non-negative? position) (< position arity))
      )
      (query-maybe-value db* "SELECT id FROM params WHERE lambda_id = ?1 AND position = ?2" lambda-id position)
    )

    (define (get-cdrs* list-node-id)
      (if (= list-node-id NIL-ID)
        '()
        (cons list-node-id (get-cdrs* (get-cell* list-node-id "cdr_id")))
      )
    )

    (define (nth-list-insertion-point* list-start-id index)
      (if (zero? index)
        list-start-id
        (nth-list-insertion-point* (get-cell* list-start-id "cdr_id") (sub1 index))
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
      (set-cell-dangerous*!! id col new-desc)
    )

    (define (delete-unreferenceable-leaf*!! loc)
      (define id (send loc get-cell))
      (define table (get-table id))
      (assert
        (format "id ~a in table ~a is not an unreferenceable leaf" id table)
        (or (equal? table "unassigned") (equal? table "atoms"))
      )
      (q!! query-exec "DELETE FROM ~a" id)
      (set-loc-dangerous*!! loc BOGUS-ID)
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
            (list "parent_id" BOGUS-ID)
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
