#lang racket

(require db)

(require "misc.rkt")

(provide (all-defined-out))

; All indexed db queries must check that id is real first. higher-level stuff need not check directly
(define-syntax-rule (assert-real-id id)
  (assert "id was non-positive" (positive? id))
)

(define-syntax-rule (assert-real-or-unassigned-id id)
  (or (= id UNASSIGNED-ID) (assert-real-id id))
)

; TODO Not sure if it's better for this to be a macro, but if we make it a function, we can use compose
; to make some things way elegant
(define-syntax-rule (sql:// a b)
  (if (sql-null? a) b a)
)

(define sql-db%
  (class object%
    (super-new)
  )
)

; TODO: we should probably make a proper type/class/whatever for this later ...
(define (row-loc->table row-loc)
  (first row-loc)
)
(define (row-loc->id row-loc)
  (second row-loc)
)
(define (make-row-loc table id)
  (list table id)
)

; query uses ~a for the table, ?1 for the id, and ?2 ... for other q-parms
(define (q* q-proc query row-loc . q-parms)
  (let (
    [id (row-loc->id row-loc)])
    (assert-real-id id)
    (apply q-proc PROTO (format query (row-loc->table row-loc)) id q-parms)
  )
)

(define (get-row-loc id)
  (if (= UNASSIGNED-ID id)
    (make-row-loc "unassigned" UNASSIGNED-ID)
    (let* (
      [tables
        (filter
          (lambda (rl) (cons? (q* query-rows "SELECT * FROM ~a WHERE id = ?1" rl)))
          (map (curryr make-row-loc id) TABLES)
        )
      ]
      [num-tables (length tables)])
      (cond
        ; TODO #f ??? this is kinda wacky
        [(< num-tables 1) #f]
        [(= num-tables 1) (car tables)]
        [else (error 'get-row-loc "multiple tables with id ~a: ~a" id tables)]
      )
    )
  )
)

(define (assert-exists row-loc)
  (assert
    (format "row ~a does not exist" row-loc)
    (cons? (q* query-rows "SELECT * FROM ~a WHERE id = ?1" row-loc))
  )
)

(define (is-nil*? list-id)
  (= NIL-LIST-ID list-id)
)

(define (assert-unassigned row-loc col)
  (let (
    [id-val (get-cell row-loc col)])
    (assert
      (format "id cell ~a ~a should be unassigned but is ~a" row-loc col id-val)
      (= UNASSIGNED-ID id-val)
    )
  )
)

(define (get-cell row-loc/id col)
  (define row-loc (if (number? row-loc/id) (get-row-loc row-loc/id) row-loc/id))
  (assert-real-id (row-loc->id row-loc))
  (define result (q* query-maybe-value (format "SELECT ~a FROM ~~a WHERE id = ?1" col) row-loc))
  (assert (format "Could not find cell ~a, ~a" row-loc col) result)
  result
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
; WARNING: set-id*!! ensures that you don't orphan something, by only allowing you to set the cell if it's unassigned
;          set-cell-dangerous*!! can orphan an id so use sparingly and document why each use is safe
(define (set-cell-dangerous*!! row-loc col value)
  (assert-exists row-loc)
  (q* query-exec (format "UPDATE ~~a SET ~a = ?2 WHERE id = ?1" col) row-loc value)
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
(define (set-id*!! row-loc col id)
  (assert (format "destination column does not end in '_id': ~a" col) (string-suffix? "_id" col))
  (assert-unassigned row-loc col)
  (set-cell-dangerous*!! row-loc col id)
)

(define (query-prog-start* q-proc query)
  (q* q-proc query PROG-START-ROW-LOC)
)

; the program start is at id 1 in the list_headers table.
(define (init-db!!)
  (assert "PROTO db is already init'd" (null? (query-prog-start* query-rows "SELECT * FROM ~a WHERE id = ?1")))
  ; TODO I think we should just make it NIL-ID. Then we can use regular insertion for 'begin and delete the end-list*!! function
  (define first-id (create-something!! "list_headers(id, short_desc, long_desc, cdr_id)" (list "Main Program" "" UNASSIGNED-ID)))
  (assert
    (format "first created item should have id ~a but has id ~a" PROG-START-ID)
    (= PROG-START-ID first-id)
  )
  (define first-list-node-row-loc (get-row-loc (create-list*!! first-id)))
  (legacy-link!! first-list-node-row-loc "car_id" DEFAULT-LIBRARY "begin")
  (end-list*!! first-list-node-row-loc)
)

(define (get-next-id)
  (add1 (apply max (map (lambda (t) (sql:// (query-value PROTO (format "SELECT MAX(id) FROM ~a" t)) (sub1 PROG-START-ID))) TABLES)))
)

(define (create-something-build-string* table-with-cols)
  (string-join
    (build-list (length (string-split table-with-cols ",")) (lambda (n) (format "?~a" (add1 n))))
    ", "
    #:before-first "INSERT INTO ~a values("
    #:after-last ")"
  )
)

; in table-with-cols, id must be first. e.g. "list_headers(id, short_desc, long_desc, cdr_id)"
; non-id-values are the values for ?2, ?3, ?4 .... as a list
(define (create-something!! table-with-cols non-id-values [dest-row-loc #f] [dest-col #f])
  (assert "you must specify both dest-col and dest-row-loc, or neither" (not (xor dest-row-loc dest-col)))
  (let (
    [id (get-next-id)])
    (when dest-row-loc (set-id*!! dest-row-loc dest-col id))
    (apply q* query-exec (create-something-build-string* table-with-cols) (make-row-loc table-with-cols id) non-id-values)
    id
  )
)

; the params will be created lazily
(define (create-lambda!! arity dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (assert (format "negative arity: ~a" arity) (non-negative? arity))
  (define lambda-id
    (create-something!! "lambdas(id, short_desc, long_desc, arity, body_id)" (list short-desc long-desc arity UNASSIGNED-ID) dest-row-loc dest-col)
  )
  (create-nil-list-header!! (get-row-loc lambda-id) "body_id")
  lambda-id
)

; TODO probably delete
(define (end-list*!! dest-row-loc)
  (set-id*!! dest-row-loc "cdr_id" NIL-LIST-ID)
)

(define (create-list*!! owner-id [dest-row-loc (get-row-loc owner-id)])
  (create-something!! "lists(id, owner_id, car_id, cdr_id)" (list owner-id UNASSIGNED-ID UNASSIGNED-ID) dest-row-loc "cdr_id")
)

(define (create-list-header*!! dest-row-loc dest-col short-desc long-desc cdr-id)
  (create-something!! "list_headers(id, short_desc, long_desc, cdr_id)" (list short-desc long-desc cdr-id) dest-row-loc dest-col)
)

; TODO probably delete. We should probably just use the nil version, with insertion
(define (create-unassigned-list-header!! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*!! dest-row-loc dest-col short-desc long-desc UNASSIGNED-ID)
)

(define (create-nil-list-header!! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*!! dest-row-loc dest-col short-desc long-desc NIL-LIST-ID)
)

; returns #f if there is no param
(define (get-param lambda-id position)
  (query-maybe-value PROTO "SELECT id FROM params WHERE lambda_id = ?1 AND position = ?2" lambda-id position)
)

(define (create-param!! lambda-id position [short-desc sql-null] [long-desc sql-null])
  (let (
    [lambda-row-loc (make-row-loc "lambdas" lambda-id)])
    (assert-exists lambda-row-loc)
    (let (
      [arity (get-cell lambda-row-loc "arity")])
      (assert
        (format "attempt to make param with position ~a for lambda ~a with arity ~a" position lambda-id arity)
        (and (non-negative? position) (< position arity))
      )
    )
  )
  (assert (format "~ath param for lambda ~a is already defined" position lambda-id) (not (get-param lambda-id position)))
  (create-something!! "params(id, short_desc, long_desc, ref_count, lambda_id, position)" (list short-desc long-desc 0 lambda-id position))
)

(define (create-atom!! type value dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  ; TODO: check that the value is valid for this type
  (let (
    [type-char (hash-ref ATOM-TYPE->ATOM-TYPE-CHAR type #f)])
    (assert (format "Invalid type: ~a" type) type-char)
    (create-something!! "atoms(id, short_desc, long_desc, type, value)" (list short-desc long-desc type-char value) dest-row-loc dest-col)
  )
)

; returns the id of the definition, not the define
(define (create-define!! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (let (
    [define-id (create-something!! "defines(id, short_desc, long_desc, expr_id)" (list short-desc long-desc UNASSIGNED-ID) dest-row-loc dest-col)])
    (create-something!! "definitions(id, ref_count, define_id)" (list 0 define-id))
    define-id
  )
)

; library and name must be vetted before calling this function
(define (get-or-create-legacy-link!! library name)
  (or
    (query-maybe-value PROTO "SELECT id FROM legacies WHERE library = ?1 AND name = ?2" library name)
    (create-something!! "legacies(id, ref_count, library, name)" (list 0 library name))
  )
)

(define (legacy-link!! dest-row-loc dest-col library name)
  (define link-id (get-or-create-legacy-link!! library name))
  (inc-ref-count!! (make-row-loc "legacies" link-id))
  (set-id*!! dest-row-loc dest-col link-id)
  link-id
)

(define (inc-ref-count!! dest-row-loc)
  (define old-ref-count (get-cell dest-row-loc "ref_count"))
  (q* query "UPDATE ~a SET ref_count = ?2 WHERE id = ?1" dest-row-loc (add1 old-ref-count))
)

; TODO this feels redundant with get-row-loc and row-loc->table .
; When we factor out into classes things should get better
(define (get-type id)
  (row-loc->table (get-row-loc id))
)

(define (get-referable-ids)
  (append (query-list PROTO "SELECT id FROM definitions") (query-list PROTO "SELECT id FROM params"))
)

(define (visit-id visitors data id)
  (assert-real-or-unassigned-id id)
  (define row-loc (get-row-loc id))
  (define type (row-loc->table row-loc))
  (define visitor (hash-ref visitors type))
  (define (get-cols* cols)
    (vector->list (q* query-row (format "SELECT ~a FROM ~~a WHERE id = ?1" (string-join cols ", ")) row-loc))
  )
  (define (visit* . cols)
    (apply visitor data id (get-cols* cols))
  )
  (define (visit-special* special . cols)
    (apply special visitor data id (get-cols* cols))
  )
  (case type
    [("lambdas") (visit-special* visit-lambda "short_desc" "long_desc" "arity" "body_id")]
    [("defines") (visit-special* visit-define "short_desc" "long_desc" "expr_id")]
    [("list_headers") (visit-special* visit-list-header "short_desc" "long_desc" "cdr_id")]
    [("params") (visit* "short_desc" "long_desc" "lambda_id" "position")]
    [("definitions") (visit* "define_id")]
    [("atoms") (visit* "short_desc" "long_desc" "type" "value")]
    [("legacies") (visit* "library" "name")]
    [("unassigned") (visitor data)]
    [else (error 'visit-id "id ~a has unvisitable type ~a" id type)]
  )
)

(define (visit-lambda lambda-visitor data id short-desc long-desc arity body-id)
  (define positions->param-ids
    (foldl (lambda (p h) (let ([param-id (get-param id p)]) (if param-id (hash-set h p param-id) h))) #hash() (range arity))
  )
  (lambda-visitor data id short-desc long-desc arity positions->param-ids body-id)
)

(define (visit-define define-visitor data id short-desc long-desc expr-id)
  (define definition-id (query-value PROTO "SELECT id FROM definitions WHERE define_id = ?1" id))
  (define-visitor data id short-desc long-desc definition-id expr-id)
)

(define (visit-list-header list-header-visitor data id short-desc long-desc cdr-id)
  (define item-ids (get-cars* cdr-id))
  (list-header-visitor data id short-desc long-desc item-ids)
)

(define (get-cars* list-node-id)
  (if (is-nil*? list-node-id)
    '()
    (cons (get-cell list-node-id "car_id") (get-cars* (get-cell list-node-id "cdr_id")))
  )
)

(define (nth-list-id list-header-id index)
  (nth-list-insertion-point* list-header-id (add1 index))
)

(define (nth-list-insertion-point* list-start-id index)
  (if (zero? index)
    list-start-id
    (nth-list-insertion-point* (get-cell list-start-id "cdr_id") (sub1 index))
  )
)

(define (insert-new-list-node*!! list-header-id index)
  (define insertion-point (nth-list-insertion-point* list-header-id index))
  (define insertion-row-loc (get-row-loc insertion-point))
  (define old-cdr (get-cell insertion-row-loc "cdr_id"))
  ; We've captured the original cdr_id, and will soon move it to the newly created node, so we can safely replace this node's cdr
  (set-cell-dangerous*!! insertion-row-loc "cdr_id" UNASSIGNED-ID)
  (define new-node-row-loc (get-row-loc (create-list*!! list-header-id insertion-row-loc)))
  (set-id*!! new-node-row-loc "cdr_id" old-cdr)
  new-node-row-loc 
)

; TODO this probably shouldn't exist in the long run
(define (bomb)
  (for-each
    (lambda (table) (query-exec PROTO (format "DELETE FROM ~a" table)))
    TABLES
  )
  (init-db!!)
)

; CONSTANTS

(define PROTO (sqlite3-connect #:database "/home/nick/veme/proto.db"))

; TODO change keys to symbols?
(define ATOM-TYPE->ATOM-TYPE-CHAR
  #hash(
    ("number" . "n")
    ("character" . "c")
    ("string" . "s")
    ("boolean" . "b")
  )
)

(define TYPE-CHAR->TABLE
  #hash(
    ("f" . "lambdas")
    ("p" . "params")
    ("d" . "defines")
    ("r" . "definitions")
    ("l" . "lists")
    ("h" . "list_headers")
    ("a" . "atoms")
    ("e" . "legacies")
  )
)

(define TABLES (hash-values TYPE-CHAR->TABLE))

(define DEFAULT-LIBRARY "")

(define UNASSIGNED-ID -1)

(define NIL-LIST-ID 0)

(define PROG-START-ID 1)

(define PROG-START-ROW-LOC (make-row-loc "list_headers" PROG-START-ID))

;; db stuff currently used by higher-level stuff
;PROG-START-ID
;UNASSIGNED-ID
;DEFAULT-LIBRARY? forcing linkage?
;sql-null and sql://
;get-cell , i.e. getting a specific col for a particular row
;nth-list-id , i.e. lower-level handling of lists
;currently dealing with stuff as low-level as ref-count
;
;; stuff that refers to ids or row-locs
;id->scheme stuff
;
;and , of course , id visitors
