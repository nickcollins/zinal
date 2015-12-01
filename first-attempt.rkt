#lang racket

(require db)
(require racket/gui/base)
(require mrlib/hierlist)

; MACROS

(define-syntax-rule (assert msg bool)
  (cond
    [(not bool) (error msg)]
  )
)

; All indexed db queries must check that id is non-zero first. higher-level stuff need not check directly
(define-syntax-rule (assert-not-nil id)
  (assert "id was nil" (not (equal? id 0)))
)

(define-syntax-rule (sql:// a b)
  (if (sql-null? a) b a)
)

; CONSTANTS

(define PROTO (sqlite3-connect #:database "/home/nick/veme/proto.db"))

(define ATOM-TYPE->ATOM-TYPE-CHAR
  (hash
    "number" "n"
    "character" "c"
    "string" "s"
    "boolean" "b"
  )
)

(define TYPE-CHAR->TABLE
  (hash
    "f" "lambdas"
    "p" "params"
    "d" "defines"
    "r" "definitions"
    "l" "lists"
    "a" "atoms"
    "e" "links"
  )
)

(define TABLES (hash-values TYPE-CHAR->TABLE))

; FUNCTIONS

; ugh - in srfi/13, but that seems to redefine string-join
(define (string-suffix? suf s)
  (let (
    [s-len (string-length s)]
    [suf-len (string-length suf)])
    (equal? (substring s (- s-len suf-len) s-len) suf)
  )
)

; DB FUNCTIONS

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
    (assert-not-nil id)
    (apply q-proc PROTO (format query (row-loc->table row-loc)) id q-parms)
  )
)

(define (get-row-loc id)
  (let* (
    [tables
      (filter
        (lambda (rl) (cons? (q* query-rows "SELECT * FROM ~a WHERE id = ?1" rl)))
	(map (lambda (t) (make-row-loc t id)) TABLES)
      )
    ]
    [num-tables (length tables)])
    (cond
      [(< num-tables 1) #f]
      [(equal? num-tables 1) (car tables)]
      [else (error 'get-row-loc "multiple tables with id ~a: ~a" id tables)]
    )
  )
)

(define (assert-exists row-loc)
  (assert
    (format "row ~a does not exist" row-loc)
    (cons? (q* query-rows "SELECT * FROM ~a WHERE id = ?1" row-loc))
  )
)

(define (assert-nil-id row-loc col)
  (let (
    [id-val (q* query-maybe-value (format "SELECT ~a FROM ~~a WHERE id = ?1" col) row-loc)])
    (assert
      (format "id cell ~a ~a should be nil but is ~a" row-loc col id-val)
      (implies id-val (equal? 0 id-val))
    )
  )
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
(define (set-cell*! row-loc col value)
  (assert-exists row-loc)
  (q* query-exec (format "UPDATE ~~a SET ~a = ?2 WHERE id = ?1" col) row-loc value)
)

(define (query-prog-start* q-proc query)
  (q* q-proc query (make-row-loc "lists" 1))
)

; the program start is at id 1 in the lists table.
(define (init-db!)
  (assert "PROTO db is already init'd" (null? (query-prog-start* query-rows "SELECT * FROM ~a WHERE id = ?1")))
  (create-something! "lists(id, short_desc, long_desc, car_id, cdr_id)" (list "Main Program" "" 0 0))
)

(define (get-next-id)
  (+ 1 (apply max (map (lambda (t) (sql:// (query-value PROTO (format "SELECT MAX(id) FROM ~a" t)) 0)) TABLES)))
)

(define (create-something-build-string*! table-with-cols)
  (string-join
    (build-list (length (string-split table-with-cols ",")) (lambda (n) (format "?~a" (+ n 1))))
    ", "
    #:before-first "INSERT INTO ~a values("
    #:after-last ")"
  )
)

; in table-with-cols, id must be first. e.g. "lists(id, short_desc, long_desc, car_id, cdr_id)"
; non-id-values are the values for ?2, ?3, ?4 .... as a list
(define (create-something! table-with-cols non-id-values [dest-row-loc #f] [dest-col #f])
  (assert "you must specify both dest-col and dest-row-loc, or neither" (not (xor dest-row-loc dest-col)))
  (let (
    [id (get-next-id)])
    (cond [dest-row-loc
      (assert (format "destination column does not end in '_id': ~a" dest-col) (string-suffix? "_id" dest-col))
      (assert-nil-id dest-row-loc dest-col)
      (set-cell*! dest-row-loc dest-col id)
    ])
    (apply q* query-exec (create-something-build-string*! table-with-cols) (make-row-loc table-with-cols id) non-id-values)
    id
  )
)

; the params will be created lazily
(define (create-lambda! arity [short-desc sql-null] [long-desc sql-null])
  (assert (format "negative arity: ~a" arity) (>= arity 0))
  (create-something! "lambdas(id, short_desc, long_desc, arity, ref_count, body_id)" (list short-desc long-desc arity 0 0))
)

(define (create-list! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-something! "lists(id, short_desc, long_desc, car_id, cdr_id)" (list short-desc long-desc 0 0) dest-row-loc dest-col)
)

; returns #f if there is no param
(define (get-param lambda-id position)
  (query-maybe-value PROTO "SELECT id FROM params WHERE lambda_id = ?1 AND position = ?2" lambda-id position)
)

(define (create-param! lambda-id position [short-desc sql-null] [long-desc sql-null])
  (let (
    [lambda-row-loc (make-row-loc "lambdas" lambda-id)])
    (assert-exists lambda-row-loc)
    (let (
      [arity (q* query-value "SELECT arity FROM ~a WHERE id = ?1" lambda-row-loc)])
      (assert
        (format "attempt to make param with position ~a for lambda ~a with arity ~a" position lambda-id arity)
        (and (>= position 0) (< position arity))
      )
    )
  )
  (assert (format "~ath param for lambda ~a is already defined" position lambda-id) (not (get-param lambda-id position)))
  (create-something! "params(id, short_desc, long_desc, ref_count, lambda_id, position)" (list short-desc long-desc 0 lambda-id position))
)

(define (create-atom! type value dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  ; TODO: check that the value is valid for this type
  (let (
    [type-char (hash-ref ATOM-TYPE->ATOM-TYPE-CHAR type #f)])
    (assert (format "Invalid type: ~a" type) type-char)
    (create-something! "atoms(id, short_desc, long_desc, type, value)" (list short-desc long-desc type-char value) dest-row-loc dest-col)
  )
)

; returns the id of the definition, not the define
(define (create-define! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (let (
    [define-id (create-something! "defines(id, short_desc, long_desc, expr_id)" (list short-desc long-desc 0) dest-row-loc dest-col)])
    (create-something! "definitions(id, ref_count, define_id)" (list 0 define-id))
  )
)

; library and public-id must be vetted before calling this function
(define (get-or-create-link! library public-id)
  (or
    (query-maybe-value PROTO "SELECT id FROM links WHERE library = ?1 AND public_id = ?2" library public-id)
    (create-something! "links(id, ref_count, library, public_id)" (list 0 library public-id))
  )
)

; GUI

(define logic-hierarchy%
  (class hierarchical-list%
    (define/override (on-char key-event)
      (case (send key-event get-key-code)
        [(#\j #\J) (send this select-next)]
        [(#\k #\K) (send this select-prev)]
        [(#\h #\H) (send this select-out)]
        [(#\l #\L) (send this select-in)]
      )
      (super on-char key-event)
    )
    (super-new)
  )
)

; lots of code liberally stolen from mred-designer
(define (new-hier-item hier label)
  (let* (
    [item (send hier new-item)]
    [ed (send item get-editor)])
    (send ed erase)
    (send ed insert label)
  )
)

(define (new-opened-sublist hier)
  (let ([sl (send hier new-list)]) (send sl open) sl)
)

(define main-window (new frame% [label "Veme"]))
(define prog-tree (new logic-hierarchy% [parent main-window]))
(new-hier-item prog-tree "1")
(let (
  [c (new-opened-sublist prog-tree)])
  (new-hier-item c "2")
  (new-hier-item c "3")
)
(new-hier-item prog-tree "4")

(send main-window show #t)

