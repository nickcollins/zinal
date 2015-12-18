#lang racket

(require db)
(require mrlib/hierlist)
(require racket/gui/base)
; for hash-union
(require racket/hash)
; for list-index
(require srfi/1)

; MACROS

(define-syntax-rule (assert msg bool)
  (cond
    [(not bool) (error msg)]
  )
)

; All indexed db queries must check that id is non-zero first. higher-level stuff need not check directly
(define-syntax-rule (assert-not-nil id)
  (assert "id was nil" (not (= id 0)))
)

(define-syntax-rule (sql:// a b)
  (if (sql-null? a) b a)
)

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
	(map (curryr make-row-loc id) TABLES)
      )
    ]
    [num-tables (length tables)])
    (cond
      [(< num-tables 1) #f]
      [(= num-tables 1) (car tables)]
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
    [id-val (get-cell row-loc col)])
    (assert
      (format "id cell ~a ~a should be nil but is ~a" row-loc col id-val)
      (implies id-val (= 0 id-val))
    )
  )
)

(define (get-cell row-loc col)
  (q* query-maybe-value (format "SELECT ~a FROM ~~a WHERE id = ?1" col) row-loc)
)

(define (get-cell-strict row-loc/id col)
  (define row-loc (if (number? row-loc/id) (get-row-loc row-loc/id) row-loc/id))
  (define result (get-cell row-loc col))
  (assert (format "Could not find cell ~a, ~a" row-loc col) result)
  result
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
; WARNING: set-id*! ensures that you don't orphan something, by only allowing you to set the value if it's 0
;          set-cell-dangerous*! can orphan an id so use sparingly and document why each use is safe
(define (set-cell-dangerous*! row-loc col value)
  (assert-exists row-loc)
  (q* query-exec (format "UPDATE ~~a SET ~a = ?2 WHERE id = ?1" col) row-loc value)
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
(define (set-id*! row-loc col id)
  (assert (format "destination column does not end in '_id': ~a" col) (string-suffix? "_id" col))
  (assert-nil-id row-loc col)
  (set-cell-dangerous*! row-loc col id)
)

(define (query-prog-start* q-proc query)
  (q* q-proc query PROG-START-ROW-LOC)
)

; the program start is at id 1 in the lists table.
(define (init-db!)
  (assert "PROTO db is already init'd" (null? (query-prog-start* query-rows "SELECT * FROM ~a WHERE id = ?1")))
  (create-something! "lists(id, short_desc, long_desc, car_id, cdr_id)" (list "Main Program" "" 0 0))
  (legacy-link! PROG-START-ROW-LOC "car_id" "" "begin")
)

(define (get-next-id)
  (add1 (apply max (map (lambda (t) (sql:// (query-value PROTO (format "SELECT MAX(id) FROM ~a" t)) 0)) TABLES)))
)

(define (create-something-build-string*! table-with-cols)
  (string-join
    (build-list (length (string-split table-with-cols ",")) (lambda (n) (format "?~a" (add1 n))))
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
    (cond [dest-row-loc (set-id*! dest-row-loc dest-col id)])
    (apply q* query-exec (create-something-build-string*! table-with-cols) (make-row-loc table-with-cols id) non-id-values)
    id
  )
)

; the params will be created lazily
(define (create-lambda! arity dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (assert (format "negative arity: ~a" arity) (>= arity 0))
  (create-something! "lambdas(id, short_desc, long_desc, arity, body_id)" (list short-desc long-desc arity 0) dest-row-loc dest-col)
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
      [arity (get-cell-strict lambda-row-loc "arity")])
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
(define (get-or-create-legacy-link! library name)
  (or
    (query-maybe-value PROTO "SELECT id FROM legacies WHERE library = ?1 AND name = ?2" library name)
    (create-something! "legacies(id, ref_count, library, name)" (list 0 library name))
  )
)

(define (legacy-link! dest-row-loc dest-col library name)
  (define link-id (get-or-create-legacy-link! library name))
  (inc-ref-count! (make-row-loc "legacies" link-id))
  (set-id*! dest-row-loc dest-col link-id)
)

(define (inc-ref-count! dest-row-loc)
  (define old-ref-count (get-cell-strict dest-row-loc "ref_count"))
  (q* query "UPDATE ~a SET ref_count = ?2 WHERE id = ?1" dest-row-loc (add1 old-ref-count))
)

(define (visit-id visitors data id [asserted-type #f])
  (cond
    [(= id 0)
      (assert
        (format "Expected type ~a but was nil list" asserted-type)
        (implies asserted-type (equal? asserted-type "lists"))
      )
      ((hash-ref visitors "nil") data)
    ]
    [else (visit-non-nil-id* visitors data id asserted-type)]
  )
)

(define (visit-non-nil-id* visitors data id [asserted-type #f])
  (define row-loc (get-row-loc id))
  (define type (row-loc->table row-loc))
  (assert
    (format "Expected id ~a to be ~a but was ~a" id asserted-type type)
    (implies asserted-type (equal? asserted-type type))
  )
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
    [("params") (visit* "short_desc" "long_desc" "lambda_id" "position")]
    [("definitions") (visit* "define_id")]
    [("lists") (visit* "short_desc" "long_desc" "car_id" "cdr_id")]
    [("atoms") (visit* "short_desc" "long_desc" "type" "value")]
    [("legacies") (visit* "library" "name")]
    [else (error 'visit-non-nil-id* "id ~a has invalid type ~a" id type)]
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

; TODO
;(define (visit-list* item-visitor+data id short-desc long-desc car-id cdr-id)
;  (define item-visitor (hash-ref item-visitor+data "item-visitor"))
;  (define data (hash-ref item-visitor+data "data"))
;  (cons (item-visitor data car-id) (visit-list item-visitor data cdr-id))
;)
;
;(define (visit-list item-visitor data list-id)
;  (define visitors (hash
;    "nil" (const '())
;    "lists" visit-list*
;  ))
;  (visit-id visitors (hash "item-visitor" item-visitor "data" data) list-id "lists")
;)

(define (get-cars list-id)
  (if (= 0 list-id)
    '()
    (cons (get-cell-strict list-id "car_id") (get-cars (get-cell-strict list-id "cdr_id")))
  )
)

(define (get-cdrs list-id)
  (if (= 0 list-id)
    '()
    (cons list-id (get-cdrs (get-cell-strict list-id "cdr_id")))
  )
)

(define (get-short-desc id)
  (define (just-short-desc* data id short-desc . etc) short-desc)
  (define (just-sql-null* data) sql-null)
  (define visitors (hash
    "nil" just-sql-null*
    "lambdas" just-short-desc*
    "params" just-short-desc*
    "definitions" (lambda (data id define-id) (get-short-desc define-id))
    "defines" just-short-desc*
    "lists" just-short-desc*
    "atoms" just-short-desc*
    "legacies" (lambda (data id library name) name)
  ))
  (visit-id visitors id)
)

(define (get-short-desc-or id alt)
  (sql:// (get-short-desc id) alt)
)

(define (insert-at-beginning-of-list*! list-id new-node-id)
  (assert-not-nil list-id)
  (define list-row-loc (get-row-loc list-id))
  (define orig-first-value (get-cell-strict list-row-loc "car_id"))
  (define rest (get-cell-strict list-row-loc "cdr_id"))

  ; We've captured original car_id and cdr_id values, and will soon move them to a new list, so we can safely replace them with new values
  (set-cell-dangerous*! list-row-loc "car_id" new-node-id)
  (set-cell-dangerous*! list-row-loc "cdr_id" 0)

  (define new-list-row-loc (get-row-loc (create-list! list-row-loc "cdr_id")))
  (set-id*! new-list-row-loc "car_id" orig-first-value)
  (set-id*! new-list-row-loc "cdr_id" rest)
)

(define (nil-list->real-list*! row-loc col first-item-id [short-desc sql-null] [long-desc sql-null])
  (assert
    (format "Don't use nil-list->real-list*! for the '() at the end of a list. Instead use an append operation: ~a" row-loc)
    (not (equal? col "cdr_id"))
  )
  (define new-list-row-loc (get-row-loc (create-list! row-loc col short-desc long-desc)))
  (set-id*! new-list-row-loc "car_id" first-item-id)
)

; TODO
;(define (create-db-list-item*! list-id pos)
;  (define (list-ref (visit-list (lambda (data id) id) #f list-id) pos)
;  
;)

; TRANSPILATION

(define (id->string* id)
  (format "veme:_~a" id)
)

(define (id->sym id)
  (string->symbol (id->string* id))
)

(define (id->scheme id [asserted-type #f])
  (define (id->sym* data id . etc) (id->sym id))
  ; We can't use #hash form, cuz it will interpret (type . proc) as '(type . proc), meaning proc is a symbol, not a proc
  ; ugh
  (define visitors (hash
    "nil" (const '())
    "lambdas" lambda-data->scheme
    "params" id->sym*
    "definitions" id->sym*
    "defines" define-data->scheme
    "lists" list-data->scheme
    "atoms" atom-data->scheme
    "legacies" legacy-link-data->scheme
  ))
  (visit-id visitors #f id asserted-type)
)

(define (legacy-link-data->scheme data id library name)
  (if (equal? library "")
    (string->symbol name)
    ; TODO implement this
    (error 'legacy-link-data->scheme "Support for non-standard libraries not yet implemented: ~a::~a" library name)
  )
)

(define (define-data->scheme data id short-desc long-desc definition-id expr-id)
  (append (list 'define (id->scheme definition-id)) (id->scheme expr-id "lists"))
)

(define (atom-data->scheme data id short-desc long-desc type value)
  (case type
    [("n") (or (string->number value) (error 'atom-data->scheme "Number atom ~a cannot be converted to number" value))]
    [("c")
      (define int-value (string->number value))
      (assert
        (format "Character ~a must either be the integer value of the desired character" value)
        (and int-value (exact-positive-integer? int-value))
      )
      (integer->char int-value)
    ]
    [("s") value]
    [("b")
      (case value
        [("f") #f]
        [("t") #t]
        [else (error 'atom-data->scheme "Boolean ~a is neither 'f' nor 't'" value)]
      )
    ]
    [else (error 'atom-data->scheme "atom ~a has invalid type char ~a" value type)]
  )
)

(define (map-params param-visitor unused-param-visitor arity positions->param-ids)
  (build-list arity (lambda (pos)
    (define param-id (hash-ref positions->param-ids pos #f))
    (if param-id
      (param-visitor param-id)
      (unused-param-visitor pos)
    )
  ))
)

(define (lambda-data->scheme data id short-desc long-desc arity positions->param-ids body-id)
  (define (unused-param->symbol pos)
    (string->symbol (format "~a:unused_~a" (id->string* id) pos))
  )
  (append
    (list 'lambda (map-params id->scheme unused-param->symbol arity positions->param-ids))
    (id->scheme body-id "lists")
  )
)

(define (list-data->scheme data id short-desc long-desc car-id cdr-id)
  (cons (id->scheme car-id) (id->scheme cdr-id "lists"))
)

(define (get-program-as-scheme*)
  (id->scheme PROG-START-ID "lists")
)

(define (build-scheme-code)
  (write (get-program-as-scheme*))
)

; CONSTANTS

(define PROTO (sqlite3-connect #:database "/home/nick/veme/proto.db"))

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
    ("a" . "atoms")
    ("e" . "legacies")
  )
)

(define TABLES (hash-values TYPE-CHAR->TABLE))

(define PROG-START-ID 1)

(define PROG-START-ROW-LOC (make-row-loc "lists" PROG-START-ID))

; GUI

(define (get-node-id prog-tree-item)
  (send prog-tree-item user-data)
)

(define (get-list-body-id* prog-tree-item)
  (define row-loc (get-row-loc (get-node-id prog-tree-item)))
  (define id (row-loc->id row-loc))
  (if (= id 0)
    0
    (case (row-loc->table row-loc)
      [("lists") id]
      [("lambdas") (get-cell-strict (make-row-loc "lambdas" id) "body_id")]
      [else #f]
    )
  )
)

; TODO
;(define (is-list-type* prog-tree-item)
;  (case (row-loc->table (get-row-loc (get-node-id prog-tree-item)))
;    [("lambdas" "lists") #t]
;    [else #f]
;  )
;)

; TODO
;(define (get-insertion-point* prog-tree-item)
;  (define insertion-list-item
;    (if (is-list-type* prog-tree-item) prog-tree-item (send get-parent prog-tree-item))
;  )
;  (cond [(not (is-list-type* insertion-list-item))
;    (error 'get-insertion-point* "Neither ~a nor its parent is a list type, and cannot be inserted into" (get-node-id prog-tree-item))
;  ])
;  (list insertion-list-item
;    (if (eq? prog-tree-item insertion-list-item)
;      0
;      (add1 (list-index (curry eq? prog-tree-item) insertion-list-item))
;    )
;  )
;)

(define (maybe-create-new-list! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-number! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-character! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-string! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-boolean! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-function! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-lambda! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-value-definition! prog-tree-item)
  #f
  ; TODO
)

(define (maybe-create-new-value-read! prog-tree-item)
  #f
  ; TODO
)

(define (request-new-node-handler*)
  (define friendly-types (hash-keys FRIENDLY-TYPE->HANDLER))
  (define choices (get-choices-from-user "Create new node" "Choose the node's type:" friendly-types))
  (cond
    [choices
      (assert (format "Multiple types chosen: ~a" choices) (= 1 (length choices)))
      (hash-ref FRIENDLY-TYPE->HANDLER (list-ref friendly-types (car choices)))
    ]
    [else #f]
  )
)

(define (find-node-index compound-node node-to-find)
  (list-index (curry eq? node-to-find) (send compound-node get-items))
)

(define (insert-new-node*! insertion-list-node list-body-id new-node-id)
  (cond
    [(= 0 list-body-id)
      (define parent-list (send insertion-list-node get-parent))
      (define parent-list-body-id (get-list-body-id* parent-list))
      (assert (format "~a's parent is not a valid list: ~a" insertion-list-node parent-list-body-id) ((conjoin identity (negate (curry = 0))) parent-list-body-id))
      (define sublist-id (list-ref (get-cdrs parent-list-body-id) (find-node-index parent-list insertion-list-node)))
      (nil-list->real-list*! (get-row-loc sublist-id) "car_id" new-node-id)
    ]
    [else
      (insert-at-beginning-of-list*! list-body-id new-node-id)
    ]
  )
)

(define (maybe-insert-new-node! selected-prog-tree-item)
  (cond [selected-prog-tree-item
    (define list-body-id (get-list-body-id* selected-prog-tree-item))
    (define handler (request-new-node-handler*))
    (cond [(and list-body-id handler)
      (define new-node-id (handler selected-prog-tree-item))
      (cond [new-node-id
        ; the ref-count of new-node-id has already been updated, so we must ensure we successfully insert
        (insert-new-node*! selected-prog-tree-item list-body-id new-node-id)
        (refresh-prog-tree)
      ])
    ])
  ])
)

(define (append-new-node! selected-prog-tree-item)
  ; TODO
  #f
)

(define logic-hierarchy%
  (class hierarchical-list%
    (define/override (on-char key-event)
      (define selected (send this get-selected))
      (case (send key-event get-key-code)
        [(#\j) (send this select-next)]
        [(#\k) (send this select-prev)]
        [(#\h) (send this select-out)]
        [(#\l) (send this select-in)]
        [(#\a) (append-new-node! selected)]
        [(#\i) (maybe-insert-new-node! selected)]
      )
      (super on-char key-event)
    )
    (super-new)
  )
)

; lots of code liberally stolen from mred-designer
(define (change-text hier new-text)
  (define ed (send hier get-editor))
  (send ed erase)
  (send ed insert new-text)
)

(define (init-prog-tree-item* item text id)
  (change-text item text)
  (send item user-data id)
  item
)

(define (new-prog-tree-item parent text id)
  (init-prog-tree-item* (send parent new-item) text id)
)

(define (new-prog-tree-list parent text id)
  (init-prog-tree-item* (new-opened-sublist parent) text id)
)

(define (new-opened-sublist hier)
  (let ([sl (send hier new-list)]) (send sl open) sl)
)

(define (add-nil-to-prog-tree* prog-tree)
  (new-prog-tree-item prog-tree "()" 0)
)

(define (add-lambda-to-prog-tree* prog-tree id short-desc long-desc arity positions->param-ids body-id)
  (define lambda-text (sql:// short-desc
    (string-join
      (map-params (curryr get-short-desc-or "<?>") (const "¯\\_(ツ)_/¯") arity positions->param-ids)
      ", "
      #:before-first "λ "
      #:after-last (format " -> ~a" (get-short-desc-or body-id "..."))
    )
  ))
  (define lambda-tree (new-prog-tree-list prog-tree (sql:// short-desc (format "λ ")) id))
  (add-all-to-prog-tree* lambda-tree body-id)
)

(define (add-define-to-prog-tree* prog-tree id short-desc long-desc definition-id expr-id)
  (define define-tree (new-prog-tree-list prog-tree (define->short-text* short-desc expr-id) id))
  (add-to-prog-tree define-tree expr-id)
)

(define (define->short-text* short-desc expr-id)
  (format "~a = ~a" (sql:// short-desc "<no desc>") (get-short-desc-or expr-id "..."))
)

(define (atom->short-text* data id short-desc long-desc type value)
  (sql:// short-desc (~a (atom-data->scheme data id short-desc long-desc type value)))
)

(define (legacy-link->short-text* data id library name)
  name
)

(define (list-item->text* id)
  (define (short-desc-or* alt)
    (lambda (data id . etc)
      (get-short-desc-or id alt)
    )
  )
  (define (define->short-text** data id short-desc long-desc definition-id expr-id)
    (format "{~a}" (define->short-text* short-desc expr-id))
  )
  (define visitors (hash
    "nil" (const "()")
    "lambdas" (short-desc-or* "λ...")
    "params" (short-desc-or* "<no desc>")
    "definitions" (short-desc-or* "<no desc>")
    "defines" define->short-text**
    "lists" (short-desc-or* "(...)")
    "atoms" atom->short-text*
    "legacies" legacy-link->short-text*
  ))
  (visit-id visitors #f id)
)

(define (list->text* list-id)
  (string-join
    ;(visit-list (lambda (data id) (list-item->text* id)) #f list-id) TODO
    (map list-item->text* (get-cars list-id))
    ", "
    #:before-first "("
    #:after-last ")"
  )
)

(define (add-list-to-prog-tree* prog-tree id short-desc long-desc car-id cdr-id)
  (define list-tree (new-prog-tree-list prog-tree (sql:// short-desc (list->text* id)) id))
  (add-all-to-prog-tree* list-tree id)
)

(define (add-atom-to-prog-tree* prog-tree id short-desc long-desc type value)
  (new-prog-tree-item prog-tree (atom->short-text* prog-tree id short-desc long-desc type value) id)
)

(define (add-legacy-link-to-prog-tree* prog-tree id library name)
  (new-prog-tree-item prog-tree (legacy-link->short-text* prog-tree id library name) id)
)

(define (add-all-to-prog-tree* prog-tree list-id)
  ;(visit-list (lambda (data id) (add-to-prog-tree prog-tree id)) prog-tree list-id) TODO
  (for-each (curry add-to-prog-tree prog-tree) (get-cars list-id))
)

(define (add-to-prog-tree prog-tree id)
  (define (just-short-desc* pt id . etc) (new-prog-tree-item prog-tree (get-short-desc-or id "<no desc>") id))

  (define prog-tree-visitors (hash
    "nil" add-nil-to-prog-tree*
    "lambdas" add-lambda-to-prog-tree*
    "params" just-short-desc*
    "definitions" just-short-desc*
    "defines" add-define-to-prog-tree*
    "lists" add-list-to-prog-tree*
    "atoms" add-atom-to-prog-tree*
    "legacies" add-legacy-link-to-prog-tree*
  ))

  (visit-id prog-tree-visitors prog-tree id)
)

; GUI CONSTANTS

(define FRIENDLY-LITERAL-TYPE->HANDLER (hash
  "list" maybe-create-new-list!
  "number" maybe-create-new-number!
  "character" maybe-create-new-character!
  "string" maybe-create-new-string!
  "boolean" maybe-create-new-boolean!
))

(define FRIENDLY-TYPE->HANDLER (hash-union FRIENDLY-LITERAL-TYPE->HANDLER (hash
  "define function" maybe-create-new-function!
  "anonymous function" maybe-create-new-lambda!
  "define value" maybe-create-new-value-definition!
  "read definition" maybe-create-new-value-read!
)))

; PROGRAM

(define main-window (new frame% [label "Veme"]))
(define (refresh-prog-tree)
  (define prog-tree (new logic-hierarchy% [parent main-window]))
  (add-to-prog-tree prog-tree PROG-START-ID)
)
(refresh-prog-tree)
(send main-window show #t)

(build-scheme-code)
