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

; All indexed db queries must check that id is real first. higher-level stuff need not check directly
(define-syntax-rule (assert-real-id id)
  (assert "id was non-positive" (positive? id))
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

(define non-negative? (negate negative?))

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
    (assert-real-id id)
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
  (define result (q* query-maybe-value (format "SELECT ~a FROM ~~a WHERE id = ?1" col) row-loc))
  (assert (format "Could not find cell ~a, ~a" row-loc col) result)
  result
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
; WARNING: set-id*! ensures that you don't orphan something, by only allowing you to set the cell if it's unassigned
;          set-cell-dangerous*! can orphan an id so use sparingly and document why each use is safe
(define (set-cell-dangerous*! row-loc col value)
  (assert-exists row-loc)
  (q* query-exec (format "UPDATE ~~a SET ~a = ?2 WHERE id = ?1" col) row-loc value)
)

; WARNING: do not use this on id columns unless ref-counts have already been updated
(define (set-id*! row-loc col id)
  (assert (format "destination column does not end in '_id': ~a" col) (string-suffix? "_id" col))
  (assert-unassigned row-loc col)
  (set-cell-dangerous*! row-loc col id)
)

(define (query-prog-start* q-proc query)
  (q* q-proc query PROG-START-ROW-LOC)
)

; the program start is at id 1 in the list_headers table.
(define (init-db!)
  (assert "PROTO db is already init'd" (null? (query-prog-start* query-rows "SELECT * FROM ~a WHERE id = ?1")))
  (define first-id (create-something! "list_headers(id, short_desc, long_desc, cdr_id)" (list "Main Program" "" UNASSIGNED-ID)))
  (assert
    (format "first created item should have id ~a but has id ~a" PROG-START-ID)
    (= PROG-START-ID first-id)
  )
  (define first-list-node-row-loc (get-row-loc (create-list*! first-id)))
  (legacy-link! first-list-node-row-loc "car_id" DEFAULT-LIBRARY "begin")
  ; TODO later, we may delete end-list in favor of a wholesale list creation thing
  (end-list*! first-list-node-row-loc)
)

(define (get-next-id)
  (add1 (apply max (map (lambda (t) (sql:// (query-value PROTO (format "SELECT MAX(id) FROM ~a" t)) (sub1 PROG-START-ID))) TABLES)))
)

(define (create-something-build-string*! table-with-cols)
  (string-join
    (build-list (length (string-split table-with-cols ",")) (lambda (n) (format "?~a" (add1 n))))
    ", "
    #:before-first "INSERT INTO ~a values("
    #:after-last ")"
  )
)

; in table-with-cols, id must be first. e.g. "list_headers(id, short_desc, long_desc, cdr_id)"
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
  (assert (format "negative arity: ~a" arity) (non-negative? arity))
  (create-something! "lambdas(id, short_desc, long_desc, arity, body_id)" (list short-desc long-desc arity UNASSIGNED-ID) dest-row-loc dest-col)
)

; TODO probably we should just have something that creates the list wholesale
(define (end-list*! dest-row-loc)
  (set-id*! dest-row-loc "cdr_id" NIL-LIST-ID)
)

(define (create-list*! owner-id [dest-row-loc (get-row-loc owner-id)])
  (create-something! "lists(id, owner_id, car_id, cdr_id)" (list owner-id UNASSIGNED-ID UNASSIGNED-ID) dest-row-loc "cdr_id")
)

(define (create-list-header*! dest-row-loc dest-col short-desc long-desc cdr-id)
  (create-something! "list_headers(id, short_desc, long_desc, cdr_id)" (list short-desc long-desc cdr-id) dest-row-loc dest-col)
)

(define (create-unassigned-list-header! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*! dest-row-loc dest-col short-desc long-desc UNASSIGNED-ID)
)

; TODO probably delete. We should just use the unassigned version, and then set-id*! to NIL-LIST-ID
(define (create-nil-list-header! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*! dest-row-loc dest-col short-desc long-desc NIL-LIST-ID)
)

; returns #f if there is no param
(define (get-param lambda-id position)
  (query-maybe-value PROTO "SELECT id FROM params WHERE lambda_id = ?1 AND position = ?2" lambda-id position)
)

(define (create-param*! lambda-id position [short-desc sql-null] [long-desc sql-null])
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
    [define-id (create-something! "defines(id, short_desc, long_desc, expr_id)" (list short-desc long-desc UNASSIGNED-ID) dest-row-loc dest-col)])
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
  (define old-ref-count (get-cell dest-row-loc "ref_count"))
  (q* query "UPDATE ~a SET ref_count = ?2 WHERE id = ?1" dest-row-loc (add1 old-ref-count))
)

; can't be used on list nodes
(define (visit-id visitors data id)
  (assert-real-id id)
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

; TODO get-cars* should become a more internal method only being used by list-header internals and not accessible to general stuff
(define (get-cars* list-id)
  (if (is-nil*? list-id)
    '()
    (cons (get-cell list-id "car_id") (get-cars* (get-cell list-id "cdr_id")))
  )
)

(define (get-short-desc id)
  (define (just-short-desc* data id short-desc . etc) short-desc)
  (define (just-sql-null* data) sql-null)
  (define visitors (hash
    "lambdas" just-short-desc*
    "params" just-short-desc*
    "definitions" (lambda (data id define-id) (get-short-desc define-id))
    "defines" just-short-desc*
    "list_headers" just-short-desc*
    "atoms" just-short-desc*
    "legacies" (lambda (data id library name) name)
  ))
  (visit-id visitors #f id)
)

(define (get-short-desc-or id alt)
  (sql:// (get-short-desc id) alt)
)

; TRANSPILATION

(define (id->string* id)
  (format "veme:_~a" id)
)

(define (id->sym id)
  (string->symbol (id->string* id))
)

(define (id->scheme id)
  (define (id->sym* data id . etc) (id->sym id))
  ; We can't use #hash form, cuz it will interpret (type . proc) as '(type . proc), meaning proc is a symbol, not a proc
  ; ugh
  (define visitors (hash
    "lambdas" lambda-data->scheme
    "params" id->sym*
    "definitions" id->sym*
    "defines" define-data->scheme
    "list_headers" list-header-data->scheme
    "atoms" atom-data->scheme
    "legacies" legacy-link-data->scheme
  ))
  (visit-id visitors #f id)
)

(define (legacy-link-data->scheme data id library name)
  (if (equal? library DEFAULT-LIBRARY)
    (string->symbol name)
    ; TODO implement this
    (error 'legacy-link-data->scheme "Support for non-standard libraries not yet implemented: ~a::~a" library name)
  )
)

(define (define-data->scheme data id short-desc long-desc definition-id expr-id)
  (append (list 'define (id->scheme definition-id)) (id->scheme expr-id))
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
    (id->scheme body-id)
  )
)

(define (list-header-data->scheme data id short-desc long-desc item-ids)
  (map id->scheme item-ids)
)

(define (get-program-as-scheme*)
  (id->scheme PROG-START-ID)
)

(define (build-scheme-code)
  (write (get-program-as-scheme*))
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

; GUI

(define (get-item-id prog-tree-item)
  (send prog-tree-item user-data)
)

; new-blah-creator is a function of form
; <nil> => (dest-row-loc, dest-col => <nil>) OR #f
; If it returns #f, it means no operation should be performed
; The returned creator is destructive and must succeed
(define (new-list-creator)
  ; TODO "dog"?
  (lambda (dest-row-loc dest-col) (create-nil-list-header! dest-row-loc dest-col "dog"))
)

(define (new-number-creator)
  ; TODO 3?
  (lambda (dest-row-loc dest-col) (create-atom! "number" "3" dest-row-loc dest-col "cat"))
)

(define (new-character-creator)
  #f
  ; TODO
)

(define (new-string-creator)
  #f
  ; TODO
)

(define (new-boolean-creator)
  #f
  ; TODO
)

(define (new-function-creator)
  #f
  ; TODO
)

(define (new-lambda-creator)
  #f
  ; TODO
)

(define (new-value-definition-creator)
  #f
  ; TODO
)

(define (new-value-read-creator)
  #f
  ; TODO
)

; TODO minor cleanup of this comment along with the prior one about creators
; <nil> => (dest-row-loc, dest-col => <nil>)
; Returned creator will, when called, create a new item (if necessary) and update ref-counts (if necessary)
; The returned creator is not allowed to fail. A failure of this function should return #f instead of a creator
(define (request-new-item-creator*)
  (define friendly-types (hash-keys FRIENDLY-TYPE->CREATOR))
  (define choices (get-choices-from-user "Create new AST node" "Choose the node's type:" friendly-types))
  (cond
    [choices
      (assert (format "Multiple types chosen: ~a" choices) (= 1 (length choices)))
      ((hash-ref FRIENDLY-TYPE->CREATOR (list-ref friendly-types (car choices))))
    ]
    [else #f]
  )
)

(define (find-item-index compound-item item-to-find)
  (list-index (curry eq? item-to-find) (send compound-item get-items))
)

(define (nth-list-insertion-point* list-start-id index)
  (if (zero? index)
    list-start-id
    (nth-list-insertion-point* (get-cell list-start-id "cdr_id") (sub1 index))
  )
)

(define (insert-new-list-node*! list-header-id index)
  (define insertion-point (nth-list-insertion-point* list-header-id index))
  (define insertion-row-loc (get-row-loc insertion-point))
  (define old-cdr (get-cell insertion-row-loc "cdr_id"))
  ; We've captured the original cdr_id, and will soon move it to the newly created node, so we can safely replace this node's cdr
  (set-cell-dangerous*! insertion-row-loc "cdr_id" UNASSIGNED-ID)
  (define new-node-row-loc (get-row-loc (create-list*! list-header-id insertion-row-loc)))
  (set-id*! new-node-row-loc "cdr_id" old-cdr)
  new-node-row-loc 
)

(define (maybe-add-item-to-list! selected before/after far/near)
  (assert (format "before/after must be 'before or 'after, but is ~a" before/after) (member before/after '(before after)))
  (assert (format "far/near must be 'near or 'far, but is ~a" far/near) (member far/near '(far near)))
  (define is-far (equal? far/near 'far))
  (define is-before (equal? before/after 'before))
  (cond [selected
    (define creator! (request-new-item-creator*))
    (cond [creator!
      (define selected-id (get-item-id selected))
      (define selected-type (row-loc->table (get-row-loc selected-id)))
      (define list-to-augment
        (if (and is-far (equal? selected-type "list_headers"))
          selected
          (send selected get-parent)
        )
      )
      (define index-to-insert-at
        (if is-far
          (if is-before 0 (length (send list-to-augment get-items)))
          (+ (find-item-index list-to-augment selected) (if is-before 0 1))
        )
      )
      (define new-list-node-row-loc (insert-new-list-node*! (get-item-id list-to-augment) index-to-insert-at))
      ; TODO looks like maybe the creator should not take a column, and just shove into "car_id".
      ; will we use the same creators for non-list creations?
      (creator! new-list-node-row-loc "car_id")
      (refresh-prog-tree)
    ])
  ])
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
        [(#\a) (maybe-add-item-to-list! selected 'after 'near)]
        [(#\A) (maybe-add-item-to-list! selected 'after 'far)]
        [(#\i) (maybe-add-item-to-list! selected 'before 'near)]
        [(#\I) (maybe-add-item-to-list! selected 'before 'far)]
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

; TODO Currently, the params aren't modeled at all. This may need to change in the future
(define (add-lambda-to-prog-tree* prog-tree id short-desc long-desc arity positions->param-ids body-id)
  (define params-text
    (string-join (map-params (curryr get-short-desc-or "<?>") (const "¯\\_(ツ)_/¯") arity positions->param-ids) ", ")
  )
  (define lambda-text
    (sql:// short-desc (format "λ ~a -> ~a" params-text (get-short-desc-or body-id "...")))
  )
  (define lambda-tree (new-prog-tree-list prog-tree lambda-text id))
  (add-to-prog-tree lambda-tree body-id)
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

(define (list-header->short-text* data id short-desc long-desc item-ids)
  (sql:// short-desc (if (null? item-ids) "()" "(...)"))
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
    "lambdas" (short-desc-or* "λ...")
    "params" (short-desc-or* "<no desc>")
    "definitions" (short-desc-or* "<no desc>")
    "defines" define->short-text**
    "list_headers" list-header->short-text* 
    "atoms" atom->short-text*
    "legacies" legacy-link->short-text*
  ))
  (visit-id visitors #f id)
)

(define (list->text* item-ids)
  (string-join
    (map list-item->text* item-ids)
    ", "
    #:before-first "("
    #:after-last ")"
  )
)

(define (add-list-header-to-prog-tree* prog-tree id short-desc long-desc item-ids)
  (define list-tree (new-prog-tree-list prog-tree (sql:// short-desc (list->text* item-ids)) id))
  (for-each (curry add-to-prog-tree list-tree) item-ids)
)

(define (add-atom-to-prog-tree* prog-tree id short-desc long-desc type value)
  (new-prog-tree-item prog-tree (atom->short-text* prog-tree id short-desc long-desc type value) id)
)

(define (add-legacy-link-to-prog-tree* prog-tree id library name)
  (new-prog-tree-item prog-tree (legacy-link->short-text* prog-tree id library name) id)
)

(define (add-to-prog-tree prog-tree id)
  (define (just-short-desc* pt id . etc) (new-prog-tree-item prog-tree (get-short-desc-or id "<no desc>") id))

  (define prog-tree-visitors (hash
    "lambdas" add-lambda-to-prog-tree*
    "params" just-short-desc*
    "definitions" just-short-desc*
    "defines" add-define-to-prog-tree*
    "list_headers" add-list-header-to-prog-tree*
    "atoms" add-atom-to-prog-tree*
    "legacies" add-legacy-link-to-prog-tree*
  ))

  (visit-id prog-tree-visitors prog-tree id)
)

; GUI CONSTANTS

(define FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "list" new-list-creator
  "number" new-number-creator
  "character" new-character-creator
  "string" new-string-creator
  "boolean" new-boolean-creator
))

(define FRIENDLY-TYPE->CREATOR (hash-union FRIENDLY-LITERAL-TYPE->CREATOR (hash
  "define function" new-function-creator
  "anonymous function" new-lambda-creator
  "define value" new-value-definition-creator
  "read definition" new-value-read-creator
)))

; PROGRAM

; (init-db!)
; (build-scheme-code)

(define main-window (new frame% [label "Veme"]))
(define main-prog-tree (new logic-hierarchy% [parent main-window]))
; dummy item
(send main-prog-tree new-item)
(define (refresh-prog-tree)
  (send main-prog-tree delete-item (car (send main-prog-tree get-items)))
  (add-to-prog-tree main-prog-tree PROG-START-ID)
)
(refresh-prog-tree)
(send main-window show #t)
