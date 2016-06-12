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
  (unless bool (error msg))
)

; All indexed db queries must check that id is real first. higher-level stuff need not check directly
(define-syntax-rule (assert-real-id id)
  (assert "id was non-positive" (positive? id))
)

; TODO Not sure if it's better for this to be a macro, but if we make it a function, we can use compose
; to make some things way elegant
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
  (define first-id (create-something!! "list_headers(id, short_desc, long_desc, cdr_id)" (list "Main Program" "" UNASSIGNED-ID)))
  (assert
    (format "first created item should have id ~a but has id ~a" PROG-START-ID)
    (= PROG-START-ID first-id)
  )
  (define first-list-node-row-loc (get-row-loc (create-list*!! first-id)))
  (legacy-link!! first-list-node-row-loc "car_id" DEFAULT-LIBRARY "begin")
  ; TODO later, we may delete end-list in favor of a wholesale list creation thing
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
  (create-something!! "lambdas(id, short_desc, long_desc, arity, body_id)" (list short-desc long-desc arity UNASSIGNED-ID) dest-row-loc dest-col)
)

; TODO probably we should just have something that creates the list wholesale
(define (end-list*!! dest-row-loc)
  (set-id*!! dest-row-loc "cdr_id" NIL-LIST-ID)
)

(define (create-list*!! owner-id [dest-row-loc (get-row-loc owner-id)])
  (create-something!! "lists(id, owner_id, car_id, cdr_id)" (list owner-id UNASSIGNED-ID UNASSIGNED-ID) dest-row-loc "cdr_id")
)

(define (create-list-header*!! dest-row-loc dest-col short-desc long-desc cdr-id)
  (create-something!! "list_headers(id, short_desc, long_desc, cdr_id)" (list short-desc long-desc cdr-id) dest-row-loc dest-col)
)

(define (create-unassigned-list-header!! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*!! dest-row-loc dest-col short-desc long-desc UNASSIGNED-ID)
)

; TODO probably delete. We should just use the unassigned version, and then set-id*!! to NIL-LIST-ID
(define (create-nil-list-header!! dest-row-loc dest-col [short-desc sql-null] [long-desc sql-null])
  (create-list-header*!! dest-row-loc dest-col short-desc long-desc NIL-LIST-ID)
)

; returns #f if there is no param
(define (get-param lambda-id position)
  (query-maybe-value PROTO "SELECT id FROM params WHERE lambda_id = ?1 AND position = ?2" lambda-id position)
)

(define (create-param*!! lambda-id position [short-desc sql-null] [long-desc sql-null])
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
  )
)

; library and public-id must be vetted before calling this function
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
)

(define (inc-ref-count!! dest-row-loc)
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

(define (get-cars* list-node-id)
  (if (is-nil*? list-node-id)
    '()
    (cons (get-cell list-node-id "car_id") (get-cars* (get-cell list-node-id "cdr_id")))
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
  (list 'define (id->scheme definition-id) (id->scheme expr-id))
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

; new-blah-creator is a function of form
; <nil> => (dest-row-loc, dest-col => id) OR #f
; If it returns #f, it means no operation should be performed
; The returned creator is destructive and must succeed
(define (new-list-creator)
  ; TODO "dog"?
  (lambda (dest-row-loc dest-col) (create-nil-list-header!! dest-row-loc dest-col "dog"))
)

(define (new-number-creator)
  (define result
    (get-text-from-user
      "Enter a number"
      "Seriously, do it"
      #f
      ""
      '(disallow-invalid)
      #:validate string->number
    )
  )
  (if (string->number result)
    (lambda (dest-row-loc dest-col) (create-atom!! "number" result dest-row-loc dest-col))
    #f
  )
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

(define (new-define-creator)
  #f
  ; TODO
)

(define (new-lambda-creator)
  #f
  ; TODO
)

(define (new-value-read-creator)
  #f
  ; TODO
)

(define (new-legacy-creator)
  #f
  ; TODO
)

(define choice-dialog%
  (class dialog%

    (define keyboard-choice%
      (class choice%
        (define/override (on-subwindow-char receiver key-event)
          (define current-selection (send this get-selection))
          (define key-code (send key-event get-key-code))
          (define (reset-chars!) (set! chars ""))
          (case key-code
            [(#\j)
              (reset-chars!)
              (unless (= current-selection (sub1 num-choices*))
                (send this set-selection (add1 current-selection))
              )
              #t
            ]
            [(#\k)
              (reset-chars!)
              (unless (= current-selection 0)
                (send this set-selection (sub1 current-selection))
              )
              #t
            ]
            [else
              (cond
                [(and (char? key-code) (char-alphabetic? key-code))
                  (define char (char-downcase key-code))
                  (set! chars (string-append chars (string char)))
                  (define candidate (findf (curryr string-prefix? chars) choices*))
                  (if candidate
                    (send this set-string-selection candidate)
                    (reset-chars!)
                  )
                  #t
                ]
                [else #f]
              )
            ]
          )
        )
  
        (super-new)

        (define chars "")
      )
    )

    (define/override (on-subwindow-char receiver key-event)
      (cond
        [(equal? (send key-event get-key-code) #\return)
          (send this on-close)
          (send this show #f)
          #t
        ]
        [else (super on-subwindow-char receiver key-event)]
      )
    )

    (define/override (on-activate activated?)
      (super on-activate activated?)
      ; TODO this is a dirty dirty hack to force the choice% to focus against its will
      ; TODO see https://groups.google.com/forum/#!msg/racket-users/ph2nfGslyuA/AjAM6wMMAwAJ
      (send this on-subwindow-char this (new key-event% [key-code #\tab]))
    )

    (define (on-close)
      (set! chosen* (send choice* get-selection))
    )
    (augment on-close)

    (init title message choices)

    (super-new [label title])

    (define chosen* #f)
    (define choices* choices)
    (define num-choices* (length choices))
    (define choice*
      (new keyboard-choice%
        [label message]
        [choices choices]
        [parent this]
        [style '(vertical-label)]
      )
    )

    (define/public (get-choice)
      chosen*
    )
  )
)

(define (get-choice-from-user title message choices)
  (define dialog (new choice-dialog% [title title] [message message] [choices choices]))
  (send dialog show #t)
  (send dialog get-choice)
)

; TODO minor cleanup of this comment along with the prior one about creators
; <nil> => (dest-row-loc, dest-col => id)
; Returned creator will, when called, create a new item (if necessary), update ref-counts (if necessary), and return the id that is inserted
; The returned creator is not allowed to fail. A failure of this function should return #f instead of a creator
(define (request-new-item-creator*)
  (define friendly-types (hash-keys FRIENDLY-TYPE->CREATOR))
  (define choice (get-choice-from-user "Choose the new node's type:" "Choose the node's type:" friendly-types))
  (if choice
    ((hash-ref FRIENDLY-TYPE->CREATOR (list-ref friendly-types choice)))
    #f
  )
)

(define (find-item-index list-item item-to-find)
  (list-index (curry eq? item-to-find) (send list-item get-items))
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

(define (maybe-add-item-to-list!! selected-gui before/after far/near)
  (assert (format "before/after must be 'before or 'after, but is ~a" before/after) (member before/after '(before after)))
  (assert (format "far/near must be 'near or 'far, but is ~a" far/near) (member far/near '(far near)))
  (define is-far (equal? far/near 'far))
  (define is-before (equal? before/after 'before))
  (when selected-gui
    (define creator!! (request-new-item-creator*))
    (when creator!!
      (define selected-model (get-model selected-gui))
      (define list-to-augment
        (if (and is-far (is-a? selected-model lite-model-list-item%))
          selected-model
          (send selected-model get-parent)
        )
      )
      (define index-to-insert-at
        (if is-far
          (if is-before 0 (length (send list-to-augment get-items)))
          (+ (find-item-index list-to-augment selected-model) (if is-before 0 1))
        )
      )
      (define new-list-node-row-loc (insert-new-list-node*!! (send list-to-augment get-backing-id) index-to-insert-at))
      ; TODO looks like maybe the creator should not take a column, and just shove into "car_id".
      ; will we use the same creators for non-list creations?
      (define inserted-id (creator!! new-list-node-row-loc "car_id"))
      (define new-item-model (send list-to-augment insert! inserted-id index-to-insert-at))
      (send new-item-model select!)
    )
  )
)

; LITE MODEL
; The lite model was introduced so we could save state about the gui, which frequently has to be rebuilt. In
; the future, we will likely have a proper model that is a true layer of abstraction between the gui and storage,
; but for now the model will be simple and do the bare minimum

(define lite-model-item%
  (class object%

    (super-new)

    (init backing-id parent top-level)
    (define backing-id* backing-id)
    (define parent* parent)
    (define top-level* top-level)
    (define gui-item* #f)

    (define/public (get-backing-id)
      backing-id*
    )

    (define/public (get-parent)
      parent*
    )

    (define/public (get-top-level)
      top-level*
    )

    (define/public (get-gui-item)
      gui-item*
    )

    (define/public (create-gui!)
      (assert (format "You must delete-gui before using create-gui: id ~a" backing-id*) (not gui-item*))
      (define parent-gui-item (get-parent-gui-item*))
      (set! gui-item* (new-gui-item parent-gui-item))
      (send gui-item* user-data this)
      (change-text*! (id->short-text* backing-id*))
      (when (is-selected*?) (select!))
    )

    (define/public (delete-gui!)
      (when gui-item*
        (define parent-gui-item (get-parent-gui-item*))
        (send parent-gui-item delete-item gui-item*)
        (set! gui-item* #f)
      )
    )

    ; The racket-y way to do this seems to be using augment, but let's not mess with a lot of weird stuff
    ; that'll make it harder to do the Great Transitioning
    ; TODO this should be protected. After the Great Transitioning make it so
    (define/public (new-gui-item parent-gui-item)
      (send parent-gui-item new-item)
    )

    (define/public (select!)
      (send top-level* select gui-item*)
    )

    (define/public (is-root?)
      (eq? parent* top-level*)
    )

    (define/private (get-parent-gui-item*)
      (if (is-root?)
        parent*
        (send parent* get-gui-item)
      )
    )

    (define/private (is-selected*?)
      (eq? this (send top-level* get-selected-model))
    )

    ; lots of code liberally stolen from mred-designer
    (define/private (change-text*! new-text)
      (define ed (send gui-item* get-editor))
      (send ed erase)
      (send ed insert new-text)
    )
  )
)

(define lite-model-list-item%
  (class lite-model-item%

    (define/override (new-gui-item parent-gui-item)
      (send parent-gui-item new-list)
    )

    (define/override (create-gui!)
      (super create-gui!)
      (create-gui-items*!)
      (define gui-item (send this get-gui-item))
      (if is-open*
        (send gui-item open)
        (send gui-item close)
      )
    )

    (define/override (delete-gui!)
      (delete-gui-items*!)
      (super delete-gui!)
    )

    (super-new)

    (init item-ids [is-open #t])
    (define is-open* is-open)
    (define create-item* (curry create-lite-model-item this (send this get-top-level)))
    (define items* (map create-item* item-ids))

    (define/public (is-open?)
      is-open*
    )

    (define/public (open!)
      (set! is-open* #t)
      (define gui-item (send this get-gui-item))
      (when gui-item (send gui-item open))
    )

    (define/public (close!)
      (set! is-open* #f)
      (define gui-item (send this get-gui-item))
      (when gui-item (send gui-item close))
    )

    (define/public (get-items)
      items*
    )

    (define/public (insert! new-id index)
      ; TODO define-values w/ split-at is more elegant, but we have to figure out about the Great Transitioning
      (define before (take items* index))
      (define after (drop items* index))
      (define new-item (create-item* new-id))
      (set! items* (append before (cons new-item after)))
      (delete-gui-items*!)
      (create-gui-items*!)
      new-item
    )

    (define/private (create-gui-items*!)
      (for-each (lambda (item) (send item create-gui!)) items*)
    )

    (define/private (delete-gui-items*!)
      (for-each (lambda (item) (send item delete-gui!)) items*)
    )
  )
)

(define (create-lite-model-item parent-model top-level id)

  (define (create-simple-item* data id . etc)
    (new lite-model-item%
      [backing-id id]
      [parent parent-model]
      [top-level top-level]
    )
  )
  (define (create-layered-item* sublist-ids)
    ; Ugh - looks like there's no simple way to do '(apply new ...)'
    (new lite-model-list-item%
      [backing-id id]
      [parent parent-model]
      [top-level top-level]
      [item-ids sublist-ids]
    )
  )
  (define (create-list-model* data id s l item-ids)
    (create-layered-item* item-ids)
  )
  (define (create-lambda-model* data id s l a p body-id)
    ; This works because create-layered-item* takes its id not from the visitor but from the closure
    (visit-id model-visitors #f body-id)
  )
  (define (create-define-model* data id s l d expr-id)
    (create-layered-item* (list expr-id))
  )

  (define model-visitors (hash
    "lambdas" create-lambda-model*
    "params" create-simple-item*
    "definitions" create-simple-item*
    "defines" create-define-model* 
    "list_headers" create-list-model*
    "atoms" create-simple-item*
    "legacies" create-simple-item*
  ))

  (visit-id model-visitors #f id)
)

(define (is-last-in-list* model-item parent-list)
  (eq? model-item (last (send parent-list get-items)))
)

(define (move-down! model-item [can-go-in #t])
  (define top-level (send model-item get-top-level))
  (define parent (send model-item get-parent))
  (cond
    [(send model-item is-root?) (send top-level select-in)]
    [(is-last-in-list* model-item parent)
      (if (and can-go-in (is-a? model-item lite-model-list-item%) (pair? (send model-item get-items)))
        (send top-level select-in)
        (begin (send parent select!) (move-down! parent #f))
      )
    ]
    [else (send top-level select-next)]
  )
)

(define (move-up! selected-model)
  (unless (send selected-model is-root?)
    (define parent-model (send selected-model get-parent))
    (define top-level (send selected-model get-top-level))
    (if (eq? selected-model (car (send parent-model get-items)))
      (send top-level select-out)
      (send top-level select-prev)
    )
  )
)

(define logic-hierarchy%
  (class hierarchical-list%

    (define/override (on-select gui-item)
      (super on-select gui-item)
      (set! selected-model* (get-model gui-item))
    )

    (define/override (on-item-opened gui-item)
      (super on-item-opened gui-item)
      (define model (get-model gui-item))
      (unless (send model is-open?) (send model open!))
    )

    (define/override (on-item-closed gui-item)
      (super on-item-closed gui-item)
      (define model (get-model gui-item))
      (when (send model is-open?) (send model close!))
    )

    (define/override (on-char key-event)
      (define selected-gui (send this get-selected))
      (define selected-model (get-model selected-gui))
      (case (send key-event get-key-code)
        [(#\j) (move-down! selected-model)]
        [(#\k) (move-up! selected-model)]
        [(#\h) (send this select-out)]
        [(#\l) (send this select-in)]
        [(#\a) (maybe-add-item-to-list!! selected-gui 'after 'near)]
        [(#\A) (maybe-add-item-to-list!! selected-gui 'after 'far)]
        [(#\i) (maybe-add-item-to-list!! selected-gui 'before 'near)]
        [(#\I) (maybe-add-item-to-list!! selected-gui 'before 'far)]
      )
      (super on-char key-event)
    )

    (super-new)

    (init prog-start-backing-id)
    (define tree-root*
      (create-lite-model-item this this prog-start-backing-id)
    )
    (define selected-model* tree-root*)
    (send tree-root* create-gui!)

    (define/public (get-root)
      tree-root*
    )

    (define/public (get-selected-model)
      selected-model*
    )
  )
)

(define (get-model gui-item)
  (send gui-item user-data)
)

(define (lambda->short-text* data id short-desc long-desc arity positions->param-ids body-id)
  (define params-text
    (string-join (map-params (curryr get-short-desc-or "<?>") (const "¯\\_(ツ)_/¯") arity positions->param-ids) ", ")
  )
  (sql:// short-desc (format "λ ~a -> ~a" params-text (get-short-desc-or body-id "...")))
)

(define (define->short-text* data id short-desc long-desc expr-id)
  (format "~a = ~a" (sql:// short-desc "<no name>") (get-short-desc-or expr-id "..."))
)

(define (atom->short-text* data id short-desc long-desc type value)
  (sql:// short-desc (~a (atom-data->scheme data id short-desc long-desc type value)))
)

(define (list-header->short-text* data id short-desc long-desc item-ids)
  (sql://
    short-desc
    (string-join
      (map list-item->text* item-ids)
      ", "
      #:before-first "("
      #:after-last ")"
    )
  )
)

(define (get-short-desc-visitor* alt)
  (lambda (data id . etc)
    (get-short-desc-or id alt)
  )
)

(define (list-item->text* id)
  (define define->short-text**
    (compose (curry format "{~a}") define->short-text*)
  )
  (define (list-header->short-text** data id short-desc long-desc item-ids)
    (sql:// short-desc (if (null? item-ids) "()" "(...)"))
  )
  (define visitors (hash
    "lambdas" (get-short-desc-visitor* "λ...")
    "params" (get-short-desc-visitor* "<no desc>")
    "definitions" (get-short-desc-visitor* "<no desc>")
    "defines" define->short-text**
    "list_headers" list-header->short-text**
    "atoms" atom->short-text*
    "legacies" (get-short-desc-visitor* "<no name>")
  ))
  (visit-id visitors #f id)
)

(define (id->short-text* id)
  (define short-text-visitors (hash
    "lambdas" lambda->short-text*
    "params" (get-short-desc-visitor* "<no desc>")
    "definitions" (get-short-desc-visitor* "<no desc>")
    "defines" define->short-text*
    "list_headers" list-header->short-text*
    "atoms" atom->short-text*
    "legacies" (get-short-desc-visitor* "<no name>")
  ))
  (visit-id short-text-visitors #f id)
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
  "define" new-define-creator
  "lambda" new-lambda-creator
  "reference" new-value-read-creator
  "legacy" new-legacy-creator
)))

; PROGRAM

; TODO this probably shouldn't exist in the long run
(define (bomb)
  (for-each
    (lambda (table) (query-exec PROTO (format "DELETE FROM ~a" table)))
    TABLES
  )
  (init-db!!)
)
; (bomb)
; (build-scheme-code)

(define main-window (new frame% [label "Veme"]))
(define main-prog-tree (new logic-hierarchy% [parent main-window] [prog-start-backing-id PROG-START-ID]))
(send main-window show #t)
(send main-window maximize #t)
(send main-prog-tree focus)
