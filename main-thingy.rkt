#lang racket

(require mrlib/hierlist)
(require racket/gui/base)
; for hash-union
(require racket/hash)
; for list-index
(require srfi/1)

(require "misc.rkt")
(require "sql-db.rkt")

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
    "unassigned" (thunk* (error 'id->scheme "Cannot generate scheme code if some nodes are unassigned"))
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
        (format "Character ~a must be the integer value of the desired character" value)
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

; GUI

(define (get-short-desc* id)
  (define (just-short-desc* data id short-desc . etc) short-desc)
  (define visitors (hash
    "lambdas" just-short-desc*
    "params" just-short-desc*
    "definitions" (lambda (data id define-id) (get-short-desc* define-id))
    "defines" just-short-desc*
    "list_headers" just-short-desc*
    "atoms" just-short-desc*
    "legacies" (lambda (data id library name) name)
    "unassigned" unassigned-text-visitor
  ))
  (visit-id visitors #f id)
)

(define (get-short-desc-or id alt)
  (sql:// (get-short-desc* id) alt)
)

; new-blah-creator is a function of form
; <nil> => (dest-row-loc, dest-col => id) OR #f
; If it returns #f, it means no operation should be performed
; The returned creator is destructive and must succeed
(define (new-list-creator)
  (lambda (dest-row-loc dest-col) (create-nil-list-header!! dest-row-loc dest-col))
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
  (if (and result (string->number result))
    (lambda (dest-row-loc dest-col) (create-atom!! "number" result dest-row-loc dest-col))
    #f
  )
)

(define (new-character-creator)
  (define validate-char (compose1 (curry = 1) string-length))
  (define result
    (get-text-from-user
      "Enter a character"
      "Seriously, do it"
      #f
      ""
      '(disallow-invalid)
      #:validate validate-char
    )
  )
  (cond
    [(and result (validate-char result))
      ; If you look at atom-data->scheme , you'll see that internal chars are stored as integer codes
      (define result-code (number->string (char->integer (string-ref result 0))))
      (lambda (dest-row-loc dest-col) (create-atom!! "character" result-code dest-row-loc dest-col))
    ]
    [else #f]
  )
)

(define (new-string-creator)
  (define result
    (get-text-from-user
      "Enter a string"
      "Seriously, do it"
      #:validate (const #t)
    )
  )
  (if result
    (lambda (dest-row-loc dest-col) (create-atom!! "string" result dest-row-loc dest-col))
    #f
  )
)

(define (new-boolean-creator)
  (define choices '("t" "f"))
  (define result (list-ref choices (get-choice-from-user "To be or not to be?" "That's the fuckin question" choices)))
  (if result
    (lambda (dest-row-loc dest-col) (create-atom!! "boolean" result dest-row-loc dest-col))
    #f
  )
)

(define (new-define-creator)
  (define result
    (get-text-from-user
      "Enter the new definition's short descriptor"
      "A short descriptor, one or a few words, to identify this variable"
      #:validate (const #t)
    )
  )
  (if result
    (lambda (dest-row-loc dest-col) (create-define!! dest-row-loc dest-col result))
    #f
  )
)

(define (new-lambda-creator)
  (define validate-natural (compose1 (conjoin integer? non-negative?) string->number))
  (define arity-string
    (get-text-from-user
      "Enter the new function's arity"
      "How many params does it have? Seriously."
      #f
      ""
      '(disallow-invalid)
      #:validate validate-natural
    )
  )
  (cond
    [(and arity-string (validate-natural arity-string))
      (define arity (string->number arity-string))
      (define param-short-names
        (build-list arity (lambda (p)
          (get-text-from-user
            (format "Enter the short descriptor of the ~ath parameter" p)
            (format "Param ~a is:" p)
            #:validate (const #t)
          )
        ))
      )
      (lambda (dest-row-loc dest-col)
        (define lambda-id (create-lambda!! arity dest-row-loc dest-col))
        (build-list arity (lambda (p) (create-param!! lambda-id p (list-ref param-short-names p))))
        lambda-id
      )
    ]
    [else #f]
  )
)

(define (new-value-read-creator)
  (define all-referable-ids (get-referable-ids))
  (cond
    [(pair? all-referable-ids)
      (define ids&choices (map (lambda (id) (list id (get-short-desc-or id "<no desc>"))) all-referable-ids))
      (define dialog
        (new auto-complete-dialog%
          [title "What definition or parameter do you want to read?"]
          [message "Start typing bits and pieces of the desired reference's short descriptor"]
          [ids&choices ids&choices]
        )
      )
      (send dialog show #t)
      (define chosen-id (send dialog get-choice))
      (if chosen-id
        (lambda (dest-row-loc dest-col)
          (inc-ref-count!! (get-row-loc chosen-id))
          (set-id*!! dest-row-loc dest-col chosen-id)
          chosen-id
        )
        #f
      )
    ]
    [else #f]
  )
)

(define (new-legacy-creator)
  (define result
    (get-text-from-user
      "Enter the standard library identifier"
      "Enter the standard library identifier"
      ; TODO we need to add a reflective validator
      #:validate (const #t)
    )
  )
  (if result
    (lambda (dest-row-loc dest-col) (legacy-link!! dest-row-loc dest-col DEFAULT-LIBRARY result))
    #f
  )
)

(define choice-dialog%
  (class dialog% ; abstract

    (define/override (on-subwindow-char receiver key-event)
      (case (send key-event get-key-code)
        [(#\return)
          (set! has-been-chosen* #t)
          (close*!)
          #t
        ]
        [('escape)
          (set! has-been-chosen* #f)
          (close*!)
          #t
        ]
        [(#\tab)
          (super on-subwindow-char receiver key-event)
          #f
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

    (abstract get-choice-from-ui*)

    (define (on-close)
      (set! chosen*
        (if has-been-chosen*
          (get-choice-from-ui*)
          #f
        )
      )
    )
    (augment on-close)

    (super-new)

    (define chosen* #f)
    (define has-been-chosen* #f)

    (define/public (get-choice)
      chosen*
    )

    (define/private (close*!)
      (send this on-close)
      (send this show #f)
    )
  )
)

(define auto-complete-dialog%
  (class choice-dialog%

    (define auto-complete-list-box%
      (class list-box%

        (define/override (on-subwindow-char receiver key-event)
          (define cur-selection-cur-index (send this get-selection))
          (define cur-selection-id (and cur-selection-cur-index (send this get-data cur-selection-cur-index)))
          (define num-cur-choices (send this get-number))
          (define key-code (send key-event get-key-code))
          (case key-code
            [(#\tab)
              (send this set-selection
                (if (= cur-selection-cur-index (sub1 num-cur-choices))
                  0
                  (add1 cur-selection-cur-index)
                )
              )
              #t
            ]
            [(#\backspace #\rubout)
              (cond
                [(pair? chars)
                  (set! chars (take chars (sub1 (length chars))))
                  (build-choices*! cur-selection-id)
                  #t
                ]
                [else #f]
              )
            ]
            [else
              (cond
                [(char? key-code)
                  (define char (char-downcase key-code))
                  (set! chars (append chars (list char)))
                  (build-choices*! cur-selection-id)
                  #t
                ]
                [else #f]
              )
            ]
          )
        )

        (super-new)

        (define/private (build-choices*! selection-id)
          (send this clear)
          (for-each
            (lambda (pair)
              (define id (car pair))
              (define choice (second pair))
              (when (subsequence? chars (string->list (string-downcase choice)))
                (send this append choice id)
                (define cur-index (sub1 (send this get-number)))
                (when (equal? selection-id id)
                  (send this set-selection cur-index)
                )
              )
            )
            ids&choices*
          )
          (when (zero? (send this get-number)) (reset-choices*!))
          (unless (send this get-selection)
            (send this set-selection 0)
          )
          ; TODO very hacky sizing
          (send this min-height (+ 60 (* 25 (send this get-number))))
        )

        (define/private (subsequence? candidate sequence)
          (cond
            [(null? candidate)
              #t
            ]
            [(null? sequence)
              #f
            ]
            [(equal? (car candidate) (car sequence))
              (subsequence? (cdr candidate) (cdr sequence))
            ]
            [else
              (subsequence? candidate (cdr sequence))
            ]
          )
        )

        (define/private (reset-choices*!)
          (set! chars '())
          (build-choices*! #f)
        )

        (define chars '())
        (reset-choices*!)
      )
    )

    (define/override (get-choice-from-ui*)
      (define cur-index (send chooser* get-selection))
      (and cur-index (send chooser* get-data cur-index))
    )

    (init title message ids&choices)

    (super-new [label title])

    (define ids&choices* (sort ids&choices (lambda (a b) (string<? (second a) (second b)))))
    (define num-all-choices* (length ids&choices))
    (define chooser*
      (new auto-complete-list-box%
        [label message]
        [choices '()]
        [parent this]
        [style '(single vertical-label)]
      )
    )
  )
)

(define discrete-choice-dialog%
  (class choice-dialog%

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
              (unless (zero? current-selection)
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

    (define/override (get-choice-from-ui*)
      (send chooser* get-selection)
    )

    (init title message choices)

    (super-new [label title])

    (define choices* choices)
    (define num-choices* (length choices))
    (define chooser*
      (new keyboard-choice%
        [label message]
        [choices choices]
        [parent this]
        [style '(vertical-label)]
      )
    )
  )
)

(define (get-choice-from-user title message choices)
  (define dialog (new discrete-choice-dialog% [title title] [message message] [choices choices]))
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

(define (maybe-add-item-to-list!! selected-model before/after far/near)
  (maybe-add-item-to-list*!! selected-model before/after far/near request-new-item-creator*)
)

(define (maybe-add-item-to-list*!! selected-model before/after far/near creator-generator)
  (assert (format "before/after must be 'before or 'after, but is ~a" before/after) (member before/after '(before after)))
  (assert (format "far/near must be 'near or 'far, but is ~a" far/near) (member far/near '(far near)))
  (define is-far? (equal? far/near 'far))
  (define is-before? (equal? before/after 'before))

  (define selected-id (send selected-model get-backing-id))
  ; TODO we need to clean up this awful casing when we go full model
  (define is-unassigned? (= UNASSIGNED-ID selected-id))
  (define is-list? (and (not is-unassigned?) (member (get-type selected-id) '("lambdas" "list_headers"))))
  (define insert-at-extreme? (or is-far? (send selected-model is-root?)))
  (define insert-list-extreme? (and is-list? insert-at-extreme?))
  (define is-define-expr?
    (and
      (not insert-list-extreme?)
      (equal? (get-type (send (send selected-model get-parent) get-backing-id)) "defines")
    )
  )

  (when (not is-define-expr?)
    (define creator!! (and creator-generator (creator-generator)))
    (when (implies creator-generator creator!!)
      (define list-to-augment
        (if insert-list-extreme?
          selected-model
          (send selected-model get-parent)
        )
      )
      (define index-to-insert-at
        (if insert-at-extreme?
          (if is-before? 0 (length (send list-to-augment get-items)))
          (+ (find-item-index list-to-augment selected-model) (if is-before? 0 1))
        )
      )
      (define list-to-augment-id (send list-to-augment get-backing-id))
      (define list-header-id
        (if (equal? (get-type list-to-augment-id) "list_headers")
          list-to-augment-id
          (get-cell list-to-augment-id "body_id")
        )
      )
      (define new-list-node-row-loc (insert-new-list-node*!! list-header-id index-to-insert-at))
      ; TODO looks like maybe the creator should not take a column, and just shove into "car_id".
      ; will we use the same creators for non-list creations?
      (define inserted-id
        (if creator-generator
          (creator!! new-list-node-row-loc "car_id")
          UNASSIGNED-ID
        )
      )
      (define new-item-model (send list-to-augment insert! inserted-id index-to-insert-at))
      (send new-item-model select!)
    )
  )
)

(define (maybe-replace-item!! selected-model)
  (define selected-id (send selected-model get-backing-id))
  ; TODO this restriction will change in the future
  (when (= selected-id UNASSIGNED-ID)
    (define creator!! (request-new-item-creator*))
    (when creator!!
      (define parent (send selected-model get-parent))
      (define parent-id (send parent get-backing-id))
      (define parent-type (get-type parent-id))
      (define new-item-model
        ; TODO encapsulation is busted. another thing to fix when going full model
        (case parent-type
          [("list_headers" "lambdas")
            (define index-to-replace (find-item-index parent selected-model))
            (define list-header-id
              (if (equal? parent-type "list_headers")
                parent-id
                (get-cell parent-id "body_id")
              )
            )
            (define list-node-to-replace-id (nth-list-id list-header-id index-to-replace))
            (define new-id (creator!! (get-row-loc list-node-to-replace-id) "car_id"))
            (send parent replace! new-id index-to-replace)
          ]
          [("defines")
            (define new-id (creator!! (get-row-loc parent-id) "expr_id"))
            (send parent replace! new-id 0)
          ]
          [else
            (error 'maybe-replace-item!! "Parent id ~a has type ~a which doesn't make any sense" parent-id parent-type)
          ]
        )
      )
      (send new-item-model select!)
    )
  )
)

(define (maybe-append-unassigned-list-item!! selected-model far/near)
  (maybe-add-item-to-list*!! selected-model 'after far/near #f)
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
      (update-text*!)
      (when (is-selected*?) (select!))
    )

    (define/public (delete-gui!)
      (when gui-item*
        (define parent-gui-item (get-parent-gui-item*))
        (send parent-gui-item delete-item gui-item*)
        (set! gui-item* #f)
      )
    )

    (define/public (update-gui!)
      (update-text*!)
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
    (define/private (update-text*!)
      (define ed (send gui-item* get-editor))
      (send ed erase)
      (send ed insert (id->short-text* backing-id*))
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

    (define/public (replace! new-id index)
      (define new-item (create-item* new-id))
      ; We must delete the gui item of the replaced model before throwing out the model itself
      (delete-gui-items*!)
      (set! items* (list-set items* index new-item))
      (refresh-gui*!)
      new-item
    )

    (define/public (insert! new-id index)
      ; TODO define-values w/ split-at is more elegant, but we have to figure out about the Great Transitioning
      (define before (take items* index))
      (define after (drop items* index))
      (define new-item (create-item* new-id))
      (set! items* (append before (cons new-item after)))
      (refresh-gui*!)
      new-item
    )

    (define/private (refresh-gui*!)
      (delete-gui-items*!)
      (create-gui-items*!)
      (send this update-gui!)
      (unless (send this is-root?) (send (send this get-parent) update-gui!))
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

  ; Uses the closed id instead of the passed one so that the unassigned case will work
  (define (create-simple-item* data . etc)
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
    "unassigned" create-simple-item*
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
  ; TODO move to last item, once we have a G command
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
      (define (maybe* proc . args)
        (when selected-gui
          (apply proc (get-model selected-gui) args)
        )
      )
      (case (send key-event get-key-code)
        [(#\j) (maybe* move-down!)]
        [(#\k) (maybe* move-up!)]
        ; TODO capital H to contract
        [(#\h) (send this select-out)]
        [(#\l) (send this select-in)]
        [(#\a) (maybe* maybe-add-item-to-list!! 'after 'near)]
        [(#\A) (maybe* maybe-add-item-to-list!! 'after 'far)]
        [(#\i) (maybe* maybe-add-item-to-list!! 'before 'near)]
        [(#\I) (maybe* maybe-add-item-to-list!! 'before 'far)]
        [(#\s) (maybe* maybe-replace-item!!)]
        [(#\o) (maybe* maybe-append-unassigned-list-item!! 'near)]
        [(#\O) (maybe* maybe-append-unassigned-list-item!! 'far)]
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
    (string-join (map-params (curryr get-short-desc-or "<no desc>") (const "¯\\_(ツ)_/¯") arity positions->param-ids) ", ")
  )
  (sql:// short-desc (format "λ ~a -> ~a" params-text (get-short-desc-or body-id "...")))
)

(define (define->short-text* data id short-desc long-desc definition-id expr-id)
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

(define unassigned-text-visitor (const "<?>"))

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
    "unassigned" unassigned-text-visitor
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
    "unassigned" unassigned-text-visitor
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

; (get-program-as-scheme*)

(define main-window (new frame% [label "Veme"]))
(define main-prog-tree (new logic-hierarchy% [parent main-window] [prog-start-backing-id PROG-START-ID]))
(send main-window show #t)
(send main-window maximize #t)
(send main-prog-tree focus)
