#lang racket

(require racket/gui/base)

(require "misc.rkt")
(require "db.rkt")
(require "sql-db.rkt")

;(define (thing)
;  (define (a) b)
;  (define b 3)
;  (cond
;    [(zero? 0)
;      (println b)
;      (a)
;    ]
;    [else
;      (a)
;    ]
;  )
;)
;
;(thing)

(define uneditable-text%
  (class text%
    (define/override (can-do-edit-operation? o [recursive #t])
      #f
    )

    (define (can-insert? start len)
      can-insert*?
    )
    (augment can-insert?)

    (define (can-delete? start len)
      can-insert*?
    )
    (augment can-delete?)

    (define can-insert*? #t)

    (super-new)

    (define styles (send this get-style-list))
    (define base-style
      (send styles find-or-create-style
        (send styles basic-style)
        (send (make-object style-delta%) set-delta-foreground "White")
      )
    )
    (send this change-style base-style)

    (define (create-new-style delta)
      (send styles find-or-create-style base-style delta)
    )

    (define thing_style
      (create-new-style
        (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Khaki")
      )
    )

    (send this hide-caret #t)

    (define keymap (make-object keymap%))
    (send keymap add-function "refresh-editor" refresh-editor)
    (send keymap map-function "right" "refresh-editor")
    (send this set-keymap keymap)

    (define/public (make-text)
      (set! can-insert*? #t)
      (send this erase)
      (define (display-node* db-handle whitespace)
        (send this insert whitespace)
        (define base-text
          (if (and (is-a? db-handle zinal:db:describable%%) (send db-handle get-short-desc))
            (send db-handle get-short-desc)
            (let-values ([(clazz bleh) (object-info db-handle)])
              clazz
            )
          )
        )
        (add-snip* (format "~a ~a\n" base-text (random 1000)) thing_style)
        (when (is-a? db-handle zinal:db:parent-node%%)
          (define child-whitespace (string-append whitespace "   "))
          (for-each (curryr display-node* child-whitespace) (send db-handle get-children))
        )
      )
  
      (define (add-snip* text style)
        (send this change-style style)
        (send this insert text)
        (send this change-style base-style)
      )
  
      (display-node* (send main-db get-root) "")
      (set! can-insert*? #f)
    )

    ;(define is-basic? #t)
    ;(build-list 50 (lambda (n)
    ;  ;(send this change-style (make-object style-delta% 'change-toggle-underline))
    ;  (build-list 35 (lambda (n)
    ;    ; (send this change-style (make-object style-delta% 'change-toggle-underline))
    ;    (cond
    ;      [is-basic?
    ;        (send this change-style base-style)
    ;        (set! is-basic? #f)
    ;      ]
    ;      [else
    ;        (send this change-style underlined_style)
    ;        (set! is-basic? #t)
    ;      ]
    ;    )
    ;    (send this insert (format "~a " (random 1000)))
    ;  ))
    ;  (send this insert #\newline)
    ;))

    (set! can-insert*? #f)
  )
)

(define (refresh-editor . blah)
  (unless (send main-canvas get-editor)
    (send main-canvas set-editor (new uneditable-text%))
  )
  (send (send main-canvas get-editor) make-text)
)

(define main-window (new frame% [label "testfuck"]))
(define main-db (new zinal:sql-db% [filename "junk.db"]))

(define main-canvas (new editor-canvas% [parent main-window]))
(send main-canvas set-canvas-background (make-object color% #x15 #x15 #x15))
(refresh-editor)

(send main-window show #t)
(send main-window maximize #t)
(send main-canvas focus)
