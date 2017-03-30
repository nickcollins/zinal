; Similar to "#lang racket"
(module main racket

(require racket/gui/base)

(require "misc.rkt")
(require "ents.rkt")
(require "db.rkt")
(require "sql-db.rkt")
(require "ui.rkt")

(define no-delta (make-object style-delta%))

; necessary for perf
(define lite-ui-info%
  (class object%

    (init root-ui-item)

    (super-make-object)

    (define pos-info% (class object%
      (init text style-delta is-selected?)
      (define text* text)
      (define style-delta* style-delta)
      (define selected*? is-selected?)
      (define/public (get-text) text*)
      (define/public (get-style-delta) style-delta*)
      (define/public (selected?) selected*?)
      (super-make-object)
    ))

    (define pos->pos-info* (make-hash))
    (define last-pos* 0)

    (define/public (get-pos->pos-info)
      pos->pos-info*
    )

    (define/public (equal-text? other-lite-ui-info)
      (define other-pos->pos-info (send other-lite-ui-info get-pos->pos-info))
      (and
        (andmap (lambda (p) (hash-ref pos->pos-info* p #f)) (hash-keys other-pos->pos-info))
        (andmap identity (hash-map pos->pos-info* (lambda (pos info)
          (define other-info (hash-ref other-pos->pos-info pos #f))
          (and
            other-info
            (equal? (send info get-text) (send other-info get-text))
            (send (send info get-style-delta) equal? (send other-info get-style-delta))
          )
        )))
      )
    )

    (define (add-snip text style-delta selected?)
      (define text-len (string-length text))
      (assert "Empty text in ui-item" (> text-len 0))
      (hash-set! pos->pos-info* last-pos* (make-object pos-info% text style-delta selected?))
      (set! last-pos* (+ last-pos* text-len))
    )

    (define (add-ui-list ui-list preceeding-whitespace selected?)
      (define children (send ui-list get-children))
      (define header (send ui-list get-header))
      (define separator (send ui-list get-horizontal-separator))
      (define bookends (send ui-list get-bookends))
      (cond
        [(send ui-list horizontal?)
          (when bookends (add-ui-item (first bookends) preceeding-whitespace selected?))
          (when header
            (add-ui-item header preceeding-whitespace selected?)
            (add-snip " " no-delta selected?)
          )
          (if (pair? children)
            (foldl
              (lambda (child prepend-separator?)
                (when prepend-separator? (add-ui-item separator preceeding-whitespace selected?))
                (add-ui-item child preceeding-whitespace selected?)
                #t
              )
              #f
              children
            )
            (unless (or header bookends)
              (add-snip "()" no-delta selected?)
            )
          )
          (when bookends (add-ui-item (second bookends) preceeding-whitespace selected?))
        ]
        [else ; vertical
          (define child-whitespace (string-append preceeding-whitespace "    "))
          (cond
            [header (add-ui-item header preceeding-whitespace selected?)]
            [bookends (add-ui-item (first bookends) preceeding-whitespace selected?)]
            [else (add-snip "(" no-delta selected?)]
          )
          (if (pair? children)
            (for-each
              (lambda (child)
                (add-snip (string-append (string #\newline) child-whitespace) no-delta #f)
                (add-ui-item child child-whitespace selected?)
              )
              children
            )
            (unless header
              (if bookends
                (add-ui-item (second bookends) preceeding-whitespace selected?)
                (add-snip ")" no-delta selected?)
              )
            )
          )
        ]
      )
    )

    (define (add-ui-item ui-item preceeding-whitespace selected-context?)
      (define selected? (or selected-context? (send ui-item selected?)))
      (if (is-a? ui-item zinal:ui:scalar%%)
        (add-snip (send ui-item get-text) (send ui-item get-style-delta) selected?)
        (add-ui-list ui-item preceeding-whitespace selected?)
      )
    )

    (add-ui-item root-ui-item "" #f)
  )
)

(define static-text%
  (class text%

    (init event-handler!! ui-info)

    (define event-handler*!! event-handler!!)
    (define ui-info* ui-info)
    (define can-insert*? #t)

    (define/override (on-event event)
      ; TODO apparently mouse-over and other spurious events are getting called, which is too much
      ; When we do support mouse events, we have to be careful about which ones
      #f
    )

    (define/override (on-char event)
      (event-handler*!! event)
    )

    (define/override (can-do-edit-operation? o [recursive #t])
      #f
    )

    (define (can-insert? start len)
      can-insert*?
    )
    (augment can-insert?)

    (define (can-delete? start len)
      #f
    )
    (augment can-delete?)

    (super-make-object)

    (define styles (send this get-style-list))
    (define base-style
      (send styles find-or-create-style
        (send styles basic-style)
        (send (make-object style-delta%) set-delta-foreground "White")
      )
    )
    (send this change-style base-style)

    (define selected-style
      (send styles find-or-create-style
        base-style
        (send (send (make-object style-delta%) set-delta-background "White") set-delta-foreground "Black")
      )
    )

    (define (get-style pos-info)
      (if (send pos-info selected?)
        selected-style
        (send styles find-or-create-style base-style (send pos-info get-style-delta))
      )
    )

    (define/public (reselect! new-ui-info)
      (define pos->pos-info (send ui-info* get-pos->pos-info))
      (define new-pos->pos-info (send new-ui-info get-pos->pos-info))
      (hash-map pos->pos-info (lambda (pos old-info)
        (define new-info (hash-ref new-pos->pos-info pos))
        (when (xor (send old-info selected?) (send new-info selected?))
          (send this change-style (get-style new-info) pos (+ pos (string-length (send new-info get-text))))
        )
      ))
      (set! ui-info* new-ui-info)
    )

    (define/public (scroll!)
      (define pos->pos-info (send ui-info* get-pos->pos-info))
      (define drop-not-selected
        (dropf
          (sort (hash-keys pos->pos-info) <)
          (lambda (p)
            (not (send (hash-ref pos->pos-info p) selected?))
          )
        )
      )
      (when (pair? drop-not-selected) (send this scroll-to-position (car drop-not-selected)))
    )

    (send this hide-caret #t)

    (for-each
      (lambda (pos)
        (define info (hash-ref (send ui-info* get-pos->pos-info) pos))
        (send this change-style (get-style info))
        (send this insert (send info get-text))
        (send this change-style base-style)
      )
      (sort (hash-keys (send ui-info* get-pos->pos-info)) <)
    )

    (set! can-insert*? #f)
  )
)

; PROGRAM

(define main-window (make-object frame% "zinal"))
(define main-canvas (make-object editor-canvas% main-window))
(send main-canvas set-canvas-background (make-object color% #x15 #x15 #x15))

(define main-db (make-object zinal:sql-db% "junk.db"))
(define main-ent-manager (make-object zinal:ent:manager% main-db))

(define last-ui-info* #f)
(define (display-ui! ui-item)
  (define new-ui-info (make-object lite-ui-info% ui-item))
  (cond
    [(and last-ui-info* (send new-ui-info equal-text? last-ui-info*))
      (send (send main-canvas get-editor) reselect! new-ui-info)
    ]
    [else
      (define new-editor (make-object static-text% handle-event!! new-ui-info))
      (send main-canvas set-editor new-editor)
    ]
  )
  (set! last-ui-info* new-ui-info)
  (send (send main-canvas get-editor) scroll!)
  (void)
)

(define (handle-event!! event)
  (display-ui! (send main-ent-manager handle-event!! event))
)

(display-ui! (send main-ent-manager get-initial-ui!))
(send main-window show #t)
(send main-window maximize #t)
(send main-canvas focus)
)
