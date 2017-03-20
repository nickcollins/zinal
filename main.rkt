; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module main racket

(require racket/gui/base)

(require "misc.rkt")
(require "ents.rkt")
(require "db.rkt")
(require "sql-db.rkt")
(require "ui.rkt")

(define static-text%
  (class text%

    (init event-handler!! root-ui-item)

    (define event-handler*!! event-handler!!)
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

    (define no-delta (make-object style-delta%))

    (define (get-style delta selected?)
      (if selected?
        selected-style
        (send styles find-or-create-style base-style delta)
      )
    )

    (send this hide-caret #t)

    (define (add-snip text style-delta selected?)
      (define style (get-style style-delta selected?))
      (define pos (send this get-start-position))
      (send this change-style style)
      (send this insert text)
      (send this change-style base-style)
      (and selected? pos)
    )

    (define (display-ui-list ui-list preceeding-whitespace selected?)
      (define children (send ui-list get-children))
      (define header (send ui-list get-header))
      (define separator (send ui-list get-horizontal-separator))
      (define bookends (send ui-list get-bookends))
      (define pos #f)
      (define (update-pos! new-pos) (unless pos (set! pos new-pos)))
      (cond
        [(send ui-list horizontal?)
          (when bookends (display-ui-item (first bookends) preceeding-whitespace selected?))
          (when header
            (update-pos! (display-ui-item header preceeding-whitespace selected?))
            (add-snip " " no-delta selected?)
          )
          (if (pair? children)
            (foldl
              (lambda (child prepend-separator?)
                (when prepend-separator? (display-ui-item separator preceeding-whitespace selected?))
                (update-pos! (display-ui-item child preceeding-whitespace selected?))
                #t
              )
              #f
              children
            )
            (unless (or header bookends)
              (add-snip "()" no-delta selected?)
            )
          )
          (when bookends (display-ui-item (second bookends) preceeding-whitespace selected?))
        ]
        [else ; vertical
          (define child-whitespace (string-append preceeding-whitespace "    "))
          (cond
            [header
              (update-pos! (display-ui-item header preceeding-whitespace selected?))
            ]
            [bookends
              (display-ui-item (first bookends) preceeding-whitespace selected?)
            ]
            [else
              (add-snip "(" no-delta selected?)
            ]
          )
          (if (pair? children)
            (for-each
              (lambda (child)
                (send this insert #\newline)
                (send this insert child-whitespace)
                (update-pos! (display-ui-item child child-whitespace selected?))
              )
              children
            )
            (unless header
              (if bookends
                (display-ui-item (second bookends) preceeding-whitespace selected?)
                (add-snip ")" no-delta selected?)
              )
            )
          )
        ]
      )
      pos
    )

    (define (display-ui-item ui-item preceeding-whitespace selected-context?)
      (define selected? (or selected-context? (send ui-item selected?)))
      (if (is-a? ui-item zinal:ui:scalar%%)
        (add-snip (send ui-item get-text) (send ui-item get-style-delta) selected?)
        (display-ui-list ui-item preceeding-whitespace selected?)
      )
    )

    (define scroll-pos* (display-ui-item root-ui-item "" #f))

    (define/public (scroll!)
      (when scroll-pos* (send this scroll-to-position scroll-pos*))
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

(define (display-ui! ui-item)
  (define new-editor (make-object static-text% handle-event!! ui-item))
  (send main-canvas set-editor new-editor)
  (send new-editor scroll!)
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
