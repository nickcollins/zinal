; Copyright 2017 Nick Collins
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

; Similar to "#lang racket"
(module main racket

(require racket/gui/base)
(require (only-in racket/exn exn->string))
(require (only-in racket/cmdline parse-command-line))
(require (only-in compiler/embed create-embedding-executable))

(require "misc.rkt")
(require "ents.rkt")
(require "db.rkt")
(require "sql-db.rkt")
(require "ui.rkt")
(require "transpile.rkt")

(define no-delta (make-object style-delta%))

(define pos-info% (class object%
  (init text style-delta is-selected? is-highlighted?)
  (define text* text)
  (define style-delta* style-delta)
  (define selected*? is-selected?)
  (define highlighted*? is-highlighted?)
  (define/public (get-text) text*)
  (define/public (get-style-delta) style-delta*)
  (define/public (selected?) selected*?)
  (define/public (highlighted?) highlighted*?)
  (super-make-object)
))

; necessary for perf
(define lite-ui-info%
  (class object%

    (init root-ui-item)

    (super-make-object)

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

    (define (add-snip text style-delta selected? [highlighted? #f])
      (define text-len (string-length text))
      (assert "Empty text in ui-item" (> text-len 0))
      (hash-set! pos->pos-info* last-pos* (make-object pos-info% text style-delta selected? highlighted?))
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
        (add-snip (send ui-item get-text) (send ui-item get-style-delta) selected? (is-highlighted? ui-item))
        (add-ui-list ui-item preceeding-whitespace selected?)
      )
    )

    (define (is-highlighted? ui-item)
      (and (is-a? ui-item zinal:ui:var-scalar%%) (send ui-item highlighted?))
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

    (define highlighted-style
      (send styles find-or-create-style
        base-style
        (send (send (make-object style-delta%) set-delta-background (make-object color% #x45 #x2A #x45)) set-delta-foreground "White")
      )
    )

    (define (get-style pos-info)
      (cond
        [(send pos-info selected?) selected-style]
        [(send pos-info highlighted?) highlighted-style]
        [else (send styles find-or-create-style base-style (send pos-info get-style-delta))]
      )
    )

    (define/public (reselect! new-ui-info)
      (define pos->pos-info (send ui-info* get-pos->pos-info))
      (define new-pos->pos-info (send new-ui-info get-pos->pos-info))
      (hash-map pos->pos-info (lambda (pos old-info)
        (define new-info (hash-ref new-pos->pos-info pos))
        (when (or (xor (send old-info selected?) (send new-info selected?)) (xor (send old-info highlighted?) (send new-info highlighted?)))
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

(define (compile-db db output-file-path)
  (define transpiled-data (transpile db))
  (define compiled-units
    (parameterize ([current-namespace (make-base-namespace)])
      (map compile (list
        (append (list 'module '__zinal_dummy_module__ 'racket) transpiled-data)
        (list 'dynamic-require '''__zinal_dummy_module__ #f)
      ))
    )
  )
  ; adding "--" to cmdline prevents command-line arguments from being treated as racket command line args instead of
  ; zinal command line args
  (create-embedding-executable output-file-path #:modules '() #:literal-expressions compiled-units #:cmdline '("--"))
)

(define (compile-db-with-dialog db output-file-path)
  (when output-file-path
    ; TODO not sure what the best way to do concurrency is but this will have to do for now
    (define dialog (make-object dialog% "Compiling ..."))
    (define compilation-finished? #f)
    (define compile-thread (thread (thunk
      (with-handlers
        ([(const #t) (lambda (e)
          (set! compilation-finished? #t)
          (message-box "Cannot compile" (exn->string e) #f '(ok caution))
          (send dialog show #f)
        )])
        (compile-db db output-file-path)
        (set! compilation-finished? #t)
        (send dialog show #f)
      )
    )))
    (make-object message% "Compiling - this may take a while ..." dialog)
    (make-object button% "Cancel" dialog (thunk* (kill-thread compile-thread) (send dialog show #f)))
    (unless compilation-finished?
      (send dialog show #t)
    )
    ; TODO hack because i don't know how to disable the x button
    (unless (thread-dead? compile-thread) (kill-thread compile-thread))
  )
)

; PROGRAM

(define command-line-compile-out-fn #f)
(define db-file-path-string #f)

(parse-command-line
  "zinal"
  (current-command-line-arguments)
  (list (list 'once-each
    [list
      '("-o" "--compile-to")
      (lambda (flag out-fn) (set! command-line-compile-out-fn out-fn))
      '("Compile the db instead of opening it" "output-filename")
    ]
  ))
  (lambda (flag-args [db-fn #f]) (set! db-file-path-string db-fn))
  '("db-filename")
)

(unless db-file-path-string
  (set! db-file-path-string (get-file "Choose a zinal sqlite db to open"))
)
(unless db-file-path-string (exit))

(define main-db (make-object zinal:sql-db% db-file-path-string))

(when command-line-compile-out-fn
  (compile-db main-db command-line-compile-out-fn)
  (exit)
)

(define main-ent-manager (make-object zinal:ent:manager% main-db))

(define main-window (make-object frame% "zinal"))
(define main-canvas (make-object editor-canvas% main-window))
(define menu-bar (make-object menu-bar% main-window))
(define compile-menu (make-object menu% "compile" menu-bar))
(void (make-object menu-item% "compile" compile-menu (thunk* (compile-db-with-dialog main-db (put-file "Choose the executable destination")))))
(send main-canvas set-canvas-background (make-object color% #x15 #x15 #x15))

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
