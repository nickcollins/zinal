#lang racket

(provide (all-defined-out))

(define zinal:ui:context%% (interface ()

  selected? ; ()

  highlighted? ; ()

  horizontal? ; ()

  get-item ; () -> zinal:ui:item%%

  ; TODO current
  ; get-parent ; () -> zinal:ui:list%% OR #f
))

(define zinal:ui:item%% (interface ()

  get-context ; () -> zinal:ui:context%%
))

(define zinal:ui:scalar%% (interface (zinal:ui:item%%)

  get-style-delta ; ()

  get-text ; ()
))

(define zinal:ui:const%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:var-scalar%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:list%% (interface (zinal:ui:item%%)

  get-children ; () -> [zinal:ui:context%%]

  get-header ; () -> zinal:ui:context%% OR #f

  get-horizontal-separator ; () -> zinal:ui:const%% OR #f
))


; TODO current delete all this

(define escalated-event-handler%% (interface ()

  handle-event!! ; (key-event)
))

(define slot% (iface escalated-event-handler%%)

  (init event-handler!! escalated-event-handler)

  (define (handle-event!! key-event)
  )
)

(define ent% (iface escalated-event-handler%%)

  (define (handle-event!! key-event)
    (send slot* handle-event!! key-event)
  )
)
