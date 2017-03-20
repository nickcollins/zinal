; Similar to "#lang racket"
(module ui racket

(provide (all-defined-out))

(define zinal:ui:item%% (interface ()

  selected? ; ()

  highlighted? ; ()

  accept ; (zinal:ui:element-visitor% [data])
))

(define zinal:ui:scalar%% (interface (zinal:ui:item%%)

  get-style-delta ; ()

  get-text ; ()
))

(define zinal:ui:const%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:var-scalar%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:list%% (interface (zinal:ui:item%%)

  get-children ; () -> [zinal:ui:item%%]

  get-header ; () -> zinal:ui:item%% OR #f

  horizontal? ; ()

  get-horizontal-separator ; () -> zinal:ui:const%% OR #f

  get-bookends ; () -> [zinal:ui:const%% zinal:ui:const%%] OR #f
))

(define zinal:ui:element-visitor%
  (class object%
    (super-make-object)

    (define/public (visit-item i data) #f)
    (define/public (visit-scalar s data) (visit-item s data))

    (define/public (visit-list l data) (visit-item l data))
    (define/public (visit-const c data) (visit-scalar c data))
    (define/public (visit-var-scalar vs data) (visit-scalar vs data))
  )
)
)
