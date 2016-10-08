#lang racket

(provide (all-defined-out))

(define zinal:ui:item%% (interface ()

  selected? ; ()

  highlighted? ; ()

  ; returns #f if there is no parent
  get-parent ; ()
))

(define zinal:ui:scalar%% (interface (zinal:ui:item%%)

  get-style ; ()

  get-text ; ()
))

(define zinal:ui:const%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:const%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:var-scalar%% (interface (zinal:ui:scalar%%)))

(define zinal:ui:vector%% (interface (zinal:ui:item%%)

  get-children ; ()
))

(define zinal:ui:hlist%% (interface (zinal:ui:vector%%)

  ; Returns #f if there is no separator
  get-separator ; ()
))

(define zinal:ui:vlist%% (interface (zinal:ui:vector%%)

  get-header ; ()
))
