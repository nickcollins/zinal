#lang racket

; TODO use prefix-out
(provide (all-defined-out))

(define veme:db%% (interface () get-root-list get-referables))

(define veme:db-element%% (interface () get-db accept equals?))
(define veme:db-describable%% (interface (veme:db-element%%) get-short-desc get-long-desc))
; get-parent will return #f for the root node
(define veme:db-node%% (interface (veme:db-element%%) get-parent unassign!!))
(define veme:db-referable%% (interface (veme:db-element%% veme:db-describable%%) get-references))
(define veme:db-reference%% (interface (veme:db-node%%) get-referable))

(define veme:db-lambda%% (interface (veme:db-node%% veme:db-describable%%) get-params get-body-list))
; TODO let's split atom into all the distinct types
(define veme:db-atom%% (interface (veme:db-node%%) get-type get-val))
(define veme:db-list%% (interface (veme:db-node%% veme:db-describable%%) get-items insert!! remove!!))
(define veme:db-def%% (interface (veme:db-node%% veme:db-referable%%) get-expr))
(define veme:db-def-ref%% (interface (veme:db-reference%%) get-def))
(define veme:db-param%% (interface (veme:db-referable%%) get-lambda get-pos))
(define veme:db-param-ref%% (interface (veme:db-reference%%) get-param))
(define veme:db-legacy-link%% (interface (veme:db-node%%) get-library get-name))

(define veme:db-unassigned%%
  (interface (veme:db-node%% veme:db-describable%%)
    assign-lambda!!
    assign-def!!
    assign-list!!
    assign-def-ref!!
    assign-param-ref!!
    assign-atom!!
    assign-legacy-link!!
  )
)

(define veme:db-element-visitor%
  (class object%
    (super-new)

    (define/public (visit-element e data) #f)
    (define/public (visit-node n data) (visit-element n data))
    (define/public (visit-reference r data) (visit-node r data))

    (define/public (visit-lambda l data) (visit-node l data))
    (define/public (visit-atom a data) (visit-node a data))
    (define/public (visit-list l data) (visit-node l data))
    (define/public (visit-def d data) (visit-node d data))
    (define/public (visit-def-ref dr data) (visit-reference dr data))
    (define/public (visit-param p data) (visit-element p data))
    (define/public (visit-param-ref pr data) (visit-reference pr data))
    (define/public (visit-legacy-link l data) (visit-node l data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
