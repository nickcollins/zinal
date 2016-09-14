#lang racket

; TODO use prefix-out
(provide (all-defined-out))

; TODO write more complete/thorough/detailed comments

(define veme:model:element%% (interface ()

  accept ; (veme:model:element-visitor%)
))

(define veme:model:describable%% (interface (veme:model:element%%)

  ; A short string description of the element, that should be much less than a line long.
  ; Comparable to an identifier, but only used by humans; logic should never use this value.
  get-short-desc ; ()

  ; An indefinitely long string explanation of the element. Comparable to documentation.
  get-long-desc ; ()
))

(define veme:model:node%% (interface (veme:model:element%%)

  ; Returns a veme:model:node%% handle to the parent of this node. Returns #f for the root node.
  get-parent ; ()
))

; Any element which can be pointed to by some type of veme:model:reference%%
(define veme:model:referable%% (interface (veme:model:element%% veme:model:describable%%)))

; TODO current

; Any element which refers to some veme:model:referable%%
(define veme:model:reference%% (interface (veme:model:node%%)

  ; Goto-declaration, effectively.
  ; Returns the unique veme:model:referable%% that this refers to
  get-referable ; ()
))

; Any node which has no children and is self-descriptive; literals, essentially.
; (A list literal is really a list of literals)
(define veme:model:atom%% (interface (veme:model:node%%)

  ; Returns the literal value represented by this node,
  ; as a scheme value of the proper type
  get-val ; ()
))

(define veme:model:lambda%% (interface (veme:model:node%% veme:model:describable%%)

  ; Returns a list of veme:model:param%%
  get-params ; ()

  ; Returns a list whose elements are the statements/expressions constituting the lambda's body.
  get-body ; ()

  ; Inserts a new unassigned node into the lambda body at the specified index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) (- a b)),
  ; then (insert-into-body!! 1) creates an unassigned node after the definition of a .
  ; Returns a veme:model:unassigned%% handle to the new unassigned node
  insert-into-body!! ; (non-negative-integer)

  ; Deletes the nth expr (and all associated data, and all children) of the lambda's body,
  ; and shifts all latter exprs in the body down by one index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) (- a b)),
  ; then (remove-from-body!! 2) deletes the (- a b) expression.
  ; Any handles associated with the deleted expr are now invalid and cannot be used.
  ; No meaningful return value.
  remove-from-body!! ; (non-negative-integer)
))

(define veme:model:number%% (interface (veme:model:atom%%)))
(define veme:model:char%% (interface (veme:model:atom%%)))
(define veme:model:string%% (interface (veme:model:atom%%)))
(define veme:model:bool%% (interface (veme:model:atom%%)))
(define veme:model:symbol%% (interface (veme:model:atom%%)))

(define veme:model:list%% (interface (veme:model:node%% veme:model:describable%%)

  ; Returns a list of veme:model:node%% , constituting the items of this list
  get-items ; ()

  ; Inserts a new unassigned node into the list at the specified index.
  ; Returns a veme:model:unassigned%% handle to the new unassigned node
  insert!! ; (non-negative-integer)

  ; Deletes the node (and all associated data, and all children) at the specified position,
  ; and shifts all latter items in the list down by one index. Any handles associated with
  ; that node are now invalid and cannot be used. No meaningful return value.
  remove!! ; (non-negative-integer)
))

(define veme:model:def%% (interface (veme:model:node%% veme:model:referable%%)

  ; Returns a veme:model:node%% handle for the expression of this define
  get-expr ; ()
))

(define veme:model:def-ref%% (interface (veme:model:reference%%)

  ; Returns a veme:model:def%% handle for the define that this reference refers to
  get-def ; ()
))

(define veme:model:param%% (interface (veme:model:referable%%)

  ; Returns a veme:model:lambda%% handle for the lambda that this param belongs to
  get-lambda ; ()

  ; Returns a non-negative integer indicating the position of this positional parameter
  get-pos ; ()
))

(define veme:model:param-ref%% (interface (veme:model:reference%%)

  ; Returns a veme:model:param%% handle for the param that this reference refers to
  get-param ; ()
))

; Used for invoking (or identifying) scheme functions, macros, constants, etc. E.g.,
; a veme:model:legacy-link%% with library #f and name "string->symbol" represents the
; scheme string->symbol function. If it were the first node in a veme:model:list%%, then
; that list is probably an invokation of string->symbol. This is used for all important
; built-ins, such as 'send, 'quote, '+, and 'list.
(define veme:model:legacy-link%% (interface (veme:model:node%%)

  ; Returns the name (as a string) of the library the specified identifier belongs to.
  ; Returns #f for the default library
  get-library ; ()

  ; Returns a string of the identifier itself
  get-name ; ()
))

; A placeholder node that has not been assigned an actual value yet. Scheme code cannot
; be generated for a db that contains these. The assign!! methods transform this node
; into one of a different type. In doing so, any handles to this node become invalid,
; and may no longer be used. The assign!! methods return a handle to the newly created
; node.
; These nodes are describable so that a little "TODO"
; note can be written, or so the intended purpose can be described before it must
; actually be implemented
(define veme:model:unassigned%%
  (interface (veme:model:node%% veme:model:describable%%)
    assign-lambda!! ; (arity [short-desc] [long-desc])
    assign-def!! ; ([short-desc] [long-desc])
    assign-list!! ; ([short-desc] [long-desc])
    assign-def-ref!! ; (veme:model:def%%)
    assign-param-ref!! ; (veme:model:param%%)
    assign-number!! ; (value)
    assign-char!! ; (value)
    assign-string!! ; (value)
    assign-bool!! ; (value)
    assign-symbol!! ; (value)
    ; Use #f for library to specify the standard library
    assign-legacy-link!! ; (library name)
  )
)

(define veme:model:element-visitor%
  (class object%
    (super-new)

    (define/public (visit-element e data) #f)
    (define/public (visit-node n data) (visit-element n data))
    (define/public (visit-reference r data) (visit-node r data))
    (define/public (visit-atom a data) (visit-node a data))

    (define/public (visit-lambda l data) (visit-node l data))
    (define/public (visit-number n data) (visit-atom n data))
    (define/public (visit-char c data) (visit-atom c data))
    (define/public (visit-string s data) (visit-atom s data))
    (define/public (visit-bool b data) (visit-atom b data))
    (define/public (visit-symbol s data) (visit-atom s data))
    (define/public (visit-list l data) (visit-node l data))
    (define/public (visit-def d data) (visit-node d data))
    (define/public (visit-def-ref dr data) (visit-reference dr data))
    (define/public (visit-param p data) (visit-element p data))
    (define/public (visit-param-ref pr data) (visit-reference pr data))
    (define/public (visit-legacy-link l data) (visit-node l data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
