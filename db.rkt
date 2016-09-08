#lang racket

; TODO use prefix-out
(provide (all-defined-out))

; TODO write more complete/thorough/detailed comments
(define veme:db%% (interface ()

  ; Returns a veme:db-list%% handle for the root node
  get-root ; ()

  ; Returns a list of all veme:db-referable%% in this db
  get-referables ; ()
))

(define veme:db-element%% (interface ()

  ; returns the veme:db%%
  get-db ; ()

  accept ; (veme:db-element-visitor%)

  ; Returns #t if and only if this handle and the other handle both refer to the same element
  equals? ; (veme:db-element%%)
))

(define veme:db-describable%% (interface (veme:db-element%%)

  ; A short string description of the element, that should be much less than a line long.
  ; Comparable to an identifier, but only used by humans; logic should never use this value.
  get-short-desc ; ()

  ; An indefinitely long string explanation of the element. Comparable to documentation.
  get-long-desc ; ()
))

(define veme:db-node%% (interface (veme:db-element%%)

  ; Returns a veme:db-node%% handle to the parent of this node. Returns #f for the root node.
  get-parent ; ()

  ; Deletes all the data associated with this node and any children it may have, converting
  ; this node to a veme:db-unassigned%% . This handle will be invalidated and cannot be used
  ; again. Returns a handle to the new veme:db-unassigned%% .
  unassign!! ; ()
))

; Any element which can be pointed to by some type of veme:db-reference%%
(define veme:db-referable%% (interface (veme:db-element%% veme:db-describable%%)

  ; Find-usages, effectively.
  ; Returns a list of all veme:db-reference%% that refer to this referable
  get-references ; ()
))

; Any element which refers to some veme:db-referable%%
(define veme:db-reference%% (interface (veme:db-node%%)

  ; Goto-declaration, effectively.
  ; Returns the unique veme:db-referable%% that this refers to
  get-referable ; ()
))

; Any node which has no children and is self-descriptive; literals, essentially.
; (A list literal is really a list of literals)
(define veme:db-atom%% (interface (veme:db-node%%)

  ; Returns the literal value represented by this node,
  ; as a scheme value of the proper type
  get-val ; ()
))

(define veme:db-lambda%% (interface (veme:db-node%% veme:db-describable%%)

  ; Returns a list of veme:db-param%%
  get-params ; ()

  ; Returns a veme:db-list%%, whose children are the statements/expressions constituting
  ; the lambda's body.
  ; TODO what if they do something like call 'unassign!!' ? Maybe we need to hide the
  ; the veme:db-list%% and provide access through the lambda interface
  ; TODO we should also try to snoop out this sort of issue elsewhere
  get-body-list ; ()
))

(define veme:db-number%% (interface (veme:db-atom%%)))
(define veme:db-char%% (interface (veme:db-atom%%)))
(define veme:db-string%% (interface (veme:db-atom%%)))
(define veme:db-bool%% (interface (veme:db-atom%%)))

(define veme:db-list%% (interface (veme:db-node%% veme:db-describable%%)

  ; Returns a list of veme:db-node%% , constituting the items of this list
  get-items ; ()

  ; Inserts a new unassigned node into the list at the specified index.
  ; Returns a veme:db-unassigned%% handle to the new unassigned node
  insert!! ; (non-negative-integer)

  ; Deletes the node (and all associated data, and all children) at the specified position,
  ; and shifts all latter items in the list down by one index. Any handles associated with
  ; that node are now invalid and cannot be used. No meaningful return value.
  remove!! ; (non-negative-integer)
))

(define veme:db-def%% (interface (veme:db-node%% veme:db-referable%%)

  ; Returns a veme:db-node%% handle for the expression of this define
  get-expr ; ()
))

(define veme:db-def-ref%% (interface (veme:db-reference%%)

  ; Returns a veme:db-def%% handle for the define that this reference refers to
  get-def ; ()
))

(define veme:db-param%% (interface (veme:db-referable%%)

  ; Returns a veme:db-lambda%% handle for the lambda that this param belongs to
  get-lambda ; ()

  ; Returns a non-negative integer indicating the position of this positional parameter
  get-pos ; ()
))

(define veme:db-param-ref%% (interface (veme:db-reference%%)

  ; Returns a veme:db-param%% handle for the param that this reference refers to
  get-param ; ()
))

; Used for invoking (or identifying) scheme functions, macros, constants, etc. E.g.,
; a veme:db-legacy-link%% with library #f and name "string->symbol" represents the
; scheme string->symbol function. If it were the first node in a veme:db-list%%, then
; that list is probably an invokation of string->symbol. This is used for all important
; built-ins, such as 'send, 'quote, '+, and 'list.
(define veme:db-legacy-link%% (interface (veme:db-node%%)

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
(define veme:db-unassigned%%
  (interface (veme:db-node%% veme:db-describable%%)
    assign-lambda!! ; (arity [short-desc] [long-desc])
    assign-def!! ; ([short-desc] [long-desc])
    assign-list!! ; ([short-desc] [long-desc])
    assign-def-ref!! ; (veme:db-def%%)
    assign-param-ref!! ; (veme:db-param%%)
    assign-number!! ; (value)
    assign-char!! ; (value)
    assign-string!! ; (value)
    assign-bool!! ; (value)
    ; Use #f for library to specify the standard library
    assign-legacy-link!! ; (library name)
  )
)

(define veme:db-element-visitor%
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
    (define/public (visit-list l data) (visit-node l data))
    (define/public (visit-def d data) (visit-node d data))
    (define/public (visit-def-ref dr data) (visit-reference dr data))
    (define/public (visit-param p data) (visit-element p data))
    (define/public (visit-param-ref pr data) (visit-reference pr data))
    (define/public (visit-legacy-link l data) (visit-node l data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
