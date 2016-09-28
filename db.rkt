#lang racket

; TODO use prefix-out
(provide (all-defined-out))

; TODO write more complete/thorough/detailed comments
(define veme:db%% (interface ()

  ; Returns a veme:db:list%% handle for the root node
  get-root ; ()

  ; Returns a list of all veme:db:referable%% in this db
  get-referables ; ()
))

(define veme:db:element%% (interface ()

  ; returns the veme:db%%
  get-db ; ()

  accept ; (veme:db:element-visitor% [data])

  ; Returns #t if and only if this handle and the other handle both refer to the same element
  equals? ; (veme:db:element%%)
))

(define veme:db:describable%% (interface (veme:db:element%%)

  ; A short string description of the element, that should be much less than a line long.
  ; Comparable to an identifier, but only used by humans; logic should never use this value.
  ; Returns #f if none is specified
  get-short-desc ; ()

  ; An indefinitely long string explanation of the element. Comparable to documentation.
  ; Returns #f if none is specified
  get-long-desc ; ()

  ; Send #f to specify that there is no short descriptor
  set-short-desc!! ; (string OR #f)

  ; Send #f to specify that there is no long descriptor
  set-long-desc!! ; (string OR #f)
))

(define veme:db:node%% (interface (veme:db:element%%)

  ; Returns a veme:db:node%% handle to the parent of this node. Returns #f for the root node.
  get-parent ; ()

  ; Returns a list of all veme:db:referable%% that are visible underneath this node.
  ; Included in the list is this node (if it's a referable) and its params (if it's a lambda).
  get-visible-referables-underneath ; ()

  ; Returns a list of all veme:db:referable%% that are visible after this node.
  ; Included in the list is this node (if it's a referable) but not its params (if it's a lambda).
  get-visible-referables-after ; ()

  ; Returns true iff unassign!! can be called without throwing an exception. Returns #f if this
  ; node is the root node, of if deleting this node as well as all associated data and children
  ; would "orphan" any extant references. Returns #t for unassigned nodes, as for them unassign!!
  ; is a no-op.
  can-unassign? ; ()

  ; Deletes all the data associated with this node and any children it may have, converting
  ; this node to a veme:db:unassigned%% . This handle will be invalidated and cannot be used
  ; again. Returns a handle to the new veme:db:unassigned%% .
  ; If this is called on a veme:db:unassigned%%, nothing happens, and this is returned.
  ; If this is called when can-unassign? would return #f, then an exception is thrown.
  unassign!! ; ()
))

(define veme:db:parent-node%% (interface (veme:db:node%%)

  ; Returns a list of veme:db:node%% handles to all children of this non-leaf node. The children
  ; are returned in "lexical" order, so if child B depends on child A, A must appear before B in
  ; the returned list
  get-children ; ()
))

; Any element which can be pointed to by some type of veme:db:reference%%
(define veme:db:referable%% (interface (veme:db:element%% veme:db:describable%%)

  ; Find-usages, effectively.
  ; Returns a list of all veme:db:reference%% that refer to this referable
  get-references ; ()
))

; Any element which refers to some veme:db:referable%%
(define veme:db:reference%% (interface (veme:db:node%%)

  ; Goto-declaration, effectively.
  ; Returns the unique veme:db:referable%% that this refers to
  get-referable ; ()
))

; Any node which has no children and is self-descriptive; literals, essentially.
; (A list literal is really a list of literals)
(define veme:db:atom%% (interface (veme:db:node%%)

  ; Returns the literal value represented by this node,
  ; as a scheme value of the proper type
  get-val ; ()
))

; TODO current update documentation
(define veme:db:lambda%% (interface (veme:db:parent-node%% veme:db:describable%%)

  get-all-params ; ()

  get-required-params ; ()

  can-remove-required-param? ; (index)

  remove-required-param!! ; (index)

  insert-required-param!! ; (index [short-desc])

  get-optional-params ; ()

  can-remove-optional-param? ; (index)

  remove-optional-param!! ; (index)

  insert-optional-param!! ; (index [short-desc])

  ; Returns a list of veme:db:node%% handles representing the statements/expressions
  ; constituting the lambda's body.
  get-body ; ()

  ; Inserts a new unassigned node into the lambda body at the specified index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) (- a b)),
  ; then (insert-into-body!! 1) creates an unassigned node after the definition of a .
  ; Returns a veme:db:unassigned%% handle to the new unassigned node
  insert-into-body!! ; (non-negative-integer)

  ; Deletes the nth expr (which must be a veme:db:unassigned%%) of the lambda's body,
  ; and shifts all latter exprs in the body down by one index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) <?>),
  ; then (remove-from-body!! 2) deletes the <?>.
  ; Any handles associated with the deleted expr are now invalid and cannot be used.
  ; No meaningful return value. If the node at the specified index isn't a
  ; veme:db:unassigned%% , an exception is thrown.
  remove-from-body!! ; (non-negative-integer)
))

(define veme:db:number%% (interface (veme:db:atom%%)))
(define veme:db:char%% (interface (veme:db:atom%%)))
(define veme:db:string%% (interface (veme:db:atom%%)))
(define veme:db:bool%% (interface (veme:db:atom%%)))
(define veme:db:symbol%% (interface (veme:db:atom%%)))
(define veme:db:keyword%% (interface (veme:db:atom%%)))

(define veme:db:list%% (interface (veme:db:parent-node%% veme:db:describable%%)

  ; Returns a list of veme:db:node%% , constituting the items of this list
  get-items ; ()

  ; Inserts a new unassigned node into the list at the specified index.
  ; Returns a veme:db:unassigned%% handle to the new unassigned node
  insert!! ; (non-negative-integer)

  ; Deletes a veme:db:unassigned%% at the specified position, and shifts all latter
  ; items in the list down by one index. Any handles associated with that node are
  ; now invalid and cannot be used. No meaningful return value. If the node at the
  ; specified index is not veme:db:unassigned%% , an exception will be thrown
  remove!! ; (non-negative-integer)
))

(define veme:db:def%% (interface (veme:db:parent-node%% veme:db:referable%%)

  ; Returns a veme:db:node%% handle for the expression of this define
  get-expr ; ()
))

(define veme:db:def-ref%% (interface (veme:db:reference%%)

  ; Returns a veme:db:def%% handle for the define that this reference refers to
  get-def ; ()
))

(define veme:db:param%% (interface (veme:db:parent-node%% veme:db:referable%%)

  ; Returns a veme:db:lambda%% handle for the lambda that this param belongs to.
  ; Equivalent to get-parent
  get-lambda ; ()

  ; Returns a veme:db:node%% handle for the default expression. Returns #f for required params.
  get-default ; ()
))

(define veme:db:param-ref%% (interface (veme:db:reference%%)

  ; Returns a veme:db:param%% handle for the param that this reference refers to
  get-param ; ()
))

; Used for invoking (or identifying) scheme functions, macros, constants, etc. E.g.,
; a veme:db:legacy-link%% with library #f and name "string->symbol" represents the
; scheme string->symbol function. If it were the first node in a veme:db:list%%, then
; that list is probably an invokation of string->symbol. This is used for all important
; built-ins, such as 'send, 'quote, '+, and 'list.
(define veme:db:legacy-link%% (interface (veme:db:node%%)

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
(define veme:db:unassigned%%
  (interface (veme:db:node%% veme:db:describable%%)
    assign-lambda!! ; ([short-desc] [long-desc])
    assign-def!! ; ([short-desc] [long-desc])
    assign-list!! ; ([short-desc] [long-desc])
    assign-def-ref!! ; (veme:db:def%%)
    assign-param-ref!! ; (veme:db:param%%)
    assign-number!! ; (value)
    assign-char!! ; (value)
    assign-string!! ; (value)
    assign-bool!! ; (value)
    assign-symbol!! ; (value)
    assign-keyword!! ; (value)
    ; Use #f for library to specify the standard library
    assign-legacy-link!! ; (library name)
  )
)

(define veme:db:element-visitor%
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
    (define/public (visit-keyword k data) (visit-atom k data))
    (define/public (visit-list l data) (visit-node l data))
    (define/public (visit-def d data) (visit-node d data))
    (define/public (visit-def-ref dr data) (visit-reference dr data))
    (define/public (visit-param p data) (visit-node p data))
    (define/public (visit-param-ref pr data) (visit-reference pr data))
    (define/public (visit-legacy-link l data) (visit-node l data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
