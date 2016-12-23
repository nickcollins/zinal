; We'd rather use #lang racket, but sadly the read function has a hard time with that, so we use
; the uglier module syntax instead
(module db racket

(provide (all-defined-out))

; TODO write more complete/thorough/detailed comments
;
; VISIBILITY RULES:
;
; All referables are visible to any reference except:
;   1) If the referable is in the same module but is out of scope. A referable that's obviously in
;      scope, is, well, in scope. I.e., the params of any parent/ancestor lambda are visible, any
;      parent/ancestor is visible, and all "older siblings" of the reference itself or any
;      parent/ancestor are visible.
;
;      Somewhat less intuitively, some "younger siblings" of the reference's parent/ancestors
;      are visible. If a parent/ancestor A is a function definition (i.e., a define whose
;      expr is a lambda), then an unbroken sequence of function-definitions immediately
;      following A are also visible to R. That is, if A is the Nth sibling, then if siblings
;      N+1, N+2, ... N+M are all function definitions, then they are all visible to R. But if
;      sibling N+M is not a function definition, then siblings younger than N+M are not visible
;      even if they are function definitions.
;
;      Somewhat less unintuitively, here's an example:
;      (define vis 0)
;      (1 2 R1)
;      (do-something
;        (lambda (p1)
;          (define thing R2)
;          (define f1 (lambda () R3))
;          (define f2 (lambda () R4))
;          (define stuff 0)
;          (define f3 (lambda () R5))
;        )
;      )
;      (def invis (lambda () 0))
;
;      vis is, of course, visible to all the references, and invis is invisible to all of them.
;      p1 and thing are visible only to R2-5 (yes, thing is visible to R2, sorry). f1 and f2
;      are visible only to R3-4. It's obvious that f1 is visible to both, but it's only due to
;      the "younger sibling" rule that f2 is visible to R3. As you can see, this rule allows
;      mutually recursing functions without allowing any other weird stuff. f3 is only visible
;      to R5. If the lines defining stuff and f3 were swapped, then f3 would be visible to
;      R3-4. Unlike in scheme, the stuff definition breaks the sequence of visible defs because
;      its evaluation could hypothetically invoke the function that depends on it, creating
;      a dependency cycle that does not necessarily involve intentional recursion.
;      If "do-something" was instead replaced by "define blah", then invis would be visible to
;      R2-5.
;   2) If the referable is in a different module and is not public.
;   3) If the referable is in a different module, and that module is not "require"d by this
;      module
;
; When a reference is first created, the referable it points to must be visible to it, or an
; exception is thrown. After that, it is possible that during the course of editing, a referable
; becomes invisible to some of its references. This is legal, but there is an invariant that is
; maintained:
; 1) if a reference points to a referable in a different module, then that referable is a
;    zinal:db:def%% and the direct child of its module.
; 2) if a reference points to a referable in the same module, then the reference and its referable
;    are not "cousins". Two nodes are cousins if neither descends from the other, and neither is
;    the direct child of their youngest common ancestor.
; Given that each reference has visibility at creation, all operations that currently exist
; preserve this invariant. This invariant makes it easy to delete nodes. A node can't be deleted
; if any references to it are not descendants of it - the invariant proves that if that is the
; case, then all references to the node's descendants are also descendants of the node, so the
; whole subtree can be deleted without any further checks. The invariant also means that to delete
; a module, we must check that there are no references in other modules to its direct child defs,
; but we don't have to check whether there are references to params or descendant defs. It is legal
; to mess up a def's publicity, or a module's requires, such that references lose vision, because
; none of these changes can violate the invariant.
(define zinal:db%% (interface ()

  ; Returns zinal:db:module%% handles for all modules
  get-all-modules ; ()

  ; Returns a zinal:db:module%% for the main module, for #f if there's no main module
  get-main-module ; ()

  ; short-desc may be #f to indicate no short descriptor. Returns a zinal:db:module%% handle
  ; for the newly minted module
  create-module!! ; ([short-desc])

  ; Returns a list of all zinal:db:referable%% in this db
  get-all-referables ; ()
))

(define zinal:db:element%% (interface ()

  ; returns the zinal:db%%
  get-db ; ()

  accept ; (zinal:db:element-visitor% [data])

  ; Returns #t if and only if this handle and the other handle both refer to the same element
  equals? ; (zinal:db:element%%)
))

(define zinal:db:describable%% (interface (zinal:db:element%%)

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

(define zinal:db:node%% (interface (zinal:db:element%%)

  ; Returns a zinal:db:node%% handle to the parent of this node. Returns #f for a module
  get-parent ; ()

  ; returns the containing zinal:db:module%%
  get-module ; ()

  ; Returns a list of all zinal:db:referable%% that are visible underneath this node.
  ; Included in the list is this node (if it's a referable) and its params (if it's a lambda).
  ; See the comment about visibility rules above to see what is and isn't visible.
  get-visible-referables-underneath ; ()

  ; Returns a list of all zinal:db:referable%% that are visible after this node.
  ; Included in the list is this node (if it's a referable) but not its params (if it's a lambda).
  ; See the comment about visibility rules above to see what is and isn't visible.
  get-visible-referables-after ; ()

  ; Returns true iff unassign!! can be called without throwing an exception. Returns #f if this
  ; node is a module, or if deleting this node as well as all associated data and children would
  ; "orphan" any extant references. Returns #t for unassigned nodes, as for them unassign!! is a
  ; no-op.
  can-unassign? ; ()

  ; Deletes all the data associated with this node and any children it may have, converting
  ; this node to a zinal:db:unassigned%% . This handle will be invalidated and cannot be used
  ; again. Returns a handle to the new zinal:db:unassigned%% .
  ; If this is called on a zinal:db:unassigned%%, nothing happens, and this is returned.
  ; If this is called when can-unassign? would return #f, then an exception is thrown.
  unassign!! ; ()
))

(define zinal:db:parent-node%% (interface (zinal:db:node%%)

  ; Returns a list of zinal:db:node%% handles to all children of this non-leaf node. The children
  ; are returned in "lexical" order, so if child B depends on child A, A must appear before B in
  ; the returned list
  get-children ; ()
))

; Any element which can be pointed to by some type of zinal:db:reference%%
(define zinal:db:referable%% (interface (zinal:db:element%% zinal:db:describable%%)

  ; Find-usages, effectively.
  ; Returns a list of all zinal:db:reference%% that refer to this referable
  get-references ; ()
))

; Any element which refers to some zinal:db:referable%%
(define zinal:db:reference%% (interface (zinal:db:node%%)

  ; Goto-declaration, effectively.
  ; Returns the unique zinal:db:referable%% that this refers to
  get-referable ; ()

  ; It is legal in the database for a reference's referable to not be visible to it. The code,
  ; of course, cannot be compiled, but it is useful to allow broken references so that small,
  ; incremental changes to the code (such as reordering things, or changing an access modifier)
  ; do not become illegal. This method returns #t if the reference is valid, and #f if it is not
  ; valid due to lack of visibility.
  ; See the comment about visibility rules above to see what is and isn't visible.
  is-referable-visible? ; ()
))

; Any node which has no children and is self-descriptive; literals, essentially.
; (A list literal is really a list of literals)
(define zinal:db:atom%% (interface (zinal:db:node%%)

  ; Returns the literal value represented by this node,
  ; as a scheme value of the proper type
  get-val ; ()
))

(define zinal:db:lambda%% (interface (zinal:db:parent-node%% zinal:db:describable%%)

  ; Returns all params, with the required first
  get-all-params ; ()

  get-required-params ; ()

  ; Returns #f if the specified param cannot be deleted. A required param can't be deleted
  ; if there are any references to it
  can-remove-required-param? ; (index)

  ; Deletes the specified param, or throws an exception if can-remove-required-param? would
  ; return #f
  remove-required-param!! ; (index)

  ; short-desc is #f by default. If #f, the param will not have a short descriptor
  insert-required-param!! ; (index [short-desc])

  ; Converts the last required param to an optional param, with an unassigned default. If
  ; there are no required params, throws an exception. Does not affect or invalidate any
  ; handles for the converted param. No meaningful return value.
  make-last-required-param-optional!! ; ()

  get-optional-params ; ()

  ; Returns #f if the specified param cannot be deleted. An optional param can't be deleted
  ; if there are any references to it that occur outside its own subtree
  can-remove-optional-param? ; (index)

  ; Deletes the specified param, or throws an exception if can-remove-optional-param? would
  ; return #f
  remove-optional-param!! ; (index)

  ; short-desc is #f by default. If #f, the param will not have a short descriptor.
  ; The newly created param's default value is unassigned
  insert-optional-param!! ; (index [short-desc])

  ; Converts the last optional param to a required param, completely deleting the default value.
  ; If there are no optional params, throws an exception. Does not affect or invalidate any
  ; handles for the converted param. No meaningful return value.
  make-last-optional-param-required!! ; ()

  ; Returns a list of zinal:db:node%% handles representing the statements/expressions
  ; constituting the lambda's body.
  get-body ; ()

  ; Inserts a new unassigned node into the lambda body at the specified index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) (- a b)),
  ; then (insert-into-body!! 1) creates an unassigned node after the definition of a .
  ; Returns a zinal:db:unassigned%% handle to the new unassigned node
  insert-into-body!! ; (non-negative-integer)

  ; Deletes the nth expr (which must be a zinal:db:unassigned%%) of the lambda's body,
  ; and shifts all latter exprs in the body down by one index.
  ; E.g. if the lambda is (lambda (x y) (define a (+ x y)) (define b (* x y)) <?>),
  ; then (remove-from-body!! 2) deletes the <?>.
  ; Any handles associated with the deleted expr are now invalid and cannot be used.
  ; No meaningful return value. If the node at the specified index isn't a
  ; zinal:db:unassigned%% , an exception is thrown.
  remove-from-body!! ; (non-negative-integer)
))

(define zinal:db:number%% (interface (zinal:db:atom%%)))
(define zinal:db:char%% (interface (zinal:db:atom%%)))
(define zinal:db:string%% (interface (zinal:db:atom%%)))
(define zinal:db:bool%% (interface (zinal:db:atom%%)))
(define zinal:db:symbol%% (interface (zinal:db:atom%%)))
(define zinal:db:keyword%% (interface (zinal:db:atom%%)))

(define zinal:db:list%% (interface (zinal:db:parent-node%% zinal:db:describable%%)

  ; Returns a list of zinal:db:node%% , constituting the items of this list
  get-items ; ()

  ; Inserts a new unassigned node into the list at the specified index.
  ; Returns a zinal:db:unassigned%% handle to the new unassigned node
  insert!! ; (non-negative-integer)

  ; Deletes a zinal:db:unassigned%% at the specified position, and shifts all latter
  ; items in the list down by one index. Any handles associated with that node are
  ; now invalid and cannot be used. No meaningful return value. If the node at the
  ; specified index is not zinal:db:unassigned%% , an exception will be thrown
  remove!! ; (non-negative-integer)
))

; A module is comparable in scope to a scheme file. It is a list of nodes,
; which are evaluated in order by any program which requires it. A module
; can require other modules in order to refer to their public defs.
; No module is allowed to require the main module, and the graph of requires must form
; a DAG.
; There can be any number of modules, but only one main module. If the program is run,
; it is the main module that is executed; if there is no main module, then the program
; cannot be run as an executable. Generally, referables are only visible within their
; own module, but each get-public-defs exposes a set of direct children that can
; be referred to by any module that requires this one.
(define zinal:db:module%% (interface (zinal:db:list%%)

  ; Returns a list of all direct child zinal:db:def%% that can be referenced in other
  ; modules, in no particular order. Each public child must be a direct child, not an
  ; indirect descendant.
  get-public-defs ; ()

  ; The first argument specifies which direct child to change the publicity of. If the
  ; index is out of bounds, or the node is not a direct child of this module, an
  ; exception is thrown. new-value is #t if the child should be public, #f otherwise.
  ; Note that it is legal to make a public def non-public, even if doing so makes its
  ; invisible to some of its references. See the comment about visibility rules.
  ; No meaningful return value.
  set-public!! ; (index OR zinal:db:def%% , new-value)

  is-main-module? ; ()

  ; Returns #t if this module can be made the main module via (set-main-module #t),
  ; #f otherwise. This module can be made the main module as long as no other module
  ; is already the main module, and as long as no other module requires this module.
  can-be-main-module? ; ()

  ; If new-value is #f, makes this not the main module, otherwise it becomes the main
  ; module. If new-value is true, but can-be-main-module? would return #f , an
  ; exception is thrown. No meaningful return value
  set-main-module!! ; (new-value)

  ; Gets a list of all zinal:db:module%% that this module requires, in no particular
  ; order
  get-required-modules ; ()

  ; Gets a list of all zinal:db:module%% that require this module, in no particular
  ; order
  get-requiring-modules ; ()

  ; Returns #t if this module is allowed to require the argument, #f otherwise. The
  ; argument can be required as long as it's not the main module, and as long as doing
  ; so would not create a cycle in the require graph.
  can-require? ; (zinal:db:module%%)

  ; If can-require? would return #t, adds the argument to this module's require list.
  ; Otherwise, throws an exception.
  ; No meaningful return value
  require!! ; (zinal:db:module%%)

  ; If the argument is currently require'd by this module, removes the argument from
  ; this module's requires. If it's not already required, this method does nothing.
  ; unrequire!! can cause references to lose vision, which is perfectly legal; see
  ; the comment about visibility rules.
  ; No meaningful return value.
  unrequire!! ; (zinal:db:module%%)

  ; Returns #t if this module can be deleted via delete!! , #f otherwise. The module
  ; can be deleted so long as no other module contains a reference to one of its
  ; children and no other module requires it.
  can-delete? ; ()

  ; If can-delete? would return #t, deletes this module. Otherwise, throws an exception.
  ; No meaningful return value.
  delete!! ; ()
))

(define zinal:db:def%% (interface (zinal:db:parent-node%% zinal:db:referable%%)

  ; Returns a zinal:db:node%% handle for the expression of this define
  get-expr ; ()
))

(define zinal:db:def-ref%% (interface (zinal:db:reference%%)

  ; Returns a zinal:db:def%% handle for the define that this reference refers to
  get-def ; ()
))

(define zinal:db:param%% (interface (zinal:db:parent-node%% zinal:db:referable%%)

  ; Returns a zinal:db:lambda%% handle for the lambda that this param belongs to.
  ; Equivalent to get-parent
  get-lambda ; ()

  ; Returns a zinal:db:node%% handle for the default expression. Returns #f for required params.
  get-default ; ()
))

(define zinal:db:param-ref%% (interface (zinal:db:reference%%)

  ; Returns a zinal:db:param%% handle for the param that this reference refers to
  get-param ; ()
))

; Used for invoking (or identifying) scheme functions, macros, constants, etc. E.g.,
; a zinal:db:legacy-link%% with library #f and name "string->symbol" represents the
; scheme string->symbol function. If it were the first node in a zinal:db:list%%, then
; that list is probably an invokation of string->symbol. This is used for all important
; built-ins, such as 'send, 'quote, '+, and 'list.
(define zinal:db:legacy-link%% (interface (zinal:db:node%%)

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
(define zinal:db:unassigned%%
  (interface (zinal:db:node%% zinal:db:describable%%)
    assign-lambda!! ; ([short-desc] [long-desc])
    assign-def!! ; ([short-desc] [long-desc])
    assign-list!! ; ([short-desc] [long-desc])
    ; If a reference is assigned to a referable that is not visible, an exception is
    ; thrown. See the section about reference visibility.
    assign-def-ref!! ; (zinal:db:def%%)
    assign-param-ref!! ; (zinal:db:param%%)
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

(define zinal:db:element-visitor%
  (class object%
    (super-new)

    (define/public (visit-element e data) #f)
    (define/public (visit-node n data) (visit-element n data))
    (define/public (visit-reference r data) (visit-node r data))
    (define/public (visit-atom a data) (visit-node a data))

    (define/public (visit-list l data) (visit-node l data))
    (define/public (visit-module m data) (visit-list m data))

    (define/public (visit-lambda l data) (visit-node l data))
    (define/public (visit-number n data) (visit-atom n data))
    (define/public (visit-char c data) (visit-atom c data))
    (define/public (visit-string s data) (visit-atom s data))
    (define/public (visit-bool b data) (visit-atom b data))
    (define/public (visit-symbol s data) (visit-atom s data))
    (define/public (visit-keyword k data) (visit-atom k data))
    (define/public (visit-def d data) (visit-node d data))
    (define/public (visit-def-ref dr data) (visit-reference dr data))
    (define/public (visit-param p data) (visit-node p data))
    (define/public (visit-param-ref pr data) (visit-reference pr data))
    (define/public (visit-legacy-link l data) (visit-node l data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
)
