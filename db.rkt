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
;      are visible. A node is "elder visible" if it's a function definition (i.e., a define whose
;      expr is a lambda), a zinal:db:define-method%% , zinal:db:override-legacy-method%% , or
;      zinal:db:define-class%% . Let's say R is a descendant of A. If A is elder visible, then an
;      unbroken sequence of elder visible nodes immediately following A are also visible to R.
;      That is, if A is the Nth sibling, then if siblings N+1, N+2, ... N+M are all elder visible,
;      then they are all visible to R. But if sibling N+M is not elder visible, then siblings N+M+X
;      are not visible to R or A, even if they are elder visible.
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
;      R2-5. Note that the function definitions in the example could be method definitions,
;      method overrides, or class definitions, without changing the visibility of anything.
;   2) If the referable is in a different module and is not public.
;   3) If the referable is in a different module, and that module is not "require"d by this
;      module
;
; When a reference is first created, the referable it points to must be visible to it, or an
; exception is thrown. After that, it is possible that during the course of editing, a referable
; becomes invisible to some of its references. This is legal, but there is an invariant that is
; maintained:
; 1) if a reference points to a referable in a different module, then that referable is a
;    zinal:db:def%% or zinal:db:define-class%% and the direct child of its module.
; 2) if a reference points to a referable in the same module, then the reference and its referable
;    are not "cousins". Two nodes are cousins if neither descends from the other, and neither is
;    the direct child of their youngest common ancestor.
; Given that each reference has visibility at creation, all operations that currently exist (should)
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

  ; Returns a list of all zinal:db:referable%% in this db, in no particular order
  get-all-referables ; ()

  ; Returns a list of all zinal:db:interface%% in this db, in no particular order. Currently,
  ; interfaces are completely public and visible to all modules, being associated with no particular module.
  get-all-interfaces ; ()

  ; Returns a zinal:db:interface%% handle for the newly minted interface
  create-interface!! ; ([short-desc] [long-desc])
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

  ; Returns #t iff unassign!! can be called without throwing an exception. Returns #f if this
  ; node is a module, or if deleting this node as well as all associated data and children would
  ; "orphan" any extant references. Returns #t for unassigned nodes, as for them unassign!! is a
  ; no-op.
  can-unassign? ; ()

  ; Deletes all the data associated with this node and any children it may have, converting
  ; this node to a zinal:db:unassigned%% . This handle will be invalidated and cannot be used
  ; again. Returns a handle to the new zinal:db:unassigned%% .
  ; If this is called on a zinal:db:unassigned%% , nothing happens, and this is returned.
  ; If this is called when can-unassign? would return #f, then an exception is thrown.
  unassign!! ; ()
))

(define zinal:db:parent-node%% (interface (zinal:db:node%%)

  ; Returns a list of zinal:db:node%% handles to all children of this non-leaf node. The children
  ; are returned in "lexical" order, so if child B depends on child A, A must appear before B in
  ; the returned list
  get-children ; ()
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

; Any element which can be pointed to by some type of zinal:db:reference%%
(define zinal:db:referable%% (interface (zinal:db:element%% zinal:db:describable%%)

  ; Find-usages, effectively.
  ; Returns a list of all zinal:db:reference%% that refer to this referable
  get-references ; ()
))

; A node which has a list of parameters.
(define zinal:db:has-params%% (interface (zinal:db:parent-node%%)

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
))

; A node which has a list of nodes that constitute its body.
(define zinal:db:has-body%% (interface (zinal:db:parent-node%%)

  ; Returns a list of zinal:db:node%% handles representing the statements/expressions
  ; constituting this node's body.
  get-body ; ()

  ; Inserts a new unassigned node into the body at the specified index.
  ; E.g. if the node is (lambda (x y) (define a (+ x y)) (define b (* x y)) (- a b)),
  ; then (insert-into-body!! 1) creates an unassigned node after the definition of a .
  ; Returns a zinal:db:unassigned%% handle to the new unassigned node
  insert-into-body!! ; (non-negative-integer)

  ; Deletes the nth expr (which must be a zinal:db:unassigned%%) of the body,
  ; and shifts all latter exprs in the body down by one index.
  ; E.g. if the node is (lambda (x y) (define a (+ x y)) (define b (* x y)) <?>),
  ; then (remove-from-body!! 2) deletes the <?>.
  ; Any handles associated with the deleted expr are now invalid and cannot be used.
  ; No meaningful return value. If the node at the specified index isn't a
  ; zinal:db:unassigned%% , an exception is thrown.
  remove-from-body!! ; (non-negative-integer)
))

; A node which has a list of nodes which represent arguments to some function or class.
(define zinal:db:has-args%% (interface (zinal:db:parent-node%%)

  ; Returns a list of zinal:db:node%% handles representing the argument expressions.
  get-args ; ()

  ; Inserts a new unassigned node into the argument list at the specified index.
  ; Returns a zinal:db:unassigned%% handle to the new unassigned node
  insert-arg!! ; (non-negative-integer)

  ; Deletes a zinal:db:unassigned%% at the argument specified position, and shifts all latter
  ; items in the list down by one index. Any handles associated with that node are
  ; now invalid and cannot be used. No meaningful return value. If the node at the
  ; specified index is not zinal:db:unassigned%% , an exception will be thrown
  remove-arg!! ; (non-negative-integer)
))

; OOP

; Common interface for any zinal element that extends/implements interfaces, i.e.
; zinal:db:interface%% , zinal:db:define-class%% , zinal:db:class-instance%% . By "direct", we mean an
; immediate super interface, as opposed to grandparent interfaces.
(define zinal:db:subtype%% (interface (zinal:db:element%%)

  ; Returns a list of zinal:db:interface%% , in no particular order, for the interfaces that this
  ; subtype explicitly extends.
  get-direct-super-interfaces ; ()

  ; Returns #t if directly extending the argument would not cause a cycle in the type DAG.
  ; Otherwise #f
  can-add-direct-super-interface? ; (zinal:db:interface%%)

  ; Makes this type directly extend the argument. Idempotent.
  ; Throws an exception if can-add-direct-super-interface? would return #f
  ; No meaningful return value
  add-direct-super-interface!! ; (zinal:db:interface%%)

  ; Returns #f if removing this super type would "orphan" a zinal:db:define-method%% . "orphaning"
  ; occurs when this or a subclass defines a method that it would not inherit if the argument were
  ; removed from this type's supers.
  ; i.e., if a method is declared by the argument or one of its supertypes, and this or a subclass
  ; which defines the method has no type DAG path (except those including the edge
  ; that is about to be severed) that reaches the declaring type, this method returns #f.
  ; Throws if the argument is not a direct super interface of this
  can-remove-direct-super-interface? ; (zinal:db:interface%%)

  ; This type will no longer directly extend the argument. Idempotent.
  ; Throws an exception if can-remove-direct-super-interface? would return #f or throw
  ; No meaningful return value
  remove-direct-super-interface!! ; (zinal:db:interface%%)

  ; Returns a list of all zinal:db:method%% that are defined directly by this or by any super
  ; type, in no particular order
  get-all-methods ; ()
))

; A defined type, which can be referenced and have subtypes. Has methods for getting, adding,
; and removing method declarations. The supertype of zinal:db:interface%% and zinal:db:define-class%% .
; By "direct", we mean a method directly defined by the type, as opposed to methods of grandparent
; types
(define zinal:db:type%% (interface (zinal:db:subtype%% zinal:db:referable%%)

  ; Returns a list of zinal:db:method%% representing the methods directly defined by this type, in
  ; no particular order.
  get-direct-methods ; ()

  ; The only way to create a new method. Returns a handle to the newly created method.
  add-direct-method!! ; ([short-desc] [long-desc])

  ; Returns #f if there is any reference to this method whatsoever, either a
  ; zinal:db:define-method%% , zinal:db:invoke-super-method%% or zinal:db:invoke-method%% .
  ; Throws an exception if the argument isn't a direct method.
  can-remove-direct-method? ; (zinal:db:method%%)

  ; Deletes the argument. All handles to the deleted method become invalid.
  ; Throws an exception if can-remove-direct-method? would throw or return #f
  ; No meaningful return value
  remove-direct-method!! ; (zinal:db:method%%)
))

; Comparable to racket interfaces. Each defines a type, the direct super types, and the methods
; of that type. Interfaces are universally public, and attached to the db as a whole, rather than
; being attached to any particular module.
(define zinal:db:interface%% (interface (zinal:db:type%%)

  ; Returns #t if this interface has no subtypes, if there are no references to it, and all
  ; of its methods can be deleted.
  ; Otherwise #f
  can-delete? ; ()

  ; Deletes this interface. All handles for this interface become invalid.
  ; Throws an exception if can-delete? would return #f
  ; No meaningful return value
  delete!! ; ()
))

(define zinal:db:interface-ref%% (interface (zinal:db:reference%%)

  ; Returns a zinal:db:interface%% handle for the interface that this reference refers to
  get-interface ; ()
))

; Super type of zinal:db:define-class%% and zinal:db:class-instance%% . The former defines a class as a
; type, complete with its own methods and the ability to be referenced. The latter instantly creates
; an instance of an anonymous class with the specified properties; it cannot define
; its own methods and cannot be referenced in any way. It may seem silly to have two different
; types of nodes for non-anonymous and anonymous classes, but there are enough practical reasons to
; do so. This interface defines the basic functionality common to both types of classes, including the
; fact that each has a body of expressions, statements, and method definitions. The body must contain
; exactly one zinal:db:super-init%% in order to transpile.
(define zinal:db:class%% (interface (zinal:db:subtype%% zinal:db:has-body%%)

  ; Gets the zinal:db:define-method%% corresponding to the argument if one exists, otherwise #f
  ; Can be called on any method of this or a super-type. Will throw exception if invoked on any
  ; other method
  get-direct-definition-of-method ; (zinal:db:method%%)

  ; Returns #t if the argument is not defined by this or by any super class. Otherwise #f
  ; Can be called on any method of this or a super-type. Will throw exception if invoked on any
  ; other method
  is-method-abstract? ; (zinal:db:method%%)

  ; Returns #t if the argument is directly defined and is also defined by some super class.
  ; Otherwise #f
  ; Can be called on any method of this or a super-type. Will throw exception if invoked on any
  ; other method
  is-method-overridden? ; (zinal:db:method%%)

  ; Returns a zinal:db:legacy-link%% or zinal:db:class-ref%% corresponding to the super class of
  ; this class. At initiation the super class is a zinal:db:legacy-link%% corresponding to the
  ; racket object% .
  get-super-class ; ()

  ; Returns #f if changing the super class would "orphan" a zinal:db:define-method%% or
  ; zinal:db:invoke-super-method%% . "orphaning" occurs when this or a subclass defines or super
  ; invokes a method that it would not inherit if the super class were changed to, say, object% .
  ; i.e., if a method is declared by the super class or one of its supertypes, and this or a subclass
  ; which defines or super invokes the method has no type DAG path (except those including the edge
  ; that is about to be severed) that reaches the declaring type, this method returns #f.
  ; Otherwise #t
  can-set-super-class? ; ()

  ; Changes the super class to be the argument. Any handles previously returned by get-super-class
  ; become invalid.
  ; Throws an exception if can-set-super-class? would return #f
  ; Returns a handle to the new zinal:db:class-ref%%
  set-super-class!! ; (zinal:db:define-class%%)

  ; Changes the super class to be a zinal:db:legacy-link%% with the name and library specified by
  ; the arguments. Any handles previously returned by get-super-class become invalid.
  ; Throws an exception if can-set-super-class? would return #f
  ; Returns a handle to the new zinal:db:legacy-link%%
  set-legacy-super-class!! ; (library name)
))

; Comparable to a racket class definition. Defines a class. The init params are represented by the
; zinal:db:has-params%% parameters. The methods are not nodes and thus not children and not part of
; the tree. If the class is instantiated (i.e. it's not abstract) then all direct methods and all
; methods of direct or indirect super types must be defined by this class or by super classes.
(define zinal:db:define-class%%
  (interface (zinal:db:class%% zinal:db:has-params%% zinal:db:type%%))
)

(define zinal:db:class-ref%% (interface (zinal:db:reference%%)

  ; Returns a zinal:db:define-class%% handle for the param that this reference refers to
  get-define-class ; ()
))

; Comparable to immediately creating an object of an anonymous class in racket, e.g.
; (make-object (class <super> ...)) . The anonymous class that the resulting object is implicitly a
; member of has no init params, cannot define its own methods, cannot be referenced, and won't
; transpile if any methods are abstract.
(define zinal:db:class-instance%% (interface (zinal:db:class%%)))

; A node which evaluates to the object it is the child of. Corresponds to the racket "this"
(define zinal:db:this%% (interface (zinal:db:node%%)))

; represents a method declaration. Each zinal:db:method%% represents a distinct, canonical method,
; which can be defined in a zinal:db:class%% via zinal:db:define-method%% or invoked anywhere via
; zinal:db:invoke-method%% . Overrides of methods are not themselves new methods - rather they are
; simply zinal:db:define-method%% for a method already defined by a super-class.
(define zinal:db:method%% (interface (zinal:db:describable%%)

  ; Returns a zinal:db:type%% for the type that directly defines this method
  get-containing-type ; ()
))

; A node which defines a method. This node is only permitted in the body of a zinal:db:class%% .
; There must be no more than one such node per method per class. If the method is defined by any
; super class, then this is an override, otherwise it's not. A class can only define a method
; which it or a supertype declares
(define zinal:db:define-method%% (interface (zinal:db:parent-node%%)

  ; Returns a zinal:db:method%% for the method, either a direct method of this type or a method of
  ; a super type.
  get-method ; ()

  ; Returns a zinal:db:lambda%% that defines the method. The returned lambda cannot be unassign!! .
  ; Thus the returned lambda is permanent in a sense - it can be manipulated but neither created nor
  ; destroyed
  get-lambda ; ()

  ; Returns #t if any super class contains a zinal:db:define-method%% for this method. Otherwise #f
  is-override? ; ()
))

; A node which overrides a method of a racket super class. Should be used sparingly
(define zinal:db:override-legacy-method%% (interface (zinal:db:parent-node%%)

  ; Returns a string for the name of the racket method that is to be overridden
  get-legacy-method-name ; ()

  ; Returns the lambda that defines the overridden method. Comparable to zinal:db:define-method%% ,
  ; the returned lambda is permanent and cannot be unassign!! , created nor destroyed
  get-lambda ; ()
))

; A node which, when evaluated, intializes the immediate/direct super class. In order to transpile,
; there must be exactly one of these per zinal:db:class%% body.
(define zinal:db:super-init%% (interface (zinal:db:has-args%%)))

; A node which, when evaluated, will invoke the specified method.
(define zinal:db:invoke-method%% (interface (zinal:db:has-args%%)

  ; Returns a node for the object to invoke the method on. At first, the node will be a
  ; zinal:db:unassigned%% , which of course cannot transpile. assign!! it to a node which will
  ; evaluate to an object of the method's class. zinal is not smart enough to understand types
  ; so type failure will occur at runtime.
  get-object ; ()

  ; Returns the zinal:db:method%% that this invokation invokes.
  get-method ; ()

  ; Sets the method that this invokation invokes. The type that contains the method must be
  ; visible at this location. No meaningful return value
  set-method!! ; (zinal:db:method%%)
))

; A node which, when evaluated, invokes a method of some racket class.
(define zinal:db:invoke-legacy-method%% (interface (zinal:db:has-args%%)

  ; Returns a node for the object to invoke the legacy method on. At first, the node will be a
  ; zinal:db:unassigned%% , which of course cannot transpile. assign! it to a node which will
  ; evaluate to an object that extends (directly or indirectly) a racket class that possesses a
  ; public method whose name corresponds to the result of get-legacy-method-name .
  get-object ; ()

  ; Returns a string corresponding to the name of the racket method to invoke.
  get-legacy-method-name ; ()

  ; Sets a string corresponding to the name of the racket method to invoke.
  ; No meaningful return value.
  set-legacy-method-name!! ; (string)
))

; A node which, when evaluated, invokes a super-class's definition of the method.
; Trying to create this node for a method which is abstract in the super class will throw an exception.
; Corresponds to racket of the form '(super <method-name> <args...>) .
(define zinal:db:invoke-super-method%% (interface (zinal:db:has-args%%)

  ; Returns the zinal:db:method%% that this invokation invokes.
  get-method ; ()

  ; Sets the method that this invokation invokes. If the method is abstract in the super class, or the
  ; type containing the method is not visible to this node, an exception is thrown.
  ; No meaningful return value
  set-method!! ; (zinal:db:method%%)
))

; A node which, when evaluated, invokes a racket super-class's definition of the method, if one
; exists. Corresponds to racket of the form '(super <method-name> <args...>) .
(define zinal:db:invoke-legacy-super-method%% (interface (zinal:db:has-args%%)

  ; Returns a string corresponding to the name of the racket method to invoke.
  get-legacy-method-name ; ()

  ; Sets a string corresponding to the name of the racket method to invoke.
  ; No meaningful return value.
  set-legacy-method-name!! ; (string)
))

; A node which, when evaluated, creates a new object of the corresponding defined class.
(define zinal:db:create-object%% (interface (zinal:db:has-args%%)

  ; Returns a node for the class to create a new instance of. At first, the node will be a
  ; zinal:db:unassigned%% , which of course cannot transpile. assign!! it to a node which will
  ; evaluate to the proper class, either a zinal:db:legacy-link%% for racket classes, a
  ; zinal:db:class-ref%% for zinal classes, or any expression which evaluates to one of those. It's
  ; not valid for it to evaluate to zinal:db:define-class%% or zinal:db:class-instance%%
  get-class-node ; ()
))

; NON-OOP

; when evaluated, it will evaluate get-assertion , and if true, nothing else happens. If false,
; an exception is thrown, whose message is composed by evaluating get-format-string and
; get-format-args and applying standard racket formatting rules to them. Unlike some languages,
; get-assertion is always evaluated, so it's safe to have it do stateful operations.
(define zinal:db:assert%% (interface (zinal:db:parent-node%%)

  ; Returns a zinal:db:node%% for the expression that is to be asserted as true.
  get-assertion ; ()

  ; Returns a zinal:db:node%% that evaluates to the format string that is used for an exception
  ; if thrown. Follows standard racket ~a formatting rules. Only evaluated if get-assertion
  ; evaluates false
  get-format-string ; ()

  ; Returns a list of zinal:db:node%% for the arguments to get-format-string . Only evaluated if
  ; get-assertion evaluates false
  get-format-args ; ()

  ; Inserts a new unassigned node into the list of format args at the specified index.
  ; Returns a zinal:db:unassigned%% handle to the new unassigned node
  insert-format-arg!! ; (index)

  ; Deletes the nth format arg . Any handles associated with the deleted arg are now invalid and
  ; cannot be used. No meaningful return value. If the node at the specified index isn't a
  ; zinal:db:unassigned%% , an exception is thrown.
  remove-format-arg!! ; (index)
))

; Any node which has no children and is self-descriptive; literals, essentially.
; (A list literal is really a list of literals)
(define zinal:db:atom%% (interface (zinal:db:node%%)

  ; Returns the literal value represented by this node,
  ; as a scheme value of the proper type
  get-val ; ()
))

(define zinal:db:lambda%%
  (interface (zinal:db:has-params%% zinal:db:has-body%% zinal:db:describable%%))
)

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

  ; Returns a list of all direct child zinal:db:def%% and zinal:db:define-class%% that
  ; can be referenced in other modules, in no particular order. Each public child must
  ; be a direct child, not an indirect descendant.
  get-public-defs ; ()

  ; The first argument specifies which direct child to change the publicity of. If the
  ; index is out of bounds, or the node is not a direct child of this module, an
  ; exception is thrown. new-value is #t if the child should be public, #f otherwise.
  ; Note that it is legal to make a public def non-public, even if doing so makes its
  ; invisible to some of its references. See the comment about visibility rules.
  ; No meaningful return value.
  set-public!! ; (index OR zinal:db:def%% OR zinal:db:define-class%% , new-value)

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

; Used for invoking (or identifying) scheme functions, macros, constants, classes, etc.
; E.g., a zinal:db:legacy-link%% with library #f and name "string->symbol" represents the
; scheme string->symbol function. If it were the first node in a zinal:db:list%% , then
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
    assign-assert!! ; ()
    ; Use #f for library to specify the standard library
    assign-legacy-link!! ; (library name)

    ; If a reference is assigned to a referable that is not visible, an exception is
    ; thrown. See the section about reference visibility.
    assign-def-ref!! ; (zinal:db:def%%)
    assign-param-ref!! ; (zinal:db:param%%)
    assign-class-ref!! ; (zinal:db:define-class%%)
    assign-interface-ref!! ; (zinal:db:interface%%)

    assign-number!! ; (value)
    assign-char!! ; (value)
    assign-string!! ; (value)
    assign-bool!! ; (value)
    assign-symbol!! ; (value)
    assign-keyword!! ; (value)

    assign-define-class!! ; ([short-desc] [long-desc])
    assign-class-instance!! ; ()
    assign-invoke-method!! ; (zinal:db:method%%)
    assign-invoke-legacy-method!! ; (string)
    assign-create-object!! ; ()
    ; attempting to call any of the following methods on a node that is not inside a
    ; zinal:db:class%% body will throw an exception
    assign-define-method!! ; (zinal:db:method%%)
    assign-override-legacy-method!! ; (string)
    assign-this!! ; ()
    assign-invoke-super-method!! ; (zinal:db:method%%)
    assign-invoke-legacy-super-method!! ; (string)
    ; attempting to call this method at any location besides the direct child of a
    ; class will throw an exception
    assign-super-init!! ; ()
  )
)

(define zinal:db:element-visitor%
  (class object%

    (super-make-object)

    (define/public (visit-element e data) #f)
    (define/public (visit-node n data) (visit-element n data))
    (define/public (visit-reference r data) (visit-node r data))
    (define/public (visit-atom a data) (visit-node a data))

    (define/public (visit-interface i data) (visit-element i data))
    (define/public (visit-interface-ref ir data) (visit-reference ir data))
    (define/public (visit-method m data) (visit-element m data))

    (define/public (visit-list l data) (visit-node l data))
    ; TODO consider changing modules from zinal:db:list%% to zinal:db:has-body%%
    (define/public (visit-module m data) (visit-list m data))

    (define/public (visit-lambda l data) (visit-node l data))
    (define/public (visit-assert a data) (visit-node a data))
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

    (define/public (visit-invoke-method im data) (visit-node im data))
    (define/public (visit-invoke-legacy-method ilm data) (visit-node ilm data))
    (define/public (visit-create-object co data) (visit-node co data))
    (define/public (visit-super-init si data) (visit-node si data))
    (define/public (visit-invoke-super-method ism data) (visit-node ism data))
    (define/public (visit-invoke-legacy-super-method ilsm data) (visit-node ilsm data))
    (define/public (visit-define-method dm data) (visit-node dm data))
    (define/public (visit-override-legacy-method olm data) (visit-node olm data))
    (define/public (visit-this t data) (visit-node t data))

    (define/public (visit-class c data) (visit-node c data))
    (define/public (visit-define-class c data) (visit-class c data))
    (define/public (visit-define-class-ref cr data) (visit-reference cr data))
    (define/public (visit-class-instance c data) (visit-class c data))

    (define/public (visit-unassigned u data) (visit-node u data))
  )
)
)
