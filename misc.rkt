; Similar to "#lang racket"
(module misc racket

(require (only-in srfi/1 list-index))

(provide assert string-suffix? non-negative? map-by-index is-one-of? tree-search-next tree-search-prev)

; MACROS

(define-syntax-rule (assert msg bool)
  (unless bool (error msg))
)

; FUNCTIONS

; ugh - in srfi/13, but that seems to redefine string-join
(define (string-suffix? suf s)
  (let (
    [s-len (string-length s)]
    [suf-len (string-length suf)])
    (equal? (substring s (- s-len suf-len) s-len) suf)
  )
)

(define non-negative? (negate negative?))

; proc index lst[index] => any
(define (map-by-index proc lst)
  (define vec (list->vector lst))
  (build-list (vector-length vec) (lambda (i) (proc i (vector-ref vec i))))
)

(define (is-one-of? object types)
  (ormap (curry is-a? object) types)
)

; for tree-search functions, get-parent and get-children take an
; element of the tree. get-parent returns #f for the root node but
; must return a valid node otherwise. get-children should return #f
; if the node has no children. Of course,
; (get-children (get-parent x)) must contain x. These functions will
; return the first found node that matches the criterion, wrapping
; around the root node all the way back to the original start -
; If no node meets the criterion, they'll return #f

(define (tree-search-next get-parent get-children criterion start)
  (tree-search-next* start get-parent get-children criterion start)
)

(define (tree-search-prev get-parent get-children criterion start)
  (tree-search-prev* start get-parent get-children criterion start)
)

(define (tree-search-next* start get-parent get-children criterion current [delve? #t])
  (define cur-parent (get-parent current))
  (define cur-children (get-children current))
  (cond
    [(and delve? cur-children (pair? cur-children))
      (check-criterion* tree-search-next* start get-parent get-children criterion (car cur-children))
    ]
    [cur-parent
      (define adjacent (get-adjacent* get-parent get-children current +))
      (if adjacent
        (check-criterion* tree-search-next* start get-parent get-children criterion adjacent)
        (tree-search-next* start get-parent get-children criterion cur-parent #f)
      )
    ]
    [else
      (check-criterion* tree-search-next* start get-parent get-children criterion current)
    ]
  )
)

(define (tree-search-prev* start get-parent get-children criterion current)
  (define cur-parent (get-parent current))
  (define adjacent (get-adjacent* get-parent get-children current -))
  (define node-to-check
    (cond
      [adjacent (tree-last* get-children adjacent)]
      [cur-parent cur-parent]
      [else (tree-last* get-children current)]
    )
  )
  (check-criterion* tree-search-prev* start get-parent get-children criterion node-to-check)
)

(define (tree-last* get-children current)
  (define children (get-children current))
  (if (and children (pair? children))
    (tree-last* get-children (last children))
    current
  )
)

(define (get-adjacent* get-parent get-children current +/-)
  (define parent (get-parent current))
  (define children (if parent (get-children parent) (list current)))
  (define cur-index (list-index (curry eq? current) children))
  (assert "node is not a child of its parent" cur-index)
  (define new-index (+/- cur-index 1))
  (and (non-negative? new-index) (< new-index (length children))
    (list-ref children new-index)
  )
)

(define (check-criterion* searcher start get-parent get-children criterion current)
  (cond
    [(criterion current) current]
    [(eq? current start) #f]
    [else (searcher start get-parent get-children criterion current)]
  )
)

)
