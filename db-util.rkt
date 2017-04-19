; Copyright 2017 Nick Collins
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

; Similar to "#lang racket"
(module db-util racket

(require "misc.rkt")
(require "db.rkt")

(provide db-search-prev db-search-next handles-equal? get-define-methods assign-reference!! get-short-desc-or)

(define (handles-equal? handle1 handle2)
  (or
    (and (not handle1) (not handle2))
    (and handle1 handle2 (send handle1 equals? handle2))
  )
)

(define (get-short-desc-or describable-handle alt)
  (or (send describable-handle get-short-desc) alt)
)

(define (assign-reference!! unassigned-handle referable-handle)
  (cond
    [(is-a? referable-handle zinal:db:param%%) (send unassigned-handle assign-param-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:def%%) (send unassigned-handle assign-def-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:define-class%%) (send unassigned-handle assign-class-ref!! referable-handle)]
    [(is-a? referable-handle zinal:db:interface%%) (send unassigned-handle assign-interface-ref!! referable-handle)]
    [else (error 'assign-reference!! "Invalid referable")]
  )
)

(define (get-define-methods class)
  (filter (curryr is-a? zinal:db:define-method%%) (send class get-body))
)

(define (db-search-prev criterion start-node)
  (db-search* tree-search-prev criterion start-node)
)

(define (db-search-next criterion start-node)
  (db-search* tree-search-next criterion start-node)
)

(define (db-search* searcher criterion start-node)
  (searcher
    (lambda (n) (send n get-parent))
    (lambda (n) (if (is-a? n zinal:db:parent-node%%) (send n get-children) #f))
    criterion
    start-node
  )
)

)
