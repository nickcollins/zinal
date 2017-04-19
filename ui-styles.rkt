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
(module ui-styles racket

(require racket/gui/base)

(require "misc.rkt")

(provide (prefix-out zinal:ui:style: (all-defined-out)))

; all GUI styles are contained within this file so that they're easy to find and change
; the styles in this file are all style-delta% , not style%

; default style
(define NO-STYLE (make-object style-delta%))

; style for primitive literal values, like ε for the empty string or ⊙ to represent the class in
; scope
(define CONST-VALUE-STYLE (send (make-object style-delta%) set-delta-foreground "Pink"))

; style for references, and bits of the ui that refer to some zinal component defined elsewhere
; since zinal short descs can contain spaces, this style is underlined
(define REF-STYLE (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Cyan"))

; style for assertions
(define ASSERT-STYLE (send (make-object style-delta%) set-delta-foreground "Lime"))

; style for most atomic elements, which are generally literals. Some atom types have their own style
(define ATOM-STYLE (send (make-object style-delta%) set-delta-foreground "Orchid"))

; style for all strings except the empty string, which must be underlined cuz they can contain spaces
(define STRING-STYLE (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Orchid"))

; style for definitions, and parts of the ui that represent the definition or ground truth of some
; zinal component that is referred to elsewhere. Since zinal short descs can contain spaces, this style is underlined
(define DEF-STYLE (send (make-object style-delta% 'change-toggle-underline) set-delta-foreground "Yellow"))

; style for legacy references to racket identifiers. They cannot contain spaces and are thus not
; underlined, which also helps distinguish them from zinal references
(define LEGACY-STYLE (send (make-object style-delta%) set-delta-foreground "Cyan"))

; style for unassigned nodes - they're bold to draw attention to unfinished parts of the program
(define UNASSIGNED-STYLE (send (make-object style-delta% 'change-bold) set-delta-foreground "Chocolate"))

; style for the header of a module
(define MODULE-STYLE (send (make-object style-delta% 'change-bold) set-delta-foreground "Lime"))

; style for the bookends of lists, i.e. something of the form (list ...) or '(...)
(define LIST-STYLE (make-object style-delta% 'change-bold))

; style for the instantiation symbol, ☼
(define CREATE-SYMBOL-STYLE (send (make-object style-delta%) set-delta-foreground "Lime"))

; style for the blurb that tells you that a module child is public
(define PUBLICITY-STYLE (send (make-object style-delta%) set-delta-foreground "Lime"))

)
