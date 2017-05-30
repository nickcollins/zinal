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
(module ui racket

(provide (all-defined-out))

(define zinal:ui:item%% (interface ()

  ; Returns #t if this item should be displayed as "selected" - only one item
  ; in the tree should return #t for this.
  selected? ; ()

  accept ; (zinal:ui:element-visitor% [data])
))

(define zinal:ui:scalar%% (interface (zinal:ui:item%%)

  get-style-delta ; ()

  get-text ; ()
))

; get-text always returns the same value
(define zinal:ui:const%% (interface (zinal:ui:scalar%%)))

; get-text can return different values if something changes
(define zinal:ui:var-scalar%% (interface (zinal:ui:scalar%%)

  ; Returns #t if this item should be "soft" highlighted - there should be a background color
  ; difference, but not one that makes it look like it's selected. Multiple items can be highlighted
  ; this way at once, independent of the selection. If an element is both selected and highlighted, highlighting is ignored.
  highlighted? ; ()
))

(define zinal:ui:list%% (interface (zinal:ui:item%%)

  get-children ; () -> [zinal:ui:item%%]

  ; displayed before the other children - in vertical mode, this is displayed
  ; on the first line without indentation
  get-header ; () -> zinal:ui:item%% OR #f

  ; If #t, this item, and all its children, should be displayed within a single
  ; line. Otherwise, each child is displayed on a separate line.
  horizontal? ; ()

  get-horizontal-separator ; () -> zinal:ui:const%% OR #f

  ; In some cases, the first item will be displayed before all children (and
  ; header) and the last item after all children.
  get-bookends ; () -> [zinal:ui:const%% zinal:ui:const%%] OR #f
))

(define zinal:ui:element-visitor%
  (class object%
    (super-make-object)

    (define/public (visit-item i data) #f)
    (define/public (visit-scalar s data) (visit-item s data))

    (define/public (visit-list l data) (visit-item l data))
    (define/public (visit-const c data) (visit-scalar c data))
    (define/public (visit-var-scalar vs data) (visit-scalar vs data))
  )
)
)
