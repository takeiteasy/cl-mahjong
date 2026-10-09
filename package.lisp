(defpackage #:cl-mahjong
  (:use #:cl)
  (:export
   ;; registry
   #:definition-error
   #:unknown-definition
   #:circular-extends
   #:invalid-definition
   ;; random
   #:make-rng
   #:rng-next
   #:rng-below
   #:shuffle-vector
   ;; tileset
   #:deftileset
   #:find-tileset
   #:tileset-tile-count
   #:tileset-kind-count
   #:tileset-kind
   #:find-kind
   #:kind-id
   #:kind-name
   #:kind-category
   #:kind-suit
   #:kind-rank
   #:kind-group
   #:kind-copies
   #:kind-honor-p
   #:terminal-p
   #:tile-kind
   #:tile-kind-id
   #:tile-attrs
   #:tile-red-p
   #:tiles-of-kind
   ;; wall
   #:make-wall
   #:wall-draw
   #:wall-draw-replacement
   #:wall-reveal-dora
   #:wall-dora-indicators
   #:wall-ura-indicators
   #:wall-dead-tiles
   #:wall-live-count
   #:wall-exhausted-p))
