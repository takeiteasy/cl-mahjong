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
   #:wall-exhausted-p
   ;; hand
   #:invalid-hand
   #:hand-counts
   #:make-meld
   #:meld-type
   #:meld-kinds
   #:meld-open-p
   #:hand-closed-p
   ;; shape
   #:defshape
   #:find-shape
   #:shape-name
   #:shape-family
   ;; analysis
   #:group-type
   #:group-kinds
   #:group-open-p
   #:decomposition-shape
   #:decomposition-groups
   #:decompose
   #:complete-p
   #:shanten
   #:ukeire
   ;; win
   #:make-win-context
   #:win-context-win-tile
   #:win-context-tsumo
   #:win-context-riichi
   #:win-context-double-riichi
   #:win-context-ippatsu
   #:win-context-seat-wind
   #:win-context-round-wind
   #:win-context-last-tile
   #:win-context-rinshan
   #:win-context-chankan
   #:win-context-tenhou
   #:win-context-chiihou
   #:win-context-renhou
   #:win-context-dealer
   #:win-context-dora-indicators
   #:win-context-ura-indicators
   #:win-context-tiles
   #:win-hand-tileset
   #:win-hand-decomposition
   #:win-hand-counts
   #:win-hand-melds
   #:win-hand-context
   #:win-hand-options
   #:win-hand-win-kind
   #:win-hand-win-group
   #:win-hand-wait
   #:win-hand-closed-p
   #:win-hand-pre-win-counts
   #:group-concealed-p
   ;; yaku
   #:defyaku
   #:find-yaku
   #:yaku-name
   #:evaluate-yaku
   #:reading-decomposition
   #:reading-win-group
   #:reading-wait
   #:reading-yaku
   #:reading-han
   #:reading-yakuman
   #:reading-hand
   ;; scoring
   #:defscoring
   #:find-scoring
   #:evaluate-score
   #:reading-score
   #:best-score
   #:score-reading
   #:score-fu
   #:score-han
   #:score-dora
   #:score-limit
   #:score-base
   #:score-points
   #:score-payments
   ;; riichi
   #:*riichi-yaku*
   #:*riichi-options*))
