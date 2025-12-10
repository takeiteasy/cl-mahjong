;; package.lisp
;;
;; Copyright (C) 2025 George Watson
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defpackage #:cl-mahjong
  (:use #:cl)
  (:export
   ;; Types and structures
   #:tile
   #:meld
   #:meld-type
   #:meld-tiles
   #:meld-from-player
   #:make-meld
   #:player
   #:player-id
   #:player-name
   #:player-seat
   #:player-hand
   #:player-melds
   #:player-discards
   #:player-score
   #:player-riichi-declared
   #:player-riichi-turn
   #:player-ippatsu-active
   #:player-furiten
   #:player-is-ready
   #:game-state
   #:game-id
   #:game-players
   #:game-wall
   #:game-phase
   #:game-dealer-seat
   #:game-round-wind
   #:game-turn-state
   #:game-honba
   #:game-riichi-sticks
   #:wall
   #:turn-state
   #:turn-current-player
   #:turn-last-discard
   #:turn-last-discard-player
   #:turn-can-call-ron
   #:turn-can-call-pon
   #:turn-can-call-chi
   #:turn-rinshan-active
   #:turn-chankan-active
   #:turn-turn-number
   #:turn-last-discard
   #:turn-last-discard-player
   #:turn-rinshan-active
   #:turn-chankan-active
   #:group
   #:group-type
   #:group-tiles
   #:group-concealed-p
   #:hand-decomposition
   #:hand-decomposition-pattern-type
   #:hand-decomposition-groups
   #:hand-decomposition-pair
   #:hand-decomposition-wait-type
   #:winning-context
   #:make-winning-context
   #:winning-context-tsumo-p
   #:winning-context-winning-tile
   #:winning-context-winner-seat
   #:winning-context-loser-seat
   #:winning-context-dealer-p
   #:winning-context-loser-is-dealer-p
   #:winning-context-round-wind
   #:winning-context-seat-wind
   #:winning-context-honba
   #:winning-context-riichi-sticks
   #:winning-context-riichi-declared-p
   #:winning-context-double-riichi-p
   #:winning-context-ippatsu-active-p
   #:winning-context-riichi-win-p
   #:winning-context-rinshan-p
   #:winning-context-chankan-p
   #:winning-context-haitei-p
   #:winning-context-houtei-p
   #:winning-context-tenhou-p
   #:winning-context-chiihou-p
   #:winning-context-dora-tiles
   #:winning-context-ura-dora-tiles
   #:winning-context-aka-dora-enabled-p
   #:fu-breakdown
   #:fu-breakdown-base-fu
   #:fu-breakdown-wait-fu
   #:fu-breakdown-pair-fu
   #:fu-breakdown-tsumo-fu
   #:fu-breakdown-group-fu-list
   #:fu-breakdown-total-fu
   #:payment-split
   #:payment-split-direct-payment
   #:payment-split-dealer-pays
   #:payment-split-non-dealer-pays
   #:payment-split-winner-receives
   #:score-result
   #:score-result-yaku-list
   #:score-result-han-from-yaku
   #:score-result-dora-count
   #:score-result-total-han
   #:score-result-fu
   #:score-result-fu-breakdown
   #:score-result-base-points
   #:score-result-limit-hand
   #:score-result-payment

   ;; Tile utilities
   #:tile-suit
   #:tile-value
   #:suited-tile-p
   #:honor-tile-p
   #:terminal-tile-p
   #:simple-tile-p
   #:wind-tile-p
   #:dragon-tile-p
   #:yaochuuhai-p
   #:red-five-p
   #:normalize-red-five
   #:next-tile
   #:calculate-dora
   #:all-tiles
   #:standard-tile-p

   ;; Wall operations
   #:create-wall
   #:wall-draw-tile
   #:wall-draw-replacement-tile
   #:wall-is-empty
   #:wall-tiles-remaining
   #:wall-dora-indicators
   #:wall-ura-dora-indicators
   #:wall-get-dora-tiles
   #:wall-get-ura-dora-tiles

   ;; Player operations
   #:create-player
   #:player-add-tile
   #:player-remove-tile
   #:player-add-meld
   #:player-discard-tile
   #:player-can-pon
   #:player-can-chi
   #:player-can-kan
   #:player-can-declare-riichi
   #:player-declare-riichi
   #:player-check-furiten
   #:player-set-furiten
   #:player-adjust-score
   #:player-reset-for-new-hand
   #:player-calculate-shanten
   #:player-is-tenpai
   #:player-has-winning-hand

   ;; Game state management
   #:create-game-state
   #:add-player-to-game
   #:remove-player-from-game
   #:get-player
   #:get-player-by-id
   #:game-player-count
   #:game-is-full
   #:all-players-ready
   #:start-game
   #:deal-initial-hands
   #:process-action
   #:game-in-progress
   #:game-waiting-for-players
   #:game-in-ready-check

   ;; Hand analysis
   #:validate-hand
   #:decompose-hand
   #:calculate-shanten
   #:calculate-ukeire
   #:get-visible-tiles

   ;; Winning context
   #:build-winning-context
   #:value-tile-p

   ;; Yaku detection
   #:detect-yaku

   ;; Scoring
   #:calculate-fu
   #:print-fu-breakdown
   #:count-dora
   #:determine-limit-hand
   #:calculate-base-points
   #:split-payment
   #:calculate-score
   #:print-score-result

   ;; Game integration (ron/tsumo/furiten)
   #:process-call-ron
   #:process-call-tsumo
   #:process-call-pon
   #:process-call-chi
   #:process-call-kan
   #:process-pass
   #:check-possible-calls
   #:check-furiten-for-ron
   #:apply-payment-to-players
   #:track-rinshan-event
   #:track-chankan-event
   #:clear-event-tracking
   #:update-ippatsu-status
   #:make-payment-split
   #:payment-split-winner-receives
   #:payment-split-direct-payment
   #:payment-split-dealer-pays
   #:payment-split-non-dealer-pays
   #:player-set-ready))
