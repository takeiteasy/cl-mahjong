;; winning-context.lisp
;;
;; This module provides functions to extract winning context from game state.
;; The winning-context struct contains all information needed for yaku detection,
;; fu calculation, and score computation.
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

(in-package #:cl-mahjong)

(defun build-winning-context (game-state winner-seat winning-tile is-tsumo &optional loser-seat)
  "Build winning-context from game-state.

   This is called when a ron or tsumo is declared.
   Gathers all necessary context for scoring.

   Arguments:
   - GAME-STATE: The current game state
   - WINNER-SEAT: Seat of the winning player
   - WINNING-TILE: The tile that completed the hand
   - IS-TSUMO: T for self-draw, NIL for ron
   - LOSER-SEAT: Seat of discarder (for ron only)

   Returns: winning-context struct"

  (let ((player (get-player game-state winner-seat))
        (wall (game-wall game-state))
        (turn-state (game-turn-state game-state))
        (context (make-winning-context
                  :tsumo-p is-tsumo
                  :winning-tile winning-tile
                  :winner-seat winner-seat
                  :loser-seat loser-seat)))
    ;; Player info
    (setf (winning-context-dealer-p context)
          (eq winner-seat (game-dealer-seat game-state))
          (winning-context-loser-is-dealer-p context)
          (and loser-seat (eq loser-seat (game-dealer-seat game-state))))
    ;; Game context
    (setf (winning-context-round-wind context) (game-round-wind game-state)
          (winning-context-seat-wind context) winner-seat
          (winning-context-honba context) (game-honba game-state)
          (winning-context-riichi-sticks context) (game-riichi-sticks game-state))
    ;; Riichi status
    (setf (winning-context-riichi-declared-p context)
          (player-riichi-declared player)
          (winning-context-ippatsu-active-p context)
          (player-ippatsu-active player)
          (winning-context-riichi-win-p context)
          (player-riichi-declared player))
    ;; Double riichi: riichi on first turn
    (when (and (player-riichi-declared player)
               (= (player-riichi-turn player) 1))
      (setf (winning-context-double-riichi-p context) t))
    ;; Situational events
    ;; Rinshan: won on kan replacement (tracked in turn-state)
    (when turn-state
      (setf (winning-context-rinshan-p context)
            (turn-rinshan-active turn-state))
      ;; Chankan: robbed kan (tracked in turn-state)
      (setf (winning-context-chankan-p context)
            (turn-chankan-active turn-state)))
    ;; Haitei/Houtei: wall is empty
    (when (and wall (wall-is-empty wall))
      (if is-tsumo
          (setf (winning-context-haitei-p context) t)
          (setf (winning-context-houtei-p context) t)))
    ;; Tenhou/Chiihou: first turn wins
    (when turn-state
      (setf (winning-context-turn-number context)
            (turn-turn-number turn-state)
            (winning-context-first-turn-p context)
            (= (turn-turn-number turn-state) 1))
      (when (and (winning-context-first-turn-p context) is-tsumo)
        (if (winning-context-dealer-p context)
            (setf (winning-context-tenhou-p context) t)
            (setf (winning-context-chiihou-p context) t))))
    ;; Dora
    (when wall
      (setf (winning-context-dora-tiles context)
            (wall-get-dora-tiles wall)
            (winning-context-ura-dora-tiles context)
            (wall-get-ura-dora-tiles wall)))
    ;; Aka-dora (red 5s) - currently disabled, make configurable later
    (setf (winning-context-aka-dora-enabled-p context) nil)
    ;; Extracted Norths (sanma only)
    (setf (winning-context-extracted-norths context)
          (player-extracted-norths player))
    context))


(defun value-tile-p (tile context)
  "Check if a tile is a value tile (yakuhai).

   Value tiles are:
   - Seat wind (player's own seat wind)
   - Round wind
   - Any dragon (white, green, red)

   Note: A wind can be worth 2 han if it's both seat and round wind."
  (or
   ;; Dragons are always value tiles
   (member tile '(:white :green :red))
   ;; Winds are value if they match seat or round wind
   (and (member tile '(:east :south :west :north))
        (or (eq tile (winning-context-seat-wind context))
            (eq tile (winning-context-round-wind context))))))
