;; player.lisp
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

(defun create-player (id name seat &optional (config +yonma-config+))
  "Create a new player instance with config-based starting score"
  (let ((player (make-instance 'player
                               :id id
                               :name name
                               :seat seat)))
    (setf (player-score player) (game-config-starting-points config))
    player))


(defun player-add-tile (player tile)
  "Add a tile to the player's hand"
  (push tile (player-hand player)))

(defun player-remove-tile (player tile)
  "Remove one instance of TILE from the player's hand.
   Returns T if tile was found and removed, NIL otherwise."
  (let ((pos (position tile (player-hand player) :test #'eq)))
    (when pos
      (setf (player-hand player)
            (remove tile (player-hand player) :test #'eq :count 1))
      t)))

(defun player-sort-hand (player)
  "Sort the player's hand in standard order"
  (setf (player-hand player)
        (sort-tiles (player-hand player))))

(defun player-hand-size (player)
  "Return the number of tiles in the player's hand"
  (length (player-hand player)))


(defun player-discard-tile (player tile)
  "Discard a tile from the player's hand.
   Returns T if successful, NIL if tile not in hand."
  (when (player-remove-tile player tile)
    (push tile (player-discards player))
    ;; Clear ippatsu when player discards
    (setf (player-ippatsu-active player) nil)
    t))

(defun player-last-discard (player)
  "Get the most recently discarded tile"
  (first (player-discards player)))


(defun player-add-meld (player meld)
  "Add a meld to the player's melds"
  (push meld (player-melds player))
  ;; Clear ippatsu when player calls
  (setf (player-ippatsu-active player) nil))

(defun player-meld-count (player)
  "Return the number of melds the player has called"
  (length (player-melds player)))

(defun player-is-open (player)
  "Check if the player has called any melds (open hand)"
  (> (player-meld-count player) 0))

(defun player-is-closed (player)
  "Check if the player has no called melds (closed hand)"
  (zerop (player-meld-count player)))


(defun player-declare-riichi (player turn-number)
  "Declare riichi for the player.
   Can only be called on a closed hand."
  (when (player-is-closed player)
    (setf (player-riichi-declared player) t
          (player-riichi-turn player) turn-number
          (player-ippatsu-active player) t)
    ;; Deduct 1000 points for riichi stick
    (decf (player-score player) 1000)
    t))

(defun player-cancel-ippatsu (player)
  "Cancel ippatsu possibility (called when another player calls a tile)"
  (setf (player-ippatsu-active player) nil))


(defun player-check-furiten (player winning-tile)
  "Check if player is in furiten for WINNING-TILE.
   Furiten occurs if the winning tile appears in player's discards."
  (member winning-tile (player-discards player) :test #'eq))

(defun player-set-furiten (player furiten)
  "Set the player's furiten status"
  (setf (player-furiten player) furiten))


(defun player-adjust-score (player delta)
  "Adjust the player's score by DELTA (positive or negative)"
  (incf (player-score player) delta))

(defun player-has-score (player amount)
  "Check if player has at least AMOUNT points"
  (>= (player-score player) amount))


(defun player-set-ready (player ready)
  "Set the player's ready status"
  (setf (player-ready player) ready))

(defun player-is-ready (player)
  "Check if the player is ready"
  (player-ready player))


(defun player-can-declare-riichi (player)
  "Check if player can declare riichi.
   Requirements:
   - Hand must be closed (no called melds)
   - Player must have at least 1000 points for riichi stick
   - Not already in riichi"
  (and (player-is-closed player)
       (player-has-score player 1000)
       (not (player-riichi-declared player))))

(defun player-can-call-pon (player tile)
  "Check if player can call pon on TILE (needs 2 more of that tile)"
  (>= (count tile (player-hand player) :test #'eq) 2))

(defun player-can-call-chi (player tile seat last-discard-seat)
  "Check if player can call chi on TILE.
   Returns list of possible sequences or NIL."
  (find-sequence-chi (player-hand player) tile seat last-discard-seat))

(defun player-can-call-kan (player tile &optional (type :open))
  "Check if player can call kan.
   For :open kan - needs 3 more of TILE in hand
   For :closed kan - needs 4 of TILE in hand
   For :added kan - needs 1 more of TILE and existing pon of that tile"
  (case type
    (:open (>= (count tile (player-hand player) :test #'eq) 3))
    (:closed (>= (count tile (player-hand player) :test #'eq) 4))
    (:added (and (>= (count tile (player-hand player) :test #'eq) 1)
                 (find-if (lambda (meld)
                           (and (eq (meld-type meld) :pon)
                                (member tile (meld-tiles meld) :test #'eq)))
                         (player-melds player))))))

(defun player-can-extract-north (player config)
  "Check if player can extract North (must be in hand and sanma variant).
   CONFIG: game-config struct"
  (and (game-config-north-nuki-enabled-p config)
       (member :north (player-hand player) :test #'eq)))

(defun player-extract-north (player)
  "Extract a North wind tile from hand (sanma only).
   Returns T if successful, NIL if no North in hand."
  (when (member :north (player-hand player) :test #'eq)
    (player-remove-tile player :north)
    (incf (player-extracted-norths player))
    t))


(defun player-reset-for-new-hand (player)
  "Reset player state for a new hand (keep score and seat)"
  (setf (player-hand player) nil
        (player-discards player) nil
        (player-melds player) nil
        (player-extracted-norths player) 0
        (player-riichi-declared player) nil
        (player-riichi-turn player) nil
        (player-ippatsu-active player) nil
        (player-furiten player) nil
        (player-ready player) nil))


(defun player-calculate-shanten (player)
  "Calculate shanten for player's current hand.
   Returns integer shanten value (0 = tenpai)"
  (calculate-shanten (player-hand player) (player-melds player)))

(defun player-is-tenpai (player)
  "Check if player is in tenpai (0-shanten).
   Returns T if tenpai, NIL otherwise"
  (zerop (player-calculate-shanten player)))

(defun player-has-winning-hand (player)
  "Check if player's hand is a complete winning hand.
   Returns (values is-valid pattern-type)
   Pattern types: :standard :chiitoitsu :kokushi or NIL"
  (validate-hand (player-hand player) (player-melds player)))
