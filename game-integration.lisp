;; game-integration.lisp
;; Game state integration for winning conditions, furiten, and payment
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

(defun check-furiten-for-ron (player winning-tile)
  "Comprehensive furiten check for ron (winning on discard).

   Furiten occurs when:
   1. The winning tile is in player's own discards (discard furiten)
   2. Player previously passed on a ron opportunity (temporary furiten)
   3. Player is in tenpai but any of their waits are in their discards (permanent furiten)

   For now, we implement discard furiten (most common case).
   Returns T if player is in furiten, NIL otherwise."

  (player-check-furiten player winning-tile))

(defun check-multiple-ron (game winning-tile discarder-seat)
  "Check for multiple ron (multiple players can win on same discard).

   Returns list of seats that can ron, in priority order.
   Priority: counter-clockwise from discarder (closest has priority).

   For now, implements single ron only (first eligible player wins)."

  (let ((eligible-seats nil))
    ;; Check each seat for valid ron
    (dolist (seat '(:east :south :west :north))
      (unless (eq seat discarder-seat)
        (let ((player (get-player game seat)))
          (when (and player
                     (not (player-furiten player))
                     (player-has-winning-hand player winning-tile))
            (push seat eligible-seats)))))

    ;; Return first eligible (atamahane - head bump rule)
    ;; In full implementation, would return all and let players declare
    (when eligible-seats
      (list (first eligible-seats)))))

(defun apply-payment-to-players (game winner-seat payment context)
  "Apply payment distribution to player scores.

   Arguments:
   - GAME: game-state instance
   - WINNER-SEAT: seat of winning player
   - PAYMENT: payment-split struct
   - CONTEXT: winning-context struct

   Updates player scores directly."

  (let ((winner (get-player game winner-seat))
        (is-tsumo (winning-context-tsumo-p context))
        (loser-seat (winning-context-loser-seat context))
        (dealer-seat (game-dealer-seat game)))

    (cond
      ;; Ron (direct payment from loser)
      ((not is-tsumo)
       (let ((loser (get-player game loser-seat))
             (amount (payment-split-direct-payment payment)))
         ;; Winner receives
         (player-adjust-score winner (payment-split-winner-receives payment))
         ;; Loser pays
         (player-adjust-score loser (- amount))

         (format t "~&[PAYMENT] ~A pays ~A to ~A (ron)~%"
                 (player-name loser)
                 (payment-split-winner-receives payment)
                 (player-name winner))))

      ;; Tsumo (all players pay)
      (t
       (let ((dealer-pays (payment-split-dealer-pays payment))
             (non-dealer-pays (payment-split-non-dealer-pays payment))
             (is-dealer-winner (eq winner-seat dealer-seat)))

         ;; All other players pay
         (dolist (seat '(:east :south :west :north))
           (unless (eq seat winner-seat)
             (let ((payer (get-player game seat))
                   (amount (if (and dealer-pays (eq seat dealer-seat))
                              dealer-pays
                              non-dealer-pays)))
               (when payer
                 (player-adjust-score payer (- amount))
                 (format t "~&[PAYMENT] ~A pays ~A to ~A (tsumo)~%"
                         (player-name payer)
                         amount
                         (player-name winner))))))

         ;; Winner receives total
         (player-adjust-score winner (payment-split-winner-receives payment)))))))

(defun process-call-ron (game player-id)
  "Handle a player declaring ron (win on discard).

   Process:
   1. Validate player can ron
   2. Check furiten
   3. Get last discard
   4. Validate winning hand
   5. Build winning context
   6. Calculate score
   7. Apply payments
   8. End hand

   Returns: (values success-p error-message score-result)"

  (multiple-value-bind (player winner-seat) (get-player-by-id game player-id)
    (cond
      ((not player)
       (values nil "Player not in game" nil))

      ;; Check if there's a discard available
      ((not (game-turn-state game))
       (values nil "No active turn" nil))

      (t
       (let* ((turn-state (game-turn-state game))
              (last-discard (turn-last-discard turn-state))
              (discarder-seat (turn-last-discard-player turn-state)))
         (cond
           ((not last-discard)
            (values nil "No discard to ron" nil))
           ((check-furiten-for-ron player last-discard)
            (values nil "Cannot ron: furiten" nil))
           ;; Validate winning hand (add discard to hand temporarily for validation)
           ((not (validate-hand (cons last-discard (player-hand player))
                               (player-melds player)))
            (values nil "Hand is not winning" nil))
           (t
            (let* ((hand (cons last-discard (player-hand player)))  ; Add winning tile
                   (melds (player-melds player))
                   (context (build-winning-context
                             game winner-seat last-discard nil discarder-seat))
                   (score (calculate-score hand melds last-discard context)))
              (cond
                ((not score)
                 (values nil "No valid yaku" nil))
                (t
                 (let ((payment (score-result-payment score)))
                   (apply-payment-to-players game winner-seat payment context)
                   (setf (game-phase game) :hand-ended)

                   (format t "~&[GAME ~A] ~A wins by RON!~%"
                           (game-id game)
                           (player-name player))
                   (format t "~&  Yaku: ~{~A~^, ~}~%"
                           (mapcar #'first (score-result-yaku-list score)))
                   (format t "~&  Score: ~A han ~A fu~%"
                           (score-result-total-han score)
                           (score-result-fu score))
                   (format t "~&  Payment: ~A points~%"
                           (payment-split-winner-receives payment))

                   (values t nil score))))))))))))

(defun process-call-tsumo (game player-id winning-tile)
  "Handle a player declaring tsumo (self-draw win).

   Arguments:
   - GAME: game-state instance
   - PLAYER-ID: ID of player declaring tsumo
   - WINNING-TILE: The tile player is declaring tsumo on (should be just drawn)

   Process:
   1. Validate player's turn
   2. Validate winning tile is in hand
   3. Validate winning hand
   4. Build winning context
   5. Calculate score
   6. Apply payments
   7. End hand

   Returns: (values success-p error-message score-result)"

  (multiple-value-bind (player winner-seat) (get-player-by-id game player-id)
    (cond
      ((not player)
       (values nil "Player not in game" nil))

      ((not (eq winner-seat (turn-current-player (game-turn-state game))))
       (values nil "Not your turn" nil))

      ((not (member (game-phase game) '(:turn-discard :turn-draw)))
       (values nil "Cannot tsumo: wrong phase" nil))

      (t
       (cond
         ((not (member winning-tile (player-hand player) :test #'eq))
          (values nil "Winning tile not in hand" nil))

         ((not (player-has-winning-hand player))
          (values nil "Hand is not winning" nil))

         ;; Valid tsumo - process it
         (t
          (let* ((hand (player-hand player))
                 (melds (player-melds player))
                 (context (build-winning-context
                           game winner-seat winning-tile t))
                 (score (calculate-score hand melds winning-tile context)))

            (cond
              ((not score)
               (values nil "No valid yaku" nil))
              (t
               (let ((payment (score-result-payment score)))
                 (apply-payment-to-players game winner-seat payment context)

                 (setf (game-phase game) :hand-ended)

                 (format t "~&[GAME ~A] ~A wins by TSUMO!~%"
                         (game-id game)
                         (player-name player))
                 (format t "~&  Yaku: ~{~A~^, ~}~%"
                         (mapcar #'first (score-result-yaku-list score)))
                 (format t "~&  Score: ~A han ~A fu~%"
                         (score-result-total-han score)
                         (score-result-fu score))
                 (format t "~&  Payment: ~A points~%"
                         (payment-split-winner-receives payment))

                 (values t nil score)))))))))))

(defun track-rinshan-event (game)
  "Mark rinshan event (win on kan replacement tile).
   Called when player draws replacement tile after kan."
  (when (game-turn-state game)
    (setf (turn-rinshan-active (game-turn-state game)) t)))

(defun track-chankan-event (game)
  "Mark chankan event (robbing a kan).
   Called when player attempts to add to an existing pon."
  (when (game-turn-state game)
    (setf (turn-chankan-active (game-turn-state game)) t)))

(defun clear-event-tracking (game)
  "Clear all event tracking flags for new turn.
   Called at the start of each turn."
  (when (game-turn-state game)
    (setf (turn-rinshan-active (game-turn-state game)) nil
          (turn-chankan-active (game-turn-state game)) nil)))

(defun update-ippatsu-status (game)
  "Update ippatsu status after each action.
   Ippatsu is cleared if any call (pon/chi/kan) is made after riichi."
  (maphash (lambda (seat player)
             (declare (ignore seat))
             (when (and (player-riichi-declared player)
                       (player-ippatsu-active player))
               ;; Check if a call was made (would clear ippatsu)
               ;; For now, this is called manually after call actions
               (setf (player-ippatsu-active player) nil)))
           (game-players game)))
