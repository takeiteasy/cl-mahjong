;; scoring.lisp
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


(defun calculate-base-fu (decomposition melds context)
  "Calculate base fu (20/25/30).

   - 20: Open hand (has called melds)
   - 25: Chiitoitsu (fixed)
   - 30: Closed hand (menzen)"
  (declare (ignore context))
  (cond
    ;; Chiitoitsu is always 25 fu
    ((eq (hand-decomposition-pattern-type decomposition) :chiitoitsu)
     25)
    ;; Open hand (has melds): 20 fu
    ((> (length melds) 0)
     20)
    ;; Closed hand: 30 fu
    (t 30)))


(defun calculate-wait-fu (decomposition)
  "Calculate fu from wait type.

   - 0 fu: Ryanmen (two-sided), Shanpon (dual pon)
   - 2 fu: Kanchan (middle), Penchan (edge), Tanki (pair)"
  (let ((wait-type (hand-decomposition-wait-type decomposition)))
    (cond
      ;; Good waits: 0 fu
      ((member wait-type '(:ryanmen :shanpon))
       0)
      ;; Bad waits: 2 fu
      ((member wait-type '(:kanchan :penchan :tanki))
       2)
      ;; Kokushi: 0 fu (special pattern)
      ((eq wait-type :kokushi-13)
       0)
      ;; Default: 0 fu
      (t 0))))


(defun calculate-group-fu (group)
  "Calculate fu for a single group (pon/chi/kan).

   Chi (sequence): 0 fu

   Pon (triplet):
   - Concealed simples: 4 fu
   - Concealed terminals/honors: 8 fu
   - Open simples: 2 fu
   - Open terminals/honors: 4 fu

   Kan:
   - Concealed simples: 16 fu
   - Concealed terminals/honors: 32 fu
   - Open simples: 8 fu
   - Open terminals/honors: 16 fu"
  (let ((group-type (group-type group))
        (tiles (group-tiles group))
        (concealed-p (group-concealed-p group)))
    (cond
      ((eq group-type :chi)
       0)
      ((eq group-type :pon)
       (let ((is-terminal-or-honor (yaochuuhai-p (first tiles))))
         (cond
           ;; Concealed pon of terminals/honors: 8 fu
           ((and concealed-p is-terminal-or-honor) 8)
           ;; Concealed pon of simples: 4 fu
           (concealed-p 4)
           ;; Open pon of terminals/honors: 4 fu
           (is-terminal-or-honor 4)
           ;; Open pon of simples: 2 fu
           (t 2))))
      ((eq group-type :kan)
       (let ((is-terminal-or-honor (yaochuuhai-p (first tiles))))
         (cond
           ;; Concealed kan of terminals/honors: 32 fu
           ((and concealed-p is-terminal-or-honor) 32)
           ;; Concealed kan of simples: 16 fu
           (concealed-p 16)
           ;; Open kan of terminals/honors: 16 fu
           (is-terminal-or-honor 16)
           ;; Open kan of simples: 8 fu
           (t 8))))

      ;; Unknown group type: 0 fu
      (t 0))))

(defun calculate-all-groups-fu (groups)
  "Calculate total fu from all groups.
   Returns list of (group . fu-value) pairs and total."
  (let ((group-fu-list nil)
        (total-fu 0))
    (dolist (group groups)
      (let ((fu (calculate-group-fu group)))
        (push (cons group fu) group-fu-list)
        (incf total-fu fu)))
    (values (nreverse group-fu-list) total-fu)))


(defun calculate-pair-fu (decomposition context)
  "Calculate fu from pair tile.

   - 2 fu: Dragon pair OR seat wind pair OR round wind pair
   - 0 fu: Other pairs

   Note: If a wind is both seat and round wind, it still only gives 2 fu for the pair."
  (let ((pair-tile (hand-decomposition-pair decomposition)))
    (cond
      ;; Dragons always give 2 fu
      ((member pair-tile '(:white :green :red))
       2)
      ;; Winds: check if seat or round wind
      ((member pair-tile '(:east :south :west :north))
       (if (or (eq pair-tile (winning-context-seat-wind context))
               (eq pair-tile (winning-context-round-wind context)))
           2
           0))
      ;; Other tiles: 0 fu
      (t 0))))


(defun calculate-tsumo-fu (melds context)
  "Calculate fu from tsumo (self-draw).

   - 2 fu: Closed hand tsumo (menzen tsumo)
   - 0 fu: Open hand tsumo OR ron"
  (cond
    ;; Not tsumo: 0 fu
    ((not (winning-context-tsumo-p context))
     0)
    ;; Closed hand tsumo: 2 fu
    ((zerop (length melds))
     2)
    ;; Open hand tsumo: 0 fu
    (t 0)))


(defun calculate-fu (hand melds winning-tile context decomposition)
  "Calculate total fu for a winning hand.

   Arguments:
   - HAND: List of concealed tiles
   - MELDS: List of meld structs
   - WINNING-TILE: The tile that completed the hand
   - CONTEXT: winning-context struct
   - DECOMPOSITION: hand-decomposition struct

   Returns: fu-breakdown struct with detailed breakdown and total fu

   Special cases:
   - Chiitoitsu: always 25 fu (no rounding)
   - Pinfu-tsumo: always 20 fu (no rounding)
   - Kokushi: 30 fu (base only)
   - All others: round UP to nearest 10"

  (unless decomposition
    (setf decomposition (decompose-hand hand melds winning-tile)))
  (unless decomposition
    (return-from calculate-fu nil))
  (let ((pattern-type (hand-decomposition-pattern-type decomposition))
        (breakdown (make-fu-breakdown)))
    (cond
      ;; Chiitoitsu: fixed 25 fu
      ((eq pattern-type :chiitoitsu)
       (setf (fu-breakdown-base-fu breakdown) 25)
       (setf (fu-breakdown-total-fu breakdown) 25))
      ;; Kokushi: base fu only (30)
      ((eq pattern-type :kokushi)
       (setf (fu-breakdown-base-fu breakdown) 30)
       (setf (fu-breakdown-total-fu breakdown) 30))
      ;; Standard pattern: calculate all components
      (t
       (let ((base-fu (calculate-base-fu decomposition melds context))
             (wait-fu (calculate-wait-fu decomposition))
             (pair-fu (calculate-pair-fu decomposition context))
             (tsumo-fu (calculate-tsumo-fu melds context))
             (groups (hand-decomposition-groups decomposition)))
         ;; Calculate group fu
         (multiple-value-bind (group-fu-list group-fu-total)
             (calculate-all-groups-fu groups)
           ;; Store breakdown
           (setf (fu-breakdown-base-fu breakdown) base-fu)
           (setf (fu-breakdown-wait-fu breakdown) wait-fu)
           (setf (fu-breakdown-pair-fu breakdown) pair-fu)
           (setf (fu-breakdown-tsumo-fu breakdown) tsumo-fu)
           (setf (fu-breakdown-group-fu-list breakdown) group-fu-list)
           ;; Calculate raw total
           (let ((raw-total (+ base-fu wait-fu pair-fu tsumo-fu group-fu-total)))
             ;; Check for pinfu-tsumo special case
             ;; Pinfu-tsumo: closed hand, all sequences, ryanmen wait, tsumo
             ;; = 20 fu exactly (no rounding)
             (if (and (zerop (length melds))
                      (winning-context-tsumo-p context)
                      (eq (hand-decomposition-wait-type decomposition) :ryanmen)
                      (every (lambda (g) (eq (group-type g) :chi)) groups)
                      (zerop pair-fu))
                 ;; Pinfu-tsumo: exactly 20 fu
                 (setf (fu-breakdown-total-fu breakdown) 20)
                 ;; Normal: round up to nearest 10
                 (setf (fu-breakdown-total-fu breakdown)
                       (* 10 (ceiling raw-total 10)))))))))

    breakdown))


(defun count-dora (hand melds dora-tiles ura-dora-tiles aka-dora-enabled-p
                   &optional (extracted-norths 0))
  "Count total dora han in a hand.

   Arguments:
   - HAND: List of concealed tiles
   - MELDS: List of meld structs
   - DORA-TILES: List of actual dora tiles (not indicators)
   - URA-DORA-TILES: List of ura-dora tiles (only for riichi wins)
   - AKA-DORA-ENABLED-P: Whether red fives count as dora
   - EXTRACTED-NORTHS: Number of extracted North winds (sanma only)

   Returns: Total dora count (1 han per dora)"

  (let ((all-tiles (append hand
                          (apply #'append
                                (mapcar #'meld-tiles melds))))
        (total-dora 0))
    ;; Count regular dora
    (dolist (tile all-tiles)
      (setf total-dora (+ total-dora (count tile dora-tiles :test #'eq))))
    ;; Count ura-dora (only if provided)
    (when ura-dora-tiles
      (dolist (tile all-tiles)
        (setf total-dora (+ total-dora (count tile ura-dora-tiles :test #'eq)))))
    ;; Count aka-dora (red fives)
    ;; Red fives are represented as :5m-r, :5p-r, :5s-r
    ;; Each red five counts as 1 dora
    (when aka-dora-enabled-p
      (dolist (tile all-tiles)
        (when (red-five-p tile)
          (incf total-dora))))
    ;; Count extracted Norths (sanma only) - each is +1 dora
    (incf total-dora extracted-norths)

    total-dora))

(defun determine-limit-hand (han fu)
  "Determine if hand is a limit hand (mangan or above).

   Limit hands:
   - :mangan    → 5 han, or 4 han 40+ fu, or 3 han 70+ fu (2000 base)
   - :haneman   → 6-7 han (3000 base)
   - :baiman    → 8-10 han (4000 base)
   - :sanbaiman → 11-12 han (6000 base)
   - :yakuman   → 13+ han (8000 base per yakuman)

   Returns: Limit hand type keyword or NIL"

  (cond
    ;; Yakuman: 13+ han
    ((>= han 13) :yakuman)
    ;; Sanbaiman: 11-12 han
    ((>= han 11) :sanbaiman)
    ;; Baiman: 8-10 han
    ((>= han 8) :baiman)
    ;; Haneman: 6-7 han
    ((>= han 6) :haneman)
    ;; Mangan: 5 han, or special fu combinations
    ((>= han 5) :mangan)
    ((and (= han 4) (>= fu 40)) :mangan)
    ((and (= han 3) (>= fu 70)) :mangan)
    ;; Not a limit hand
    (t nil)))


(defun calculate-base-points (han fu limit-hand)
  "Calculate base points from han and fu.

   For limit hands, base points are fixed.
   For non-limit hands: base-points = fu × 2^(han+2)

   Arguments:
   - HAN: Total han (from yaku + dora)
   - FU: Total fu
   - LIMIT-HAND: Limit hand type or NIL

   Returns: Base points"

  (cond
    ;; Yakuman: 8000 base × number of yakuman
    ((eq limit-hand :yakuman)
     (* 8000 (floor han 13)))
    ;; Sanbaiman: 6000 base
    ((eq limit-hand :sanbaiman) 6000)
    ;; Baiman: 4000 base
    ((eq limit-hand :baiman) 4000)
    ;; Haneman: 3000 base
    ((eq limit-hand :haneman) 3000)
    ;; Mangan: 2000 base
    ((eq limit-hand :mangan) 2000)
    ;; Non-limit hand: fu × 2^(han+2)
    (t
     (* fu (expt 2 (+ han 2))))))


(defun round-up-100 (value)
  "Round value up to nearest 100."
  (* 100 (ceiling value 100)))

(defun split-payment (base-points context &optional (config +yonma-config+))
  "Calculate payment distribution based on win type and dealer status.

   Arguments:
   - BASE-POINTS: Base points calculated from han/fu
   - CONTEXT: winning-context struct
   - CONFIG: Optional game-config (for variant-specific rules)

   Returns: payment-split struct with detailed breakdown

   Ron (direct hit):
   - Non-dealer winner from non-dealer: 4 × base
   - Non-dealer winner from dealer: 6 × base
   - Dealer winner: 6 × base

   Tsumo (self-draw):
   4-player (yonma):
   - Non-dealer winner: dealer pays 2× base, others pay 1× base
   - Dealer winner: each pays 2× base

   3-player (sanma):
   - Non-dealer winner: each other pays 2× base
   - Dealer winner: each other pays 2× base

   Bonuses:
   - Honba: +300 per honba counter
   - Riichi sticks: Winner receives all × 1000

   All payments rounded up to nearest 100."

  (let ((payment (make-payment-split))
        (is-dealer (winning-context-dealer-p context))
        (is-tsumo (winning-context-tsumo-p context))
        (honba (winning-context-honba context))
        (riichi-sticks (winning-context-riichi-sticks context))
        (num-players (game-config-num-players config)))

    (cond
      ;; Ron
      ((not is-tsumo)
       (let* ((loser-is-dealer (winning-context-loser-is-dealer-p context))
              ;; Dealer winner or winner from dealer: 6× base
              ;; Non-dealer winner from non-dealer: 4× base
              (multiplier (if (or is-dealer loser-is-dealer) 6 4))
              (direct-payment (round-up-100 (* base-points multiplier)))
              ;; Honba bonus: +300 per honba
              (honba-bonus (* 300 honba))
              ;; Riichi stick bonus: +1000 per stick
              (riichi-bonus (* 1000 riichi-sticks))
              (total (+ direct-payment honba-bonus riichi-bonus)))

         (setf (payment-split-direct-payment payment) direct-payment)
         (setf (payment-split-winner-receives payment) total)
         (setf (payment-split-winner-seat payment) (winning-context-winner-seat context))
         (setf (payment-split-loser-seat payment) (winning-context-loser-seat context))))
      ;; Tsumo
      (t
       (cond
         ;; Dealer wins by tsumo
         (is-dealer
          (let* ((each-pays (round-up-100 (* base-points 2)))
                 ;; Honba bonus: +100 per player per honba
                 (honba-bonus (* 100 honba))
                 (each-pays-total (+ each-pays honba-bonus))
                 ;; Winner receives from (num-players - 1) players
                 (num-payers (1- num-players))
                 (riichi-bonus (* 1000 riichi-sticks))
                 (total (+ (* num-payers each-pays-total) riichi-bonus)))

            (setf (payment-split-non-dealer-pays payment) each-pays-total)
            (setf (payment-split-winner-receives payment) total)))
         ;; Non-dealer wins by tsumo
         (t
          (case num-players
            ;; 4-player: dealer pays 2×, non-dealers pay 1×
            (4
             (let* ((dealer-pays (round-up-100 (* base-points 2)))
                    (non-dealer-pays (round-up-100 base-points))
                    ;; Honba bonus: +100 per player per honba
                    (honba-bonus (* 100 honba))
                    (dealer-pays-total (+ dealer-pays honba-bonus))
                    (non-dealer-pays-total (+ non-dealer-pays honba-bonus))
                    ;; Winner receives from dealer + 2 non-dealers
                    (riichi-bonus (* 1000 riichi-sticks))
                    (total (+ dealer-pays-total
                             (* 2 non-dealer-pays-total)
                             riichi-bonus)))
               (setf (payment-split-dealer-pays payment) dealer-pays-total)
               (setf (payment-split-non-dealer-pays payment) non-dealer-pays-total)
               (setf (payment-split-winner-receives payment) total)))
            ;; 3-player: each other player pays 2× base (no dealer premium)
            (3
             (let* ((each-pays (round-up-100 (* base-points 2)))
                    ;; Honba bonus: +100 per player per honba
                    (honba-bonus (* 100 honba))
                    (each-pays-total (+ each-pays honba-bonus))
                    ;; Winner receives from 2 other players
                    (riichi-bonus (* 1000 riichi-sticks))
                    (total (+ (* 2 each-pays-total) riichi-bonus)))
               (setf (payment-split-non-dealer-pays payment) each-pays-total)
               (setf (payment-split-winner-receives payment) total))))))))
    payment))


(defun calculate-score (hand melds winning-tile context &optional decomposition)
  "Calculate complete score for a winning hand.

   Arguments:
   - HAND: List of concealed tiles
   - MELDS: List of meld structs
   - WINNING-TILE: The tile that completed the hand
   - CONTEXT: winning-context struct
   - DECOMPOSITION: Optional hand-decomposition struct

   Returns: score-result struct with complete breakdown

   Process:
   1. Detect all yaku
   2. Count dora
   3. Calculate total han
   4. Calculate fu
   5. Determine if limit hand
   6. Calculate base points
   7. Split payment
   8. Return complete result"

  ;; Generate decomposition if not provided
  (unless decomposition
    (setf decomposition (decompose-hand hand melds winning-tile)))
  (unless decomposition
    (return-from calculate-score nil))
  (multiple-value-bind (yaku-list han-from-yaku)
      (detect-yaku hand melds winning-tile context decomposition)
    ;; Must have at least 1 han from yaku (dora alone doesn't count)
    (when (zerop han-from-yaku)
      (return-from calculate-score nil))
    ;; Count dora
    (let* ((dora-tiles (winning-context-dora-tiles context))
           (ura-dora-tiles (if (winning-context-riichi-win-p context)
                              (winning-context-ura-dora-tiles context)
                              nil))
           (aka-dora-enabled-p (winning-context-aka-dora-enabled-p context))
           (dora-count (count-dora hand melds dora-tiles ura-dora-tiles aka-dora-enabled-p))
           (total-han (+ han-from-yaku dora-count)))
      ;; Calculate fu
      (let* ((fu-breakdown (calculate-fu hand melds winning-tile context decomposition))
             (fu (fu-breakdown-total-fu fu-breakdown))
             (limit-hand (determine-limit-hand total-han fu))
             (base-points (calculate-base-points total-han fu limit-hand))
             (payment (split-payment base-points context)))
        ;; Build result
        (make-score-result
         :yaku-list yaku-list
         :han-from-yaku han-from-yaku
         :dora-count dora-count
         :total-han total-han
         :fu fu
         :fu-breakdown fu-breakdown
         :base-points base-points
         :limit-hand limit-hand
         :payment payment)))))


(defun print-fu-breakdown (breakdown)
  "Print a human-readable fu breakdown."
  ;; Disabled to avoid interfering with TUI
  (declare (ignore breakdown))
  nil)

(defun print-score-result (result)
  "Print a human-readable score result."
  ;; Disabled to avoid interfering with TUI
  (declare (ignore result))
  nil)
