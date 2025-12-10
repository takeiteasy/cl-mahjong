;; hand-analysis.lisp
;; Hand validation, shanten calculation, and ukeire (acceptance) calculation
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

(defconstant +terminals-and-honors+
  '(:1m :9m :1p :9p :1s :9s :east :south :west :north :white :green :red)
  "13 terminal and honor tiles for kokushi musou")

(defun hand-to-counts (tiles)
  "Convert list of tiles to hash table of counts.
   Uses existing count-tiles from tile-utils.lisp"
  (count-tiles tiles))

(defun counts-to-tiles (counts)
  "Convert hash table of counts back to tile list.
   Used for debugging and validation."
  (let (tiles)
    (maphash (lambda (tile count)
               (dotimes (i count)
                 (push tile tiles)))
             counts)
    tiles))

(defun yaochuuhai-p (tile)
  "Check if tile is terminal or honor (yaochuuhai)"
  (or (terminal-tile-p tile) (honor-tile-p tile)))

(defun next-sequential-tile (tile)
  "Get next tile in sequence (5m -> 6m), or NIL if honor or 9"
  (let ((suit (tile-suit tile))
        (val (tile-value tile)))
    (when (and val (< val 9))
      (intern (format nil "~D~A" (1+ val)
                     (case suit
                       (:man "M")
                       (:pin "P")
                       (:sou "S")))
              :keyword))))

(defun can-form-chi-at-position-p (counts tile)
  "Check if tile can form a chi sequence (tile, tile+1, tile+2)"
  (let ((tile2 (next-sequential-tile tile))
        (tile3 (when (next-sequential-tile tile)
                 (next-sequential-tile (next-sequential-tile tile)))))
    (and tile2 tile3
         (>= (gethash tile counts 0) 1)
         (>= (gethash tile2 counts 0) 1)
         (>= (gethash tile3 counts 0) 1))))

(defun copy-counts (counts)
  "Create a copy of a hash table of counts"
  (let ((new-counts (make-hash-table :test 'eq)))
    (maphash (lambda (tile count)
               (setf (gethash tile new-counts) count))
             counts)
    new-counts))

(defun validate-chiitoitsu (tiles)
  "Check if tiles form 7 different pairs.
   Requirements:
   - Exactly 14 tiles
   - 7 pairs (each tile appears exactly 2 times)
   - All 7 pairs must be different tiles"
  (when (/= (length tiles) 14)
    (return-from validate-chiitoitsu nil))

  (let ((counts (hand-to-counts tiles)))
    (and (= (hash-table-count counts) 7)
         (loop for count being the hash-values of counts
               always (= count 2)))))

(defun validate-kokushi (tiles)
  "Check if tiles form kokushi musou pattern.
   Requirements:
   - One of each 13 terminal/honor tiles
   - Plus one duplicate of any terminal/honor (the pair)
   - Total 14 tiles"
  (when (/= (length tiles) 14)
    (return-from validate-kokushi nil))

  (let ((counts (hand-to-counts tiles))
        (unique-yaochuuhai 0)
        (has-pair nil))

    ;; Check each terminal/honor tile
    (dolist (tile +terminals-and-honors+)
      (let ((count (gethash tile counts 0)))
        (cond
          ((= count 0)
           ;; Missing a required terminal/honor
           (return-from validate-kokushi nil))
          ((= count 1)
           (incf unique-yaochuuhai))
          ((= count 2)
           (incf unique-yaochuuhai)
           (when has-pair
             ;; Can't have two pairs
             (return-from validate-kokushi nil))
           (setf has-pair t))
          ((> count 2)
           ;; Too many of one tile
           (return-from validate-kokushi nil)))))
    ;; Must have all 13 terminals/honors and exactly one pair
    (and (= unique-yaochuuhai 13) has-pair)))

(defun validate-standard (tiles melds)
  "Check if tiles + melds form standard 4 groups + 1 pair.
   Uses backtracking to try all possible groupings.

   Melds are already formed groups, so we need:
   - (4 - num-melds) additional groups from tiles
   - 1 pair from tiles"
  (let* ((num-groups-needed (- 4 (length melds)))
         (counts (hand-to-counts tiles)))
    (validate-standard-recursive counts num-groups-needed t)))

(defun validate-standard-recursive (counts groups-needed need-pair)
  "Recursive helper for standard validation.
   COUNTS: Hash table of tile -> count
   GROUPS-NEEDED: Number of groups (pon/chi) still needed
   NEED-PAIR: Whether we still need to find the pair

   Returns T if valid grouping exists, NIL otherwise."

  ;; Base case: found everything
  (when (and (zerop groups-needed) (not need-pair))
    (return-from validate-standard-recursive
      ;; Check if all tiles used
      (every (lambda (count) (zerop count))
             (loop for count being the hash-values of counts collect count))))

  ;; Get first tile with non-zero count
  (let ((tile (find-if (lambda (tile) (> (gethash tile counts 0) 0))
                       +all-tiles+)))
    (unless tile
      ;; No tiles left but still need groups/pair
      (return-from validate-standard-recursive nil))

    (let ((tile-count (gethash tile counts)))

      ;; Try using this tile as pair (if we need one)
      (when (and need-pair (>= tile-count 2))
        (decf (gethash tile counts) 2)
        (when (validate-standard-recursive counts groups-needed nil)
          (incf (gethash tile counts) 2)
          (return-from validate-standard-recursive t))
        (incf (gethash tile counts) 2))

      ;; Try using as pon (if we need groups)
      (when (and (> groups-needed 0) (>= tile-count 3))
        (decf (gethash tile counts) 3)
        (when (validate-standard-recursive counts (1- groups-needed) need-pair)
          (incf (gethash tile counts) 3)
          (return-from validate-standard-recursive t))
        (incf (gethash tile counts) 3))

      ;; Try using as chi (if suited tile and we need groups)
      (when (and (> groups-needed 0)
                 (not (honor-tile-p tile))
                 (can-form-chi-at-position-p counts tile))
        (let ((tile2 (next-sequential-tile tile))
              (tile3 (next-sequential-tile (next-sequential-tile tile))))
          (decf (gethash tile counts))
          (decf (gethash tile2 counts))
          (decf (gethash tile3 counts))
          (when (validate-standard-recursive counts (1- groups-needed) need-pair)
            (incf (gethash tile counts))
            (incf (gethash tile2 counts))
            (incf (gethash tile3 counts))
            (return-from validate-standard-recursive t))
          (incf (gethash tile counts))
          (incf (gethash tile2 counts))
          (incf (gethash tile3 counts))))

      ;; No valid grouping found with this tile
      nil)))

(defun validate-hand (tiles melds)
  "Validate if tiles + melds form a winning hand.
   Returns (values pattern-type is-valid)
   Pattern types: :standard :chiitoitsu :kokushi or NIL

   TILES: List of concealed tiles (13 for open hand, 14 for closed)
   MELDS: List of meld structs"
  (let ((total-tiles (+ (length tiles)
                        (* 3 (count-if (lambda (m)
                                         (member (meld-type m)
                                                 '(:pon :chi)))
                                       melds))
                        (* 4 (count-if (lambda (m)
                                         (member (meld-type m)
                                                 '(:kan-open :kan-closed :kan-added)))
                                       melds)))))
    (unless (= total-tiles 14)
      (return-from validate-hand (values nil nil)))
    (cond
      ;; Chiitoitsu: 7 pairs, must be closed hand
      ((and (zerop (length melds))
            (= (length tiles) 14)
            (validate-chiitoitsu tiles))
       (values :chiitoitsu t))
      ;; Kokushi: 13 terminals/honors + pair, must be closed
      ((and (zerop (length melds))
            (= (length tiles) 14)
            (validate-kokushi tiles))
       (values :kokushi t))
      ;; Standard: 4 groups + pair
      ((validate-standard tiles melds)
       (values :standard t))
      (t (values nil nil)))))

(defun decompose-hand (tiles melds winning-tile)
  "Decompose a winning hand into exact groups for fu calculation and yaku detection.
   Returns hand-decomposition struct or NIL if hand is not valid.

   TILES: List of concealed tiles
   MELDS: List of meld structs
   WINNING-TILE: The tile that completed the hand

   This extends validate-hand to return the exact grouping found."
  (let ((total-tiles (+ (length tiles)
                        (* 3 (count-if (lambda (m)
                                         (member (meld-type m)
                                                 '(:pon :chi)))
                                       melds))
                        (* 4 (count-if (lambda (m)
                                         (member (meld-type m)
                                                 '(:kan-open :kan-closed :kan-added)))
                                       melds)))))
    (unless (= total-tiles 14)
      (return-from decompose-hand nil))
    (cond
      ;; Chiitoitsu: 7 pairs
      ((and (zerop (length melds))
            (= (length tiles) 14)
            (validate-chiitoitsu tiles))
       (decompose-chiitoitsu tiles winning-tile))
      ;; Kokushi: 13 terminals/honors + pair
      ((and (zerop (length melds))
            (= (length tiles) 14)
            (validate-kokushi tiles))
       (decompose-kokushi tiles winning-tile))
      ;; Standard: 4 groups + pair
      (t
       (decompose-standard tiles melds winning-tile)))))

(defun decompose-chiitoitsu (tiles winning-tile)
  "Decompose chiitoitsu hand (7 pairs).
   Wait type is always :tanki (pair wait)."
  (make-hand-decomposition
   :pattern-type :chiitoitsu
   :groups nil  ; No groups in chiitoitsu
   :pair winning-tile
   :wait-type :tanki))

(defun decompose-kokushi (tiles winning-tile)
  "Decompose kokushi musou hand.
   Wait type is :tanki unless 13-way wait (for double yakuman)."
  (let ((counts (hand-to-counts tiles))
        (thirteen-way nil))
    ;; Check if 13-way wait (all 13 terminals/honors present with count 1)
    (setf thirteen-way
          (every (lambda (tile) (= (gethash tile counts 0) 1))
                 +terminals-and-honors+))
    (make-hand-decomposition
     :pattern-type :kokushi
     :groups nil  ; No groups in kokushi
     :pair winning-tile
     :wait-type (if thirteen-way :kokushi-13 :tanki))))

(defun decompose-standard (tiles melds winning-tile)
  "Decompose standard hand (4 groups + pair) with exact grouping.
   Returns hand-decomposition struct or NIL."
  (let* ((num-groups-needed (- 4 (length melds)))
         (counts (hand-to-counts tiles))
         (groups nil)
         (pair-tile nil))
    ;; Add called melds as groups (marked as not concealed)
    (dolist (meld melds)
      (push (make-group
             :type (cond
                     ((eq (meld-type meld) :pon) :pon)
                     ((eq (meld-type meld) :chi) :chi)
                     ((member (meld-type meld) '(:kan-open :kan-closed :kan-added)) :kan))
             :tiles (meld-tiles meld)
             :concealed-p nil)
            groups))
    ;; Find decomposition of concealed tiles
    (let ((result (decompose-standard-recursive counts num-groups-needed t nil nil)))
      (unless result
        (return-from decompose-standard nil))
      (destructuring-bind (found-groups found-pair) result
        (setf groups (append groups found-groups))
        (setf pair-tile found-pair)
        ;; Identify wait type
        (let ((wait-type (identify-wait-type groups pair-tile winning-tile)))
          (make-hand-decomposition
           :pattern-type :standard
           :groups groups
           :pair pair-tile
           :wait-type wait-type))))))

(defun decompose-standard-recursive (counts groups-needed need-pair current-groups current-pair)
  "Recursive helper for standard decomposition that returns the actual grouping.
   Returns (groups pair-tile) or NIL if no valid decomposition."
  ;; Base case: found everything
  (when (and (zerop groups-needed) (not need-pair))
    ;; Check if all tiles used
    (when (every (lambda (count) (zerop count))
                 (loop for count being the hash-values of counts collect count))
      (return-from decompose-standard-recursive (list current-groups current-pair))))
  ;; Get first tile with non-zero count
  (let ((tile (find-if (lambda (tile) (> (gethash tile counts 0) 0))
                       +all-tiles+)))
    (unless tile
      (return-from decompose-standard-recursive nil))
    (let ((tile-count (gethash tile counts)))
      ;; Try using this tile as pair (if we need one)
      (when (and need-pair (>= tile-count 2))
        (decf (gethash tile counts) 2)
        (let ((result (decompose-standard-recursive counts groups-needed nil
                                                     current-groups tile)))
          (when result
            (incf (gethash tile counts) 2)
            (return-from decompose-standard-recursive result)))
        (incf (gethash tile counts) 2))
      ;; Try using as pon (if we need groups)
      (when (and (> groups-needed 0) (>= tile-count 3))
        (decf (gethash tile counts) 3)
        (let ((new-group (make-group :type :pon
                                      :tiles (list tile tile tile)
                                      :concealed-p t))
              (new-groups (cons (make-group :type :pon
                                            :tiles (list tile tile tile)
                                            :concealed-p t)
                                current-groups)))
          (let ((result (decompose-standard-recursive counts (1- groups-needed) need-pair
                                                       new-groups current-pair)))
            (when result
              (incf (gethash tile counts) 3)
              (return-from decompose-standard-recursive result))))
        (incf (gethash tile counts) 3))
      ;; Try using as chi (if suited tile and we need groups)
      (when (and (> groups-needed 0)
                 (not (honor-tile-p tile))
                 (can-form-chi-at-position-p counts tile))
        (let* ((tile2 (next-sequential-tile tile))
               (tile3 (next-sequential-tile tile2)))
          (decf (gethash tile counts))
          (decf (gethash tile2 counts))
          (decf (gethash tile3 counts))
          (let ((new-groups (cons (make-group :type :chi
                                              :tiles (list tile tile2 tile3)
                                              :concealed-p t)
                                  current-groups)))
            (let ((result (decompose-standard-recursive counts (1- groups-needed) need-pair
                                                         new-groups current-pair)))
              (when result
                (incf (gethash tile counts))
                (incf (gethash tile2 counts))
                (incf (gethash tile3 counts))
                (return-from decompose-standard-recursive result))))
          (incf (gethash tile counts))
          (incf (gethash tile2 counts))
          (incf (gethash tile3 counts))))
      ;; No valid grouping found with this tile
      nil)))

(defun identify-wait-type (groups pair-tile winning-tile)
  "Identify the wait type based on winning tile and hand structure.

   Wait types:
   - :ryanmen - Two-sided sequence wait (e.g., 4-5 waiting for 3 or 6)
   - :kanchan - Middle wait (e.g., 3-5 waiting for 4)
   - :penchan - Edge wait (e.g., 1-2 waiting for 3, or 8-9 waiting for 7)
   - :tanki - Pair wait (waiting to complete the pair)
   - :shanpon - Dual pon wait (two different pairs, waiting to make either a pon)"
  ;; Check if winning tile is the pair
  (when (eq winning-tile pair-tile)
    (return-from identify-wait-type :tanki))
  ;; Check if winning tile completes a triplet (shanpon)
  ;; This means we had a pair and winning tile made it a triplet
  (let ((winning-pon (find-if (lambda (g)
                                 (and (eq (group-type g) :pon)
                                      (eq (first (group-tiles g)) winning-tile)))
                               groups)))
    (when winning-pon
      (return-from identify-wait-type :shanpon)))
  ;; Winning tile must complete a sequence (chi)
  ;; Find which chi contains the winning tile
  (let ((winning-chi (find-if (lambda (g)
                                 (and (eq (group-type g) :chi)
                                      (member winning-tile (group-tiles g))))
                               groups)))
    (unless winning-chi
      ;; Default to tanki if we can't determine
      (return-from identify-wait-type :tanki))

    (let ((chi-tiles (sort (copy-list (group-tiles winning-chi)) #'tile<)))
      (cond
        ;; Check position of winning tile in sequence
        ((eq winning-tile (first chi-tiles))
         ;; Won on first tile (X-?-?)
         ;; Check if edge wait: must be 1-2-3 or 7-8-9
         (let ((val (tile-value (first chi-tiles))))
           (if (or (= val 1) (= val 7))
               :penchan  ; Edge wait (1-2-3 or 7-8-9)
               :ryanmen)))  ; Two-sided

        ((eq winning-tile (third chi-tiles))
         ;; Won on last tile (?-?-X)
         ;; Check if edge wait: must be 1-2-3 or 7-8-9
         (let ((val (tile-value (third chi-tiles))))
           (if (or (= val 3) (= val 9))
               :penchan  ; Edge wait (1-2-3 or 7-8-9)
               :ryanmen)))  ; Two-sided
        (t
         ;; Won on middle tile (?-X-?)
         :kanchan)))))  ; Middle wait

(defun calculate-chiitoitsu-shanten (tiles)
  "Calculate shanten for chiitoitsu (7 pairs).
   Shanten = 6 - (number of pairs) + (number of kinds > 7)

   The second term prevents using same tile 4 times as 2 pairs."
  (let ((counts (hand-to-counts tiles))
        (num-pairs 0)
        (num-kinds 0))

    (maphash (lambda (tile count)
               (declare (ignore tile))
               (incf num-kinds)
               (when (>= count 2)
                 (incf num-pairs)))
             counts)
    (let ((kinds-penalty (max 0 (- num-kinds 7))))
      (+ (- 6 num-pairs) kinds-penalty))))

(defun calculate-kokushi-shanten (tiles)
  "Calculate shanten for kokushi musou.
   Shanten = 13 - (number of unique terminals/honors) - (1 if any has pair)

   Need one of each of 13 terminal/honor types, plus a pair of any."
  (let ((counts (hand-to-counts tiles))
        (unique-yaochuuhai 0)
        (has-pair nil))
    (dolist (tile +terminals-and-honors+)
      (let ((count (gethash tile counts 0)))
        (when (> count 0)
          (incf unique-yaochuuhai)
          (when (>= count 2)
            (setf has-pair t)))))
    (- 13 unique-yaochuuhai (if has-pair 1 0))))


(defun calculate-standard-shanten (tiles melds)
  "Calculate shanten for standard pattern (4 groups + 1 pair).

   Algorithm:
   1. Melds already formed count as completed groups
   2. Find optimal decomposition of remaining tiles into:
      - Completed groups (pon/chi): worth 2 shanten reduction
      - Partial groups (taatsu): worth 1 shanten reduction
      - Pair: worth 1 shanten reduction (only one needed)
   3. Shanten = 8 - (2 × completed) - (1 × taatsu) - (1 if has pair)"

  (let* ((counts (hand-to-counts tiles))
         (num-melds (length melds))
         (groups-needed (- 4 num-melds))
         (result (find-best-standard-decomposition counts groups-needed)))
    ;; Shanten formula for standard
    (- 8
       (* 2 (getf result :completed-groups))
       (getf result :taatsu)
       (if (getf result :has-pair) 1 0))))

(defun find-best-standard-decomposition (counts groups-needed)
  "Find optimal decomposition to minimize shanten.
   Returns plist: (:completed-groups N :taatsu N :has-pair BOOL)"

  (let ((best-completed 0)
        (best-taatsu 0)
        (best-has-pair nil)
        (max-taatsu groups-needed))  ;; Can't have more taatsu than groups needed
    (labels ((search-decomposition (temp-counts completed taatsu has-pair)
               ;; Update best if this is better
               (let ((current-value (+ (* 2 completed) taatsu (if has-pair 1 0))))
                 (when (> current-value
                         (+ (* 2 best-completed) best-taatsu (if best-has-pair 1 0)))
                   (setf best-completed completed
                         best-taatsu taatsu
                         best-has-pair has-pair)))
               ;; Find next tile to process
               (let ((tile (find-if (lambda (tile-key) (> (gethash tile-key temp-counts 0) 0))
                                   +all-tiles+)))
                 (unless tile
                   (return-from search-decomposition))

                 (let ((count (gethash tile temp-counts)))
                   ;; Try removing as pon (completed group)
                   (when (and (< completed groups-needed)
                             (>= count 3))
                     (decf (gethash tile temp-counts) 3)
                     (search-decomposition temp-counts (1+ completed) taatsu has-pair)
                     (incf (gethash tile temp-counts) 3))
                   ;; Try removing as chi (completed group)
                   (when (and (< completed groups-needed)
                             (not (honor-tile-p tile))
                             (can-form-chi-at-position-p temp-counts tile))
                     (let ((t2 (next-sequential-tile tile))
                           (t3 (next-sequential-tile (next-sequential-tile tile))))
                       (decf (gethash tile temp-counts))
                       (decf (gethash t2 temp-counts))
                       (decf (gethash t3 temp-counts))
                       (search-decomposition temp-counts (1+ completed) taatsu has-pair)
                       (incf (gethash tile temp-counts))
                       (incf (gethash t2 temp-counts))
                       (incf (gethash t3 temp-counts))))
                   ;; Try removing as pair
                   (when (and (not has-pair) (>= count 2))
                     (decf (gethash tile temp-counts) 2)
                     (search-decomposition temp-counts completed taatsu t)
                     (incf (gethash tile temp-counts) 2))
                   ;; Try removing as protorun (2 sequential tiles - taatsu)
                   (when (and (< (+ completed taatsu) max-taatsu)
                             (not (honor-tile-p tile))
                             (let ((t2 (next-sequential-tile tile)))
                               (and t2 (>= (gethash t2 temp-counts 0) 1))))
                     (let ((t2 (next-sequential-tile tile)))
                       (decf (gethash tile temp-counts))
                       (decf (gethash t2 temp-counts))
                       (search-decomposition temp-counts completed (1+ taatsu) has-pair)
                       (incf (gethash tile temp-counts))
                       (incf (gethash t2 temp-counts))))
                   ;; Try removing as kanchan (gap sequence - taatsu)
                   (when (and (< (+ completed taatsu) max-taatsu)
                             (not (honor-tile-p tile))
                             (let* ((t2 (next-sequential-tile tile))
                                    (t3 (when t2 (next-sequential-tile t2))))
                               (and t3 (>= (gethash t3 temp-counts 0) 1))))
                     (let* ((t2 (next-sequential-tile tile))
                            (t3 (next-sequential-tile t2)))
                       (decf (gethash tile temp-counts))
                       (decf (gethash t3 temp-counts))
                       (search-decomposition temp-counts completed (1+ taatsu) has-pair)
                       (incf (gethash tile temp-counts))
                       (incf (gethash t3 temp-counts))))
                   ;; Try removing as pair (taatsu - can become pon)
                   (when (and (< (+ completed taatsu) max-taatsu)
                             (not has-pair)  ;; Only if no pair yet
                             (>= count 2))
                     (decf (gethash tile temp-counts) 2)
                     (search-decomposition temp-counts completed (1+ taatsu) has-pair)
                     (incf (gethash tile temp-counts) 2))
                   ;; Skip this tile (treat as isolated)
                   (decf (gethash tile temp-counts) count)
                   (search-decomposition temp-counts completed taatsu has-pair)
                   (incf (gethash tile temp-counts) count)))))
      ;; Start search with copy of counts
      (let ((temp-counts (copy-counts counts)))
        (search-decomposition temp-counts 0 0 nil)))
    (list :completed-groups best-completed
          :taatsu best-taatsu
          :has-pair best-has-pair)))

(defun calculate-shanten (tiles melds)
  "Calculate minimum shanten for hand.
   Shanten = number of tile changes to reach tenpai (0-shanten)
   Returns: integer shanten value

   For open hands, only standard pattern is possible."
  (let ((is-closed (zerop (length melds)))
        (min-shanten 8))  ; Start with max possible
    ;; Standard shanten (always calculate)
    (let ((std-shanten (calculate-standard-shanten tiles melds)))
      (when (< std-shanten min-shanten)
        (setf min-shanten std-shanten)))
    ;; Chiitoitsu and Kokushi only for closed hands
    (when is-closed
      (let ((chii-shanten (calculate-chiitoitsu-shanten tiles)))
        (when (< chii-shanten min-shanten)
          (setf min-shanten chii-shanten)))
      (let ((kokushi-shanten (calculate-kokushi-shanten tiles)))
        (when (< kokushi-shanten min-shanten)
          (setf min-shanten kokushi-shanten))))
    min-shanten))

(defun get-visible-tiles (game-state player-seat)
  "Return list of all visible tiles from player's perspective:
   - All discards from all players
   - All tiles in called melds (all players)
   - Dora indicators
   - Own hand
   Does NOT include: live wall, dead wall, opponents' concealed tiles"
  (let ((visible nil))
    ;; Collect from all players
    (loop for seat in '(:east :south :west :north)
          for player = (get-player game-state seat)
          when player do
            ;; Add discards
            (setf visible (append (player-discards player) visible))
            ;; Add meld tiles
            (dolist (meld (player-melds player))
              (setf visible (append (meld-tiles meld) visible)))
            ;; Add own hand
            (when (eq seat player-seat)
              (setf visible (append (player-hand player) visible))))
    ;; Add dora indicators
    (when (game-wall game-state)
      (setf visible (append (wall-dora-indicators (game-wall game-state)) visible)))
    visible))

(defun count-remaining-tiles (tile visible-tiles)
  "Count how many copies of TILE remain unseen.
   Each tile has 4 copies total."
  (- 4 (count tile visible-tiles :test #'eq)))


(defun calculate-ukeire (game-state player-seat)
  "Calculate ukeire (acceptance) for all possible discards.
   Returns list of:
   ((:discard TILE
     :resulting-shanten N
     :waits ((:tile TILE :count N :improves-to-shanten M) ...)) ...)

   For each possible discard:
   - Calculate what tiles would improve shanten
   - Count how many of each wait tile remain unseen
   - Return detailed wait information"

  (let* ((player (get-player game-state player-seat))
         (hand (player-hand player))
         (melds (player-melds player))
         (visible (get-visible-tiles game-state player-seat))
         (results nil))
    ;; For each possible discard
    (dolist (discard-tile (remove-duplicates hand :test #'eq))
      (let* ((new-hand (remove discard-tile hand :test #'eq :count 1))
             (current-shanten (calculate-shanten new-hand melds))
             (waits nil))
        ;; Try adding each possible tile
        (dolist (add-tile +all-tiles+)
          (let* ((test-hand (cons add-tile new-hand))
                 (new-shanten (calculate-shanten test-hand melds))
                 (remaining (count-remaining-tiles add-tile visible)))
            ;; Only record if it improves shanten and tiles remain
            (when (and (< new-shanten current-shanten)
                      (> remaining 0))
              (push (list :tile add-tile
                         :count remaining
                         :improves-to-shanten new-shanten)
                    waits))))
        (push (list :discard discard-tile
                   :resulting-shanten current-shanten
                   :waits (sort waits #'> :key (lambda (w) (getf w :count))))
               results)))
    ;; Sort by best shanten first, then by most waits
    (sort results (lambda (a b)
                    (let ((sha (getf a :resulting-shanten))
                          (shb (getf b :resulting-shanten)))
                      (or (< sha shb)
                          (and (= sha shb)
                               (> (length (getf a :waits))
                                  (length (getf b :waits))))))))))
