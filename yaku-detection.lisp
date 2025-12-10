;; yaku-detection.lisp
;;
;; Each yaku has a detection function that returns:
;; - NIL if yaku is not present
;; - Han value (integer) if yaku is present
;; The main entry point is detect-yaku which coordinates all detection.
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


(defun make-tile-keyword (suit value)
  "Create a tile keyword from suit (m/p/s) and value (1-9)"
  (intern (format nil "~A~A" value suit) :keyword))

(defun tile< (tile1 tile2)
  "Compare two tiles for sorting (suited tiles by value, honors by type)"
  (let ((suit1 (tile-suit tile1))
        (suit2 (tile-suit tile2))
        (val1 (tile-value tile1))
        (val2 (tile-value tile2)))
    (cond
      ;; Both suited tiles
      ((and val1 val2)
       (cond
         ;; Different suits
         ((not (eq suit1 suit2))
          (string< (symbol-name suit1) (symbol-name suit2)))
         ;; Same suit, compare values
         (t (< val1 val2))))
      ;; One or both honors - sort by symbol name
      (t (string< (symbol-name tile1) (symbol-name tile2))))))

(defun yaochuuhai-p (tile)
  "Check if tile is terminal or honor (yaochuuhai)"
  (or (terminal-tile-p tile) (honor-tile-p tile)))

(defun all-tiles-in-hand (hand melds)
  "Return all tiles including both hand and meld tiles"
  (append hand
          (loop for meld in melds
                append (meld-tiles meld))))

(defun hand-is-closed-p (melds)
  "Check if hand is closed (no open melds)"
  (zerop (length melds)))

(defun count-groups-by-type (groups group-type)
  "Count how many groups match the given type (:pon :chi :kan)"
  (count-if (lambda (g) (eq (group-type g) group-type)) groups))

(defun find-groups-by-tile (groups tile-predicate)
  "Find all groups where tiles satisfy predicate"
  (remove-if-not (lambda (g)
                   (every tile-predicate (group-tiles g)))
                 groups))


(defun yaku-riichi-p (context)
  "Riichi (立直) - 1 han
   Declared riichi"
  (when (winning-context-riichi-declared-p context)
    1))

(defun yaku-double-riichi-p (context)
  "Double Riichi (両立直) - 2 han
   Riichi declared on first turn
   Replaces regular riichi (not stackable)"
  (when (winning-context-double-riichi-p context)
    2))

(defun yaku-ippatsu-p (context)
  "Ippatsu (一発) - 1 han
   Win within one turn cycle after declaring riichi
   Requires riichi to be active"
  (when (and (winning-context-riichi-declared-p context)
             (winning-context-ippatsu-active-p context))
    1))

(defun yaku-menzen-tsumo-p (hand melds context)
  "Menzen Tsumo (門前清自摸和) - 1 han
   Self-draw with closed hand"
  (when (and (winning-context-tsumo-p context)
             (hand-is-closed-p melds))
    1))

(defun yaku-tanyao-p (hand melds)
  "Tanyao (断么九) - 1 han
   All simples: all tiles are 2-8, no terminals or honors"
  (let ((all-tiles (all-tiles-in-hand hand melds)))
    (when (every #'simple-tile-p all-tiles)
      1)))

(defun yaku-yakuhai-p (hand melds decomposition context)
  "Yakuhai (役牌) - 1 han per triplet/kan of value tiles
   Value tiles: seat wind, round wind, dragons
   Note: A wind that is both seat and round wind counts as 2 han"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((total-han 0))
      ;; Check each group for value tiles
      (dolist (group (hand-decomposition-groups decomposition))
        ;; Only triplets and kans count
        (when (member (group-type group) '(:pon :kan))
          (let ((tile (first (group-tiles group))))
            ;; Dragons are always 1 han each
            (cond
              ((eq tile :white) (incf total-han))
              ((eq tile :green) (incf total-han))
              ((eq tile :red) (incf total-han))
              ;; Winds: check if seat or round wind
              ((member tile '(:east :south :west :north))
               (let ((han 0))
                 (when (eq tile (winning-context-seat-wind context))
                   (incf han))
                 (when (eq tile (winning-context-round-wind context))
                   (incf han))
                 (incf total-han han)))))))
      ;; Return total han if any value tiles found
      (when (> total-han 0)
        total-han))))

(defun yaku-toitoi-p (decomposition)
  "Toitoi (対々和) - 2 han
   All groups are triplets (pon/kan), no sequences"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((groups (hand-decomposition-groups decomposition)))
      ;; All groups must be pon or kan (no chi)
      (when (every (lambda (g) (member (group-type g) '(:pon :kan)))
                   groups)
        2))))

(defun yaku-pinfu-p (hand melds decomposition)
  "Pinfu (平和) - 1 han
   All sequences, ryanmen wait, no fu from groups/pair
   Requirements:
   - Closed hand only
   - All groups must be chi (sequences)
   - Ryanmen wait (two-sided)
   - Pair must not be value tiles (dragons or seat/round wind)"
  (when (and decomposition
             (hand-is-closed-p melds)
             (eq (hand-decomposition-pattern-type decomposition) :standard)
             (eq (hand-decomposition-wait-type decomposition) :ryanmen))
    (let ((groups (hand-decomposition-groups decomposition)))
      ;; All groups must be chi (sequences)
      (when (every (lambda (g) (eq (group-type g) :chi)) groups)
        1))))

(defun yaku-iipeikou-p (melds decomposition)
  "Iipeikou (一盃口) - 1 han
   One set of identical sequences (closed hand only)"
  (when (and decomposition
             (hand-is-closed-p melds)
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (chi-groups (remove-if-not (lambda (g) (eq (group-type g) :chi)) groups))
           (chi-sequences (mapcar (lambda (g) (sort (copy-list (group-tiles g)) #'tile<))
                                  chi-groups)))
      ;; Check for duplicate sequences
      (loop for i from 0 below (length chi-sequences)
            for seq1 = (nth i chi-sequences)
            do (loop for j from (1+ i) below (length chi-sequences)
                     for seq2 = (nth j chi-sequences)
                     when (equal seq1 seq2)
                       do (return-from yaku-iipeikou-p 1)))
      nil)))

(defun yaku-chanta-p (melds decomposition)
  "Chanta (混全帯么九) - 2 han closed, 1 han open
   Terminal or honor in every group and the pair"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((groups (hand-decomposition-groups decomposition))
          (pair-tile (hand-decomposition-pair decomposition)))
      ;; Pair must be terminal or honor
      (unless (yaochuuhai-p pair-tile)
        (return-from yaku-chanta-p nil))

      ;; Every group must contain at least one terminal/honor
      (unless (every (lambda (g)
                       (some #'yaochuuhai-p (group-tiles g)))
                     groups)
        (return-from yaku-chanta-p nil))

      ;; Must contain at least one sequence (not all triplets = honroutou)
      (unless (some (lambda (g) (eq (group-type g) :chi)) groups)
        (return-from yaku-chanta-p nil))

      ;; 2 han if closed, 1 han if open
      (if (hand-is-closed-p melds) 2 1))))

(defun yaku-chiitoitsu-p (decomposition)
  "Chiitoitsu (七対子) - 2 han
   Seven different pairs (always closed)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :chiitoitsu))
    2))

(defun yaku-honroutou-p (hand melds decomposition)
  "Honroutou (混老頭) - 2 han
   All tiles are terminals or honors (all triplets + pair)
   This is essentially toitoi + all terminals/honors"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((all-tiles (all-tiles-in-hand hand melds)))
      ;; All tiles must be terminals or honors
      (when (every #'yaochuuhai-p all-tiles)
        2))))

(defun yaku-shousangen-p (decomposition)
  "Shousangen (小三元) - 2 han
   Two dragon triplets + one dragon pair"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (pair-tile (hand-decomposition-pair decomposition))
           (dragon-pons (count-if (lambda (g)
                                    (and (member (group-type g) '(:pon :kan))
                                         (member (first (group-tiles g))
                                                 '(:white :green :red))))
                                  groups))
           (dragon-pair (member pair-tile '(:white :green :red))))
      (when (and (= dragon-pons 2) dragon-pair)
        2))))

(defun yaku-sanshoku-doujun-p (melds decomposition)
  "Sanshoku Doujun (三色同順) - 2 han closed, 1 han open
   Same sequence (e.g., 1-2-3) in all three suits"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (chi-groups (remove-if-not (lambda (g) (eq (group-type g) :chi)) groups)))
      ;; Need at least 3 sequences
      (when (< (length chi-groups) 3)
        (return-from yaku-sanshoku-doujun-p nil))
      ;; Check for same number sequence in all 3 suits
      (loop for chi in chi-groups
            for tiles = (group-tiles chi)
            for val1 = (tile-value (first tiles))
            when val1
              do (let ((man-seq (list (make-tile-keyword 'm val1)
                                      (make-tile-keyword 'm (+ val1 1))
                                      (make-tile-keyword 'm (+ val1 2))))
                       (pin-seq (list (make-tile-keyword 'p val1)
                                      (make-tile-keyword 'p (+ val1 1))
                                      (make-tile-keyword 'p (+ val1 2))))
                       (sou-seq (list (make-tile-keyword 's val1)
                                      (make-tile-keyword 's (+ val1 1))
                                      (make-tile-keyword 's (+ val1 2)))))
                   (when (and (find-if (lambda (g)
                                         (equal (sort (copy-list (group-tiles g)) #'tile<)
                                                (sort (copy-list man-seq) #'tile<)))
                                       chi-groups)
                              (find-if (lambda (g)
                                         (equal (sort (copy-list (group-tiles g)) #'tile<)
                                                (sort (copy-list pin-seq) #'tile<)))
                                       chi-groups)
                              (find-if (lambda (g)
                                         (equal (sort (copy-list (group-tiles g)) #'tile<)
                                                (sort (copy-list sou-seq) #'tile<)))
                                       chi-groups))
                     (return-from yaku-sanshoku-doujun-p
                       (if (hand-is-closed-p melds) 2 1)))))
      nil)))

(defun yaku-ittsu-p (melds decomposition)
  "Ittsu (一気通貫) - 2 han closed, 1 han open
   Sequences 1-2-3, 4-5-6, 7-8-9 in same suit"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (chi-groups (remove-if-not (lambda (g) (eq (group-type g) :chi)) groups)))
      ;; Need at least 3 sequences
      (when (< (length chi-groups) 3)
        (return-from yaku-ittsu-p nil))
      ;; Check each suit for 1-2-3, 4-5-6, 7-8-9
      (dolist (suit '(m p s))
        (let ((seq123 (list (make-tile-keyword suit 1)
                            (make-tile-keyword suit 2)
                            (make-tile-keyword suit 3)))
              (seq456 (list (make-tile-keyword suit 4)
                            (make-tile-keyword suit 5)
                            (make-tile-keyword suit 6)))
              (seq789 (list (make-tile-keyword suit 7)
                            (make-tile-keyword suit 8)
                            (make-tile-keyword suit 9))))
          (when (and (find-if (lambda (g)
                                (equal (sort (copy-list (group-tiles g)) #'tile<)
                                       (sort (copy-list seq123) #'tile<)))
                              chi-groups)
                     (find-if (lambda (g)
                                (equal (sort (copy-list (group-tiles g)) #'tile<)
                                       (sort (copy-list seq456) #'tile<)))
                              chi-groups)
                     (find-if (lambda (g)
                                (equal (sort (copy-list (group-tiles g)) #'tile<)
                                       (sort (copy-list seq789) #'tile<)))
                              chi-groups))
            (return-from yaku-ittsu-p
              (if (hand-is-closed-p melds) 2 1)))))
      nil)))

(defun yaku-sanankou-p (decomposition winning-tile context)
  "Sanankou (三暗刻) - 2 han
   Three concealed triplets (pon/kan)
   Note: On tsumo, all triplets are concealed
   On ron, winning triplet doesn't count as concealed"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (is-tsumo (winning-context-tsumo-p context))
           (pon-groups (remove-if-not (lambda (g)
                                        (member (group-type g) '(:pon :kan)))
                                      groups))
           (concealed-pons (if is-tsumo
                               ;; Tsumo: all concealed triplets count
                               (count-if #'group-concealed-p pon-groups)
                               ;; Ron: winning triplet doesn't count
                               (count-if (lambda (g)
                                           (and (group-concealed-p g)
                                                (not (eq (first (group-tiles g))
                                                         winning-tile))))
                                         pon-groups))))
      (when (>= concealed-pons 3)
        2))))

(defun yaku-sankantsu-p (decomposition)
  "Sankantsu (三槓子) - 2 han
   Three kans (open or closed)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((groups (hand-decomposition-groups decomposition)))
      (when (>= (count-groups-by-type groups :kan) 3)
        2))))


(defun yaku-honitsu-p (hand melds decomposition)
  "Honitsu (混一色) - 3 han closed, 2 han open
   One suit + honors (no other suits)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((all-tiles (all-tiles-in-hand hand melds))
           ;; Only count suited tiles (not honors)
           (suited-tiles (remove-if #'honor-tile-p all-tiles))
           (suits (remove-duplicates
                   (mapcar #'tile-suit suited-tiles)))
           (has-honors (some #'honor-tile-p all-tiles)))
      ;; Must have exactly one suit + honors
      (when (and (= (length suits) 1)
                 has-honors)
        (if (hand-is-closed-p melds) 3 2)))))

(defun yaku-chinitsu-p (hand melds decomposition)
  "Chinitsu (清一色) - 6 han closed, 5 han open
   One suit only (no honors)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((all-tiles (all-tiles-in-hand hand melds))
           ;; Only count suited tiles (not honors)
           (suited-tiles (remove-if #'honor-tile-p all-tiles))
           (suits (remove-duplicates
                   (mapcar #'tile-suit suited-tiles)))
           (has-honors (some #'honor-tile-p all-tiles)))
      ;; Must have exactly one suit and no honors
      (when (and (= (length suits) 1)
                 (not has-honors))
        (if (hand-is-closed-p melds) 6 5)))))

(defun yaku-junchan-p (melds decomposition)
  "Junchan (純全帯么九) - 3 han closed, 2 han open
   Terminal in every group (no honors)
   Like chanta but with terminals only, no honors"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((groups (hand-decomposition-groups decomposition))
          (pair-tile (hand-decomposition-pair decomposition)))
      ;; Pair must be terminal (not honor)
      (unless (terminal-tile-p pair-tile)
        (return-from yaku-junchan-p nil))

      ;; Every group must contain at least one terminal
      (unless (every (lambda (g)
                       (some #'terminal-tile-p (group-tiles g)))
                     groups)
        (return-from yaku-junchan-p nil))

      ;; Must contain at least one sequence (not all triplets)
      (unless (some (lambda (g) (eq (group-type g) :chi)) groups)
        (return-from yaku-junchan-p nil))

      ;; Must not contain any honors
      (let ((all-tiles (append (list pair-tile)
                               (loop for g in groups
                                     append (group-tiles g)))))
        (when (notany #'honor-tile-p all-tiles)
          (if (hand-is-closed-p melds) 3 2))))))

(defun yaku-ryanpeikou-p (melds decomposition)
  "Ryanpeikou (二盃口) - 3 han
   Two sets of identical sequences (closed hand only)
   Note: Replaces iipeikou when detected"
  (when (and decomposition
             (hand-is-closed-p melds)
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (chi-groups (remove-if-not (lambda (g) (eq (group-type g) :chi)) groups))
           (chi-sequences (mapcar (lambda (g) (sort (copy-list (group-tiles g)) #'tile<))
                                  chi-groups))
           (pairs-found 0))
      ;; Need at least 4 sequences for 2 pairs
      (when (< (length chi-sequences) 4)
        (return-from yaku-ryanpeikou-p nil))
      ;; Count pairs of identical sequences
      (let ((used (make-hash-table :test 'equal)))
        (loop for i from 0 below (length chi-sequences)
              for seq1 = (nth i chi-sequences)
              unless (gethash seq1 used)
                do (let ((matches 0))
                     ;; Count how many times this sequence appears
                     (loop for j from 0 below (length chi-sequences)
                           for seq2 = (nth j chi-sequences)
                           when (equal seq1 seq2)
                             do (incf matches))
                     ;; If we have a pair (2 or more), mark them as used
                     (when (>= matches 2)
                       (incf pairs-found)
                       (setf (gethash seq1 used) t)))))

      ;; Need exactly 2 pairs (4 sequences total in 2 pairs)
      (when (>= pairs-found 2)
        3))))

(defun yaku-sanshoku-doukou-p (decomposition)
  "Sanshoku Doukou (三色同刻) - 2 han
   Same triplet (same number) in all three suits"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (pon-groups (remove-if-not (lambda (g)
                                        (member (group-type g) '(:pon :kan)))
                                      groups)))
      ;; Need at least 3 triplets
      (when (< (length pon-groups) 3)
        (return-from yaku-sanshoku-doukou-p nil))
      ;; Check each triplet value (1-9)
      (loop for value from 1 to 9
            do (let ((man-pon (find-if (lambda (g)
                                         (and (member (group-type g) '(:pon :kan))
                                              (eq (first (group-tiles g))
                                                  (make-tile-keyword 'm value))))
                                       pon-groups))
                     (pin-pon (find-if (lambda (g)
                                         (and (member (group-type g) '(:pon :kan))
                                              (eq (first (group-tiles g))
                                                  (make-tile-keyword 'p value))))
                                       pon-groups))
                     (sou-pon (find-if (lambda (g)
                                         (and (member (group-type g) '(:pon :kan))
                                              (eq (first (group-tiles g))
                                                  (make-tile-keyword 's value))))
                                       pon-groups)))
                 (when (and man-pon pin-pon sou-pon)
                   (return-from yaku-sanshoku-doukou-p 2))))
      nil)))


(defun yaku-kokushi-p (decomposition)
  "Kokushi Musou (国士無双) - 13 han (yakuman)
   13 orphans: one of each terminal/honor + pair of any
   Double yakuman (26 han) if 13-way wait"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :kokushi))
    ;; Check if 13-way wait (special case for double yakuman)
    (if (eq (hand-decomposition-wait-type decomposition) :kokushi-13)
        26  ; Double yakuman
        13)))  ; Regular yakuman

(defun yaku-suuankou-p (decomposition winning-tile context)
  "Suuankou (四暗刻) - 13 han (yakuman)
   Four concealed triplets
   Double yakuman (26 han) if tanki wait"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard)
             (winning-context-tsumo-p context))  ; Must be tsumo
    (let* ((groups (hand-decomposition-groups decomposition))
           (pon-groups (remove-if-not (lambda (g)
                                        (member (group-type g) '(:pon :kan)))
                                      groups))
           (concealed-pons (count-if #'group-concealed-p pon-groups)))
      (when (= concealed-pons 4)
        ;; Check if tanki wait (pair wait) for double yakuman
        (if (eq (hand-decomposition-wait-type decomposition) :tanki)
            26  ; Double yakuman
            13)))))  ; Regular yakuman

(defun yaku-daisangen-p (decomposition)
  "Daisangen (大三元) - 13 han (yakuman)
   Triplets of all three dragons"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (dragon-pons (count-if (lambda (g)
                                    (and (member (group-type g) '(:pon :kan))
                                         (member (first (group-tiles g))
                                                 '(:white :green :red))))
                                  groups)))
      (when (= dragon-pons 3)
        13))))

(defun yaku-shousuushii-p (decomposition)
  "Shousuushii (小四喜) - 13 han (yakuman)
   Triplets of three winds + pair of fourth wind"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (pair-tile (hand-decomposition-pair decomposition))
           (wind-pons (count-if (lambda (g)
                                  (and (member (group-type g) '(:pon :kan))
                                       (member (first (group-tiles g))
                                               '(:east :south :west :north))))
                                groups))
           (wind-pair (member pair-tile '(:east :south :west :north))))
      (when (and (= wind-pons 3)
                 wind-pair)
        13))))

(defun yaku-daisuushii-p (decomposition)
  "Daisuushii (大四喜) - 26 han (double yakuman)
   Triplets of all four winds"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((groups (hand-decomposition-groups decomposition))
           (wind-pons (count-if (lambda (g)
                                  (and (member (group-type g) '(:pon :kan))
                                       (member (first (group-tiles g))
                                               '(:east :south :west :north))))
                                groups)))
      (when (= wind-pons 4)
        26))))  ; Double yakuman

(defun yaku-tsuuiisou-p (hand melds decomposition)
  "Tsuuiisou (字一色) - 13 han (yakuman)
   All honors (winds and dragons only)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((all-tiles (all-tiles-in-hand hand melds)))
      (when (every #'honor-tile-p all-tiles)
        13))))

(defun yaku-chinroutou-p (hand melds decomposition)
  "Chinroutou (清老頭) - 13 han (yakuman)
   All terminals (1s and 9s only, no honors)"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((all-tiles (all-tiles-in-hand hand melds)))
      ;; All tiles must be terminals and no honors
      (when (and (every #'terminal-tile-p all-tiles)
                 (notany #'honor-tile-p all-tiles))
        13))))

(defun yaku-ryuuiisou-p (hand melds decomposition)
  "Ryuuiisou (緑一色) - 13 han (yakuman)
   All green tiles: 2s, 3s, 4s, 6s, 8s, green dragon"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((all-tiles (all-tiles-in-hand hand melds))
           (green-tiles '(:2s :3s :4s :6s :8s :green)))
      (when (every (lambda (tile) (member tile green-tiles)) all-tiles)
        13))))

(defun yaku-chuuren-poutou-p (hand melds decomposition winning-tile)
  "Chuuren Poutou (九蓮宝燈) - 13 han (yakuman)
   Pure hand with 1112345678999 + any tile of same suit
   Double yakuman (26 han) if 9-way wait (waiting on any tile 1-9)
   Must be closed hand"
  (when (and decomposition
             (hand-is-closed-p melds)
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let* ((all-tiles (all-tiles-in-hand hand melds))
           ;; Check if pure suit (one suit, no honors)
           (suited-tiles (remove-if #'honor-tile-p all-tiles))
           (suits (remove-duplicates (mapcar #'tile-suit suited-tiles)))
           (has-honors (some #'honor-tile-p all-tiles)))
      (when (and (= (length suits) 1)
                 (not has-honors))
        ;; Count tiles by value for the suit
        (let* ((suit (first suits))
               (counts (make-hash-table))
               (pattern-1112345678999 t))
          ;; Initialize counts
          (loop for val from 1 to 9
                do (setf (gethash val counts) 0))
          ;; Count actual tiles
          (dolist (tile all-tiles)
            (let ((val (tile-value tile)))
              (when val
                (incf (gethash val counts)))))

          ;; Check for 1112345678999 pattern
          ;; Should have: 3x1, 1x2-8, 3x9, plus one extra tile
          (unless (and (>= (gethash 1 counts) 3)
                       (>= (gethash 9 counts) 3)
                       (>= (gethash 2 counts) 1)
                       (>= (gethash 3 counts) 1)
                       (>= (gethash 4 counts) 1)
                       (>= (gethash 5 counts) 1)
                       (>= (gethash 6 counts) 1)
                       (>= (gethash 7 counts) 1)
                       (>= (gethash 8 counts) 1))
            (return-from yaku-chuuren-poutou-p nil))

          ;; Check if 9-way wait (pure junsei chuuren)
          ;; This means before winning tile, we had exactly 1112345678999
          ;; So winning tile can be any 1-9 of the suit
          (let ((nine-way nil))
            ;; Remove the winning tile and check if we have exact pattern
            (let ((val (tile-value winning-tile)))
              (when val
                (decf (gethash val counts))
                (setf nine-way
                      (and (= (gethash 1 counts) 3)
                           (= (gethash 2 counts) 1)
                           (= (gethash 3 counts) 1)
                           (= (gethash 4 counts) 1)
                           (= (gethash 5 counts) 1)
                           (= (gethash 6 counts) 1)
                           (= (gethash 7 counts) 1)
                           (= (gethash 8 counts) 1)
                           (= (gethash 9 counts) 3)))))

            (if nine-way
                26  ; Double yakuman (junsei chuuren)
                13)))))))  ; Regular yakuman

(defun yaku-suukantsu-p (decomposition)
  "Suukantsu (四槓子) - 13 han (yakuman)
   Four kans"
  (when (and decomposition
             (eq (hand-decomposition-pattern-type decomposition) :standard))
    (let ((groups (hand-decomposition-groups decomposition)))
      (when (= (count-groups-by-type groups :kan) 4)
        13))))

(defun yaku-tenhou-p (context)
  "Tenhou (天和) - 13 han (yakuman)
   Dealer wins on initial deal (before any discards)"
  (when (and (winning-context-dealer-p context)
             (winning-context-tsumo-p context)
             (winning-context-tenhou-p context))
    13))

(defun yaku-chiihou-p (context)
  "Chiihou (地和) - 13 han (yakuman)
   Non-dealer wins on first draw (before own first discard)"
  (when (and (not (winning-context-dealer-p context))
             (winning-context-tsumo-p context)
             (winning-context-chiihou-p context))
    13))


(defun yaku-rinshan-kaihou-p (context)
  "Rinshan Kaihou (嶺上開花) - 1 han
   Win on kan replacement tile"
  (when (winning-context-rinshan-p context)
    1))

(defun yaku-chankan-p (context)
  "Chankan (槍槓) - 1 han
   Rob the kan (win on someone else's added kan tile)"
  (when (winning-context-chankan-p context)
    1))

(defun yaku-haitei-raoyue-p (context)
  "Haitei Raoyue (海底摸月) - 1 han
   Win on last tile from wall (tsumo)"
  (when (winning-context-haitei-p context)
    1))

(defun yaku-houtei-raoyui-p (context)
  "Houtei Raoyui (河底撈魚) - 1 han
   Win on last discard (ron)"
  (when (winning-context-houtei-p context)
    1))


(defmacro check-yaku (yaku-name yaku-function &rest args)
  "Macro to check a single yaku and add it to yaku-list if present.

   Usage: (check-yaku :riichi yaku-riichi-p context)"
  (let ((han-var (gensym "HAN")))
    `(let ((,han-var (,yaku-function ,@args)))
       (when ,han-var
         (push (list ,yaku-name ,han-var) yaku-list)
         (incf total-han ,han-var)))))

(defmacro check-yaku-exclusive (higher-name higher-fn lower-name lower-fn &rest args)
  "Macro for mutually exclusive yaku (check higher priority first).
   If higher yaku exists, skip lower. Otherwise check lower.

   Usage: (check-yaku-exclusive :daisuushii yaku-daisuushii-p
                                  :shousuushii yaku-shousuushii-p
                                  decomposition)"
  (let ((higher-han (gensym "HIGHER-HAN"))
        (lower-han (gensym "LOWER-HAN")))
    `(let ((,higher-han (,higher-fn ,@args)))
       (if ,higher-han
           (progn
             (push (list ,higher-name ,higher-han) yaku-list)
             (incf total-han ,higher-han))
           ;; Only check lower if higher not present
           (let ((,lower-han (,lower-fn ,@args)))
             (when ,lower-han
               (push (list ,lower-name ,lower-han) yaku-list)
               (incf total-han ,lower-han)))))))


(defun detect-yaku (hand melds winning-tile context &optional decomposition)
  "Detect all applicable yaku in a winning hand.

   Arguments:
   - HAND: List of concealed tiles
   - MELDS: List of meld structs
   - WINNING-TILE: The tile that completed the hand
   - CONTEXT: winning-context struct
   - DECOMPOSITION: Optional hand-decomposition struct (for fu-dependent yaku)

   Returns: (values yaku-list total-han)
   where yaku-list is ((:yaku-name han-value) ...)"

  ;; Generate decomposition if not provided
  (unless decomposition
    (setf decomposition (decompose-hand hand melds winning-tile)))

  (let ((yaku-list nil)
        (total-han 0))
    (check-yaku :tenhou yaku-tenhou-p context)
    (check-yaku :chiihou yaku-chiihou-p context)
    (check-yaku :kokushi yaku-kokushi-p decomposition)
    (check-yaku :suuankou yaku-suuankou-p decomposition winning-tile context)
    (check-yaku-exclusive :daisuushii yaku-daisuushii-p
                          :shousuushii yaku-shousuushii-p
                          decomposition)
    (check-yaku :daisangen yaku-daisangen-p decomposition)
    (check-yaku :tsuuiisou yaku-tsuuiisou-p hand melds decomposition)
    (check-yaku :chinroutou yaku-chinroutou-p hand melds decomposition)
    (check-yaku :ryuuiisou yaku-ryuuiisou-p hand melds decomposition)
    (check-yaku :chuuren-poutou yaku-chuuren-poutou-p hand melds decomposition winning-tile)
    (check-yaku :suukantsu-yakuman yaku-suukantsu-p decomposition)
    (check-yaku-exclusive :double-riichi yaku-double-riichi-p
                          :riichi yaku-riichi-p
                          context)
    (check-yaku :ippatsu yaku-ippatsu-p context)
    (check-yaku :menzen-tsumo yaku-menzen-tsumo-p hand melds context)
    (check-yaku :tanyao yaku-tanyao-p hand melds)
    (check-yaku :yakuhai yaku-yakuhai-p hand melds decomposition context)
    (check-yaku :toitoi yaku-toitoi-p decomposition)
    (check-yaku :pinfu yaku-pinfu-p hand melds decomposition)
    (check-yaku-exclusive :ryanpeikou yaku-ryanpeikou-p
                          :iipeikou yaku-iipeikou-p
                          melds decomposition)
    (check-yaku :chanta yaku-chanta-p melds decomposition)
    (check-yaku :chiitoitsu yaku-chiitoitsu-p decomposition)
    (check-yaku :honroutou yaku-honroutou-p hand melds decomposition)
    (check-yaku :shousangen yaku-shousangen-p decomposition)
    (check-yaku :sanshoku-doujun yaku-sanshoku-doujun-p melds decomposition)
    (check-yaku :ittsu yaku-ittsu-p melds decomposition)
    (check-yaku :sanankou yaku-sanankou-p decomposition winning-tile context)
    (check-yaku :sankantsu yaku-sankantsu-p decomposition)
    (check-yaku :honitsu yaku-honitsu-p hand melds decomposition)
    (check-yaku :chinitsu yaku-chinitsu-p hand melds decomposition)
    (check-yaku :junchan yaku-junchan-p melds decomposition)
    (check-yaku :sanshoku-doukou yaku-sanshoku-doukou-p decomposition)
    (check-yaku :rinshan-kaihou yaku-rinshan-kaihou-p context)
    (check-yaku :chankan yaku-chankan-p context)
    (check-yaku :haitei-raoyue yaku-haitei-raoyue-p context)
    (check-yaku :houtei-raoyui yaku-houtei-raoyui-p context)
    (values (sort yaku-list #'> :key #'second)
            total-han)))
