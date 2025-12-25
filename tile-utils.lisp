;; tile-utils.lisp
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

(defun tile-suit (tile)
  "Returns the suit of a tile: :man :pin :sou :wind or :dragon"
  (let ((name (symbol-name tile)))
    (cond
      ((find (char name (1- (length name))) "mMpPsS")
       (case (char-upcase (char name (1- (length name))))
         (#\M :man)
         (#\P :pin)
         (#\S :sou)))
      ((member tile '(:east :south :west :north)) :wind)
      ((member tile '(:white :green :red)) :dragon))))

(defun tile-value (tile)
  "Returns numeric value 1-9 for suited tiles, nil for honors"
  (let ((name (symbol-name tile)))
    (when (find (char name (1- (length name))) "mMpPsS")
      (parse-integer name :junk-allowed t))))

(defun red-five-p (tile)
  "Returns T if tile is a red five (aka-dora)"
  (member tile '(:5m-r :5p-r :5s-r)))

(defun normalize-red-five (tile)
  "Convert red five to normal five for pattern matching.
   :5m-r -> :5m, :5p-r -> :5p, :5s-r -> :5s"
  (case tile
    (:5m-r :5m)
    (:5p-r :5p)
    (:5s-r :5s)
    (t tile)))

(defun honor-tile-p (tile)
  "Returns T if tile is wind or dragon"
  (member (tile-suit tile) '(:wind :dragon)))

(defun terminal-tile-p (tile)
  "Returns T if tile is 1 or 9 of a suit"
  (let ((val (tile-value tile)))
    (and val (or (= val 1) (= val 9)))))

(defun simple-tile-p (tile)
  "Returns T if tile is 2-8 of a suit"
  (let ((val (tile-value tile)))
    (and val (>= val 2) (<= val 8))))


(defun tile-sort-order (tile)
  "Return numeric sort order for tile"
  (let ((suit (tile-suit tile))
        (val (tile-value tile)))
    (cond
      ((eq suit :man) (+ 0 val))
      ((eq suit :pin) (+ 10 val))
      ((eq suit :sou) (+ 20 val))
      ((eq tile :east) 31)
      ((eq tile :south) 32)
      ((eq tile :west) 33)
      ((eq tile :north) 34)
      ((eq tile :white) 35)
      ((eq tile :green) 36)
      ((eq tile :red) 37))))

(defun sort-tiles (tiles)
  "Sort tiles in standard order (man, pin, sou, winds, dragons)"
  (sort (copy-list tiles) #'< :key #'tile-sort-order))


(defun count-tiles (tiles)
  "Return hash table mapping tile -> count"
  (let ((counts (make-hash-table :test 'eq)))
    (dolist (tile tiles)
      (incf (gethash tile counts 0)))
    counts))

(defun tiles-of-type (tiles tile-type)
  "Return all tiles matching TILE-TYPE"
  (remove-if-not (lambda (tile) (eq tile tile-type)) tiles))


(defun next-tile (tile)
  "Return the tile that follows the indicator (dora).
   For suited tiles: 9 wraps to 1
   For winds: cycles east->south->west->north->east
   For dragons: cycles white->green->red->white"
  (let ((suit (tile-suit tile))
        (val (tile-value tile)))
    (cond
      ;; Suited tiles: wrap 9 -> 1
      ((member suit '(:man :pin :sou))
       (intern (format nil "~D~A"
                      (if (= val 9) 1 (1+ val))
                      (case suit
                        (:man "M")
                        (:pin "P")
                        (:sou "S")))
              :keyword))
      ;; Winds: cycle east->south->west->north->east
      ((eq suit :wind)
       (case tile
         (:east :south)
         (:south :west)
         (:west :north)
         (:north :east)))
      ;; Dragons: cycle white->green->red->white
      ((eq suit :dragon)
       (case tile
         (:white :green)
         (:green :red)
         (:red :white))))))

(defun calculate-dora (indicators)
  "Calculate actual dora tiles from indicators"
  (mapcar #'next-tile indicators))


(defun find-triplet (hand tile)
  "Check if player has at least 2 more of TILE for pon (returns count)"
  (let ((count (count tile hand :test #'eq)))
    (when (>= count 2)
      count)))

(defun find-sequence-chi (hand tile seat last-discard-seat &optional config)
  "Check if player can call chi with TILE. Returns list of possible sequences.
   Chi can only be called by the next player (counter-clockwise)."
  ;; Early return if chi disabled in config
  (when (and config (not (game-config-chi-enabled-p config)))
    (return-from find-sequence-chi nil))
  ;; Check seat relation
  (when (not (eq seat (next-seat last-discard-seat config)))
    (return-from find-sequence-chi nil))
  (let ((suit (tile-suit tile))
        (val (tile-value tile))
        sequences)
    ;; Can only chi suited tiles
    (when (and suit (member suit '(:man :pin :sou)))
      ;; Check for sequence with tile in middle (e.g., tile=5, need 4 and 6)
      (when (and (>= val 2) (<= val 8))
        (let ((before (intern (format nil "~D~A" (1- val)
                                     (case suit (:man "M") (:pin "P") (:sou "S")))
                             :keyword))
              (after (intern (format nil "~D~A" (1+ val)
                                    (case suit (:man "M") (:pin "P") (:sou "S")))
                            :keyword)))
          (when (and (member before hand) (member after hand))
            (push (list before tile after) sequences))))
      ;; Check for sequence with tile at start (e.g., tile=3, need 4 and 5)
      (when (<= val 7)
        (let ((next1 (intern (format nil "~D~A" (1+ val)
                                    (case suit (:man "M") (:pin "P") (:sou "S")))
                            :keyword))
              (next2 (intern (format nil "~D~A" (+ val 2)
                                    (case suit (:man "M") (:pin "P") (:sou "S")))
                            :keyword)))
          (when (and (member next1 hand) (member next2 hand))
            (push (list tile next1 next2) sequences))))
      ;; Check for sequence with tile at end (e.g., tile=7, need 5 and 6)
      (when (>= val 3)
        (let ((prev1 (intern (format nil "~D~A" (1- val)
                                    (case suit (:man "M") (:pin "P") (:sou "S")))
                            :keyword))
              (prev2 (intern (format nil "~D~A" (- val 2)
                                    (case suit (:man "M") (:pin "P") (:sou "S")))
                            :keyword)))
          (when (and (member prev1 hand) (member prev2 hand))
            (push (list prev2 prev1 tile) sequences)))))

    sequences))


(defun next-seat (seat &optional config)
  "Return the next seat counter-clockwise.
   CONFIG: Optional game-config (defaults to 4-player)"
  (let ((seats (if config
                   (game-config-seats config)
                   '(:east :south :west :north))))
    (let ((pos (position seat seats)))
      (when pos
        (nth (mod (1+ pos) (length seats)) seats)))))

(defun prev-seat (seat &optional config)
  "Return the previous seat clockwise.
   CONFIG: Optional game-config (defaults to 4-player)"
  (let ((seats (if config
                   (game-config-seats config)
                   '(:east :south :west :north))))
    (let ((pos (position seat seats)))
      (when pos
        (nth (mod (1- pos) (length seats)) seats)))))

(defun seat-relation (from-seat to-seat &optional config)
  "Return the relation from FROM-SEAT to TO-SEAT.
   For 4-player: :left :across :right
   For 3-player: :left :right (no :across)"
  (let ((seats (if config
                   (game-config-seats config)
                   '(:east :south :west :north))))
    (let ((from-pos (position from-seat seats))
          (to-pos (position to-seat seats)))
      (when (and from-pos to-pos)
        (let ((distance (mod (- to-pos from-pos) (length seats))))
          (cond
            ((= distance 1) :left)
            ((= distance 2) (if (= (length seats) 4) :across :right))
            ((= distance 3) :right)
            (t nil)))))))


;;; ============================================================================
;;; Call Options (for multi-option modal support)
;;; ============================================================================

(defstruct call-option
  "Represents a single call option for a discarded tile."
  (type nil)        ; :ron, :pon, :kan, :chi
  (label "")        ; Display label (e.g., "PON", "CHI 2m-3m-4m")
  (tiles nil))      ; For chi: the sequence tiles. For pon/kan: tile. For ron: nil

(defun format-tile-short (tile)
  "Format a tile for display in call option label."
  (let ((suit (tile-suit tile))
        (val (tile-value tile)))
    (cond
      ((and suit val (member suit '(:man :pin :sou)))
       (format nil "~D~A" val (case suit (:man "m") (:pin "p") (:sou "s"))))
      (t (symbol-name tile)))))

(defun format-chi-label (sequence)
  "Format a chi sequence for display (e.g., '2m-3m-4m')."
  (format nil "CHI ~{~A~^-~}" (mapcar #'format-tile-short (sort-tiles sequence))))

(defun collect-call-options (player tile player-seat discarder-seat config
                                    &key can-ron-p can-kan-p can-pon-p chi-sequences)
  "Collect all available call options for PLAYER on TILE.
   Returns list of call-option structs, or NIL if no calls available.
   
   Arguments:
     PLAYER - The player who may call
     TILE - The discarded tile
     PLAYER-SEAT - Seat of the calling player  
     DISCARDER-SEAT - Seat of the player who discarded
     CONFIG - Game configuration
     :CAN-RON-P - T if player can call ron on this tile
     :CAN-KAN-P - T if player can call open kan on this tile
     :CAN-PON-P - T if player can call pon on this tile
     :CHI-SEQUENCES - List of possible chi sequences (from find-sequence-chi)"
  (declare (ignore player player-seat discarder-seat config))
  (let ((options nil))
    ;; Ron - highest priority, add first
    (when can-ron-p
      (push (make-call-option :type :ron
                              :label "RON"
                              :tiles nil)
            options))
    
    ;; Kan - higher than pon
    (when can-kan-p
      (push (make-call-option :type :kan
                              :label "KAN"
                              :tiles (list tile))
            options))
    
    ;; Pon
    (when can-pon-p
      (push (make-call-option :type :pon
                              :label "PON"
                              :tiles (list tile))
            options))
    
    ;; Chi - each sequence is a separate option
    (dolist (seq chi-sequences)
      (push (make-call-option :type :chi
                              :label (format-chi-label seq)
                              :tiles seq)
            options))
    
    ;; Return in priority order (ron first)
    (nreverse options)))
