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

(defun find-sequence-chi (hand tile seat last-discard-seat)
  "Check if player can call chi with TILE. Returns list of possible sequences.
   Chi can only be called by the next player (counter-clockwise)."
  (when (not (eq seat (next-seat last-discard-seat)))
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


(defun next-seat (seat)
  "Return the next seat counter-clockwise (east -> south -> west -> north -> east)"
  (case seat
    (:east :south)
    (:south :west)
    (:west :north)
    (:north :east)))

(defun prev-seat (seat)
  "Return the previous seat clockwise"
  (case seat
    (:east :north)
    (:north :west)
    (:west :south)
    (:south :east)))

(defun seat-relation (from-seat to-seat)
  "Return the relation from FROM-SEAT to TO-SEAT (:left :across :right)"
  (cond
    ((eq to-seat (next-seat from-seat)) :left)
    ((eq to-seat (next-seat (next-seat from-seat))) :across)
    ((eq to-seat (prev-seat from-seat)) :right)
    (t nil)))
