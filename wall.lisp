;; wall.lisp
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

(defun create-wall ()
  "Create and shuffle a new wall with all 136 tiles (4 of each unique tile).
   Split into live wall (122 tiles) and dead wall (14 tiles).
   Initialize dora and ura-dora indicators."
  (let ((all-tiles (loop for tile in +all-tiles+
                        nconc (make-list 4 :initial-element tile))))
    ;; Shuffle using Fisher-Yates algorithm
    (loop for i from (1- (length all-tiles)) downto 1
          for j = (random (1+ i))
          do (rotatef (nth i all-tiles) (nth j all-tiles)))
    ;; Split into live wall and dead wall
    ;; Dead wall is the last 14 tiles
    ;; Live wall is the first 122 tiles
    (let* ((dead-wall (subseq all-tiles 122 136))  ; Last 14 tiles
           (live-wall (subseq all-tiles 0 122))    ; First 122 tiles
           (wall (make-instance 'wall)))

      (setf (wall-live-wall wall) live-wall
            (wall-dead-wall wall) dead-wall
            (wall-remaining-draws wall) (length live-wall))
      ;; Set up dora indicators
      ;; Dead wall structure (14 tiles):
      ;; Positions 0-4: dora indicators (revealed one at a time)
      ;; Positions 5-9: ura-dora indicators (hidden until riichi win)
      ;; Positions 10-13: kan replacement tiles
      (setf (wall-dora-indicators wall) (list (nth 0 dead-wall))
            (wall-ura-dora-indicators wall) (list (nth 5 dead-wall)))

      wall)))


(defun wall-draw-tile (wall)
  "Draw a tile from the live wall. Returns the tile or NIL if wall is empty."
  (when (wall-live-wall wall)
    (let ((tile (pop (wall-live-wall wall))))
      (decf (wall-remaining-draws wall))
      tile)))

(defun wall-draw-kan-replacement (wall)
  "Draw a replacement tile from the dead wall (from the end).
   Used after declaring a kan."
  (when (> (length (wall-dead-wall wall)) 0)
    (let ((tile (car (last (wall-dead-wall wall)))))
      (setf (wall-dead-wall wall)
            (butlast (wall-dead-wall wall)))
      tile)))


(defun wall-reveal-next-dora (wall)
  "Reveal next dora indicator after a kan.
   Can reveal up to 5 dora indicators total (initial + 4 kans).
   Also adds the corresponding ura-dora indicator."
  (when (< (wall-kan-count wall) 4)
    (incf (wall-kan-count wall))
    ;; Dead wall positions:
    ;; Dora indicators at: 0, 2, 4, 6, 8
    ;; Ura-dora indicators at: 5, 7, 9, 11, 13
    (let* ((dora-index (* 2 (wall-kan-count wall)))
           (ura-index (+ 5 (* 2 (wall-kan-count wall))))
           (dora-indicator (nth dora-index (wall-dead-wall wall)))
           (ura-indicator (nth ura-index (wall-dead-wall wall))))
      (push dora-indicator (wall-dora-indicators wall))
      (push ura-indicator (wall-ura-dora-indicators wall)))))

(defun wall-get-dora-tiles (wall)
  "Get the actual dora tiles (not indicators) from the wall"
  (calculate-dora (wall-dora-indicators wall)))

(defun wall-get-ura-dora-tiles (wall)
  "Get the actual ura-dora tiles (not indicators) from the wall.
   Only valid after a riichi win."
  (calculate-dora (wall-ura-dora-indicators wall)))


(defun wall-tiles-remaining (wall)
  "Return the number of tiles remaining to draw"
  (wall-remaining-draws wall))

(defun wall-is-empty (wall)
  "Check if the wall has no more tiles to draw (exhaustive draw)"
  (zerop (wall-remaining-draws wall)))

(defun wall-can-call-kan (wall)
  "Check if there are enough tiles in the dead wall to support a kan call.
   Need at least 1 tile remaining in dead wall for replacement."
  (> (length (wall-dead-wall wall)) 0))
