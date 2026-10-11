(in-package #:cl-mahjong)

(defstruct (wall (:constructor %make-wall
                     (tileset tiles live-end dead dora-revealed)))
  tileset tiles dead dora-revealed
  (live-start 0 :type fixnum)
  (live-end 0 :type fixnum)
  (replacements-drawn 0 :type fixnum)
  (nuki-drawn 0 :type fixnum))

(defun stacked-tiles (tileset tiles)
  (let ((count (tileset-tile-count tileset)))
    (unless (and (= (length tiles) count)
                 (equal (sort (copy-seq (coerce tiles 'list)) #'<)
                        (loop for tile below count collect tile)))
      (invalid "A stacked wall needs every tile id of the tileset exactly once"))
    (coerce tiles 'simple-vector)))

(defun make-wall (tileset &key seed tiles)
  "A shuffled wall, or with TILES (every tile id once) one stacked in that order."
  (let* ((layout (tileset-wall-layout tileset))
         (all (if tiles
                  (stacked-tiles tileset tiles)
                  (shuffle-vector (coerce (loop for tile below (tileset-tile-count tileset)
                                                collect tile)
                                          'simple-vector)
                                  (make-rng seed))))
         (live-end (- (length all) (wall-layout-dead-wall layout))))
    (%make-wall tileset
                (subseq all 0 live-end)
                live-end
                (subseq all live-end)
                (min 1 (length (wall-layout-dora layout))))))

(defun wall-live-count (wall)
  (- (wall-live-end wall) (wall-live-start wall)))

(defun wall-exhausted-p (wall)
  (zerop (wall-live-count wall)))

(defun wall-dead-tiles (wall)
  (coerce (wall-dead wall) 'list))

(defun wall-draw (wall)
  "(values tile new-wall); (values nil wall) when the live wall is empty."
  (if (wall-exhausted-p wall)
      (values nil wall)
      (let ((new (copy-wall wall)))
        (incf (wall-live-start new))
        (values (svref (wall-tiles wall) (wall-live-start wall)) new))))

(defun take-dead-slot (wall)
  "(values tile new-wall): the dead wall slot after those already drawn, cycling; the last live tile refills it."
  (let* ((slots (wall-layout-replacement (tileset-wall-layout (wall-tileset wall))))
         (slot (nth (mod (+ (wall-replacements-drawn wall) (wall-nuki-drawn wall)) (length slots))
                    slots))
         (new (copy-wall wall))
         (dead (copy-seq (wall-dead wall))))
    (decf (wall-live-end new))
    (setf (svref dead slot) (svref (wall-tiles wall) (wall-live-end new))
          (wall-dead new) dead)
    (values (svref (wall-dead wall) slot) new)))

(defun wall-draw-replacement (wall)
  "Draw from the next :replacement slot; the last live tile refills the slot.
Returns (values nil wall) when no slot or live tile is left."
  (let ((slots (wall-layout-replacement (tileset-wall-layout (wall-tileset wall)))))
    (if (or (>= (wall-replacements-drawn wall) (length slots)) (wall-exhausted-p wall))
        (values nil wall)
        (multiple-value-bind (tile new) (take-dead-slot wall)
          (incf (wall-replacements-drawn new))
          (values tile new)))))

(defun wall-draw-nuki (wall)
  "Draw the replacement for a set-aside tile, up to the layout's :nuki count.
Returns (values nil wall) when none is left."
  (if (or (>= (wall-nuki-drawn wall) (wall-layout-nuki (tileset-wall-layout (wall-tileset wall))))
          (wall-exhausted-p wall))
      (values nil wall)
      (multiple-value-bind (tile new) (take-dead-slot wall)
        (incf (wall-nuki-drawn new))
        (values tile new))))

(defun wall-reveal-dora (wall)
  "New wall with one more dora indicator revealed, or NIL when all are."
  (when (< (wall-dora-revealed wall)
           (length (wall-layout-dora (tileset-wall-layout (wall-tileset wall)))))
    (let ((new (copy-wall wall)))
      (incf (wall-dora-revealed new))
      new)))

(defun revealed-indicators (wall positions)
  (loop for position in positions
        repeat (wall-dora-revealed wall)
        collect (svref (wall-dead wall) position)))

(defun wall-dora-indicators (wall)
  (revealed-indicators wall (wall-layout-dora (tileset-wall-layout (wall-tileset wall)))))

(defun wall-ura-indicators (wall)
  (revealed-indicators wall (wall-layout-ura (tileset-wall-layout (wall-tileset wall)))))

(defun wall-stacked-order (wall)
  "The tile ids of WALL in the order `make-wall' :tiles takes them, or NIL once tiles have been drawn."
  (when (and (zerop (wall-live-start wall))
             (zerop (wall-replacements-drawn wall))
             (zerop (wall-nuki-drawn wall))
             (= (wall-live-end wall) (length (wall-tiles wall)))
             (= (wall-dora-revealed wall)
                (min 1 (length (wall-layout-dora (tileset-wall-layout (wall-tileset wall)))))))
    (concatenate 'list (wall-tiles wall) (wall-dead wall))))
