(in-package #:cl-mahjong)

(defstruct (wall (:constructor %make-wall
                     (tileset tiles live-end dead dora-revealed)))
  tileset tiles dead dora-revealed
  (live-start 0 :type fixnum)
  (live-end 0 :type fixnum)
  (replacements-drawn 0 :type fixnum))

(defun make-wall (tileset &key seed)
  (let* ((layout (tileset-wall-layout tileset))
         (all (shuffle-vector (coerce (loop for tile below (tileset-tile-count tileset)
                                            collect tile)
                                      'simple-vector)
                              (make-rng seed)))
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

(defun wall-draw-replacement (wall)
  "Draw from the next :replacement slot; the last live tile refills the slot.
Returns (values nil wall) when no slot or live tile is left."
  (let* ((slots (wall-layout-replacement (tileset-wall-layout (wall-tileset wall))))
         (slot (nth (wall-replacements-drawn wall) slots)))
    (if (or (null slot) (wall-exhausted-p wall))
        (values nil wall)
        (let ((new (copy-wall wall))
              (dead (copy-seq (wall-dead wall))))
          (decf (wall-live-end new))
          (setf (svref dead slot) (svref (wall-tiles wall) (wall-live-end new))
                (wall-dead new) dead)
          (incf (wall-replacements-drawn new))
          (values (svref (wall-dead wall) slot) new)))))

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
