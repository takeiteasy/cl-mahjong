(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun drain (wall)
  (loop with tiles = '()
        do (multiple-value-bind (tile next) (cl-mahjong:wall-draw wall)
             (unless tile (return (nreverse tiles)))
             (push tile tiles)
             (setf wall next))))

(defun all-tiles (wall)
  (sort (append (drain wall) (cl-mahjong:wall-dead-tiles wall)) #'<))

(test same-seed-same-wall
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (is (equal (drain (cl-mahjong:make-wall ts :seed 42))
               (drain (cl-mahjong:make-wall ts :seed 42))))
    (is (not (equal (drain (cl-mahjong:make-wall ts :seed 42))
                    (drain (cl-mahjong:make-wall ts :seed 43)))))))

(test riichi-wall-split
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1)))
    (is (= 122 (cl-mahjong:wall-live-count wall)))
    (is (= 14 (length (cl-mahjong:wall-dead-tiles wall))))
    (is (= 122 (length (drain wall))))
    (is (equal (loop for i below 136 collect i) (all-tiles wall)))))

(test sanma-wall-split
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'sanma-108) :seed 1)))
    (is (= 94 (cl-mahjong:wall-live-count wall)))
    (is (equal (loop for i below 108 collect i) (all-tiles wall)))))

(test draw-is-persistent
  (let* ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1)))
    (multiple-value-bind (tile next) (cl-mahjong:wall-draw wall)
      (is (= 121 (cl-mahjong:wall-live-count next)))
      (is (= 122 (cl-mahjong:wall-live-count wall)))
      (is (= tile (cl-mahjong:wall-draw wall))))))

(test exhausted-wall
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1)))
    (dotimes (i 122)
      (setf wall (nth-value 1 (cl-mahjong:wall-draw wall))))
    (is (cl-mahjong:wall-exhausted-p wall))
    (is (null (cl-mahjong:wall-draw wall)))
    (is (null (cl-mahjong:wall-draw-replacement wall)))))

(test replacement-draws-shorten-the-live-wall
  (let* ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1))
         (expected (nth 0 (cl-mahjong:wall-dead-tiles wall)))
         (before (cl-mahjong:wall-live-count wall)))
    (multiple-value-bind (tile next) (cl-mahjong:wall-draw-replacement wall)
      (is (= expected tile))
      (is (= (1- before) (cl-mahjong:wall-live-count next)))
      (is (= 14 (length (cl-mahjong:wall-dead-tiles next))))
      (is (equal (loop for i below 136 collect i)
                 (sort (cons tile (all-tiles next)) #'<))))))

(test replacement-slots-run-out
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1)))
    (dotimes (i 4)
      (setf wall (nth-value 1 (cl-mahjong:wall-draw-replacement wall))))
    (is (null (cl-mahjong:wall-draw-replacement wall)))
    (is (= 118 (cl-mahjong:wall-live-count wall)))))

(test dora-indicators
  (let* ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1))
         (dead (cl-mahjong:wall-dead-tiles wall)))
    (is (equal (list (nth 4 dead)) (cl-mahjong:wall-dora-indicators wall)))
    (is (equal (list (nth 5 dead)) (cl-mahjong:wall-ura-indicators wall)))
    (let ((revealed (cl-mahjong:wall-reveal-dora wall)))
      (is (equal (list (nth 4 dead) (nth 6 dead)) (cl-mahjong:wall-dora-indicators revealed)))
      (is (equal (list (nth 5 dead) (nth 7 dead)) (cl-mahjong:wall-ura-indicators revealed))))))

(test dora-reveals-run-out
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'riichi-136) :seed 1)))
    (dotimes (i 4)
      (setf wall (cl-mahjong:wall-reveal-dora wall)))
    (is (= 5 (length (cl-mahjong:wall-dora-indicators wall))))
    (is (null (cl-mahjong:wall-reveal-dora wall)))))

(test tileset-without-wall-layout
  (cl-mahjong:deftileset wall-bare (:suits man) (:copies 1))
  (let ((wall (cl-mahjong:make-wall (cl-mahjong:find-tileset 'wall-bare) :seed 1)))
    (is (= 9 (cl-mahjong:wall-live-count wall)))
    (is (null (cl-mahjong:wall-dora-indicators wall)))))

(test stacked-wall
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (order (reverse (loop for i below 136 collect i)))
         (wall (cl-mahjong:make-wall ts :tiles order)))
    (is (equal (subseq order 0 122) (drain wall)))
    (is (equal (subseq order 122) (cl-mahjong:wall-dead-tiles wall)))))

(test stacked-wall-needs-every-tile-once
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (signals cl-mahjong:invalid-definition
      (cl-mahjong:make-wall ts :tiles (loop for i below 135 collect i)))
    (signals cl-mahjong:invalid-definition
      (cl-mahjong:make-wall ts :tiles (cons 0 (loop for i below 135 collect i))))))
