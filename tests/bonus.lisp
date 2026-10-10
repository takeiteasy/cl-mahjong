(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(cl-mahjong:deftileset bonus-test :extends riichi-136
  (:bonus (flower plum orchid chrysanthemum bamboo)
          (season spring summer autumn winter))
  (:wall (:dead-wall 16) (:replacement 0 1 2 3) (:nuki 8)))

(cl-mahjong:defvariant bonus-test :extends riichi
  (:tileset bonus-test)
  (:set :bonus-tiles t))

(cl-mahjong:defvariant bonus-test-off :extends riichi
  (:tileset bonus-test))

(cl-mahjong:defyaku bonus-own (:han 1)
  (:when (bonus 1 seat-bonus)))
(cl-mahjong:defyaku bonus-none (:han 1)
  (:when (not (bonus 1))))
(cl-mahjong:defyaku bonus-flowers (:han 2)
  (:when (bonus 4 flower)))

(defun bonus-tile (tileset name)
  (first (cl-mahjong:tiles-of-kind tileset (cl-mahjong:find-kind tileset name))))

(defun bonus-order (tileset &key live dead)
  "A stacked wall order: tiles 0-51 are dealt, then LIVE leads the live wall and DEAD the dead wall."
  (let* ((rest (loop for tile from 52 below (cl-mahjong:tileset-tile-count tileset)
                     unless (or (member tile live) (member tile dead)) collect tile))
         (live-rest (- 76 (length live))))
    (append (loop for tile below 52 collect tile)
            live (subseq rest 0 live-rest)
            dead (nthcdr live-rest rest))))

(test grouped-bonus-kinds-take-rank-and-group
  (let* ((ts (cl-mahjong:find-tileset 'bonus-test))
         (orchid (cl-mahjong:find-kind ts 'orchid))
         (autumn (cl-mahjong:find-kind ts 'autumn)))
    (is (= 144 (cl-mahjong:tileset-tile-count ts)))
    (is (eq :bonus (cl-mahjong:kind-category orchid)))
    (is (eq :flower (cl-mahjong:kind-group orchid)))
    (is (= 2 (cl-mahjong:kind-rank orchid)))
    (is (eq :season (cl-mahjong:kind-group autumn)))
    (is (= 3 (cl-mahjong:kind-rank autumn)))
    (is (= 1 (cl-mahjong:kind-copies orchid)))))

(test bonus-tiles-need-the-option
  (signals cl-mahjong:illegal-action (cl-mahjong:start-game 'bonus-test-off :seed 1)))

(test bonus-patterns
  (let ((ts (cl-mahjong:find-tileset 'bonus-test)))
    (flet ((won (seat-wind &rest names)
             (names (best ts "234m567p345s678s66p" "8s"
                          :yaku '(bonus-own bonus-none bonus-flowers)
                          :context-args (list :seat-wind seat-wind
                                              :bonus (mapcar (lambda (name) (bonus-tile ts name))
                                                             names))))))
      (is (equal '(:bonus-none) (won :east)))
      (is (equal '(:bonus-own) (won :east 'plum 'summer)))
      (is (equal '(:bonus-own) (won :south 'plum 'orchid)))
      (is (equal '() (won :west 'plum 'orchid)))
      (is (equal '(:bonus-flowers :bonus-own) (won :east 'plum 'orchid 'chrysanthemum 'bamboo)))
      (is (equal '(:bonus-own) (won :south 'spring 'summer 'autumn 'winter)))
      (is (equal '() (won :west 'spring 'summer 'winter))))))

(test deal-sets-bonus-tiles-aside-and-replaces-them
  (let* ((ts (cl-mahjong:find-tileset 'bonus-test))
         (plum (bonus-tile ts 'plum))
         (order (loop for tile below 144 collect tile))
         (wall (progn (rotatef (nth 0 order) (nth plum order))
                      (cl-mahjong:make-wall ts :tiles order))))
    (multiple-value-bind (game events) (cl-mahjong:start-game 'bonus-test :walls (list wall))
      (let* ((deal (cl-mahjong:game-deal game))
             (dealer (cl-mahjong::deal-player deal 0)))
        (is (equal (list plum) (cl-mahjong:player-bonus dealer)))
        (is (= 14 (length (cl-mahjong:player-tiles dealer))))
        (is (= 13 (length (cl-mahjong:player-tiles (cl-mahjong::deal-player deal 1)))))
        (is (find (list :bonus :seat 0 :tile plum) events :test #'equal))
        (is (find-if (lambda (event) (and (eq :draw (first event)) (getf (rest event) :bonus)))
                     events))))))

(test a-drawn-bonus-tile-chains-to-the-next-replacement
  (let* ((ts (cl-mahjong:find-tileset 'bonus-test))
         (plum (bonus-tile ts 'plum))
         (orchid (bonus-tile ts 'orchid))
         (wall (cl-mahjong:make-wall
                ts :tiles (bonus-order ts :live (list plum) :dead (list orchid 100)))))
    (multiple-value-bind (game events) (cl-mahjong:start-game 'bonus-test :walls (list wall))
      (let* ((deal (cl-mahjong:game-deal game))
             (dealer (cl-mahjong::deal-player deal 0)))
        (is (equal (list plum orchid) (cl-mahjong:player-bonus dealer)))
        (is (= 100 (cl-mahjong:deal-drawn deal)))
        (is (= 14 (length (cl-mahjong:player-tiles dealer))))
        (is (eq :bonus (cl-mahjong:deal-rinshan deal)))
        (is (equal (list :bonus :seat 0 :tile orchid)
                   (find-if (lambda (event) (and (eq :bonus (first event))
                                                 (eql orchid (getf (rest event) :tile))))
                            events)))))))

(test random-bonus-games-finish-and-conserve-points
  (dolist (seed '(1 2 3))
    (let ((game (play-game (cl-mahjong:start-game 'bonus-test :seed seed))))
      (is (cl-mahjong:game-over-p game))
      (is (= 100000 (reduce #'+ (getf (cl-mahjong:game-final game) :points)))))))
