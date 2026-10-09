(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test akadora-frenzy-has-six-red-fives
  (let* ((variant (cl-mahjong:find-variant 'akadora-frenzy))
         (ts (cl-mahjong:variant-tileset variant)))
    (is (= 136 (cl-mahjong:tileset-tile-count ts)))
    (is (= 6 (count-if (lambda (tile) (cl-mahjong:tile-red-p ts tile))
                       (loop for tile below 136 collect tile))))))

(test yaku-festival-scores-daisharin
  (with-riichi (ts)
    (let* ((variant (cl-mahjong:find-variant 'yaku-festival))
           (reading (best ts "22334455667788p" "8p"
                          :yaku (cl-mahjong:variant-yaku variant)
                          :options (cl-mahjong:variant-options variant))))
      (is (equal '(:daisharin) (names reading)))
      (is (= 1 (cl-mahjong:reading-yakuman reading))))))

(test yaku-festival-turns-renhou-into-a-yakuman
  (let ((variant (cl-mahjong:find-variant 'yaku-festival)))
    (is (eq :yakuman (cl-mahjong:variant-option variant :renhou)))
    (is (not (member 'cl-mahjong::renhou-mangan (cl-mahjong:variant-yaku variant))))))

(test sanma-tonpuu-extends-sanma
  (let ((variant (cl-mahjong:find-variant 'sanma-tonpuu)))
    (is (= 3 (cl-mahjong:variant-players variant)))
    (is (eq :tonpuu (cl-mahjong:variant-option variant :length)))
    (is (null (cl-mahjong:variant-option variant :chi)))
    (let ((game (play-game (cl-mahjong:start-game variant :seed 2))))
      (is (cl-mahjong:game-over-p game))
      (is (<= (cl-mahjong:game-kyoku game) 6)))))

(test dora-bomb-starts-with-three-indicators
  (multiple-value-bind (game events) (cl-mahjong:start-game 'dora-bomb :seed 1)
    (declare (ignore game))
    (is (= 3 (length (getf (rest (find :deal-start events :key #'first)) :dora))))))

(test other-variants-start-with-one-indicator
  (multiple-value-bind (game events) (cl-mahjong:start-game 'riichi :seed 1)
    (declare (ignore game))
    (is (= 1 (length (getf (rest (find :deal-start events :key #'first)) :dora))))))
