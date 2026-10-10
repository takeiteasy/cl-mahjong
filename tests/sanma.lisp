(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun sanma-option (key) (cl-mahjong:variant-option (cl-mahjong:find-variant 'sanma) key))

(test sanma-is-a-delta-on-riichi
  (let ((sanma (cl-mahjong:find-variant 'sanma)))
    (is (= 3 (cl-mahjong:variant-players sanma)))
    (is (= 108 (cl-mahjong:tileset-tile-count (cl-mahjong:variant-tileset sanma))))
    (is (equal cl-mahjong:*riichi-yaku* (cl-mahjong:variant-yaku sanma)))
    (is (null (sanma-option :chi)))
    (is (= 2000 (sanma-option :noten-payment)))))

(test sanma-deals-three-seats-without-chi
  (let ((game (cl-mahjong:start-game 'sanma :seed 3)))
    (is (= 3 (length (cl-mahjong:deal-players (cl-mahjong:game-deal game)))))
    (is (equalp #(35000 35000 35000) (cl-mahjong::game-scores game)))
    (is (null (find :chi (loop for seat below 3
                               append (cl-mahjong:legal-actions game seat))
                    :key #'first)))))

(test sanma-tsumo-is-paid-by-the-two-other-seats
  (multiple-value-bind (payments total)
      (cl-mahjong:limit-payments 'riichi :mangan :dealer nil :players 3)
    (is (equal '(:dealer 4000 :other 2000) payments))
    (is (= 6000 total)))
  (multiple-value-bind (payments total)
      (cl-mahjong:limit-payments 'riichi :mangan :dealer t :players 3)
    (is (equal '(:dealer nil :other 4000) payments))
    (is (= 8000 total))))

(test sanma-honba-is-200-split-between-the-payers
  (flet ((payments (&rest args)
           (multiple-value-list
            (apply #'cl-mahjong::payments-for
                   (cl-mahjong::scoring-payments (cl-mahjong::resolve-scoring 'sanma))
                   2000 args))))
    (is (equal '((:ron 8000) 8000) (payments nil nil 0 3)))
    (is (equal '((:ron 8600) 8600) (payments nil nil 3 3)))
    (is (equal '((:dealer 4100 :other 2100) 6200) (payments nil t 1 3)))))

(test sanma-games-finish-and-conserve-points
  (dolist (seed '(1 2 3 4))
    (let* ((game (play-game (cl-mahjong:start-game 'sanma :seed seed)))
           (final (cl-mahjong:game-final game)))
      (is (cl-mahjong:game-over-p game))
      (is (= 105000 (reduce #'+ (getf final :points))))
      (is (= 0 (reduce #'+ (getf final :scores)))))))

(test sanma-standings-use-three-place-uma-and-oka
  (let* ((game (cl-mahjong:start-game 'sanma :seed 1))
         (final (cl-mahjong::standings game (vector 50000 35000 20000) 0)))
    (is (equal '(0 1 2) (getf final :order)))
    (is (equalp #(40000 -5000 -35000) (getf final :scores)))))

(test sanma-three-matching-winds-do-not-abort
  (is (null (sanma-option :suufon-renda)))
  (is (null (sanma-option :suucha-riichi))))
