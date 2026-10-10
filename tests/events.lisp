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

(test house-yaku-need-their-options
  (with-riichi (ts)
    (let ((variant (cl-mahjong:find-variant 'house-rules)))
      (is (zerop (cl-mahjong:reading-yakuman
                  (best ts "22334455667788p" "8p"
                        :yaku (cl-mahjong:variant-yaku variant)
                        :options cl-mahjong:*riichi-options*)))))))

(test daisharin-in-other-suits-needs-the-all-option
  (with-riichi (ts)
    (let ((variant (cl-mahjong:find-variant 'house-rules)))
      (flet ((scored (notation win daisharin)
               (names (best ts notation win :yaku (cl-mahjong:variant-yaku variant)
                                            :options (list* :daisharin daisharin
                                                            cl-mahjong:*riichi-options*)))))
        (is (equal '(:daisuurin) (scored "22334455667788m" "8m" :all)))
        (is (not (member :daisuurin (scored "22334455667788m" "8m" t))))
        (is (equal '(:daisharin) (scored "22334455667788p" "8p" t)))))))

(test house-rules-pao
  (with-riichi (ts)
    (let* ((variant (cl-mahjong:find-variant 'house-rules))
           (options (cl-mahjong:variant-options variant))
           (yaku (cl-mahjong:variant-yaku variant)))
      (let ((reading (best ts "11p" "1p" :yaku yaku :options options
                                         :context-args '(:kan-from 2)
                                         :melds (list (meld ts :quad "1111m" :open t :from 1)
                                                      (meld ts :quad "2222m" :open t :from 3)
                                                      (meld ts :quad "3333m")
                                                      (meld ts :quad "4444m" :open t :from 2)))))
        (is (equal '(:suukantsu-pao) (names reading)))
        (is (equal '((2 . 1)) (cl-mahjong:reading-pao reading))))
      (is (equal '((2 . 1))
                 (cl-mahjong:reading-pao
                  (best ts "777z234m11p" "2m" :yaku yaku :options options
                        :melds (list (meld ts :triplet "555z" :open t :from 1)
                                     (meld ts :triplet "666z" :open t :from 2 :held "777z"))))))
      (is (equal '(:ron 2700 :from 2)
                 (cl-mahjong:score-payments
                  (best-scored ts "234m567p345s11p" "2m"
                               :melds (list (meld ts :quad "5555z" :open t :from 2))
                               :context-args '(:tsumo t :rinshan t :kan-from 2)
                               :scoring (cl-mahjong:variant-scoring variant)
                               :options options)))))))

(test open-riichi-replaces-riichi
  (with-riichi (ts)
    (let ((reading (best ts "123444s234456m66p" "4s"
                         :yaku (cl-mahjong:variant-yaku (cl-mahjong:find-variant 'house-rules))
                         :context-args '(:riichi t :open-riichi t))))
      (is (equal '(:open-riichi) (names reading)))
      (is (= 2 (cl-mahjong:reading-han reading))))))

(test sashikomi-is-a-ron-in-open-riichi
  (with-riichi (ts)
    (flet ((scored (&rest context)
             (best ts "123444s234456m66p" "4s"
                   :yaku (cl-mahjong:variant-yaku (cl-mahjong:find-variant 'house-rules))
                   :options (list* :sashikomi t cl-mahjong:*riichi-options*)
                   :context-args (list* :riichi t :open-riichi t context))))
      (is (equal '(:sashikomi) (names (scored))))
      (is (not (member :sashikomi (names (scored :from-riichi t)))))
      (is (not (member :sashikomi (names (scored :tsumo t))))))))

(test paarenchan-scores-a-yakuman-per-dealer-win
  (with-riichi (ts)
    (let ((yaku (cl-mahjong:variant-yaku (cl-mahjong:find-variant 'house-rules))))
      (let ((reading (best ts "111222777p44455s" "7p" :yaku yaku
                                                      :options cl-mahjong:*riichi-options*
                                                      :context-args '(:tsumo t :paarenchan 4))))
        (is (equal '(:paarenchan :suuankou) (names reading)))
        (is (= 5 (cl-mahjong:reading-yakuman reading)))))))

(test paarenchan-needs-another-yaku-unless-the-option-is-set
  (with-riichi (ts)
    (let ((yaku (cl-mahjong:variant-yaku (cl-mahjong:find-variant 'house-rules))))
      (flet ((scored (without-yaku)
               (best ts "12367778p678s456m" "7p"
                     :yaku yaku
                     :options (list* :paarenchan-without-yaku without-yaku
                                     cl-mahjong:*riichi-options*)
                     :context-args '(:paarenchan 1))))
        (is (null (cl-mahjong:reading-yaku (scored nil))))
        (is (equal '(:paarenchan) (names (scored t))))))))

(test aotenjou-variant-stacks-daisharin-with-tsumo
  (with-riichi (ts)
    (let* ((variant (cl-mahjong:find-variant 'aotenjou))
           (score (cl-mahjong:best-score
                   (cl-mahjong:evaluate-score
                    (cl-mahjong:variant-scoring variant) ts (cl-mahjong:variant-shapes variant)
                    (cl-mahjong:variant-yaku variant) (hand ts "22334455667788p")
                    nil (context ts "7p" :tsumo t)
                    :options (cl-mahjong:variant-options variant)))))
      (is (= 14 (cl-mahjong:score-han score)))
      (is (equal '(:daisharin :menzen-tsumo)
                 (sort (mapcar #'car (cl-mahjong:reading-yaku (cl-mahjong:score-reading score)))
                       #'string<))))))

;;; Open riichi in game flow

(defparameter *open-riichi-hands*
  '("123m456m789p234s1z" "99m123p456p555z78s" "246m246p246s6677z" "357m357p357s3344z"))

(defun open-riichi-game (&rest draws)
  (new-game :variant 'house-rules :hands *open-riichi-hands* :draws draws))

(defun win-yaku-names (events)
  (mapcar #'car (getf (rest (win-event events)) :yaku)))

(test open-riichi-is-offered-with-the-option
  (let ((on (open-riichi-game "9m"))
        (off (new-game :hands *open-riichi-hands* :draws '("9m"))))
    (is (equal (mapcar #'second (remove :riichi (cl-mahjong:legal-actions on 0)
                                        :key #'first :test-not #'eq))
               (mapcar #'second (remove :open-riichi (cl-mahjong:legal-actions on 0)
                                        :key #'first :test-not #'eq))))
    (is (member :open-riichi (action-types on 0)))
    (is (not (member :open-riichi (action-types off 0))))
    (signals cl-mahjong:illegal-action
      (cl-mahjong:apply-action off 0 (list :open-riichi (drawn-tile off))))))

(defun declare-and-pass (kind)
  "(values game events): seat 0 declares KIND (:riichi or :open-riichi) on a drawn 9m and everyone passes."
  (multiple-value-bind (game events) (act (open-riichi-game "9m") 0 kind "9m")
    (multiple-value-bind (game more) (pass-all game)
      (values game (append events more)))))

(test open-riichi-pays-a-stick-and-reveals-the-hand
  (multiple-value-bind (game events) (declare-and-pass :open-riichi)
    (is (eq :open (getf (rest (find :discard events :key #'first)) :riichi)))
    (let ((riichi (rest (find :riichi events :key #'first))))
      (is (getf riichi :open))
      (is (= 13 (length (getf riichi :tiles)))))
    (is (cl-mahjong:player-open-riichi (seat-player game 0)))
    (is (eq :active (cl-mahjong:player-riichi (seat-player game 0))))
    (is (= 24000 (aref (cl-mahjong:game-points game) 0)))))

(test a-plain-riichi-does-not-reveal-the-hand
  (multiple-value-bind (game events) (declare-and-pass :riichi)
    (is (find :riichi events :key #'first))
    (is (null (getf (rest (find :riichi events :key #'first)) :tiles)))
    (is (not (cl-mahjong:player-open-riichi (seat-player game 0))))))

(test sashikomi-is-a-ron-on-a-discarder-not-in-riichi
  (let ((game (pass-all (act (open-riichi-game "9m" "1z") 0 :open-riichi "9m"))))
    (multiple-value-bind (game events)
        (cl-mahjong:apply-action (act game 1 :discard "1z") 0 '(:ron))
      (declare (ignore game))
      (is (member :sashikomi (win-yaku-names events))))))

(test no-sashikomi-on-a-discarder-in-riichi
  (let ((game (open-riichi-game "9m" "2z" "3z" "4z" "2s" "1z")))
    (setf game (pass-all (act game 0 :open-riichi "9m")))
    (setf game (pass-all (act game 1 :riichi "2z")))
    (setf game (discard-and-pass game 2))
    (setf game (discard-and-pass game 3))
    (setf game (discard-and-pass game 0))
    (multiple-value-bind (game events) (cl-mahjong:apply-action (act game 1 :discard "1z") 0 '(:ron))
      (declare (ignore game))
      (let ((names (win-yaku-names events)))
        (is (member :double-open-riichi names))
        (is (not (member :sashikomi names)))))))

(test sashikomi-on-a-riichi-declaration-tile
  (let ((game (pass-all (act (open-riichi-game "9m" "1z") 0 :open-riichi "9m"))))
    (setf game (act game 1 :riichi "1z"))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 0 '(:ron))
      (declare (ignore game))
      (is (member :sashikomi (win-yaku-names events))))))
