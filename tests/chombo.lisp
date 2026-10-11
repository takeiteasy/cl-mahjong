(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defparameter *noten-dealer*
  (cons "159m159p159s2345z" (rest *junk-hands*)))

(defun chombo-game (chombo &key (hands *junk-hands*) draws (honba 0) (sticks :return) (false-win :void))
  "A riichi game with the :chombo option CHOMBO, whose riichi sticks of a void deal go STICKS.
A false win ends the deal, or with FALSE-WIN :dead makes the hand dead."
  (variant-with 'chombo-test :chombo chombo :chombo-sticks sticks :false-win false-win)
  (cl-mahjong:start-game 'chombo-test :honba honba
                                      :walls (list (stacked :hands hands :draws draws))))

(defun chombo-events (&rest args)
  (multiple-value-bind (game events) (apply #'cl-mahjong:apply-action args)
    (values game (find :chombo events :key #'first))))

(test wrong-declarations-still-signal-with-the-option-off
  (let ((game (new-game :draws '("9m"))))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 0 '(:tsumo)))
    (signals cl-mahjong:illegal-action (act (new-game :hands *noten-dealer* :draws '("9m"))
                                            0 :riichi "9m"))
    (setf game (act game 0 :discard "9m"))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 1 '(:ron)))))

(test a-false-tsumo-is-a-chombo
  (multiple-value-bind (game event) (chombo-events (chombo-game :mangan :draws '("9m")) 0 '(:tsumo))
    (is (equal '(:chombo :seat 0 :reason :false-tsumo) event))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (eq :chombo (result-of game :type)))
    (is (equal '(0) (result-of game :seats)))
    (is (equal '(13000 29000 29000 29000) (points-of game)))))

(test a-chombo-repeats-the-dealer-and-keeps-the-honba
  (let ((game (cl-mahjong:apply-action (chombo-game :mangan :draws '("9m") :honba 2) 0 '(:tsumo))))
    (is (= 0 (cl-mahjong:game-kyoku game)))
    (is (= 2 (cl-mahjong:game-honba game)))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (= 0 (cl-mahjong:deal-dealer (cl-mahjong:game-deal (cl-mahjong:next-deal game)))))))

(test every-seat-answers-a-discard-with-the-option-on
  (let ((game (act (chombo-game :mangan :draws '("9m")) 0 :discard "9m")))
    (is (equal '(1 2 3) (cl-mahjong:deciding-seats game)))
    (is (equal '((:pass)) (cl-mahjong:legal-actions game 3)))))

(test a-false-ron-is-a-chombo
  (let ((game (act (chombo-game :mangan :draws '("9m")) 0 :discard "9m")))
    (multiple-value-bind (game event) (chombo-events game 1 '(:ron))
      (is (equal '(:chombo :seat 1 :reason :false-ron) event))
      (is (eq :chombo (result-of game :type)))
      (is (equal '(1) (result-of game :seats)))
      (is (equal '(29000 17000 27000 27000) (points-of game))))))

(test a-ron-while-furiten-is-a-chombo
  (variant-with 'chombo-test :chombo :mangan)
  (let ((game (furiten-game "9s" 'chombo-test)))
    (is (equal '((:pass)) (cl-mahjong:legal-actions game 3)))
    (is (eq :chombo (result-of (cl-mahjong:apply-action game 3 '(:ron)) :type)))))

(test a-ron-without-a-yaku-is-a-chombo
  (let* ((game (chombo-game :mangan :hands (list* "123m456m789p234s1z" "123p456p789m23s11z"
                                                  (cddr *junk-hands*))
                                    :draws '("1s")))
         (game (act game 0 :discard "1s")))
    (is (not (member :ron (action-types game 1))))
    (is (eq :chombo (result-of (cl-mahjong:apply-action game 1 '(:ron)) :type)))))

(test an-open-riichi-while-noten-is-a-chombo-at-once
  (variant-with 'chombo-open :chombo :mangan :open-riichi t)
  (let ((game (cl-mahjong:start-game 'chombo-open
                                     :walls (list (stacked :hands *noten-dealer* :draws '("9m"))))))
    (multiple-value-bind (game event) (chombo-events game 0 (list :open-riichi (kind-tile game 0 "9m")))
      (is (equal '(:chombo :seat 0 :reason :noten-open-riichi) event))
      (is (eq :chombo (result-of game :type))))))

(test a-riichi-while-noten-is-a-chombo-at-the-exhaustive-draw
  (let ((game (act (chombo-game :mangan :hands *noten-dealer* :draws '("9m")) 0 :riichi "9m")))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (is (cl-mahjong:player-false-riichi (seat-player game 0)))
    (setf game (play-deal game))
    (is (eq :chombo (result-of game :type)))
    (is (equal '(0) (result-of game :seats)))
    (is (= 0 (cl-mahjong:game-kyoku game)))))

(test a-noten-riichi-is-not-penalised-when-someone-wins
  (let* ((game (chombo-game :mangan :hands (list* "159m159p159s2345z" "99m123p456p555z78s"
                                                  (cddr *junk-hands*))
                                    :draws '("6s")))
         (game (act game 0 :riichi "6s")))
    (is (eq :ron (result-of (pass-all (cl-mahjong:apply-action game 1 '(:ron))) :type)))))

(test bots-play-a-chombo-game-through
  (variant-with 'chombo-test :chombo :mangan)
  (dolist (seed '(1 2 3))
    (let ((game (play-game (cl-mahjong:start-game 'chombo-test :seed seed))))
      (is (cl-mahjong:game-over-p game))
      (is (= 100000 (reduce #'+ (getf (cl-mahjong:game-final game) :points)))))))

(test a-split-chombo-shares-the-penalty-between-the-others
  (let ((game (cl-mahjong:apply-action
               (act (chombo-game '(:split 20000) :draws '("9m")) 0 :discard "9m") 1 '(:ron))))
    (is (equal '(31600 5200 31600 31600) (points-of game)))))

(test a-final-chombo-takes-the-penalty-from-the-final-score
  (let* ((game (cl-mahjong:apply-action
                (act (chombo-game '(:final 20000) :draws '("9m")) 0 :discard "9m") 1 '(:ron)))
         (clean (cl-mahjong::copy-game game)))
    (is (equal '(25000 25000 25000 25000) (points-of game)))
    (is (equal '(0 20000 0 0) (coerce (cl-mahjong::game-penalties game) 'list)))
    (setf (cl-mahjong::game-penalties clean) (make-array 4 :initial-element 0))
    (is (equal '(0 -20000 0 0)
               (map 'list #'- (getf (cl-mahjong::standings game (cl-mahjong:game-points game) 0) :scores)
                    (getf (cl-mahjong::standings clean (cl-mahjong:game-points clean) 0) :scores))))))

(defun riichi-then-false-tsumo (sticks)
  "Seat 0 declares riichi; seat 1 then declares a tsumo it does not have."
  (let ((game (chombo-game :mangan :draws '("9m" "9m") :sticks sticks)))
    (setf game (pass-all (act game 0 :riichi "9m")))
    (cl-mahjong:apply-action game 1 '(:tsumo))))

(test a-chombo-returns-the-riichi-sticks-of-the-deal
  (let ((game (riichi-then-false-tsumo :return)))
    (is (equal '(29000 17000 27000 27000) (points-of game)))
    (is (= 0 (cl-mahjong:game-sticks game)))))

(test a-chombo-can-leave-the-riichi-sticks-on-the-table
  (let ((game (riichi-then-false-tsumo :keep)))
    (is (equal '(28000 17000 27000 27000) (points-of game)))
    (is (= 1 (cl-mahjong:game-sticks game)))))

(test bad-chombo-options-are-rejected-when-the-variant-is-built
  (dolist (options '((:chombo :mangna) (:chombo (:split)) (:chombo (:split 0)) (:chombo (:final 1 2))
                     (:chombo 20000) (:chombo :mangan :chombo-sticks :retrun)))
    (cl-mahjong::register-definition
     :variant 'chombo-bad 'riichi
     (list (list :name 'chombo-bad) (list* :options (append options cl-mahjong:*riichi-options*))))
    (signals cl-mahjong:invalid-definition (cl-mahjong:find-variant 'chombo-bad)))
  (dolist (chombo '(nil :mangan (:split 20000) (:final 20000)))
    (variant-with 'chombo-good :chombo chombo)
    (is (cl-mahjong:find-variant 'chombo-good))))

;;; Dead hands

(defun dead-game (&rest args)
  (apply #'chombo-game :mangan :false-win :dead args))

(test a-false-tsumo-makes-the-hand-dead
  (multiple-value-bind (game events) (cl-mahjong:apply-action (dead-game :draws '("9m")) 0 '(:tsumo))
    (is (equal '(:dead-hand :seat 0 :reason :false-tsumo) (find :dead-hand events :key #'first)))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (is (eq :false-tsumo (cl-mahjong:player-dead (seat-player game 0))))
    (is (= 0 (cl-mahjong:deal-turn (cl-mahjong:game-deal game))))
    (is (equal '(25000 25000 25000 25000) (points-of game)))))

(test a-dead-seat-only-discards
  (let ((game (cl-mahjong:apply-action (dead-game :draws '("9m")) 0 '(:tsumo))))
    (is (equal '(:discard) (action-types game 0)))
    (is (= 14 (length (cl-mahjong:legal-actions game 0))))))

(test a-dead-seat-cannot-declare-again
  (let ((game (cl-mahjong:apply-action (dead-game :draws '("9m")) 0 '(:tsumo))))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 0 '(:tsumo)))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 0 (list :riichi (kind-tile game 0 "9m"))))))

(test a-false-ron-makes-the-hand-dead-and-the-claim-goes-on
  (let ((game (act (dead-game :draws '("9m")) 0 :discard "9m")))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 1 '(:ron))
      (is (equal '(:dead-hand :seat 1 :reason :false-ron) (find :dead-hand events :key #'first)))
      (is (eq :playing (cl-mahjong:game-phase game)))
      (is (equal '(2 3) (cl-mahjong:deciding-seats game)))
      (setf game (pass-all game))
      (is (= 1 (cl-mahjong:deal-turn (cl-mahjong:game-deal game))))
      (is (equal '(:discard) (action-types game 1))))))

(test a-dead-seat-is-skipped-by-later-claims
  (let* ((game (pass-all (cl-mahjong:apply-action (act (dead-game :draws '("9m" "9m" "9m")) 0 :discard "9m")
                                                  1 '(:ron))))
         (game (pass-all (act game 1 :discard "9m")))
         (game (act game 2 :discard "9m")))
    (is (equal '(0 3) (cl-mahjong:deciding-seats game)))
    (is (null (cl-mahjong:legal-actions game 1)))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 1 '(:ron)))))

(test an-open-riichi-while-noten-makes-the-hand-dead
  (variant-with 'dead-open :chombo :mangan :open-riichi t :false-win :dead)
  (let ((game (cl-mahjong:start-game 'dead-open
                                     :walls (list (stacked :hands *noten-dealer* :draws '("9m"))))))
    (setf game (cl-mahjong:apply-action game 0 (list :open-riichi (kind-tile game 0 "9m"))))
    (is (eq :noten-open-riichi (cl-mahjong:player-dead (seat-player game 0))))
    (is (null (cl-mahjong:player-riichi (seat-player game 0))))
    (is (equal '(:discard) (action-types game 0)))))

(test a-noten-riichi-is-still-a-chombo-at-the-draw-under-dead-hands
  (let ((game (act (dead-game :hands *noten-dealer* :draws '("9m")) 0 :riichi "9m")))
    (is (eq :chombo (result-of (play-deal game) :type)))))

(test a-dead-seat-is-noten-at-the-exhaustive-draw
  (let* ((game (cl-mahjong:apply-action
                (dead-game :hands (cons "123m456m789p234s1z" (rest *junk-hands*)) :draws '("9m"))
                0 '(:tsumo)))
         (game (play-deal game)))
    (is (eq :exhaustive (result-of game :type)))
    (is (not (member 0 (result-of game :tenpai))))
    (is (= 1 (cl-mahjong:game-kyoku game)))))

(test bots-play-on-with-a-dead-seat
  (let ((game (play-game (cl-mahjong:apply-action (dead-game :draws '("9m")) 0 '(:tsumo)))))
    (is (cl-mahjong:game-over-p game))
    (is (= 100000 (reduce #'+ (getf (cl-mahjong:game-final game) :points))))))

(test a-dead-hand-needs-a-chombo-penalty
  (signals cl-mahjong:invalid-definition (variant-with-options :false-win :dead))
  (signals cl-mahjong:invalid-definition (variant-with-options :false-win :alive :chombo :mangan))
  (is (variant-with-options :false-win :dead :chombo :mangan)))

(test a-none-penalty-accepts-the-declaration-and-charges-nothing
  (let ((game (cl-mahjong:apply-action (chombo-game :none :draws '("9m")) 0 '(:tsumo))))
    (is (eq :chombo (result-of game :type)))
    (is (equal '(25000 25000 25000 25000) (points-of game)))))

(test a-dead-hand-costs-nothing-with-the-none-penalty
  (let ((game (cl-mahjong:apply-action (chombo-game :none :false-win :dead :draws '("9m")) 0 '(:tsumo))))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (is (eq :false-tsumo (cl-mahjong:player-dead (seat-player game 0))))))
