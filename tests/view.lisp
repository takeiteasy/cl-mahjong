(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

;;; Hands

(test a-seat-sees-its-own-hand-and-drawn-tile
  (let* ((game (new-game :hands *ron-hands* :draws '("6z")))
         (view (view-of game 0)))
    (is (equal (cl-mahjong:player-tiles (seat-player game 0)) (cl-mahjong:view-hand view)))
    (is (eql (drawn-tile game) (cl-mahjong:view-drawn view)))
    (is (= 14 (cl-mahjong:seen-hand-size (seen-by view 0))))))

(test other-seats-show-a-hand-size-and-no-tiles
  (let* ((game (new-game :hands *ron-hands* :draws '("6z")))
         (view (view-of game 1)))
    (is (null (cl-mahjong:view-drawn view)))
    (is (null (cl-mahjong:view-forbidden view)))
    (dolist (seat '(0 2 3))
      (let ((seen (seen-by view seat)))
        (is (null (cl-mahjong:seen-shown seen)))
        (is (= (length (cl-mahjong:player-tiles (seat-player game seat)))
               (cl-mahjong:seen-hand-size seen)))))))

(test open-riichi-shows-the-hand-to-the-others-only
  (let ((game (with-seat (new-game :hands *ron-hands*) 1 :riichi t :open t)))
    (is (equal (cl-mahjong:player-tiles (seat-player game 1))
               (cl-mahjong:seen-shown (seen-by (view-of game 0) 1))))
    (is (null (cl-mahjong:seen-shown (seen-by (view-of game 1) 1))))))

(test closed-riichi-keeps-the-hand-hidden
  (let ((game (with-seat (new-game :hands *ron-hands*) 1 :riichi t)))
    (is (null (cl-mahjong:seen-shown (seen-by (view-of game 0) 1))))
    (is (eq :active (cl-mahjong:seen-riichi (seen-by (view-of game 0) 1))))))

;;; Table

(test the-view-carries-the-table
  (let* ((game (new-game :hands *ron-hands* :draws '("6z")))
         (view (view-of game 2)))
    (is (eq :playing (cl-mahjong:view-phase view)))
    (is (eq :turn (cl-mahjong:view-deal-phase view)))
    (is (= 0 (cl-mahjong:view-dealer view)))
    (is (eq :east (cl-mahjong:view-round-wind view)))
    (is (eq :west (cl-mahjong:view-seat-wind view)))
    (is (equal (coerce (cl-mahjong:game-points game) 'list)
               (coerce (cl-mahjong:view-points view) 'list)))
    (is (equal (cl-mahjong:wall-dora-indicators (cl-mahjong:deal-wall (cl-mahjong:game-deal game)))
               (cl-mahjong:view-dora view)))))

(test a-claim-view-names-the-tile-and-no-options
  (let* ((game (act (new-game :hands *ron-hands* :draws '("6z")) 0 :discard "6z"))
         (claim (cl-mahjong:view-claim (view-of game 3))))
    (is (eq :claim (cl-mahjong:view-deal-phase (view-of game 3))))
    (is (equal '(:kind :tile :from) (loop for key in claim by #'cddr collect key)))
    (is (= 0 (getf claim :from)))))

(test the-view-carries-the-rules-and-the-legal-actions
  (let ((game (new-game :hands *ron-hands* :draws '("6z"))))
    (is (eq (cl-mahjong:game-variant game) (cl-mahjong:view-variant (view-of game 0))))
    (is (equal (cl-mahjong:legal-actions game 0) (cl-mahjong:view-actions (view-of game 0))))
    (is (cl-mahjong:view-actions (view-of game 0)))
    (is (null (cl-mahjong:view-actions (view-of game 1)))))
  (let ((game (act (new-game :hands *ron-hands* :draws '("6z")) 0 :discard "6z")))
    (dolist (seat '(0 1 2 3))
      (is (equal (cl-mahjong:legal-actions game seat)
                 (cl-mahjong:view-actions (view-of game seat)))))))

;;; Game end

(test views-work-between-deals-and-after-the-game
  (let ((game (cl-mahjong:play-bots (cl-mahjong:start-game 'riichi :seed 5)
                                    (loop repeat 4 collect (cl-mahjong:make-bot :dead))
                                    :deals 1)))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (dolist (seat '(0 1 2 3))
      (let ((view (view-of game seat)))
        (is (eq :ended (cl-mahjong:view-deal-phase view)))
        (is (null (cl-mahjong:view-claim view)))
        (is (null (cl-mahjong:view-drawn view)))
        (is (cl-mahjong:view-result view))))
    (let ((game (cl-mahjong:play-bots game (loop repeat 4 collect (cl-mahjong:make-bot :dead)))))
      (is (cl-mahjong:game-over-p game))
      (is (cl-mahjong:view-final (view-of game 0))))))

;;; Events

(test seat-events-hide-the-draws-of-other-seats
  (let* ((events '((:draw :seat 0 :tile 17) (:draw :seat 1 :tile 20 :rinshan t)
                   (:discard :seat 0 :tile 17 :riichi nil)))
         (seen (cl-mahjong:seat-events 0 events)))
    (is (equal '(:draw :seat 0 :tile 17) (first seen)))
    (is (equal '(:draw :seat 1 :rinshan t) (second seen)))
    (is (equal (third events) (third seen)))))

(test bots-are-not-told-the-draws-of-other-seats
  (let* ((recorders (loop repeat 4 collect (make-instance 'recording-bot))))
    (cl-mahjong:play-bots (cl-mahjong:start-game 'riichi :seed 5) recorders :deals 1)
    (loop for recorder in recorders
          for seat from 0
          do (is (every (lambda (event)
                          (or (not (eq (first event) :draw))
                              (= seat (getf (rest event) :seat))
                              (not (getf (rest event) :tile))))
                        (seen recorder))))))

(test bots-are-given-only-a-view
  (let* ((dead (cl-mahjong:make-bot :dead))
         (calls 0)
         (game (cl-mahjong:play-bots
                (cl-mahjong:start-game 'riichi :seed 5)
                (loop repeat 4
                      collect (lambda (view)
                                (incf calls)
                                (is (typep view 'cl-mahjong:seat-view))
                                (is (member (cl-mahjong:choose-action dead view)
                                            (cl-mahjong:view-actions view) :test #'equal))
                                (cl-mahjong:choose-action dead view)))
                :deals 1)))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (plusp calls))))

(test a-dead-hand-shows-to-every-seat
  (variant-with 'dead-view :chombo :mangan :false-win :dead)
  (let ((game (cl-mahjong:apply-action
               (cl-mahjong:start-game 'dead-view :walls (list (stacked :draws '("9m"))))
               0 '(:tsumo))))
    (dolist (viewer '(0 1 2 3))
      (is (eq :false-tsumo (cl-mahjong:seen-dead (seen-by (view-of game viewer) 0))))
      (is (null (cl-mahjong:seen-dead (seen-by (view-of game viewer) 1)))))))
