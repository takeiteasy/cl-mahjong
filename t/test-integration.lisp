;; t/test-integration.lisp

(in-package #:cl-mahjong/test)
(in-suite integration-tests)

(test furiten-check
  "Test furiten checking"
  (let* ((player (create-player "p1" "Alice" :south)))
    ;; Add some discards
    (setf (player-discards player) '(:5m :2p :white))

    ;; Check furiten for :5m (should be true - in discards)
    (is (check-furiten-for-ron player :5m)
        "Should be furiten for :5m (in discards)")

    ;; Check furiten for :3s (should be false - not in discards)
    (is (not (check-furiten-for-ron player :3s))
        "Should not be furiten for :3s (not in discards)")))

(test payment-ron
  "Test payment application for ron"
  (let* ((game (create-game-state "test-game")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set up scores
    (dolist (seat '(:east :south :west :north))
      (let ((p (get-player game seat)))
        (setf (player-score p) 25000)))

    ;; Create a simple payment (non-dealer from non-dealer)
    (let* ((payment (make-payment-split
                     :direct-payment 3900
                     :winner-receives 3900))
           (context (make-winning-context
                     :tsumo-p nil
                     :winning-tile :5m
                     :winner-seat :south
                     :loser-seat :west
                     :dealer-p nil)))

      (let ((winner-before (player-score (get-player game :south)))
            (loser-before (player-score (get-player game :west))))

        (apply-payment-to-players game :south payment context)

        (is (= (player-score (get-player game :south))
               (+ winner-before 3900))
            "Winner should gain 3900 points")
        (is (= (player-score (get-player game :west))
               (- loser-before 3900))
            "Loser should lose 3900 points")))))

(test payment-tsumo
  "Test payment application for tsumo"
  (let* ((game (create-game-state "test-game")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set dealer
    (setf (game-dealer-seat game) :east)

    ;; Set up scores
    (dolist (seat '(:east :south :west :north))
      (let ((p (get-player game seat)))
        (setf (player-score p) 25000)))

    ;; Non-dealer wins by tsumo
    (let* ((payment (make-payment-split
                     :dealer-pays 1300
                     :non-dealer-pays 700
                     :winner-receives 2700))
           (context (make-winning-context
                     :tsumo-p t
                     :winning-tile :5m
                     :winner-seat :south
                     :dealer-p nil)))

      (let ((winner-before (player-score (get-player game :south)))
            (dealer-before (player-score (get-player game :east)))
            (west-before (player-score (get-player game :west)))
            (north-before (player-score (get-player game :north))))

        (apply-payment-to-players game :south payment context)

        (is (= (player-score (get-player game :south))
               (+ winner-before 2700))
            "Winner should gain 2700 points")
        (is (= (player-score (get-player game :east))
               (- dealer-before 1300))
            "Dealer should pay 1300 points")
        (is (= (player-score (get-player game :west))
               (- west-before 700))
            "West should pay 700 points")
        (is (= (player-score (get-player game :north))
               (- north-before 700))
            "North should pay 700 points")))))

(test event-tracking
  "Test event tracking (rinshan, chankan)"
  (let* ((game (create-game-state "test-game")))
    ;; Initialize turn state
    (setf (game-turn-state game) (make-instance 'turn-state))

    ;; Check initial state
    (is (not (turn-rinshan-active (game-turn-state game)))
        "Rinshan should not be active initially")
    (is (not (turn-chankan-active (game-turn-state game)))
        "Chankan should not be active initially")

    ;; Track rinshan
    (track-rinshan-event game)
    (is (turn-rinshan-active (game-turn-state game))
        "Rinshan should be active after tracking")

    ;; Clear events
    (clear-event-tracking game)
    (is (not (turn-rinshan-active (game-turn-state game)))
        "Rinshan should be cleared")
    (is (not (turn-chankan-active (game-turn-state game)))
        "Chankan should be cleared")))

(test ippatsu-update
  "Test ippatsu status updates"
  (let* ((game (create-game-state "test-game")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set ippatsu for one player
    (setf (player-ippatsu-active (get-player game :south)) t)

    (is (player-ippatsu-active (get-player game :south))
        "South should have ippatsu active")

    ;; Update ippatsu (updates all players based on turn state)
    (update-ippatsu-status game)

    ;; South should still have ippatsu (it doesn't get cleared automatically)
    (is (player-ippatsu-active (get-player game :south))
        "South should still have ippatsu active after update")))
