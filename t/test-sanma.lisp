;; test-sanma.lisp
;; Tests for 3-player (sanma) mahjong implementation
;;
;; Copyright (C) 2025 George Watson

(in-package #:cl-mahjong/test)
(in-suite sanma-tests)

(test sanma-config
  "Test sanma configuration"
  (let ((sanma-config +sanma-config+)
        (yonma-config +yonma-config+))
    ;; Test sanma config
    (is (eq (game-config-variant sanma-config) :sanma))
    (is (= (game-config-num-players sanma-config) 3))
    (is (equal (game-config-seats sanma-config) '(:east :south :west)))
    (is (= (game-config-starting-points sanma-config) 35000))
    (is (= (game-config-wall-size sanma-config) 108))
    (is (= (game-config-dead-wall-size sanma-config) 17))
    (is (not (game-config-chi-enabled-p sanma-config)))
    (is (game-config-north-nuki-enabled-p sanma-config))

    ;; Test yonma config
    (is (eq (game-config-variant yonma-config) :yonma))
    (is (= (game-config-num-players yonma-config) 4))
    (is (equal (game-config-seats yonma-config) '(:east :south :west :north)))
    (is (= (game-config-starting-points yonma-config) 25000))
    (is (= (game-config-wall-size yonma-config) 136))
    (is (= (game-config-dead-wall-size yonma-config) 14))
    (is (game-config-chi-enabled-p yonma-config))
    (is (not (game-config-north-nuki-enabled-p yonma-config)))))

(test seat-navigation
  "Test seat navigation with 3 and 4 players"
  ;; Test 3-player navigation
  (is (eq (next-seat :east +sanma-config+) :south))
  (is (eq (next-seat :south +sanma-config+) :west))
  (is (eq (next-seat :west +sanma-config+) :east))

  (is (eq (prev-seat :east +sanma-config+) :west))
  (is (eq (prev-seat :west +sanma-config+) :south))
  (is (eq (prev-seat :south +sanma-config+) :east))

  ;; Test seat relations in 3-player
  (is (eq (seat-relation :east :south +sanma-config+) :left))
  (is (eq (seat-relation :east :west +sanma-config+) :right))
  (is (eq (seat-relation :south :west +sanma-config+) :left))
  (is (eq (seat-relation :south :east +sanma-config+) :right))

  ;; Test 4-player navigation (backward compatibility)
  (is (eq (next-seat :east +yonma-config+) :south))
  (is (eq (next-seat :north +yonma-config+) :east))
  (is (eq (seat-relation :east :west +yonma-config+) :across)))

(test wall-creation
  "Test wall creation for both variants"
  ;; Test sanma wall
  (let ((sanma-wall (create-wall +sanma-config+)))
    (is (= (length (wall-live-wall sanma-wall)) 91))
    (is (= (length (wall-dead-wall sanma-wall)) 17))
    (is (= (wall-remaining-draws sanma-wall) 91))

    ;; Count unique tiles (should be 27 unique * 4 copies = 108 total)
    (let* ((all-tiles (append (wall-live-wall sanma-wall)
                              (wall-dead-wall sanma-wall)))
           (tile-counts (make-hash-table :test 'eq)))
      (dolist (tile all-tiles)
        (incf (gethash tile tile-counts 0)))
      (is (= (length all-tiles) 108))

      ;; Verify man 2-8 are not present
      (is (zerop (gethash :2m tile-counts 0)))
      (is (zerop (gethash :3m tile-counts 0)))
      (is (zerop (gethash :4m tile-counts 0)))
      (is (zerop (gethash :5m tile-counts 0)))
      (is (zerop (gethash :6m tile-counts 0)))
      (is (zerop (gethash :7m tile-counts 0)))
      (is (zerop (gethash :8m tile-counts 0)))

      ;; Verify 1m and 9m are present (4 copies each)
      (is (= (gethash :1m tile-counts 0) 4))
      (is (= (gethash :9m tile-counts 0) 4))))

  ;; Test yonma wall (backward compatibility)
  (let ((yonma-wall (create-wall +yonma-config+)))
    (is (= (length (wall-live-wall yonma-wall)) 122))
    (is (= (length (wall-dead-wall yonma-wall)) 14))
    (is (= (wall-remaining-draws yonma-wall) 122))))

(test north-extraction
  "Test North wind extraction mechanics"
  (let ((player (create-player "p1" "TestPlayer" :east +sanma-config+)))
    ;; Initially no norths extracted
    (is (= (player-extracted-norths player) 0))

    ;; Add a North to hand
    (player-add-tile player :north)
    (is (= (player-hand-size player) 1))

    ;; Test can-extract-north
    (is (player-can-extract-north player +sanma-config+))

    ;; Extract the North
    (is (player-extract-north player))
    (is (= (player-extracted-norths player) 1))
    (is (= (player-hand-size player) 0))

    ;; Add multiple Norths
    (player-add-tile player :north)
    (player-add-tile player :north)
    (player-add-tile player :north)
    (is (= (player-hand-size player) 3))

    ;; Extract all
    (dotimes (i 3)
      (player-extract-north player))
    (is (= (player-extracted-norths player) 4))
    (is (= (player-hand-size player) 0))

    ;; Reset for new hand
    (player-reset-for-new-hand player)
    (is (= (player-extracted-norths player) 0)))

  ;; Test that North extraction is disabled in yonma
  (let ((player (create-player "p2" "TestPlayer2" :east +yonma-config+)))
    (player-add-tile player :north)
    (is (not (player-can-extract-north player +yonma-config+)))))

(test chi-disabled
  "Test that chi is disabled in sanma"
  (let ((hand '(:2p :3p :5p :6p :7p)))
    ;; In sanma, chi should return nil even with valid tiles
    (let ((result (find-sequence-chi hand :4p :south :east +sanma-config+)))
      (is (null result)))

    ;; In yonma, chi should work
    (let ((result (find-sequence-chi hand :4p :south :east +yonma-config+)))
      (is result)
      (is (member '(:2p :3p :4p) result :test #'equal)))))

(test starting-points
  "Test starting points for both variants"
  (let ((sanma-player (create-player "p1" "SanmaPlayer" :east +sanma-config+))
        (yonma-player (create-player "p2" "YonmaPlayer" :east +yonma-config+)))
    (is (= (player-score sanma-player) 35000))
    (is (= (player-score yonma-player) 25000))))

(test tsumo-payments
  "Test tsumo payment calculations for sanma"
  (let ((context (make-winning-context
                  :tsumo-p t
                  :winning-tile :1m
                  :dealer-p nil
                  :honba 0
                  :riichi-sticks 0)))
    ;; Test sanma non-dealer tsumo: each pays 2× base
    (let ((payment (split-payment 1000 context +sanma-config+)))
      (is (= (payment-split-non-dealer-pays payment) 2000))
      (is (= (payment-split-winner-receives payment) 4000)))

    ;; Test sanma dealer tsumo: each pays 2× base
    (setf (winning-context-dealer-p context) t)
    (let ((payment (split-payment 1000 context +sanma-config+)))
      (is (= (payment-split-non-dealer-pays payment) 2000))
      (is (= (payment-split-winner-receives payment) 4000)))

    ;; Test yonma for comparison
    (setf (winning-context-dealer-p context) nil)
    (let ((payment (split-payment 1000 context +yonma-config+)))
      (is (= (payment-split-dealer-pays payment) 2000))
      (is (= (payment-split-non-dealer-pays payment) 1000))
      (is (= (payment-split-winner-receives payment) 4000)))))

(test dora-counting
  "Test dora counting with extracted Norths"
  (let ((hand '(:1m :2m :3m :4m :5m :6m :7m :8m :9m :east :south :west :white))
        (melds nil)
        (dora-tiles '(:1m :5m))
        (ura-dora-tiles nil))
    ;; Without extracted norths
    (let ((dora-count (count-dora hand melds dora-tiles ura-dora-tiles nil 0)))
      (is (= dora-count 2))) ; 1m and 5m in hand

    ;; With 3 extracted norths
    (let ((dora-count (count-dora hand melds dora-tiles ura-dora-tiles nil 3)))
      (is (= dora-count 5))))) ; 2 from hand + 3 from extracted norths

(test game-creation
  "Test creating games with different variants"
  ;; Create sanma game
  (let ((sanma-game (create-game-state "sanma-test" :sanma)))
    (is (eq (game-config-variant (game-config sanma-game)) :sanma))
    (is (= (game-config-num-players (game-config sanma-game)) 3))

    ;; Add 3 players
    (add-player-to-game sanma-game "p1" "Player 1" :east)
    (add-player-to-game sanma-game "p2" "Player 2" :south)
    (add-player-to-game sanma-game "p3" "Player 3" :west)

    ;; Check game is full
    (is (game-is-full sanma-game))
    (is (= (game-player-count sanma-game) 3))

    ;; Check all players have correct starting points
    (dolist (seat '(:east :south :west))
      (let ((player (get-player sanma-game seat)))
        (is player)
        (is (= (player-score player) 35000)))))

  ;; Create yonma game (default)
  (let ((yonma-game (create-game-state "yonma-test")))
    (is (eq (game-config-variant (game-config yonma-game)) :yonma))
    (is (= (game-config-num-players (game-config yonma-game)) 4))))

(test game-flow
  "Test basic game flow in sanma"
  (let ((game (create-game-state "flow-test" :sanma)))
    ;; Add players
    (add-player-to-game game "p1" "Player 1" :east)
    (add-player-to-game game "p2" "Player 2" :south)
    (add-player-to-game game "p3" "Player 3" :west)

    ;; Mark all ready
    (dolist (seat '(:east :south :west))
      (player-set-ready (get-player game seat) t))

    ;; Check all players ready
    (is (all-players-ready game))

    ;; Start game
    (is (start-game game))

    ;; Check wall was created with correct size
    (let ((wall (game-wall game)))
      (is wall)
      (is (<= (wall-remaining-draws wall) 91)))

    ;; Check all players have hands
    (dolist (seat '(:east :south :west))
      (let ((player (get-player game seat)))
        (is (> (player-hand-size player) 0))))

    ;; Check dealer has 14 tiles
    (let ((dealer (get-player game :east)))
      (is (or (= (player-hand-size dealer) 14)
              ;; Account for possible North extractions
              (>= (+ (player-hand-size dealer)
                    (player-extracted-norths dealer)) 14))))

    ;; Test turn advancement
    (is (eq (turn-current-player (game-turn-state game)) :east))
    (advance-turn game)
    (is (eq (turn-current-player (game-turn-state game)) :south))
    (advance-turn game)
    (is (eq (turn-current-player (game-turn-state game)) :west))
    (advance-turn game)
    (is (eq (turn-current-player (game-turn-state game)) :east))))
