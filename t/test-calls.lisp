;; t/test-calls.lisp

(in-package #:cl-mahjong-tests)
(in-suite call-mechanics-tests)

(test pon-call
  "Test pon call mechanics"
  (let ((game (create-game-state "test-game")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set up hands for testing
    (let ((p2 (get-player game :south)))
      ;; Give Bob two 5m tiles
      (setf (player-hand p2) '(:5m :5m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))

      ;; Manually set up game state for call window
      (setf (game-phase game) :call-window
            (turn-last-discard (game-turn-state game)) :5m
            (turn-last-discard-player (game-turn-state game)) :east
            (turn-current-player (game-turn-state game)) :east)

      ;; Check for possible calls
      (check-possible-calls game :5m :east)

      (is (member :south (turn-can-call-pon (game-turn-state game)))
          "Bob should be able to call pon")

      ;; Bob calls pon
      (multiple-value-bind (success error)
          (process-call-pon game "p2" nil)
        (is success (format nil "Pon call should succeed: ~A" error))
        (is (= (length (player-hand p2)) 11) "Hand should have 11 tiles after pon")
        (is (= (length (player-melds p2)) 1) "Should have 1 meld")
        (is (eq (meld-type (first (player-melds p2))) :pon) "Meld should be pon")
        (is (eq (turn-current-player (game-turn-state game)) :south)
            "Turn should transfer to caller")
        (is (eq (game-phase game) :turn-discard) "Phase should be turn-discard")))))

(test chi-call
  "Test chi call mechanics"
  (let ((game (create-game-state "test-game-2")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set up hands for testing
    (let ((p2 (get-player game :south)))
      ;; Give Bob 4m and 6m for chi with 5m
      (setf (player-hand p2) '(:4m :6m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))

      ;; Manually set up game state for call window
      (setf (game-phase game) :call-window
            (turn-last-discard (game-turn-state game)) :5m
            (turn-last-discard-player (game-turn-state game)) :east
            (turn-current-player (game-turn-state game)) :east)

      ;; Check for possible calls
      (check-possible-calls game :5m :east)

      (is (eq (turn-can-call-chi (game-turn-state game)) :south)
          "Bob (next player) should be able to call chi")

      ;; Bob calls chi
      (multiple-value-bind (success error)
          (process-call-chi game "p2" nil)
        (is success (format nil "Chi call should succeed: ~A" error))
        (is (= (length (player-hand p2)) 11) "Hand should have 11 tiles after chi")
        (is (= (length (player-melds p2)) 1) "Should have 1 meld")
        (is (eq (meld-type (first (player-melds p2))) :chi) "Meld should be chi")
        (is (eq (turn-current-player (game-turn-state game)) :south)
            "Turn should transfer to caller")))))

(test open-kan-call
  "Test open kan call mechanics"
  (let ((game (create-game-state "test-game-3")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set all players ready and start game to initialize wall
    (dolist (seat '(:east :south :west :north))
      (let ((p (get-player game seat)))
        (player-set-ready p t)))
    (start-game game)

    ;; Set up hands for testing
    (let ((p2 (get-player game :south)))
      ;; Give Bob three 5m tiles (plus one from discard = 4 for kan)
      (setf (player-hand p2) '(:5m :5m :5m :2p :3p :4p :6s :7s :8s :east :east :west :west))

      ;; Manually set up game state for call window
      (setf (game-phase game) :call-window
            (turn-last-discard (game-turn-state game)) :5m
            (turn-last-discard-player (game-turn-state game)) :east
            (turn-current-player (game-turn-state game)) :east)

      ;; Check for possible calls (pon includes kan)
      (check-possible-calls game :5m :east)

      (is (member :south (turn-can-call-pon (game-turn-state game)))
          "Bob should be able to call kan (via pon check)")

      ;; Bob calls open kan
      (multiple-value-bind (success error)
          (process-call-kan game "p2" '(:kan-type :open))
        (is success (format nil "Open kan call should succeed: ~A" error))
        ;; Hand should have 11 tiles (14 - 3 for kan + 1 replacement = 12, then -1 for pending discard)
        (is (= (length (player-melds p2)) 1) "Should have 1 meld")
        (is (eq (meld-type (first (player-melds p2))) :kan-open) "Meld should be open kan")
        (is (turn-rinshan-active (game-turn-state game))
            "Rinshan flag should be set")
        (is (eq (game-phase game) :turn-discard) "Phase should be turn-discard")))))

(test pass-call
  "Test pass call mechanics"
  (let ((game (create-game-state "test-game-4")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set up hands for testing
    (let ((p2 (get-player game :south))
          (p3 (get-player game :west)))
      ;; Give Bob and Charlie pairs for pon
      (setf (player-hand p2) '(:5m :5m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))
      (setf (player-hand p3) '(:5m :5m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))

      ;; Manually set up game state for call window
      (setf (game-phase game) :call-window
            (turn-last-discard (game-turn-state game)) :5m
            (turn-last-discard-player (game-turn-state game)) :east
            (turn-current-player (game-turn-state game)) :east)

      ;; Check for possible calls
      (check-possible-calls game :5m :east)

      (is (= (length (turn-can-call-pon (game-turn-state game))) 2)
          "Two players should be able to call pon")

      ;; Bob passes
      (multiple-value-bind (success error)
          (process-pass game "p2")
        (is success (format nil "Pass should succeed: ~A" error))
        (is (= (length (turn-can-call-pon (game-turn-state game))) 1)
            "Only one player should remain"))

      ;; Charlie also passes
      (multiple-value-bind (success error)
          (process-pass game "p3")
        (is success (format nil "Pass should succeed: ~A" error))
        (is (eq (game-phase game) :turn-draw) "Phase should advance to turn-draw")
        (is (eq (turn-current-player (game-turn-state game)) :south)
            "Turn should advance to next player")))))

(test aka-dora-counting
  "Test red five (aka-dora) counting"
  (let* ((hand '(:2m :3m :4m :5m-r :6m :7m :2s :3s :4s :6s :7s :8s :white :white))
         (melds nil)
         (dora-indicators '(:1m))
         (ura-dora-indicators '(:5s))
         (dora-tiles (calculate-dora dora-indicators))
         (ura-dora-tiles (calculate-dora ura-dora-indicators))
         (aka-dora-enabled-p t))

    (let ((dora-count (count-dora hand melds dora-tiles ura-dora-tiles aka-dora-enabled-p)))
      (is (= dora-count 3)
          "Should have 3 dora total: 1 regular + 1 ura + 1 aka"))))

(test call-detection
  "Test automatic call detection after discard"
  (let ((game (create-game-state "test-game")))
    ;; Add players
    (add-player-to-game game "p1" "Alice" :east)
    (add-player-to-game game "p2" "Bob" :south)
    (add-player-to-game game "p3" "Charlie" :west)
    (add-player-to-game game "p4" "Diana" :north)

    ;; Set up hands
    (setf (player-hand (get-player game :south)) '(:5m :5m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))
    (setf (player-hand (get-player game :west)) '(:4m :6m :2p :3p :4p :6s :7s :8s :east :east :east :west :west))

    ;; Check calls for :5m discarded by east
    (check-possible-calls game :5m :east)

    (is (member :south (turn-can-call-pon (game-turn-state game)))
        "South should be able to call pon")
    (is (eq (turn-can-call-chi (game-turn-state game)) :south)
        "South (next player) should be able to call chi")))
