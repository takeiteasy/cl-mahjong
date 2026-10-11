(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun play-randomly (game events seed &key until-deal-over)
  "(values game events): GAME, which started with EVENTS, played with random actions."
  (let ((rng (cl-mahjong::make-rng seed))
        (batches (list events)))
    (loop until (or (cl-mahjong:game-over-p game)
                    (and until-deal-over (eq (cl-mahjong:game-phase game) :deal-over)))
          do (multiple-value-bind (next new)
                 (if (eq (cl-mahjong:game-phase game) :deal-over)
                     (cl-mahjong:next-deal game)
                     (let ((seat (first (cl-mahjong:deciding-seats game))))
                       (cl-mahjong:apply-action game seat (random-action game seat rng))))
               (setf game next)
               (push new batches)))
    (values game (apply #'append (nreverse batches)))))

(defun replays-p (game &optional events)
  "Whether replaying GAME's log gives an equal game, and equal EVENTS when given."
  (multiple-value-bind (replayed replayed-events)
      (cl-mahjong:replay-game (cl-mahjong:game-variant game) (cl-mahjong:game-log game))
    (and (equalp game replayed)
         (or (null events) (equalp events replayed-events)))))

(test self-play-games-replay-from-their-log
  (dolist (variant '(riichi tenhou tenhou-sanma))
    (dolist (seed '(1 2 3))
      (multiple-value-bind (game events) (cl-mahjong:start-game variant :seed seed)
        (multiple-value-bind (game events) (play-randomly game events seed)
          (is (replays-p game events) "~a seed ~a" variant seed))))))

(test a-game-started-part-way-replays
  (multiple-value-bind (game events)
      (cl-mahjong:start-game 'riichi :seed 5 :points '(30000 20000 25000 25000)
                                     :kyoku 2 :honba 1 :sticks 2)
    (multiple-value-bind (game events) (play-randomly game events 5)
      (let ((log (cl-mahjong:game-log game)))
        (is (equal '(30000 20000 25000 25000) (getf log :points)))
        (is (= 2 (getf log :kyoku)))
        (is (= 1 (getf log :honba)))
        (is (= 2 (getf log :sticks))))
      (is (replays-p game events)))))

(test the-log-holds-the-actions-in-order
  (let* ((game (act (new-game :draws '("9m")) 0 :discard "9m"))
         (actions (getf (cl-mahjong:game-log game) :actions)))
    (is (= 1 (length actions)))
    (is (eq 0 (first (first actions))))
    (is (eq :discard (first (second (first actions)))))))

(test a-stacked-wall-is-logged-and-replays
  (let ((game (pass-all (act (new-game :draws '("9m" "8m")) 0 :discard "9m"))))
    (is (= 1 (length (getf (cl-mahjong:game-log game) :walls))))
    (is (replays-p game))))

(test a-game-saved-mid-claim-replays
  (let ((game (act (chombo-game :mangan :draws '("9m")) 0 :discard "9m")))
    (is (eq :claim (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (replays-p game))))

(test a-game-saved-just-after-next-deal-replays
  (multiple-value-bind (game events) (cl-mahjong:start-game 'riichi :seed 7)
    (multiple-value-bind (over events) (play-randomly game events 7 :until-deal-over t)
      (multiple-value-bind (next more) (cl-mahjong:next-deal over)
        (is (equal '(:next-deal) (car (last (getf (cl-mahjong:game-log next) :actions)))))
        (is (eq :playing (cl-mahjong:game-phase next)))
        (is (replays-p next (append events more)))))))

(test a-deal-that-ends-with-no-actions-replays
  (let ((game (hk-dealt-game (subseq *flowers* 0 8))))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (null (getf (cl-mahjong:game-log game) :actions)))
    (is (replays-p game))))

(test chombo-games-replay
  (let ((void (cl-mahjong:apply-action (chombo-game :mangan :draws '("9m")) 0 '(:tsumo)))
        (dead (cl-mahjong:apply-action (chombo-game :none :draws '("9m") :false-win :dead)
                                       0 '(:tsumo))))
    (is (eq :deal-over (cl-mahjong:game-phase void)))
    (is (replays-p void))
    (is (eq :playing (cl-mahjong:game-phase dead)))
    (is (replays-p dead))))

(test a-record-restores-through-a-stream
  (multiple-value-bind (game events) (cl-mahjong:start-game 'tenhou :seed 11)
    (multiple-value-bind (game events) (play-randomly game events 11)
      (let ((text (with-output-to-string (out) (cl-mahjong:write-game game out))))
        (multiple-value-bind (restored restored-events)
            (with-input-from-string (in text) (cl-mahjong:read-game in))
          (is (equalp game restored))
          (is (equalp events restored-events)))))))

(test a-record-names-its-variant
  (let ((game (cl-mahjong:start-game 'tenhou :seed 1)))
    (is (eq :tenhou (getf (cl-mahjong:game-record game) :variant)))))

(test an-unregistered-variant-cannot-be-recorded
  (let ((game (cl-mahjong:start-game 'riichi :seed 1)))
    (setf (cl-mahjong::game-variant game) (cl-mahjong::copy-variant (cl-mahjong:game-variant game)))
    (signals cl-mahjong:invalid-definition (cl-mahjong:game-record game))))

(test a-wall-drawn-from-cannot-be-stacked
  (let* ((variant (cl-mahjong:find-variant 'riichi))
         (wall (cl-mahjong:make-wall (cl-mahjong:variant-tileset variant) :seed 1)))
    (signals cl-mahjong:illegal-action
      (cl-mahjong:start-game variant :walls (list (nth-value 1 (cl-mahjong:wall-draw wall)))))))
