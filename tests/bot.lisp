(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun choose (level game seat)
  (cl-mahjong:choose-action (cl-mahjong:make-bot level) game seat
                            (cl-mahjong:legal-actions game seat)))

(defun visible-count (game seat notation)
  (aref (cl-mahjong:seat-visible game seat) (first (kind-ids (tileset-of game) notation))))

(defun indicators-of (game notation)
  (let ((ts (tileset-of game))
        (id (first (kind-ids (tileset-of game) notation))))
    (count id (cl-mahjong:wall-dora-indicators (cl-mahjong:deal-wall (cl-mahjong:game-deal game)))
           :key (lambda (tile) (cl-mahjong:tile-kind-id ts tile)))))

;;; Visible tiles

(defun tile-of (game notation)
  (first (cl-mahjong:tiles-of-kind (tileset-of game)
                                   (cl-mahjong:tileset-kind (tileset-of game)
                                                            (first (kind-ids (tileset-of game)
                                                                             notation))))))

(defun with-seat (game seat &key discards riichi open)
  "GAME where SEAT has discarded the kinds of the DISCARDS notations and is in RIICHI, openly if OPEN."
  (let ((player (copy-structure (seat-player game seat))))
    (setf (cl-mahjong:player-discards player)
          (append (cl-mahjong:player-discards player)
                  (mapcar (lambda (notation) (tile-of game notation)) discards)))
    (when riichi
      (setf (cl-mahjong:player-riichi player) :active
            (cl-mahjong:player-open-riichi player) open))
    (cl-mahjong::replace-deal game (cl-mahjong::set-player (cl-mahjong:game-deal game) seat player))))

(test a-called-discard-is-seen-once
  (let ((game (new-game :hands *ron-hands* :draws '("6z"))))
    (setf game (act game 0 :discard "6z"))
    (is (= (+ 1 (indicators-of game "6z")) (visible-count game 3 "6z")))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (= (+ 3 (indicators-of game "6z")) (visible-count game 3 "6z")))
    (is (= (indicators-of game "6z") (visible-count game 2 "6z")))))

(test hands-are-not-visible
  (let ((game (new-game :hands *ron-hands*)))
    (dolist (seat '(0 1 2 3))
      (is (= (indicators-of game "5z") (visible-count game seat "5z"))))))

(test open-riichi-hands-are-visible
  (let ((game (with-seat (new-game :hands *ron-hands*) 1 :riichi t :open t)))
    (is (= (+ 3 (indicators-of game "5z")) (visible-count game 0 "5z")))
    (is (= (indicators-of game "5z") (visible-count game 1 "5z")))))

;;; Waits

(defparameter *waiting-hands*
  '("123m456m789p234s1z" "123456789p11z23s" "246m246p246s6677z" "357m357p357s3344z"))

(test tenpai-waits-count-the-remaining-copies
  (let* ((game (new-game :hands *waiting-hands*))
         (waits (cl-mahjong:tenpai-waits game 1)))
    (is (equal (kind-ids (tileset-of game) "14s")
               (mapcar (lambda (wait) (cl-mahjong:kind-id (car wait))) waits)))
    (is (equal '(4 4) (mapcar #'cdr waits)))
    (is (null (cl-mahjong:tenpai-waits game 2)))))

(test tenpai-waits-take-a-discard
  (let* ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("9s")))
         (waits (cl-mahjong:tenpai-waits game 0 :discard (drawn-tile game))))
    (is (= 2 (length waits)))
    (signals cl-mahjong:invalid-hand (cl-mahjong:tenpai-waits game 0))))

(test furiten-follows-the-discards
  (let ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("9s"))))
    (is (not (cl-mahjong:seat-furiten-p game 0 :discard (drawn-tile game)))))
  (let ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("4s"))))
    (is (cl-mahjong:seat-furiten-p game 0 :discard (drawn-tile game)))
    (setf game (act game 0 :discard "4s"))
    (is (cl-mahjong:seat-furiten-p game 0))))

;;; Discards

(test discard-options-rank-the-discards
  (let* ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("9s")))
         (options (cl-mahjong:discard-options game 0))
         (best (first options)))
    (is (= (drawn-tile game) (cl-mahjong:discard-option-tile best)))
    (is (= 0 (cl-mahjong:discard-option-shanten best)))
    (is (= 8 (cl-mahjong:discard-option-total best)))
    (is (apply #'<= (mapcar #'cl-mahjong:discard-option-shanten options)))
    (is (<= (cl-mahjong:discard-option-total (first (cl-mahjong:discard-options game 0 :live t)))
            8))))

(test discard-options-list-each-discard-once
  (let* ((game (new-game :hands *ron-hands*))
         (ts (tileset-of game))
         (keys (mapcar (lambda (option)
                         (let ((tile (cl-mahjong:discard-option-tile option)))
                           (cons (cl-mahjong:tile-kind-id ts tile) (cl-mahjong:tile-red-p ts tile))))
                       (cl-mahjong:discard-options game 0))))
    (is (equal keys (remove-duplicates keys :test #'equal)))
    (is (every (lambda (option)
                 (member (list :discard (cl-mahjong:discard-option-tile option))
                         (cl-mahjong:legal-actions game 0) :test #'equal))
               (cl-mahjong:discard-options game 0)))))

;;; Safety

(defun riichi-game ()
  "Seat 0 has declared riichi on 9m; seat 1 holds 1z, 9m, 5m and 6m."
  (let ((game (new-game :hands (list (first *tenpai-and-junk*) "159m159p159s2345z"
                                     "246m246p246s6677z" "357m357p357s3344z")
                        :draws '("9m"))))
    (act game 0 :riichi "9m")))

(defun discarding-game ()
  "Seat 0 is in riichi having discarded 4m and 9p; seat 1 holds 1m, 5m, 9m and 5z."
  (with-seat (new-game :hands *tenpai-and-junk*) 0 :discards '("4m" "9p") :riichi t))

(test genbutsu-is-the-safest-tile
  (let ((game (discarding-game)))
    (is (= 4 (cl-mahjong:tile-safety game 1 (tile-of game "4m"))))
    (is (= 4 (cl-mahjong:tile-safety game 1 (tile-of game "9p"))))))

(test suji-and-terminals-are-safer-than-middle-tiles
  (let ((game (discarding-game)))
    (is (= 2 (cl-mahjong:tile-safety game 1 (kind-tile game 1 "1m"))))
    (is (= 2 (cl-mahjong:tile-safety game 1 (tile-of game "7m"))))
    (is (= 1 (cl-mahjong:tile-safety game 1 (kind-tile game 1 "9m"))))
    (is (= 1 (cl-mahjong:tile-safety game 1 (kind-tile game 1 "5z"))))
    (is (= 0 (cl-mahjong:tile-safety game 1 (kind-tile game 1 "5m"))))
    (is (= 0 (cl-mahjong:tile-safety game 1 (tile-of game "6p"))))))

(test a-middle-tile-needs-both-its-suji
  (let ((game (with-seat (discarding-game) 0 :discards '("3m"))))
    (is (= 0 (cl-mahjong:tile-safety game 1 (tile-of game "6m")))))
  (let ((game (with-seat (discarding-game) 0 :discards '("3m" "9m"))))
    (is (= 2 (cl-mahjong:tile-safety game 1 (tile-of game "6m"))))))

(test nothing-is-dangerous-without-a-riichi
  (let ((game (new-game :hands *ron-hands*)))
    (is (= 4 (cl-mahjong:tile-safety game 1 (kind-tile game 1 "5z"))))))

;;; Bots

(test dead-bot-discards-the-drawn-tile-and-passes
  (let ((game (new-game :hands *ron-hands* :draws '("6z"))))
    (is (equal (list :discard (drawn-tile game)) (choose :dead game 0)))
    (setf game (act game 0 :discard "6z"))
    (is (equal '(:pass) (choose :dead game 2)))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (null (drawn-tile game)))
    (is (member (choose :dead game 2) (cl-mahjong:legal-actions game 2) :test #'equal))
    (is (eq :discard (first (choose :dead game 2))))))

(test dead-bot-does-not-take-a-ron
  (let ((game (act (new-game :hands *ron-hands* :draws '("6s")) 0 :discard "6s")))
    (is (equal '(:pass) (choose :dead game 1)))
    (is (equal '(:ron) (choose :easy game 1)))
    (is (equal '(:ron) (choose :hard game 1)))))

(test easy-bot-declares-tsumo-and-riichi
  (let ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("1s"))))
    (is (equal '(:tsumo) (choose :easy game 0))))
  (let ((game (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*)) :draws '("9s"))))
    (is (equal (list :riichi (drawn-tile game)) (choose :easy game 0)))
    (is (equal (list :riichi (drawn-tile game)) (choose :hard game 0)))))

(test hard-bot-does-not-riichi-into-furiten
  (let ((game (with-seat (new-game :hands (cons "123m456m789p23s11z" (rest *waiting-hands*))
                                   :draws '("9s"))
                0 :discards '("1s"))))
    (is (equal (list :riichi (drawn-tile game)) (choose :easy game 0)))
    (is (equal (list :discard (drawn-tile game)) (choose :hard game 0)))))

(test hard-bot-pons-yakuhai
  (let ((game (act (new-game :hands *ron-hands* :draws '("6z")) 0 :discard "6z")))
    (is (eq :pon (first (choose :hard game 2))))
    (is (equal '(:pass) (choose :easy game 2)))))

(test hard-bot-folds-against-a-riichi
  (let ((game (riichi-game)))
    (setf game (pass-all game))
    (let ((action (choose :hard game 1)))
      (is (eq :discard (first action)))
      (is (< 1 (cl-mahjong:tile-safety game 1 (second action)))))))

(defclass recording-bot (cl-mahjong:bot)
  ((seen :initform '() :accessor seen)))

(defmethod cl-mahjong:choose-action ((bot recording-bot) game seat actions)
  (cl-mahjong:choose-action (cl-mahjong:make-bot :dead) game seat actions))

(defmethod cl-mahjong:observe ((bot recording-bot) seat events)
  (declare (ignore seat))
  (setf (seen bot) (append (seen bot) events)))

(test custom-bots-and-function-bots-play-a-game
  (let* ((recorder (make-instance 'recording-bot))
         (dead (cl-mahjong:make-bot :dead))
         (game (cl-mahjong:play-bots
                (cl-mahjong:start-game 'riichi :seed 5)
                (list recorder
                      (lambda (game seat actions) (cl-mahjong:choose-action dead game seat actions))
                      dead dead))))
    (is (cl-mahjong:game-over-p game))
    (is (find :game-end (seen recorder) :key #'first))))

(test unknown-bot-levels-are-rejected
  (signals error (cl-mahjong:make-bot :godlike)))

(defun self-play (variant seed levels &key deals)
  "Play bots of LEVELS for DEALS deals, or to the end, and check that no points appear or vanish."
  (multiple-value-bind (game events) (cl-mahjong:start-game variant :seed seed)
    (let* ((game (cl-mahjong:play-bots game (mapcar #'cl-mahjong:make-bot levels)
                                       :events events :deals deals))
           (variant (cl-mahjong:game-variant game))
           (total (* (cl-mahjong:variant-players variant)
                     (cl-mahjong:variant-option variant :start-points))))
      (if deals
          (is (= total (+ (reduce #'+ (cl-mahjong:game-points game))
                          (* (cl-mahjong:variant-option variant :riichi-cost)
                             (cl-mahjong:game-sticks game)))))
          (progn (is (cl-mahjong:game-over-p game))
                 (is (= total (reduce #'+ (getf (cl-mahjong:game-final game) :points)))))))))

(test bots-play-every-variant
  (self-play 'riichi 1 '(:dead :easy :easy :dead))
  (self-play 'sanma 2 '(:easy :dead :easy))
  (self-play 'hk 3 '(:easy :dead :easy :dead) :deals 3)
  (self-play 'wrc 4 '(:easy :easy :dead :dead) :deals 3))

(test hard-bots-play-a-game
  (self-play 'riichi 5 '(:hard :easy :dead :hard))
  (self-play 'sanma 6 '(:hard :easy :dead)))
