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

(test sanma-riichi-needs-three-live-tiles
  (is (= 3 (sanma-option :riichi-min-tiles))))

(test sanma-three-matching-winds-do-not-abort
  (is (null (sanma-option :suufon-renda)))
  (is (null (sanma-option :suucha-riichi))))

;;; Nukidora

(defparameter *kita-hands*
  '("19m147p258s12356z" "19m258p147s12357z" "19m369p369s12567z"))

(defparameter *kokushi-north-hands*
  '("19m147p258s12356z" "19m19p19s1123567z" "19m369p369s12567z"))

(cl-mahjong:defvariant sanma-no-kita :extends sanma (:set :nukidora nil))
(cl-mahjong:defvariant sanma-kita-no-ron :extends sanma (:set :kita-ron nil))
(cl-mahjong:defvariant sanma-kita-ippatsu :extends sanma (:set :kita-ippatsu t))
(cl-mahjong:defvariant sanma-no-kita-rinshan :extends sanma (:set :kita-rinshan nil))
(cl-mahjong:defvariant sanma-kita-continues :extends sanma (:set :kita-interrupts nil))

(defun sanma-game (variant &rest args)
  (apply #'new-game :variant variant :hands *kita-hands* args))

(test kita-is-offered-for-a-north-in-hand
  (let ((game (sanma-game 'sanma :draws '("4z"))))
    (is (member '(:kita) (cl-mahjong:legal-actions game 0) :test #'equal)))
  (let ((game (sanma-game 'sanma :draws '("1m"))))
    (is (not (member :kita (action-types game 0))))))

(test kita-needs-the-option
  (let ((game (sanma-game 'sanma-no-kita :draws '("4z"))))
    (is (not (member :kita (action-types game 0))))))

(test riichi-sets-aside-only-the-drawn-north
  (flet ((second-turn (draw)
           (let ((game (new-game :variant 'sanma
                                 :hands '("234p567p789p12s44z" "19m258p147s12357z"
                                          "19m369p369s12567z")
                                 :draws (list "9m" "5s" "7s" draw))))
             (setf game (act game 0 :riichi "9m"))
             (setf game (pass-all game))
             (dolist (seat '(1 2))
               (setf game (cl-mahjong:apply-action game seat (list :discard (drawn-tile game))))
               (setf game (pass-all game)))
             game)))
    (is (not (member :kita (action-types (second-turn "1m") 0))))
    (is (member :kita (action-types (second-turn "4z") 0)))))

(test kita-sets-the-north-aside-and-draws-a-replacement
  (let* ((game (sanma-game 'sanma :draws '("4z") :rinshan '("5p")))
         (live (cl-mahjong:wall-live-count (cl-mahjong:deal-wall (cl-mahjong:game-deal game)))))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 0 '(:kita))
      (is (equal '(:kita :draw) (mapcar #'first events)))
      (is (getf (rest (second events)) :kita))
      (is (= 1 (length (cl-mahjong:player-nuki (seat-player game 0)))))
      (is (= 14 (length (cl-mahjong:player-tiles (seat-player game 0)))))
      (is (null (cl-mahjong:player-melds (seat-player game 0))))
      (is (= (first (kind-ids (tileset-of game) "5p"))
             (cl-mahjong:tile-kind-id (tileset-of game) (drawn-tile game))))
      (is (= 0 (turn-of game)))
      (is (= (1- live) (cl-mahjong:wall-live-count (cl-mahjong:deal-wall (cl-mahjong:game-deal game))))))))

(test kita-can-repeat-until-the-nuki-slots-run-out
  (let ((game (sanma-game 'sanma :draws '("4z") :rinshan '("4z" "4z" "4z" "5p"))))
    (dotimes (i 4)
      (is (member '(:kita) (cl-mahjong:legal-actions game 0) :test #'equal))
      (setf game (cl-mahjong:apply-action game 0 '(:kita))))
    (is (= 4 (length (cl-mahjong:player-nuki (seat-player game 0)))))
    (is (not (member :kita (action-types game 0))))))

(test kita-breaks-ippatsu-and-interrupts-by-default
  (flet ((after-kita (variant)
           (let ((game (new-game :variant variant
                                 :hands '("123p456p789p12s11z" "19m258p147s12357z"
                                          "19m369p369s12567z")
                                 :draws '("9m" "4z") :rinshan '("5p"))))
             (setf game (act game 0 :riichi "9m"))
             (setf game (pass-all game))
             (is (cl-mahjong:player-ippatsu (seat-player game 0)))
             (setf game (cl-mahjong:apply-action game 1 '(:kita)))
             game)))
    (let ((game (after-kita 'sanma)))
      (is (null (cl-mahjong:player-ippatsu (seat-player game 0))))
      (is (cl-mahjong:deal-interrupted (cl-mahjong:game-deal game))))
    (let ((game (after-kita 'sanma-kita-ippatsu)))
      (is (cl-mahjong:player-ippatsu (seat-player game 0))))
    (let ((game (after-kita 'sanma-kita-continues)))
      (is (not (cl-mahjong:deal-interrupted (cl-mahjong:game-deal game)))))))

(test kita-replacement-is-rinshan-by-default
  (let ((game (sanma-game 'sanma :draws '("4z") :rinshan '("5p"))))
    (is (cl-mahjong:deal-rinshan (cl-mahjong:game-deal
                                  (cl-mahjong:apply-action game 0 '(:kita))))))
  (let ((game (sanma-game 'sanma-no-kita-rinshan :draws '("4z") :rinshan '("5p"))))
    (is (not (cl-mahjong:deal-rinshan (cl-mahjong:game-deal
                                       (cl-mahjong:apply-action game 0 '(:kita))))))))

(test ron-on-a-kita
  (let ((game (new-game :variant 'sanma :hands *kokushi-north-hands* :draws '("4z")
                        :rinshan '("5p"))))
    (setf game (cl-mahjong:apply-action game 0 '(:kita)))
    (is (equal '((:ron) (:pass)) (cl-mahjong:legal-actions game 1)))
    (is (null (cl-mahjong:legal-actions game 2)))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 1 '(:ron))
      (is (eq :ron (result-of game :type)))
      (let ((win (getf (rest (win-event events)) :score)))
        (is (equal '((:kokushi . 1))
                   (cl-mahjong:reading-yaku (cl-mahjong:score-reading win)))))
      (is (= 0 (getf (rest (win-event events)) :from))))))

(test passing-on-a-kita-ron-is-furiten
  (let ((game (new-game :variant 'sanma :hands *kokushi-north-hands* :draws '("4z")
                        :rinshan '("5p"))))
    (setf game (cl-mahjong:apply-action game 0 '(:kita)))
    (setf game (cl-mahjong:apply-action game 1 '(:pass)))
    (is (cl-mahjong:player-temp-furiten (seat-player game 1)))
    (is (= 1 (length (cl-mahjong:player-nuki (seat-player game 0)))))
    (is (= 0 (turn-of game)))))

(test no-ron-on-a-kita-by-option
  (let ((game (new-game :variant 'sanma-kita-no-ron :hands *kokushi-north-hands*
                        :draws '("4z") :rinshan '("5p"))))
    (setf game (cl-mahjong:apply-action game 0 '(:kita)))
    (is (null (cl-mahjong:legal-actions game 1)))
    (is (= 1 (length (cl-mahjong:player-nuki (seat-player game 0)))))))

(test kita-counts-as-dora-han
  (let* ((ts (cl-mahjong:find-tileset 'sanma-108))
         (tiles (cl-mahjong:tiles-of-kind ts (cl-mahjong:find-kind ts 'north)))
         (hand (hand ts "234p567p789p12s11z3s"))
         (context (context ts "3s" :tsumo t :riichi t :nuki (list (first tiles) (second tiles))))
         (score (cl-mahjong:best-score
                 (cl-mahjong:evaluate-score 'sanma ts *riichi-shapes* cl-mahjong:*riichi-yaku*
                                            hand '() context :players 3))))
    (is (equal '(:dora 0 :ura 0 :aka 0 :nuki 2) (cl-mahjong:score-dora score)))
    (is (= 4 (cl-mahjong:score-han score)))))

(test a-north-dora-indicator-counts-the-kita-again
  (let* ((ts (cl-mahjong:find-tileset 'sanma-108))
         (north (cl-mahjong:find-kind ts 'north))
         (west (first (cl-mahjong:tiles-of-kind ts (cl-mahjong:find-kind ts 'west))))
         (tiles (cl-mahjong:tiles-of-kind ts north))
         (hand (hand ts "234p567p789p12s11z3s"))
         (context (context ts "3s" :tsumo t :riichi t :nuki (list (first tiles) (second tiles))
                                   :dora-indicators (list west)))
         (score (cl-mahjong:best-score
                 (cl-mahjong:evaluate-score 'sanma ts *riichi-shapes* cl-mahjong:*riichi-yaku*
                                            hand '() context :players 3))))
    (is (= 2 (getf (cl-mahjong:score-dora score) :dora)))
    (is (= 2 (getf (cl-mahjong:score-dora score) :nuki)))))

(test kita-alone-is-no-yaku
  (let* ((ts (cl-mahjong:find-tileset 'sanma-108))
         (tiles (cl-mahjong:tiles-of-kind ts (cl-mahjong:find-kind ts 'north)))
         (hand (hand ts "234p567p789p12s11z3s"))
         (context (context ts "3s" :nuki (list (first tiles) (second tiles)))))
    (is (null (cl-mahjong:evaluate-score 'sanma ts *riichi-shapes* cl-mahjong:*riichi-yaku*
                                         hand '() context :players 3)))))

(defun play-kita-deal (game)
  "Like `play-deal', but a kita is made whenever it is legal."
  (loop while (eq (cl-mahjong:game-phase game) :playing)
        do (let* ((seat (first (cl-mahjong:deciding-seats game)))
                  (action (if (member '(:kita) (cl-mahjong:legal-actions game seat) :test #'equal)
                              (list :kita)
                              (bot-action game seat))))
             (setf game (cl-mahjong:apply-action game seat action))))
  game)

(defun play-kita-game (game)
  (loop until (cl-mahjong:game-over-p game)
        do (setf game (if (eq (cl-mahjong:game-phase game) :deal-over)
                          (cl-mahjong:next-deal game)
                          (play-kita-deal game))))
  game)

(test sanma-self-play-with-kita-conserves-points
  (dolist (seed '(11 12 13 14 15 16))
    (let ((game (play-kita-game (cl-mahjong:start-game 'sanma :seed seed))))
      (is (cl-mahjong:game-over-p game))
      (is (= 105000 (reduce #'+ (getf (cl-mahjong:game-final game) :points)))))))

;;; Split north share

(defun split-payments (base &rest args)
  (multiple-value-list
   (apply #'cl-mahjong::payments-for
          (cl-mahjong::scoring-payments (cl-mahjong::resolve-scoring 'sanma-split))
          base args)))

(test sanma-split-shares-the-missing-north-between-the-payers
  (is (equal '((:dealer 700 :other 500) 1200) (split-payments 240 nil t 0 3)))
  (is (equal '((:dealer nil :other 800) 1600) (split-payments 240 t t 0 3)))
  (is (equal '((:dealer 5000 :other 3000) 8000) (split-payments 2000 nil t 0 3)))
  (is (equal '((:dealer nil :other 6000) 12000) (split-payments 2000 t t 0 3))))

(test sanma-split-leaves-ron-and-honba-alone
  (is (equal '((:ron 8000) 8000) (split-payments 2000 nil nil 0 3)))
  (is (equal '((:dealer 5100 :other 3100) 8200) (split-payments 2000 nil t 1 3))))

(test sanma-split-limit-payments-and-variant
  (multiple-value-bind (payments total) (cl-mahjong:limit-payments 'sanma-split :mangan :dealer nil :players 3)
    (is (equal '(:dealer 5000 :other 3000) payments))
    (is (= 8000 total)))
  (is (eq (cl-mahjong:find-scoring 'sanma-split)
          (cl-mahjong:find-scoring (cl-mahjong:variant-scoring (cl-mahjong:find-variant 'sanma-split)))))
  (is (equal '((:dealer 4000 :other 2000) 6000)
             (multiple-value-list
              (cl-mahjong::payments-for (cl-mahjong::scoring-payments (cl-mahjong::resolve-scoring 'sanma))
                                        2000 nil t 0 3)))))

(test sanma-split-games-finish-and-conserve-points
  (dolist (seed '(1 2 3))
    (let ((game (play-game (cl-mahjong:start-game 'sanma-split :seed seed))))
      (is (cl-mahjong:game-over-p game))
      (is (= 105000 (reduce #'+ (getf (cl-mahjong:game-final game) :points)))))))

;;; Wait checks count the nuki

(defun notation-tiles (tileset notation &key (skip 0))
  "Distinct tiles for NOTATION; SKIP leaves that many copies of each kind unused first."
  (let ((counts (hand tileset notation)))
    (loop for id below (length counts)
          when (plusp (aref counts id))
            append (subseq (cl-mahjong:tiles-of-kind tileset (cl-mahjong:tileset-kind tileset id))
                           skip (+ skip (aref counts id))))))

(defun north-tiles (tileset count)
  (subseq (cl-mahjong:tiles-of-kind tileset (cl-mahjong:find-kind tileset 'north)) 0 count))

(defun north-wait-live-p (nuki-count)
  (let* ((game (cl-mahjong:start-game 'sanma :seed 1))
         (ts (tileset-of game))
         (north (kind-of ts 'north)))
    (member north (cl-mahjong::winning-kinds game (hand ts "234p567p789p123s4z") '()
                                             :nuki (north-tiles ts nuki-count)))))

(test winning-kinds-counts-the-tiles-set-aside
  (is (north-wait-live-p 2))
  (is (not (north-wait-live-p 3))))

(defun tenpai-with (game notation &key nuki melds)
  (let ((player (cl-mahjong::make-player (notation-tiles (tileset-of game) notation))))
    (setf (cl-mahjong:player-nuki player) nuki
          (cl-mahjong:player-melds player) melds)
    (cl-mahjong::tenpai-p game player)))

(test a-wait-on-four-held-tiles-is-noten
  (let ((game (new-game)))
    (is (tenpai-with game "123m456m789p1112s"))
    (is (not (tenpai-with game "123m456m789p1111s")))))

(test a-north-wait-set-aside-in-full-is-noten
  (let* ((game (sanma-game 'sanma))
         (ts (tileset-of game)))
    (is (tenpai-with game "234p567p789p123s4z"))
    (is (not (tenpai-with game "234p567p789p123s4z" :nuki (north-tiles ts 3))))))

(test a-tanki-on-the-fourth-tile-of-a-pon-is-tenpai
  (let ((game (new-game)))
    (is (tenpai-with game "123m456m789p5s"
                     :melds (list (cl-mahjong:make-meld :triplet (kind-ids (tileset-of game) "555s")
                                                        :open t))))))
