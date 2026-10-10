(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

;;; Helpers

(defparameter *junk-hands*
  '("123m456m789p234s1z" "159m159p159s2345z" "246m246p246s6677z" "357m357p357s3344z"))

(defun stack-wall (&key hands draws last rinshan dora ura (dealer 0) (tileset 'riichi-136))
  "A wall of TILESET from tile ids: HANDS per seat, then DRAWS in draw order, LAST the final live tile, and
RINSHAN (every replacement draw, kita included), DORA and URA by dead wall slot. Everything unplaced fills the gaps."
  (let* ((ts (if (symbolp tileset) (cl-mahjong:find-tileset tileset) tileset))
         (layout (cl-mahjong::tileset-wall-layout ts))
         (count (cl-mahjong:tileset-tile-count ts))
         (live (- count (cl-mahjong::wall-layout-dead-wall layout)))
         (seats (length hands))
         (slots (cl-mahjong::wall-layout-replacement layout))
         (order (make-array count :initial-element nil))
         (used '()))
    (flet ((place (index tile)
             (setf (aref order index) tile)
             (push tile used))
           (dead (position) (+ live position)))
      (loop for hand in hands
            for seat from 0
            for slot = (mod (- seat dealer) seats)
            do (loop for tile in hand
                     for j from 0
                     do (place (if (< j 12)
                                   (+ (* 4 seats (floor j 4)) (* 4 slot) (mod j 4))
                                   (+ (* 12 seats) slot))
                               tile)))
      (loop for tile in draws for index from (* 13 seats) do (place index tile))
      (when last (place (1- live) last))
      ;; A drawn slot is refilled from the end of the live wall, so later draws come from there.
      (loop for tile in rinshan
            for k from 0
            do (place (if (< k (length slots)) (dead (nth k slots)) (- live 1 (- k (length slots))))
                      tile))
      (loop for tile in dora
            for position in (cl-mahjong::wall-layout-dora layout)
            do (place (dead position) tile))
      (loop for tile in ura
            for position in (cl-mahjong::wall-layout-ura layout)
            do (place (dead position) tile))
      (let ((rest (loop for tile below count unless (member tile used) collect tile)))
        (dotimes (index count)
          (unless (aref order index)
            (setf (aref order index) (pop rest))))))
    (cl-mahjong:make-wall ts :tiles (coerce order 'list))))

(defun stacked (&key (hands *junk-hands*) draws last rinshan dora ura (dealer 0)
                  (tileset (cl-mahjong:find-tileset 'riichi-136)))
  "A wall of TILESET: HANDS are the seats' notations, DRAWS single-tile notations in draw order."
  (let* ((ts tileset)
         (pool (make-hash-table))
         (used '()))
    (flet ((take (id)
             (let ((available (multiple-value-bind (list found) (gethash id pool)
                                (if found
                                    list
                                    (reverse (cl-mahjong:tiles-of-kind
                                              ts (cl-mahjong:tileset-kind ts id)))))))
               (unless available (error "No tile of kind ~a left" id))
               (setf (gethash id pool) (rest available))
               (push (first available) used)
               (first available))))
      (flet ((tiles (notations)
               (mapcar (lambda (notation) (take (first (kind-ids ts notation)))) notations)))
        (stack-wall :hands (mapcar (lambda (hand) (mapcar #'take (kind-ids ts hand))) hands)
                    :draws (tiles draws) :last (first (tiles (and last (list last))))
                    :rinshan (tiles rinshan) :dora (tiles dora) :ura (tiles ura)
                    :dealer dealer :tileset ts)))))

(defun new-game (&rest args &key (variant 'riichi) seed &allow-other-keys)
  (let ((wall-args (loop for (key value) on args by #'cddr
                         unless (member key '(:variant :seed)) append (list key value)))
        (tileset (cl-mahjong:variant-tileset (cl-mahjong:find-variant variant))))
    (cl-mahjong:start-game variant :seed seed
                                   :walls (list (apply #'stacked :tileset tileset wall-args)))))

(defun tileset-of (game)
  (cl-mahjong:variant-tileset (cl-mahjong:game-variant game)))

(defun kind-tile (game seat notation)
  "A tile in SEAT's hand of the kind NOTATION names."
  (let ((ts (tileset-of game))
        (id (first (kind-ids (tileset-of game) notation))))
    (find id (cl-mahjong:player-tiles
              (svref (cl-mahjong:deal-players (cl-mahjong:game-deal game)) seat))
          :key (lambda (tile) (cl-mahjong:tile-kind-id ts tile)))))

(defun seat-player (game seat)
  (svref (cl-mahjong:deal-players (cl-mahjong:game-deal game)) seat))

(defun act (game seat type &rest args)
  "Apply an action given as TYPE and kind notations; returns the game and the events.
A notation picks a tile of that kind from SEAT's hand, a different one each time."
  (let ((taken '()))
    (cl-mahjong:apply-action
     game seat
     (cons type (loop for arg in args
                      collect (if (stringp arg)
                                  (let ((id (first (kind-ids (tileset-of game) arg))))
                                    (car (push (find-if (lambda (tile)
                                                          (and (= id (cl-mahjong:tile-kind-id
                                                                      (tileset-of game) tile))
                                                               (not (member tile taken))))
                                                        (cl-mahjong:player-tiles
                                                         (seat-player game seat)))
                                               taken)))
                                  arg))))))

(defun drawn-tile (game)
  (cl-mahjong:deal-drawn (cl-mahjong:game-deal game)))

(defun turn-of (game)
  (cl-mahjong:deal-turn (cl-mahjong:game-deal game)))

(defun action-types (game seat)
  (remove-duplicates (mapcar #'first (cl-mahjong:legal-actions game seat))))

(defun pass-all (game)
  "(values game events): everyone passes while a claim is pending."
  (let ((all '()))
    (loop while (member (cl-mahjong:deal-phase (cl-mahjong:game-deal game)) '(:claim :chankan))
          do (multiple-value-bind (next events)
                 (cl-mahjong:apply-action game (first (cl-mahjong:deciding-seats game)) '(:pass))
               (setf game next
                     all (append all events))))
    (values game all)))

(defun bot-action (game seat)
  (let ((actions (cl-mahjong:legal-actions game seat)))
    (or (find :tsumo actions :key #'first)
        (find :ron actions :key #'first)
        (let ((drawn (drawn-tile game)))
          (and (eq (cl-mahjong:deal-phase (cl-mahjong:game-deal game)) :turn)
               (or (find (list :discard drawn) actions :test #'equal)
                   (find :discard actions :key #'first))))
        (list :pass))))

(defun play-deal (game)
  "Play until the deal ends; everyone discards what they draw and takes wins."
  (loop while (eq (cl-mahjong:game-phase game) :playing)
        do (let ((seat (first (cl-mahjong:deciding-seats game))))
             (setf game (cl-mahjong:apply-action game seat (bot-action game seat)))))
  game)

(defun result-of (game key)
  (getf (cl-mahjong:game-result game) key))

;;; Dealing

(test deal-gives-thirteen-tiles-and-the-dealer-draws
  (let ((game (new-game :draws '("5m"))))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (is (= 0 (turn-of game)))
    (is (= 14 (length (cl-mahjong:player-tiles (seat-player game 0)))))
    (dolist (seat '(1 2 3))
      (is (= 13 (length (cl-mahjong:player-tiles (seat-player game seat)))))
      (is (null (cl-mahjong:legal-actions game seat))))
    (is (equal '(25000 25000 25000 25000) (coerce (cl-mahjong:game-points game) 'list)))))

(test dealt-tiles-follow-the-wall-order
  (let ((game (new-game :draws '("5m"))))
    (is (= (first (kind-ids (tileset-of game) "5m"))
           (cl-mahjong:tile-kind-id (tileset-of game) (drawn-tile game))))
    (is (equal (kind-ids (tileset-of game) "123m456m789p234s1z")
               (sort (mapcar (lambda (tile) (cl-mahjong:tile-kind-id (tileset-of game) tile))
                             (remove (drawn-tile game)
                                     (cl-mahjong:player-tiles (seat-player game 0))))
                     #'<)))))

;;; Winning

(defun win-event (events)
  (find :win events :key #'first))

(test dealer-tenhou
  (let ((game (new-game :draws '("1z"))))
    (is (member '(:tsumo) (cl-mahjong:legal-actions game 0) :test #'equal))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 0 '(:tsumo))
      (let ((win (getf (rest (win-event events)) :score)))
        (is (equal '((:tenhou . 1)) (cl-mahjong:reading-yaku (cl-mahjong:score-reading win))))
        (is (= 48000 (cl-mahjong:score-points win))))
      (is (eq :deal-over (cl-mahjong:game-phase game)))
      (is (eq :tsumo (result-of game :type)))
      (is (equal '(73000 9000 9000 9000) (coerce (cl-mahjong:game-points game) 'list)))
      (is (= 1 (cl-mahjong:game-honba game)))
      (is (= 0 (cl-mahjong:game-kyoku game))))))

(test non-dealer-chiihou-rotates-the-dealer
  (let ((game (new-game :hands '("159m159p159s2345z" "123m456m789p234s1z" "246m246p246s6677z"
                                 "357m357p357s3344z")
                        :draws '("9m" "1z"))))
    (setf game (act game 0 :discard "9m"))
    (is (= 1 (turn-of game)))
    (setf game (cl-mahjong:apply-action game 1 '(:tsumo)))
    (is (= 0 (cl-mahjong:game-honba game)))
    (is (= 1 (cl-mahjong:game-kyoku game)))
    (is (equal '(9000 57000 17000 17000) (coerce (cl-mahjong:game-points game) 'list)))))

(test tsumo-on-a-later-turn
  (let ((game (new-game :draws '("9m" "8p" "8s" "8m" "1z"))))
    (setf game (act game 0 :discard "9m"))
    (dolist (seat '(1 2 3))
      (setf game (cl-mahjong:apply-action game seat (list :discard (drawn-tile game)))))
    (is (= 0 (turn-of game)))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 0 '(:tsumo))
      (let ((win (getf (rest (win-event events)) :score)))
        (is (= 1 (cl-mahjong:score-han win)))
        (is (= 30 (cl-mahjong:score-fu win)))
        (is (= 1500 (cl-mahjong:score-points win))))
      (is (equal '(26500 24500 24500 24500) (coerce (cl-mahjong:game-points game) 'list))))))

(test discarding-passes-the-turn
  (let ((game (new-game :draws '("9m" "2m"))))
    (setf game (act game 0 :discard "9m"))
    (is (= 1 (turn-of game)))
    (is (= (first (kind-ids (tileset-of game) "2m"))
           (cl-mahjong:tile-kind-id (tileset-of game) (drawn-tile game))))
    (is (null (cl-mahjong:legal-actions game 0)))
    (signals cl-mahjong:illegal-action (cl-mahjong:apply-action game 0 '(:pass)))))

(defparameter *ron-hands*
  '("123m456m789p234s1z" "99m123p456p555z78s" "246m246p246s6677z" "357m357p357s3344z"))

(test ron-on-a-discard
  (let ((game (new-game :hands *ron-hands* :draws '("6s"))))
    (setf game (act game 0 :discard "6s"))
    (is (eq :claim (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (equal '(1) (cl-mahjong:deciding-seats game)))
    (is (equal '(:chi :pass :ron) (sort (action-types game 1) #'string<)))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 1 '(:ron))
      (let ((win (getf (rest (win-event events)) :score)))
        (is (= 1 (cl-mahjong:score-han win)))
        (is (= 40 (cl-mahjong:score-fu win)))
        (is (= 1300 (cl-mahjong:score-points win))))
      (is (eq :ron (result-of game :type)))
      (is (equal '(23700 26300 25000 25000) (coerce (cl-mahjong:game-points game) 'list)))
      (is (= 1 (cl-mahjong:game-kyoku game)))
      (is (= 0 (cl-mahjong:game-honba game))))))

(test passing-a-ron-continues-the-game
  (let ((game (new-game :hands *ron-hands* :draws '("6s" "3p"))))
    (setf game (pass-all (act game 0 :discard "6s")))
    (is (= 1 (turn-of game)))
    (is (eq :turn (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (cl-mahjong:player-temp-furiten (seat-player game 1)))))

;;; Calls

(defparameter *call-hands*
  '("123m456m789p234s1z" "78s2456p3m6m8m9m" "246m246p246s6677z" "78s135p135s2z3z4z"))

(test pon-takes-the-turn
  (let ((game (new-game :hands *ron-hands* :draws '("6z"))))
    (setf game (act game 0 :discard "6z"))
    (is (equal '(2) (cl-mahjong:deciding-seats game)))
    (is (equal '(:pass :pon) (sort (action-types game 2) #'string<)))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (= 2 (turn-of game)))
    (is (null (drawn-tile game)))
    (is (= 11 (length (cl-mahjong:player-tiles (seat-player game 2)))))
    (let ((meld (first (cl-mahjong:player-melds (seat-player game 2)))))
      (is (eq :triplet (cl-mahjong:meld-type meld)))
      (is (cl-mahjong:meld-open-p meld))
      (is (= 0 (cl-mahjong:meld-from meld)))
      (is (= 3 (length (cl-mahjong:meld-tiles meld)))))
    (is (equal '(:discard) (action-types game 2)))
    (setf game (act game 2 :discard "7z"))
    (is (= 3 (turn-of game)))))

(test chi-is-only-for-the-next-seat
  (let ((game (new-game :hands '("123m456m789p234s1z" "78s2456p3m6m8m9m" "78s246p246m5z6z7z1p"
                                 "357m357p357s3344z")
                        :draws '("9s"))))
    (setf game (act game 0 :discard "9s"))
    (is (equal '(1) (cl-mahjong:deciding-seats game)))
    (is (equal '(:chi :pass) (sort (action-types game 1) #'string<)))
    (is (null (cl-mahjong:legal-actions game 2)))))

(defun variant-with (name &rest options)
  "A riichi variant called NAME whose options start with OPTIONS."
  (cl-mahjong::register-definition
   :variant name 'riichi
   (list (list :name name)
         (list* :options (append options cl-mahjong:*riichi-options*)))))

;;; Exhaustive draw

(test exhaustive-draw-pays-noten
  (let ((game (play-deal (new-game :ura '("1z" "1z" "1z")))))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (eq :exhaustive (result-of game :type)))
    (is (equal '(0) (result-of game :tenpai)))
    (is (equal '(28000 24000 24000 24000) (coerce (cl-mahjong:game-points game) 'list)))
    (is (= 1 (cl-mahjong:game-honba game)))
    (is (= 0 (cl-mahjong:game-kyoku game)))))

(test noten-dealer-passes-the-deal
  (let ((game (play-deal (new-game :hands '("159m159p159s2345z" "123m456m789p234s1z"
                                            "246m246p246s6677z" "357m357p357s3344z")
                                   :ura '("1z" "1z" "1z")))))
    (is (eq :exhaustive (result-of game :type)))
    (is (equal '(1) (result-of game :tenpai)))
    (is (equal '(24000 28000 24000 24000) (coerce (cl-mahjong:game-points game) 'list)))
    (is (= 1 (cl-mahjong:game-kyoku game)))
    (is (= 1 (cl-mahjong:game-honba game)))))

;;; Kuikae

(defparameter *kuikae-hands*
  '("123m456m789p234s1z" "123m456m567p67s59s" "246m246p246s6677z" "357m357p357s3344z"))

(defun discards-after-chi (&rest options)
  (let* ((variant (and options (apply #'variant-with 'kuikae-test options)))
         (game (cl-mahjong:start-game (or variant 'riichi)
                                      :walls (list (stacked :hands *kuikae-hands* :draws '("8s"))))))
    (setf game (act game 0 :discard "8s"))
    (setf game (act game 1 :chi "6s" "7s"))
    (is (= 1 (turn-of game)))
    (is (null (drawn-tile game)))
    (mapcar (lambda (action) (cl-mahjong:tile-kind-id (tileset-of game) (second action)))
            (cl-mahjong:legal-actions game 1))))

(test kuikae-forbids-the-called-kind-and-the-far-end
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (ids (discards-after-chi)))
    (is (not (member (first (kind-ids ts "8s")) ids)))
    (is (not (member (first (kind-ids ts "5s")) ids)))
    (is (member (first (kind-ids ts "9s")) ids))
    (is (member (first (kind-ids ts "1m")) ids))))

(test kuikae-same-kind-only
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (ids (discards-after-chi :kuikae :same-kind)))
    (is (member (first (kind-ids ts "5s")) ids))))

(test kuikae-allowed
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (ids (discards-after-chi :kuikae nil)))
    (is (member (first (kind-ids ts "5s")) ids))))

;;; Kans

(defparameter *kan-hands*
  '("123m456m789p234s1z" "555z123p456p789s9m" "246m246p246s6677z" "357m357p357s3344z"))

(defun dora-count (game)
  (length (cl-mahjong:wall-dora-indicators (cl-mahjong:deal-wall (cl-mahjong:game-deal game)))))

(test daiminkan-draws-a-replacement-and-reveals-dora-after-the-discard
  (let ((game (new-game :hands *kan-hands* :draws '("5z") :rinshan '("2s") :dora '("1m" "2m"))))
    (setf game (act game 0 :discard "5z"))
    (is (member :daiminkan (action-types game 1)))
    (setf game (act game 1 :daiminkan "5z" "5z" "5z"))
    (let ((meld (first (cl-mahjong:player-melds (seat-player game 1)))))
      (is (eq :quad (cl-mahjong:meld-type meld)))
      (is (= 0 (cl-mahjong:meld-from meld))))
    (is (= 1 (turn-of game)))
    (is (= (first (kind-ids (tileset-of game) "2s"))
           (cl-mahjong:tile-kind-id (tileset-of game) (drawn-tile game))))
    (is (= 1 (dora-count game)))
    (setf game (act game 1 :discard "9m"))
    (is (= 2 (dora-count game)))))

(test immediate-kan-dora-option
  (variant-with 'kan-immediate :kan-dora :immediate)
  (let ((game (cl-mahjong:start-game
               'kan-immediate
               :walls (list (stacked :hands *kan-hands* :draws '("5z") :rinshan '("2s")
                                     :dora '("1m" "2m"))))))
    (setf game (act game 0 :discard "5z"))
    (setf game (act game 1 :daiminkan "5z" "5z" "5z"))
    (is (= 2 (dora-count game)))))

(test on-discard-kan-dora-appears-before-the-discard-is-claimed
  (variant-with 'kan-on-discard :kan-dora :on-discard)
  (let ((game (cl-mahjong:start-game
               'kan-on-discard
               :walls (list (stacked :hands *kan-hands* :draws '("5z") :rinshan '("6z")
                                     :dora '("1m" "2m"))))))
    (setf game (act game 0 :discard "5z"))
    (setf game (act game 1 :daiminkan "5z" "5z" "5z"))
    (is (= 1 (dora-count game)))
    (setf game (act game 1 :discard "6z"))
    (is (= 2 (dora-count game)))))

(test rinshan-win
  (let ((game (new-game :hands *kan-hands* :draws '("5z") :rinshan '("9m"))))
    (setf game (act game 0 :discard "5z"))
    (setf game (act game 1 :daiminkan "5z" "5z" "5z"))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 1 '(:tsumo))
      (let ((names (mapcar #'car (cl-mahjong:reading-yaku
                                  (cl-mahjong:score-reading
                                   (getf (rest (win-event events)) :score))))))
        (is (member 'cl-mahjong::rinshan names :test #'string=))
        (is (member 'cl-mahjong::haku names :test #'string=)))
      (is (eq :tsumo (result-of game :type))))))

(test ankan-keeps-the-hand-closed
  (let ((game (new-game :hands '("111m456p789s2345z" "259m259p259s2345z" "246m246p246s6677z"
                                 "357m357p357s3344z")
                        :draws '("1m") :rinshan '("2s") :dora '("5z" "6z"))))
    (let ((id (first (kind-ids (tileset-of game) "1m"))))
      (is (member (list :ankan id) (cl-mahjong:legal-actions game 0) :test #'equal))
      (setf game (cl-mahjong:apply-action game 0 (list :ankan id))))
    (let ((meld (first (cl-mahjong:player-melds (seat-player game 0)))))
      (is (eq :quad (cl-mahjong:meld-type meld)))
      (is (not (cl-mahjong:meld-open-p meld))))
    (is (= 2 (dora-count game)))
    (is (= 0 (turn-of game)))
    (is (= (first (kind-ids (tileset-of game) "2s"))
           (cl-mahjong:tile-kind-id (tileset-of game) (drawn-tile game))))))

(defparameter *chankan-hands*
  '("123m789m789p234s1z" "55m123p456p789s9m" "246m246p246s6677z" "46m123p456p789s77z"))

(defun shouminkan-game ()
  "Seat 1 has ponned 5m and draws the fourth one."
  (let ((game (new-game :hands *chankan-hands* :draws '("5m" "2z" "3z" "4z" "5m"))))
    (setf game (act game 0 :discard "5m"))
    (setf game (act game 1 :pon "5m" "5m"))
    (setf game (act game 1 :discard "9m"))
    (dolist (seat '(2 3 0))
      (setf game (pass-all (cl-mahjong:apply-action game seat (list :discard (drawn-tile game))))))
    game))

(test shouminkan-completes-when-nobody-takes-the-tile
  (let ((game (shouminkan-game)))
    (is (= 1 (turn-of game)))
    (is (member :shouminkan (action-types game 1)))
    (setf game (act game 1 :shouminkan "5m"))
    (is (eq :chankan (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (equal '(3) (cl-mahjong:deciding-seats game)))
    (setf game (pass-all game))
    (is (eq :quad (cl-mahjong:meld-type (first (cl-mahjong:player-melds (seat-player game 1))))))
    (is (eq :turn (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))))

(test chankan-wins-on-the-added-tile
  (let ((game (shouminkan-game)))
    (setf game (act game 1 :shouminkan "5m"))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 3 '(:ron))
      (is (member 'cl-mahjong::chankan
                  (mapcar #'car (cl-mahjong:reading-yaku
                                 (cl-mahjong:score-reading
                                  (getf (rest (win-event events)) :score))))
                  :test #'string=))
      (is (eq :ron (result-of game :type)))
      (is (< (aref (cl-mahjong:game-points game) 1) 25000)))))

;;; Riichi

(defun discard-and-pass (game seat)
  "SEAT discards its drawn tile and everyone passes."
  (pass-all (cl-mahjong:apply-action game seat (list :discard (drawn-tile game)))))

(defparameter *tenpai-and-junk*
  '("123m456m789p234s1z" "159m159p159s2345z" "246m246p246s6677z" "357m357p357s3344z"))

(test riichi-needs-a-closed-tenpai-hand
  (let* ((game (new-game :draws '("9m")))
         (riichi (remove :riichi (cl-mahjong:legal-actions game 0) :key #'first :test-not #'eq)))
    (is (equal (sort (list (list :riichi (kind-tile game 0 "9m"))
                           (list :riichi (kind-tile game 0 "1z")))
                     #'< :key #'second)
               (sort (copy-list riichi) #'< :key #'second))))
  (let ((game (new-game :hands (cons "159m159p159s2345z" (rest *tenpai-and-junk*)) :draws '("9m"))))
    (is (not (member :riichi (action-types game 0))))))

(test riichi-pays-a-stick-and-sets-ippatsu
  (multiple-value-bind (game events) (act (new-game :draws '("9m")) 0 :riichi "9m")
    (is (find :riichi events :key #'first))
    (is (eq :active (cl-mahjong:player-riichi (seat-player game 0))))
    (is (cl-mahjong:player-double-riichi (seat-player game 0)))
    (is (cl-mahjong:player-ippatsu (seat-player game 0)))
    (is (= 24000 (aref (cl-mahjong:game-points game) 0)))
    (is (= 1 (cl-mahjong:game-sticks game)))))

(test riichi-is-not-established-when-the-discard-is-ronned
  (let ((game (new-game :hands (cons (first *tenpai-and-junk*) (rest *ron-hands*))
                        :draws '("6s"))))
    (setf game (act game 0 :riichi "6s"))
    (setf game (cl-mahjong:apply-action game 1 '(:ron)))
    (is (= 23700 (aref (cl-mahjong:game-points game) 0)))
    (is (= 0 (cl-mahjong:game-sticks game)))))

(test riichi-locks-the-hand
  (let ((game (new-game :draws '("9m" "2z" "3z" "4z" "6z"))))
    (setf game (pass-all (act game 0 :riichi "9m")))
    (dolist (seat '(1 2 3))
      (setf game (discard-and-pass game seat)))
    (is (= 0 (turn-of game)))
    (is (equal (list (list :discard (drawn-tile game))) (cl-mahjong:legal-actions game 0)))))

(test riichi-ippatsu-ura-and-tsumo
  (let ((game (new-game :draws '("9m" "2z" "3z" "4z" "1z") :ura '("2m") :dora '("5z"))))
    (setf game (pass-all (act game 0 :riichi "9m")))
    (dolist (seat '(1 2 3))
      (setf game (discard-and-pass game seat)))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 0 '(:tsumo))
      (let* ((score (getf (rest (win-event events)) :score))
             (names (mapcar #'car (cl-mahjong:reading-yaku (cl-mahjong:score-reading score)))))
        (is (subsetp '("DOUBLE-RIICHI" "IPPATSU" "MENZEN-TSUMO") (mapcar #'string names)
                     :test #'string=))
        (is (equal '(:dora 0 :ura 1 :aka 0 :nuki 0) (cl-mahjong:score-dora score)))
        (is (= 5 (cl-mahjong:score-han score)))
        (is (= 13000 (cl-mahjong:score-points score))))
      (is (equal '(37000 21000 21000 21000) (coerce (cl-mahjong:game-points game) 'list)))
      (is (= 0 (cl-mahjong:game-sticks game))))))

(test a-call-records-the-triplets-the-caller-held
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (game (new-game :hands '("123m456m789p234s1z" "78s2456p3m6m8m9m" "246m246p24s66777z"
                                  "78s135p135s2z3z4z")
                         :draws '("6z"))))
    (setf game (act game 0 :discard "6z"))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (equal (kind-ids ts "7z")
               (cl-mahjong:meld-held (first (cl-mahjong:player-melds (seat-player game 2))))))))

(test calls-break-ippatsu
  (let ((game (new-game :draws '("9m" "6z"))))
    (setf game (pass-all (act game 0 :riichi "9m")))
    (is (cl-mahjong:player-ippatsu (seat-player game 0)))
    (setf game (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game))))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (not (cl-mahjong:player-ippatsu (seat-player game 0))))))

;;; Furiten

(defparameter *furiten-hands*
  '("123m789m789p234s1z" "159m159p159s2345z" "246m246p246s6677z" "99m123p456p555z78s"))

(test passing-a-ron-makes-you-furiten-until-your-next-discard
  (let ((game (new-game :hands *furiten-hands* :draws '("6s" "9s"))))
    (is (not (cl-mahjong:player-temp-furiten (seat-player game 3))))
    (setf game (act game 0 :discard "6s"))
    (is (member :ron (action-types game 3)))
    (setf game (pass-all game))
    (is (cl-mahjong:player-temp-furiten (seat-player game 3)))
    (setf game (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game))))
    (is (null (cl-mahjong:legal-actions game 3)))
    (is (not (member 3 (cl-mahjong:deciding-seats game))))))

(defun furiten-game (fourth-draw)
  "Seat 3 waits on 6s and 9s; round one ends with seat 3 discarding FOURTH-DRAW, then seat 0 draws 6s."
  (let ((game (new-game :hands *furiten-hands* :draws (list "2z" "3z" "4z" fourth-draw "6s"))))
    (dolist (seat '(0 1 2 3))
      (setf game (discard-and-pass game seat)))
    (setf game (cl-mahjong:apply-action game 0 (list :discard (drawn-tile game))))
    game))

(test discarding-a-wait-makes-you-furiten
  (let ((game (furiten-game "9s")))
    (is (null (cl-mahjong:legal-actions game 3)))
    (is (not (member 3 (cl-mahjong:deciding-seats game))))))

(test no-furiten-without-a-wait-in-the-discards
  (let ((game (furiten-game "2p")))
    (is (member :ron (action-types game 3)))))

;;; Abortive draws

(defun abort-reason (game)
  (getf (cl-mahjong:game-result game) :reason))

(test kyuushu-kyuuhai
  (let ((game (new-game :hands '("19m246m19p19s1234z" "123m456m789p234s5z" "246m246p246s6677z"
                                 "357m357p357s3344z")
                        :draws '("5z"))))
    (is (member :kyuushu (action-types game 0)))
    (setf game (cl-mahjong:apply-action game 0 '(:kyuushu)))
    (is (eq :abortive (result-of game :type)))
    (is (eq :kyuushu (abort-reason game)))
    (is (= 0 (cl-mahjong:game-kyoku game)))
    (is (= 1 (cl-mahjong:game-honba game)))
    (is (equal '(25000 25000 25000 25000) (coerce (cl-mahjong:game-points game) 'list)))))

(test kyuushu-needs-nine-kinds-on-the-first-turn
  (let ((game (new-game :draws '("5z"))))
    (is (not (member :kyuushu (action-types game 0)))))
  (variant-with 'no-kyuushu :kyuushu nil)
  (let ((game (cl-mahjong:start-game
               'no-kyuushu
               :walls (list (stacked :hands '("19m246m19p19s1234z" "123m456m789p234s5z"
                                              "246m246p246s6677z" "357m357p357s3344z")
                                     :draws '("5z"))))))
    (is (not (member :kyuushu (action-types game 0))))))

(test suufon-renda
  (let ((game (new-game :hands '("123m456m789p234s1z" "159m159p159s1345z" "246m246p246s1677z"
                                 "357m357p357s1344z")
                        :draws '("9m" "9p" "9s" "8m"))))
    (dolist (seat '(0 1 2))
      (setf game (pass-all (act game seat :discard "1z"))))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (setf game (pass-all (act game 3 :discard "1z")))
    (is (eq :abortive (result-of game :type)))
    (is (eq :suufon-renda (abort-reason game)))))

(test suucha-riichi
  (let ((game (new-game :hands '("123m456m789p234s1z" "123m456m789p234s2z" "123m456m789p234s3z"
                                 "123m456m789p234s4z")
                        :draws '("9m" "8m" "7m" "9s"))))
    (dolist (seat '(0 1 2))
      (setf game (pass-all (cl-mahjong:apply-action game seat (list :riichi (drawn-tile game))))))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (setf game (pass-all (cl-mahjong:apply-action game 3 (list :riichi (drawn-tile game)))))
    (is (eq :suucha-riichi (abort-reason game)))
    (is (= 4 (cl-mahjong:game-sticks game)))
    (is (equal '(24000 24000 24000 24000) (coerce (cl-mahjong:game-points game) 'list)))))

(test suukaikan
  (let ((game (new-game :hands '("111m222p333s4445z" "555s666p777m6677z" "248m348p248s2345z"
                                 "368m378p368s1177z")
                        :draws '("1m" "5s") :rinshan '("2p" "9s" "6p" "9m"))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "1m")))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "2p")))))
    (setf game (pass-all (cl-mahjong:apply-action game 0 (list :discard (drawn-tile game)))))
    (is (= 1 (turn-of game)))
    (setf game (cl-mahjong:apply-action game 1 (list :ankan (first (kind-ids (tileset-of game) "5s")))))
    (setf game (cl-mahjong:apply-action game 1 (list :ankan (first (kind-ids (tileset-of game) "6p")))))
    (is (eq :playing (cl-mahjong:game-phase game)))
    (setf game (pass-all (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game)))))
    (is (eq :suukaikan (abort-reason game)))))

;;; Multiple ron

(defparameter *multi-ron-hands*
  '("123m789m789p234s1z" "99m123p456p555z78s" "88m123p456p666z78s" "77m123p456p777z78s"))

(defun multi-ron-game (&key (variant 'riichi) (hands *multi-ron-hands*) (honba 0))
  (let ((game (cl-mahjong:start-game variant :walls (list (stacked :hands hands :draws '("6s"))))))
    (setf (cl-mahjong::game-honba game) honba)
    (setf game (act game 0 :discard "6s"))
    (dolist (seat (cl-mahjong:deciding-seats game) game)
      (setf game (cl-mahjong:apply-action game seat '(:ron))))))

(defun points-of (game)
  (coerce (cl-mahjong:game-points game) 'list))

(test double-ron-pays-both-and-honba-goes-to-the-first
  (let ((game (multi-ron-game :hands (list* (first *multi-ron-hands*) (second *multi-ron-hands*)
                                            (third *multi-ron-hands*)
                                            '("357m357p357s3344z"))
                              :honba 2)))
    (is (eq :ron (result-of game :type)))
    (is (equal '(1 2) (result-of game :winners)))
    (is (equal '(21800 26900 26300 25000) (points-of game)))
    (is (= 1 (cl-mahjong:game-kyoku game)))
    (is (= 0 (cl-mahjong:game-honba game)))))

(test head-bump-pays-the-nearest-winner-only
  (variant-with 'head-bump :multi-ron :head-bump)
  (let ((game (multi-ron-game :variant 'head-bump
                              :hands (list (first *multi-ron-hands*) (second *multi-ron-hands*)
                                           (third *multi-ron-hands*) "357m357p357s3344z"))))
    (is (equal '(1) (result-of game :winners)))
    (is (equal '(23700 26300 25000 25000) (points-of game)))))

(test triple-ron-aborts-by-default
  (let ((game (cl-mahjong:start-game
               'riichi :walls (list (stacked :hands *multi-ron-hands* :draws '("6s"))))))
    (setf game (act game 0 :discard "6s"))
    (is (equal '(1 2 3) (cl-mahjong:deciding-seats game)))
    (dolist (seat '(1 2 3))
      (setf game (cl-mahjong:apply-action game seat '(:ron))))
    (is (eq :sanchahou (abort-reason game)))
    (is (equal '(25000 25000 25000 25000) (points-of game)))))

(test triple-ron-when-allowed
  (variant-with 'triple-ron :multi-ron :triple)
  (let ((game (multi-ron-game :variant 'triple-ron)))
    (is (equal '(1 2 3) (result-of game :winners)))
    (is (equal '(21100 26300 26300 26300) (points-of game)))))

;;; Rotation and game end

(defparameter *south-four-hands*
  '("99m123p456p555z78s" "159m159p159s2345z" "246m246p246s6677z" "123m456m789p234s1z"))

(defun dealer-rons-game (&rest args &key (kyoku 7) points (variant 'riichi) (hands *south-four-hands*))
  "Seat 0 rons the dealer's 6s; the dealer is KYOKU mod 4."
  (declare (ignore args))
  (let ((game (cl-mahjong:start-game variant :kyoku kyoku :points points
                                             :walls (list (stacked :hands hands :draws '("6s")
                                                                   :dealer (mod kyoku 4))))))
    (setf game (act game (mod kyoku 4) :discard "6s"))
    (cl-mahjong:apply-action game 0 '(:ron))))

(test west-extension-follows-an-unfinished-hanchan
  (let ((game (dealer-rons-game :points '(24000 24000 24000 28000))))
    (is (eq :deal-over (cl-mahjong:game-phase game)))
    (is (= 8 (cl-mahjong:game-kyoku game)))
    (is (not (cl-mahjong:game-over-p game)))))

(test hanchan-ends-when-someone-reaches-the-target
  (let ((game (dealer-rons-game :points '(29000 24000 24000 23000))))
    (is (cl-mahjong:game-over-p game))
    (is (= 0 (first (getf (cl-mahjong:game-final game) :order))))))

(test no-west-extension
  (variant-with 'no-extension :west-extension nil)
  (is (cl-mahjong:game-over-p
       (dealer-rons-game :variant 'no-extension :points '(24000 24000 24000 28000)))))

(test the-extension-ends-after-west-four
  (is (cl-mahjong:game-over-p (dealer-rons-game :kyoku 11 :points '(24000 24000 24000 28000)))))

(test tonpuu-ends-after-east-four
  (variant-with 'tonpuu :length :tonpuu)
  (is (cl-mahjong:game-over-p
       (dealer-rons-game :variant 'tonpuu :kyoku 3 :points '(29000 24000 24000 23000)))))

(test tobi-ends-the-game
  (let ((game (cl-mahjong:start-game
               'riichi :points '(500 25000 25000 49500)
                       :walls (list (stacked :hands *ron-hands* :draws '("6s"))))))
    (setf game (act game 0 :discard "6s"))
    (setf game (cl-mahjong:apply-action game 1 '(:ron)))
    (is (cl-mahjong:game-over-p game))
    (is (minusp (aref (cl-mahjong:game-points game) 0)))))

(test the-extension-continues-when-a-trailing-dealer-repeats
  (let ((game (cl-mahjong:start-game 'riichi :walls (list (stacked)))))
    (is (not (cl-mahjong::game-ends-p game #(36000 20000 20000 24000) 11 t :agari)))
    (is (cl-mahjong::game-ends-p game #(24000 20000 20000 36000) 11 t :agari))
    (is (cl-mahjong::game-ends-p game #(36000 20000 20000 24000) 11 nil :agari))))

(defun tenhou-game (&rest options)
  (let ((game (cl-mahjong:start-game
               (if options (apply #'variant-with 'tenhou-test options) 'riichi)
               :kyoku 7 :points '(20000 20000 20000 40000)
               :walls (list (stacked :hands '("159m159p159s2345z" "246m246p246s6677z"
                                              "357m357p357s3344z" "123m456m789p234s1z")
                                     :draws '("1z") :dealer 3)))))
    (cl-mahjong:apply-action game 3 '(:tsumo))))

(test agari-yame-ends-the-game-for-a-leading-dealer
  (let ((game (tenhou-game)))
    (is (cl-mahjong:game-over-p game))
    (is (= 7 (cl-mahjong:game-kyoku game)))))

(test the-dealer-repeats-without-agari-yame
  (let ((game (tenhou-game :agari-yame nil)))
    (is (not (cl-mahjong:game-over-p game)))
    (is (= 7 (cl-mahjong:game-kyoku game)))
    (is (= 1 (cl-mahjong:game-honba game)))))

(test standings-apply-uma-and-oka
  (let* ((game (cl-mahjong:start-game 'riichi :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(40000 30000 20000 10000) 0)))
    (is (equal '(0 1 2 3) (getf standings :order)))
    (is (equal '(45000 5000 -15000 -35000) (coerce (getf standings :scores) 'list))))
  (let* ((game (cl-mahjong:start-game 'riichi :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(25000 25000 25000 25000) 1)))
    (is (equal '(0 1 2 3) (getf standings :order)))
    (is (= 26000 (aref (getf standings :points) 0)))))

;;; Nagashi mangan

(test nagashi-mangan-is-paid-like-a-mangan-tsumo
  (let* ((game (new-game))
         (deal (cl-mahjong:game-deal game))
         (tileset (tileset-of game))
         (honors (cl-mahjong:tiles-of-kind tileset (cl-mahjong:find-kind tileset 'east)))
         (player (copy-structure (svref (cl-mahjong:deal-players deal) 1)))
         (dealer (copy-structure (svref (cl-mahjong:deal-players deal) 0))))
    (setf (slot-value player 'cl-mahjong::discards) (subseq honors 0 2)
          (slot-value player 'cl-mahjong::tiles) (subseq (cl-mahjong:player-tiles player) 0 13)
          (slot-value dealer 'cl-mahjong::tiles) (subseq (cl-mahjong:player-tiles dealer) 0 13))
    (let ((game (cl-mahjong::replace-deal
                 game (cl-mahjong::set-player (cl-mahjong::set-player deal 1 player) 0 dealer)))
          (cl-mahjong::*events* '()))
      (let ((game (cl-mahjong::exhaustive-draw game)))
        (is (eq :exhaustive (result-of game :type)))
        (is (equal '(1) (result-of game :nagashi)))
        (is (equal '((0 . -4000) (1 . 8000) (2 . -2000) (3 . -2000)) (result-of game :deltas)))))))

(test a-called-discard-ends-nagashi
  (let* ((game (new-game))
         (tileset (tileset-of game))
         (honors (cl-mahjong:tiles-of-kind tileset (cl-mahjong:find-kind tileset 'east)))
         (player (copy-structure (svref (cl-mahjong:deal-players (cl-mahjong:game-deal game)) 1))))
    (setf (slot-value player 'cl-mahjong::discards) (subseq honors 0 2)
          (slot-value player 'cl-mahjong::called-away) 1)
    (is (not (cl-mahjong::nagashi-seat-p game player)))))

;;; Whole games

(defun play-game (game)
  (loop until (cl-mahjong:game-over-p game)
        do (setf game (if (eq (cl-mahjong:game-phase game) :deal-over)
                          (cl-mahjong:next-deal game)
                          (play-deal game))))
  game)

(test random-games-finish-and-conserve-points
  (dolist (seed '(1 2 3 4 5 6 7 8))
    (let* ((game (play-game (cl-mahjong:start-game 'riichi :seed seed)))
           (final (cl-mahjong:game-final game)))
      (is (cl-mahjong:game-over-p game))
      (is (= 100000 (reduce #'+ (getf final :points))))
      (is (= 0 (reduce #'+ (getf final :scores)))))))

;;; Last tile, riichi restrictions and options

(defun yaku-names (events)
  (mapcar #'string (mapcar #'car (cl-mahjong:reading-yaku
                                  (cl-mahjong:score-reading
                                   (getf (rest (win-event events)) :score))))))

(defun finishing-events (game)
  "The events of the deal's last action when GAME is played out by the bot."
  (let ((events '()))
    (loop while (eq (cl-mahjong:game-phase game) :playing)
          do (let ((seat (first (cl-mahjong:deciding-seats game))))
               (multiple-value-setq (game events)
                 (cl-mahjong:apply-action game seat (bot-action game seat)))))
    (values game events)))

(test haitei-on-the-last-tile
  (multiple-value-bind (game events)
      (finishing-events (new-game :hands '("159m159p159s2345z" "123m456m789p234s2z"
                                           "246m246p246s6677z" "357m357p357s3344z")
                                  :last "2z" :ura '("2z")))
    (is (eq :tsumo (result-of game :type)))
    (is (equal '(1) (result-of game :winners)))
    (is (member "HAITEI" (yaku-names events) :test #'string=))))

(test houtei-on-the-last-discard
  (multiple-value-bind (game events)
      (finishing-events (new-game :hands '("159m159p159s2345z" "246m246p246s6677z"
                                           "123m456m789p234s2z" "357m357p357s3344z")
                                  :last "2z" :ura '("2z")))
    (is (eq :ron (result-of game :type)))
    (is (equal '(2) (result-of game :winners)))
    (is (member "HOUTEI" (yaku-names events) :test #'string=))))

(test passing-a-ron-in-riichi-is-permanent-furiten
  (let ((game (new-game :hands *furiten-hands* :draws '("2z" "3z" "4z" "2p" "6s" "9s"))))
    (dolist (seat '(0 1 2))
      (setf game (discard-and-pass game seat)))
    (setf game (pass-all (cl-mahjong:apply-action game 3 (list :riichi (drawn-tile game)))))
    (setf game (cl-mahjong:apply-action game 0 (list :discard (drawn-tile game))))
    (is (member :ron (action-types game 3)))
    (setf game (pass-all game))
    (is (cl-mahjong:player-riichi-furiten (seat-player game 3)))
    (setf game (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game))))
    (is (not (member 3 (cl-mahjong:deciding-seats game))))))

(test riichi-players-cannot-call
  (let ((game (new-game :hands '("123m456m789p23s11z" "159m159p159s2345z" "246m246p246s6677z"
                                 "357m357p357s3344z")
                        :draws '("9m" "1z"))))
    (setf game (pass-all (act game 0 :riichi "9m")))
    (setf game (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game))))
    (is (not (member 0 (cl-mahjong:deciding-seats game))))
    (is (= 2 (turn-of game)))))

(test ankan-is-allowed-in-riichi-when-the-wait-is-unchanged
  (let ((game (new-game :hands '("111m456p789s12s55z" "259m259p259s2345z" "246m246p246s6677z"
                                 "357m357p357s3344z")
                        :draws '("9p" "2z" "3z" "4z" "1m"))))
    (setf game (pass-all (act game 0 :riichi "9p")))
    (dolist (seat '(1 2 3))
      (setf game (discard-and-pass game seat)))
    (is (member (list :ankan (first (kind-ids (tileset-of game) "1m")))
                (cl-mahjong:legal-actions game 0) :test #'equal))))

(test chi-can-be-switched-off
  (variant-with 'no-chi :chi nil)
  (let ((game (cl-mahjong:start-game
               'no-chi
               :walls (list (stacked :hands '("123m456m789p234s1z" "78s2456p3m6m8m9m"
                                              "246m246p246s6677z" "357m357p357s3344z")
                                     :draws '("9s"))))))
    (setf game (act game 0 :discard "9s"))
    (is (eq :turn (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (= 1 (turn-of game)))))

;;; Kan dora flushing, robbing a closed kan, kuikae traps

(test pending-kan-dora-appears-when-the-discard-is-called
  (let ((game (new-game :hands *kan-hands* :draws '("5z") :rinshan '("6z") :dora '("1m" "2m"))))
    (setf game (act game 0 :discard "5z"))
    (setf game (act game 1 :daiminkan "5z" "5z" "5z"))
    (setf game (act game 1 :discard "6z"))
    (is (= 1 (dora-count game)))
    (setf game (act game 2 :pon "6z" "6z"))
    (is (= 2 (dora-count game)))))

(defparameter *kokushi-hands*
  '("111m456p789s2345z" "99m19p19s1234567z" "246m246p246s6677z" "357m357p357s3344z"))

(test kokushi-robs-a-closed-kan
  (let ((game (new-game :hands *kokushi-hands* :draws '("1m"))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "1m")))))
    (is (eq :chankan (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (equal '(1) (cl-mahjong:deciding-seats game)))
    (multiple-value-bind (game events) (cl-mahjong:apply-action game 1 '(:ron))
      (is (eq :ron (result-of game :type)))
      (is (member "KOKUSHI" (yaku-names events) :test #'string=)))))

(test a-closed-kan-completes-when-the-kokushi-player-passes
  (let ((game (new-game :hands *kokushi-hands* :draws '("1m") :rinshan '("2s"))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "1m")))))
    (setf game (cl-mahjong:apply-action game 1 '(:pass)))
    (is (eq :turn (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))
    (is (eq :quad (cl-mahjong:meld-type (first (cl-mahjong:player-melds (seat-player game 0))))))))

(test only-kokushi-robs-a-closed-kan
  (variant-with 'no-ankan-chankan :ankan-chankan nil)
  (let ((game (cl-mahjong:start-game
               'no-ankan-chankan
               :walls (list (stacked :hands *kokushi-hands* :draws '("1m") :rinshan '("2s"))))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "1m")))))
    (is (eq :turn (cl-mahjong:deal-phase (cl-mahjong:game-deal game))))))

(defun chi-options-with (kept)
  "What seat 1 may do with a discarded 8s when it holds three melds and the tiles KEPT."
  (let* ((game (new-game))
         (deal (cl-mahjong:game-deal game))
         (ts (tileset-of game))
         (player (copy-structure (svref (cl-mahjong:deal-players deal) 1)))
         (used '()))
    (flet ((take (notation)
             (let ((tile (find-if (lambda (tile) (not (member tile used)))
                                  (cl-mahjong:tiles-of-kind
                                   ts (cl-mahjong:tileset-kind ts (first (kind-ids ts notation)))))))
               (push tile used)
               tile)))
      (let ((called (take "8s")))
        (setf (slot-value player 'cl-mahjong::tiles) (mapcar #'take kept)
              (slot-value player 'cl-mahjong::melds)
              (mapcar (lambda (notation)
                        (cl-mahjong:make-meld :triplet (kind-ids ts notation) :open t))
                      '("111z" "222z" "333z")))
        (let ((deal (cl-mahjong::set-player deal 1 player)))
          (setf (cl-mahjong::deal-interrupted deal) t)
          (cl-mahjong::seat-claim-options (cl-mahjong::replace-deal game deal) 1 called 0))))))

(test a-chi-that-leaves-only-forbidden-discards-is-not-offered
  (is (notany (lambda (option) (eq :chi (first option))) (chi-options-with '("6s" "7s" "5s" "8s"))))
  (is (some (lambda (option) (eq :chi (first option))) (chi-options-with '("6s" "7s" "5s" "2m")))))

;;; Random play

(defun random-deal-bot (game rng)
  (loop while (eq (cl-mahjong:game-phase game) :playing)
        do (let* ((seat (first (cl-mahjong:deciding-seats game)))
                  (actions (cl-mahjong:legal-actions game seat)))
             (setf game (cl-mahjong:apply-action game seat
                                                 (nth (cl-mahjong:rng-below rng (length actions))
                                                      actions)))
             (is (= 100000 (+ (reduce #'+ (cl-mahjong:game-points game))
                              (* 1000 (cl-mahjong:game-sticks game)))))))
  game)

(test random-legal-play-conserves-points
  (dolist (seed (loop for seed from 1 to 12 collect seed))
    (let ((rng (cl-mahjong:make-rng seed))
          (game (cl-mahjong:start-game 'riichi :seed seed)))
      (loop until (cl-mahjong:game-over-p game)
            do (setf game (random-deal-bot game rng))
               (unless (cl-mahjong:game-over-p game)
                 (setf game (cl-mahjong:next-deal game))))
      (is (= 100000 (reduce #'+ (getf (cl-mahjong:game-final game) :points))))
      (is (= 0 (reduce #'+ (getf (cl-mahjong:game-final game) :scores)))))))

(defun discard-shanten (game seat tile)
  (let ((variant (cl-mahjong:game-variant game))
        (player (seat-player game seat)))
    (or (cl-mahjong:shanten (tileset-of game) (cl-mahjong:variant-shapes variant)
                            (cl-mahjong:hand-counts (tileset-of game)
                                                    (remove tile (cl-mahjong:player-tiles player)))
                            (cl-mahjong:player-melds player))
        99)))

(defun greedy-action (game seat rng)
  "Win when possible, riichi when possible, otherwise discard for the lowest shanten; calls now and then."
  (let* ((actions (cl-mahjong:legal-actions game seat))
         (discards (remove :discard actions :key #'first :test-not #'eq)))
    (flet ((pick (type) (find type actions :key #'first)))
      (cond ((pick :tsumo))
            ((pick :ron))
            ((pick :riichi))
            ((and (pick :kyuushu) (zerop (cl-mahjong:rng-below rng 2))) (pick :kyuushu))
            (discards
             (let ((extra (remove-if (lambda (a) (member (first a) '(:discard :riichi :tsumo :kyuushu)))
                                     actions)))
               (if (and extra (zerop (cl-mahjong:rng-below rng 4)))
                   (nth (cl-mahjong:rng-below rng (length extra)) extra)
                   (first (sort (copy-list discards) #'<
                                :key (lambda (a) (discard-shanten game seat (second a))))))))
            ((zerop (cl-mahjong:rng-below rng 5))
             (nth (cl-mahjong:rng-below rng (length actions)) actions))
            (t (list :pass))))))

(defun greedy-deal-bot (game rng)
  (loop while (eq (cl-mahjong:game-phase game) :playing)
        do (let ((seat (first (cl-mahjong:deciding-seats game))))
             (setf game (cl-mahjong:apply-action game seat (greedy-action game seat rng)))
             (is (= 100000 (+ (reduce #'+ (cl-mahjong:game-points game))
                              (* 1000 (cl-mahjong:game-sticks game)))))))
  game)

(test greedy-play-conserves-points
  (dolist (seed '(1 2 3 4 5 6))
    (let ((rng (cl-mahjong:make-rng seed))
          (game (cl-mahjong:start-game 'riichi :seed seed)))
      (loop until (cl-mahjong:game-over-p game)
            do (setf game (greedy-deal-bot game rng))
               (unless (cl-mahjong:game-over-p game)
                 (setf game (cl-mahjong:next-deal game))))
      (is (= 100000 (reduce #'+ (getf (cl-mahjong:game-final game) :points))))
      (is (= 0 (reduce #'+ (getf (cl-mahjong:game-final game) :scores)))))))

;;; Pao

(defun pao-deltas (winner from &key tsumo)
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (game (cl-mahjong:start-game 'riichi :seed 1))
         (score (best-scored ts "234m11p" "2m"
                             :melds (loop for dragon in '("555z" "666z" "777z")
                                          for giver in '(1 2 3)
                                          collect (meld ts :triplet dragon :open t :from giver))
                             :context-args (list :tsumo tsumo))))
    (cl-mahjong::merge-deltas
     (cl-mahjong::win-deltas game (cl-mahjong::game-deal game) score winner from))))

(test pao-seat-pays-for-the-yakuman
  (is (equal '((0 . 0) (1 . 32000) (2 . 0) (3 . -32000)) (pao-deltas 1 nil :tsumo t)))
  (is (equal '((1 . 32000) (2 . -16000) (3 . -16000)) (pao-deltas 1 2)))
  (is (equal '((1 . 32000) (3 . -32000)) (pao-deltas 1 3))))

(test score-rounding-rounds-each-seat-and-pays-first-the-remainder
  (variant-with 'rounded :score-rounding :half-away)
  (let* ((game (cl-mahjong:start-game 'rounded :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(18700 25700 4500 51100) 0)))
    (is (equal '(3 1 0 2) (getf standings :order)))
    (is (equal '(-16000 1000 -40000 55000)
               (coerce (getf standings :scores) 'list)))))

(test score-rounding-rounds-each-seat-and-pays-first-the-remainder
  (variant-with 'rounded :score-rounding :half-away)
  (let* ((game (cl-mahjong:start-game 'rounded :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(18700 25700 4500 51100) 0)))
    (is (equal '(3 1 0 2) (getf standings :order)))
    (is (equal '(-16000 1000 -40000 55000)
               (coerce (getf standings :scores) 'list)))))

(test the-extension-ends-when-a-leading-dealer-repeats
  (let ((game (cl-mahjong:start-game
               'riichi :kyoku 11 :points '(24000 20000 20000 36000)
                       :walls (list (stacked :hands '("159m159p159s2345z" "246m246p246s6677z"
                                                      "357m357p357s3344z" "123m456m789p234s1z")
                                             :draws '("1z") :dealer 3)))))
    (is (cl-mahjong:game-over-p (cl-mahjong:apply-action game 3 '(:tsumo))))))
