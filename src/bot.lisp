(in-package #:cl-mahjong)

;;; Protocol

(defgeneric choose-action (bot game seat actions)
  (:documentation "The action BOT takes for SEAT, one of ACTIONS, the `legal-actions' of SEAT."))

(defgeneric observe (bot seat events)
  (:documentation "Tell BOT, playing SEAT, the events that just happened. Does nothing by default.")
  (:method (bot seat events) (declare (ignore bot seat events)) nil))

(defmethod choose-action ((bot function) game seat actions)
  (funcall bot game seat actions))

(defclass bot () ())
(defclass dead-bot (bot) ())
(defclass easy-bot (bot) ())
(defclass hard-bot (easy-bot)
  ((fold-shanten :initarg :fold-shanten :initform 2 :reader bot-fold-shanten
                 :documentation "Fold against a riichi at this shanten or worse.")))

(defun make-bot (level &rest initargs)
  "A built-in bot: :dead, :easy or :hard."
  (apply #'make-instance
         (case level
           (:dead 'dead-bot)
           (:easy 'easy-bot)
           (:hard 'hard-bot)
           (t (error "Unknown bot level ~s, expected :dead, :easy or :hard" level)))
         initargs))

;;; Choosing

(defun find-action (type actions)
  (find type actions :key #'first))

(defun turn-phase-p (game)
  (eq (deal-phase (game-deal game)) :turn))

(defun fallback-action (actions)
  (or (find-action :pass actions) (find-action :discard actions) (first actions)))

(defmethod choose-action ((bot dead-bot) game seat actions)
  (declare (ignore seat))
  (let ((drawn (deal-drawn (game-deal game))))
    (or (and drawn (find (list :discard drawn) actions :test #'equal))
        (find-action :pass actions)
        (find-action :discard actions)
        (first actions))))

(defun waits-total (waits)
  (reduce #'+ waits :key #'cdr))

(defun riichi-choice (game seat actions &key avoid-furiten)
  "The (:riichi tile) action whose discard leaves the most copies of its waits, or NIL when none has a live wait."
  (let ((tileset (game-tileset game))
        (best nil)
        (best-total 0))
    (dolist (action (sort (copy-list (remove :riichi actions :key #'first :test-not #'eq))
                          #'< :key (lambda (action)
                                     (let ((tile (second action)))
                                       (+ (* 2 (tile-kind-id tileset tile))
                                          (if (tile-red-p tileset tile) 1 0))))))
      (let ((total (waits-total (tenpai-waits game seat :discard (second action)))))
        (when (and (> total best-total)
                   (not (and avoid-furiten (seat-furiten-p game seat :discard (second action)))))
          (setf best action
                best-total total))))
    best))

(defun best-discard-action (game seat &key live)
  (let ((option (first (discard-options game seat :live live))))
    (and option (list :discard (discard-option-tile option)))))

(defmethod choose-action ((bot easy-bot) game seat actions)
  (or (find-action :tsumo actions)
      (find-action :ron actions)
      (and (turn-phase-p game)
           (or (find-action :kita actions)
               (riichi-choice game seat actions)
               (best-discard-action game seat)))
      (fallback-action actions)))

;;; Hard

(defun riichi-threat-p (game seat)
  (let ((deal (game-deal game)))
    (loop for other below (game-seats game)
          thereis (and (/= other seat) (player-riichi (deal-player deal other))))))

(defun hand-shanten (game seat)
  (let* ((player (deal-player (game-deal game) seat))
         (tileset (game-tileset game)))
    (shanten tileset (variant-shapes (game-variant game))
             (hand-counts tileset (player-tiles player)) (player-melds player))))

(defun safest-discard-action (game seat)
  (let ((options (stable-sort (copy-list (discard-options game seat :live t)) #'>
                              :key (lambda (option)
                                     (tile-safety game seat (discard-option-tile option))))))
    (and options (list :discard (discard-option-tile (first options))))))

(defun ankan-keeps-shanten-p (game seat id)
  "Whether a closed kan of kind ID leaves SEAT no further from tenpai."
  (let* ((player (deal-player (game-deal game) seat))
         (tileset (game-tileset game))
         (shapes (variant-shapes (game-variant game)))
         (before (hand-shanten game seat))
         (after (shanten tileset shapes
                         (hand-counts tileset (remove id (player-tiles player)
                                                      :key (lambda (tile)
                                                             (tile-kind-id tileset tile))))
                         (append (player-melds player)
                                 (list (make-meld :quad (make-list 4 :initial-element id)))))))
    (and before after (<= after before))))

(defun yakuhai-kind-p (game seat kind)
  (let ((winds (opt game :winds))
        (seats (game-seats game)))
    (case (kind-group kind)
      (:dragon t)
      (:wind (or (eq (kind-name kind)
                     (nth (mod (- seat (deal-dealer (game-deal game))) seats) winds))
                 (eq (kind-name kind)
                     (nth (mod (floor (game-kyoku game) seats) (length winds)) winds)))))))

;; TODO: calls only yakuhai pons; call when the hand keeps a yaku or gains speed (https://github.com/takeiteasy/cl-mahjong/issues/42)
(defun yakuhai-pon (game seat actions)
  (let ((claim (deal-claim (game-deal game))))
    (when (and claim (yakuhai-kind-p game seat (tile-kind (game-tileset game) (claim-tile claim))))
      (find-action :pon actions))))

(defmethod choose-action ((bot hard-bot) game seat actions)
  (or (find-action :tsumo actions)
      (find-action :ron actions)
      (if (turn-phase-p game)
          (or (find-action :kita actions)
              (find-if (lambda (action)
                         (and (eq (first action) :ankan)
                              (ankan-keeps-shanten-p game seat (second action))))
                       actions)
              (riichi-choice game seat actions :avoid-furiten t)
              (and (riichi-threat-p game seat)
                   (let ((shanten (hand-shanten game seat)))
                     (and shanten (>= shanten (bot-fold-shanten bot))))
                   (safest-discard-action game seat))
              (best-discard-action game seat :live t))
          (yakuhai-pon game seat actions))
      (fallback-action actions)))

;;; Runner

(defun play-bots (game bots &key events on-events deals)
  "Play GAME to its end with one bot per seat in BOTS, a list or vector. EVENTS are those that started
GAME, announced first. Every batch of events goes to ON-EVENTS, a function, and to each bot's `observe'.
With DEALS, stop once that many deals are over. Returns the game."
  (let ((bots (coerce bots 'simple-vector))
        (played 0))
    (unless (= (length bots) (game-seats game))
      (error "Expected ~a bots, got ~a" (game-seats game) (length bots)))
    (flet ((announce (events)
             (when events
               (when on-events (funcall on-events events))
               (loop for bot across bots
                     for seat from 0
                     do (observe bot seat events)))))
      (announce events)
      (loop until (or (game-over-p game) (and deals (>= played deals)))
            do (multiple-value-bind (new events)
                   (if (eq (game-phase game) :deal-over)
                       (next-deal game)
                       (let* ((seat (first (deciding-seats game)))
                              (actions (legal-actions game seat)))
                         (apply-action game seat
                                       (choose-action (svref bots seat) game seat actions))))
                 (when (and (eq (game-phase game) :playing) (eq (game-phase new) :deal-over))
                   (incf played))
                 (setf game new)
                 (announce events)))
      game)))
