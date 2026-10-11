(in-package #:cl-mahjong)

;;; Protocol

(defgeneric choose-action (bot view)
  (:documentation "The action BOT takes at VIEW, a `seat-view': one of its `view-actions'."))

(defgeneric observe (bot seat events)
  (:documentation "Tell BOT, playing SEAT, the events that just happened. Does nothing by default.")
  (:method (bot seat events) (declare (ignore bot seat events)) nil))

(defmethod choose-action ((bot function) view)
  (funcall bot view))

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

(defun turn-phase-p (view)
  (eq (view-deal-phase view) :turn))

(defun fallback-action (actions)
  (or (find-action :pass actions) (find-action :discard actions) (first actions)))

(defmethod choose-action ((bot dead-bot) view)
  (let ((actions (view-actions view))
        (drawn (view-drawn view)))
    (or (and drawn (find (list :discard drawn) actions :test #'equal))
        (find-action :pass actions)
        (find-action :discard actions)
        (first actions))))

(defun waits-total (waits)
  (reduce #'+ waits :key #'cdr))

(defun riichi-choice (view &key avoid-furiten)
  "The (:riichi tile) action whose discard leaves the most copies of its waits, or NIL when none has a live wait."
  (let ((tileset (view-tileset view))
        (best nil)
        (best-total 0))
    (dolist (action (sort (copy-list (remove :riichi (view-actions view) :key #'first :test-not #'eq))
                          #'< :key (lambda (action)
                                     (let ((tile (second action)))
                                       (+ (* 2 (tile-kind-id tileset tile))
                                          (if (tile-red-p tileset tile) 1 0))))))
      (let ((total (waits-total (tenpai-waits view :discard (second action)))))
        (when (and (> total best-total)
                   (not (and avoid-furiten (seat-furiten-p view :discard (second action)))))
          (setf best action
                best-total total))))
    best))

(defun best-discard-action (view &key live)
  (let ((option (first (discard-options view :live live))))
    (and option (list :discard (discard-option-tile option)))))

(defmethod choose-action ((bot easy-bot) view)
  (let ((actions (view-actions view)))
    (or (find-action :tsumo actions)
        (find-action :ron actions)
        (and (turn-phase-p view)
             (or (find-action :kita actions)
                 (riichi-choice view)
                 (best-discard-action view)))
        (fallback-action actions))))

;;; Hard

(defun riichi-threat-p (view)
  (loop for other across (view-seats view)
        for index from 0
        thereis (and (/= index (view-seat view)) (seen-riichi other))))

(defun hand-shanten (view)
  (let ((tileset (view-tileset view)))
    (shanten tileset (variant-shapes (view-variant view))
             (hand-counts tileset (view-hand view)) (seen-melds (view-own view)))))

(defun safest-discard-action (view)
  (let ((options (stable-sort (copy-list (discard-options view :live t)) #'>
                              :key (lambda (option)
                                     (tile-safety view (discard-option-tile option))))))
    (and options (list :discard (discard-option-tile (first options))))))

(defun ankan-keeps-shanten-p (view id)
  "Whether a closed kan of kind ID leaves the seat of VIEW no further from tenpai."
  (let* ((tileset (view-tileset view))
         (before (hand-shanten view))
         (after (shanten tileset (variant-shapes (view-variant view))
                         (hand-counts tileset (remove id (view-hand view)
                                                      :key (lambda (tile)
                                                             (tile-kind-id tileset tile))))
                         (append (seen-melds (view-own view))
                                 (list (make-meld :quad (make-list 4 :initial-element id)))))))
    (and before after (<= after before))))

(defun yakuhai-kind-p (view kind)
  (case (kind-group kind)
    (:dragon t)
    (:wind (member (kind-name kind) (list (view-seat-wind view) (view-round-wind view))))))

;; TODO: calls only yakuhai pons; call when the hand keeps a yaku or gains speed (https://github.com/takeiteasy/cl-mahjong/issues/42)
(defun yakuhai-pon (view)
  (let ((claim (view-claim view)))
    (when (and claim (yakuhai-kind-p view (tile-kind (view-tileset view) (getf claim :tile))))
      (find-action :pon (view-actions view)))))

(defmethod choose-action ((bot hard-bot) view)
  (let ((actions (view-actions view)))
    (or (find-action :tsumo actions)
        (find-action :ron actions)
        (if (turn-phase-p view)
            (or (find-action :kita actions)
                (find-if (lambda (action)
                           (and (eq (first action) :ankan)
                                (ankan-keeps-shanten-p view (second action))))
                         actions)
                (riichi-choice view :avoid-furiten t)
                (and (riichi-threat-p view)
                     (let ((shanten (hand-shanten view)))
                       (and shanten (>= shanten (bot-fold-shanten bot))))
                     (safest-discard-action view))
                (best-discard-action view :live t))
            (yakuhai-pon view))
        (fallback-action actions))))

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
                     do (observe bot seat (seat-events seat events))))))
      (announce events)
      (loop until (or (game-over-p game) (and deals (>= played deals)))
            do (multiple-value-bind (new events)
                   (if (eq (game-phase game) :deal-over)
                       (next-deal game)
                       (let ((seat (first (deciding-seats game))))
                         (apply-action game seat
                                       (choose-action (svref bots seat) (game-view game seat)))))
                 (when (and (eq (game-phase game) :playing) (eq (game-phase new) :deal-over))
                   (incf played))
                 (setf game new)
                 (announce events)))
      game)))
