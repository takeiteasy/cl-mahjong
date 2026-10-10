(in-package #:cl-mahjong)

(define-condition illegal-action (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream) (write-string (error-message c) stream))))

(defun illegal (control &rest args)
  (error 'illegal-action :message (apply #'format nil control args)))

(defparameter *game-defaults*
  '(:start-points 25000 :target-points 30000 :length :hanchan :west-extension t
    :tobi t :agari-yame t :tenpai-yame nil
    :kuikae :forbid :kan-dora :after-discard :chi t :ankan-chankan :kokushi
    :multi-ron :double :sanchahou t
    :kyuushu t :suufon-renda t :suucha-riichi t :suukaikan t
    :nagashi-mangan :mangan :noten-payment 3000
    :riichi-cost 1000 :riichi-min-tiles 4 :renchan :tenpai
    :uma (15 5 -5 -15) :oka t :initial-dora 1
    :winds (:east :south :west :north))
  "Game-flow options a variant's :options may override.")

(defparameter *hand-size* 13)

(defun variant-option (variant key)
  (let ((value (getf (variant-options variant) key '%missing)))
    (if (eq value '%missing) (getf *game-defaults* key) value)))

;;; State

(defstruct (player (:constructor make-player (tiles)))
  tiles melds discards
  (called-away 0)
  riichi double-riichi ippatsu temp-furiten riichi-furiten)

(defstruct claim kind tile from options responses)

(defstruct deal
  wall players dealer turn phase drawn rinshan forbidden claim
  (kans 0) (pending-dora 0) interrupted points (sticks 0))

;; kyoku counts rounds from east 1; the dealer is kyoku mod players.
(defstruct game
  variant seed walls (dealt 0) scores (kyoku 0) (honba 0) (table-sticks 0)
  deal (phase :playing) result final)

(defvar *events*)

(defun emit (type &rest plist)
  (push (cons type plist) *events*))

(defun game-over-p (game)
  (eq (game-phase game) :game-over))

(defun game-points (game)
  "Every seat's points; during a deal, the points after riichi deposits."
  (if (eq (game-phase game) :playing)
      (deal-points (game-deal game))
      (game-scores game)))

(defun game-sticks (game)
  "Riichi sticks on the table; during a deal, including this deal's deposits."
  (if (eq (game-phase game) :playing)
      (deal-sticks (game-deal game))
      (game-table-sticks game)))

(defun game-tileset (game) (variant-tileset (game-variant game)))
(defun game-seats (game) (variant-players (game-variant game)))
(defun opt (game key) (variant-option (game-variant game) key))

(defun deal-player (deal seat) (svref (deal-players deal) seat))

(defun set-player (deal seat player)
  (let ((players (copy-seq (deal-players deal)))
        (new (copy-deal deal)))
    (setf (svref players seat) player
          (deal-players new) players)
    new))

(defmacro updating-player ((player deal seat) &body body)
  "A copy of DEAL where the copy of SEAT's player, bound to PLAYER, is changed by BODY."
  `(let ((,player (copy-player (deal-player ,deal ,seat))))
     ,@body
     (set-player ,deal ,seat ,player)))

(defun replace-deal (game deal)
  (let ((new (copy-game game)))
    (setf (game-deal new) deal)
    new))

(defmacro updating-deal ((deal game) &body body)
  "GAME with its deal replaced by the value of BODY, which sees the current deal as DEAL."
  `(let ((,deal (game-deal ,game)))
     (replace-deal ,game (progn ,@body))))

(defun seat-after (game seat &optional (steps 1))
  (mod (+ seat steps) (game-seats game)))

(defun seat-distance (game from seat)
  (mod (- seat from) (game-seats game)))

(defun player-closed-p (player)
  (hand-closed-p (player-melds player)))

(defun player-all-tiles (player)
  (append (player-tiles player)
          (loop for meld in (player-melds player) append (meld-tiles meld))))

(defun sorted-tiles (tiles)
  (sort (copy-list tiles) #'<))

(defun tile-kind-ids (game tiles)
  (mapcar (lambda (tile) (tile-kind-id (game-tileset game) tile)) tiles))

;;; Starting

(defun wall-seed (game)
  (rng-next (make-rng (+ (* (game-seed game) 1000003) (game-dealt game)))))

(defun deal-hands (game wall dealer)
  "(values wall hands): HANDS holds each seat's tiles, dealt in rounds of four from the dealer."
  (let ((hands (make-array (game-seats game) :initial-element '())))
    (loop with remaining = *hand-size*
          while (plusp remaining)
          do (let ((chunk (min 4 remaining)))
               (dotimes (step (game-seats game))
                 (let ((seat (seat-after game dealer step)))
                   (dotimes (i chunk)
                     (multiple-value-bind (tile next) (wall-draw wall)
                       (push tile (svref hands seat))
                       (setf wall next)))))
               (decf remaining chunk)))
    (values wall hands)))

(defun start-game (variant &key seed walls points (kyoku 0) (honba 0) (sticks 0))
  "(values game events). WALLS are stacked walls for the first deals; later deals are shuffled from SEED.
POINTS, KYOKU, HONBA and STICKS start the game part way through."
  (let* ((variant (if (variant-p variant) variant (find-variant variant)))
         (players (variant-players variant))
         (game (make-game :variant variant
                          :seed (or seed (random most-positive-fixnum))
                          :walls walls
                          :kyoku kyoku :honba honba :table-sticks sticks
                          :scores (if points
                                      (coerce points 'simple-vector)
                                      (make-array players
                                                  :initial-element (variant-option variant
                                                                                   :start-points)))))
         (*events* '()))
    (unless (= players (length (game-scores game)))
      (illegal "Expected points for ~a players, got ~s" players points))
    (let ((game (begin-deal game)))
      (values game (reverse *events*)))))

(defun begin-deal (game)
  (let* ((dealer (mod (game-kyoku game) (game-seats game)))
         (stacked (game-walls game))
         (wall (or (first stacked) (make-wall (game-tileset game) :seed (wall-seed game)))))
    (multiple-value-bind (wall hands) (deal-hands game wall dealer)
      (dotimes (i (1- (opt game :initial-dora)))
        (setf wall (or (wall-reveal-dora wall) wall)))
      (let ((game (copy-game game)))
        (setf (game-walls game) (rest stacked)
              (game-dealt game) (1+ (game-dealt game))
              (game-phase game) :playing
              (game-result game) nil
              (game-deal game)
              (make-deal :wall wall :dealer dealer :turn dealer :phase :turn
                         :points (copy-seq (game-scores game)) :sticks (game-table-sticks game)
                         :players (map 'simple-vector
                                       (lambda (tiles) (make-player (sorted-tiles tiles)))
                                       hands)))
        (emit :deal-start :kyoku (game-kyoku game) :dealer dealer :honba (game-honba game)
                          :dora (wall-dora-indicators wall))
        (draw-tile game dealer)))))

(defun next-deal (game)
  "(values game events): start the deal that follows a finished one."
  (unless (eq (game-phase game) :deal-over)
    (illegal "The game is not between deals"))
  (let ((*events* '()))
    (let ((game (begin-deal game)))
      (values game (reverse *events*)))))

;;; Draws

(defun take-tile (game seat tile wall rinshan)
  (updating-deal (deal game)
    (let ((deal (copy-deal deal)))
      (setf (deal-wall deal) wall
            (deal-turn deal) seat
            (deal-phase deal) :turn
            (deal-drawn deal) tile
            (deal-rinshan deal) rinshan
            (deal-forbidden deal) nil
            (deal-claim deal) nil)
      (updating-player (player deal seat)
        (setf (player-tiles player) (sorted-tiles (cons tile (player-tiles player))))))))

(defun draw-tile (game seat)
  (multiple-value-bind (tile wall) (wall-draw (deal-wall (game-deal game)))
    (cond ((null tile) (exhaustive-draw game))
          (t (emit :draw :seat seat :tile tile)
             (take-tile game seat tile wall nil)))))

(defun replacement-draw (game seat)
  (multiple-value-bind (tile wall) (wall-draw-replacement (deal-wall (game-deal game)))
    (emit :draw :seat seat :tile tile :rinshan t)
    (take-tile game seat tile wall t)))

;;; Scoring a win

(defun first-turn-p (deal player)
  (and (not (deal-interrupted deal)) (null (player-discards player))))

(defun win-setup (game deal seat tile &key tsumo chankan)
  "(values context counts melds) for SEAT winning on TILE."
  (let* ((player (deal-player deal seat))
         (tileset (game-tileset game))
         (wall (deal-wall deal))
         (tiles (if tsumo (player-tiles player) (cons tile (player-tiles player))))
         (first-turn (first-turn-p deal player))
         (dealer (= seat (deal-dealer deal)))
         (winds (opt game :winds))
         (seats (game-seats game)))
    (values (make-win-context
             :win-tile tile
             :tsumo tsumo
             :riichi (eq (player-riichi player) :active)
             :double-riichi (player-double-riichi player)
             :ippatsu (player-ippatsu player)
             :seat-wind (nth (mod (- seat (deal-dealer deal)) seats) winds)
             :round-wind (nth (mod (floor (game-kyoku game) seats) (length winds)) winds)
             :last-tile (and (wall-exhausted-p wall) (not (and tsumo (deal-rinshan deal))) (not chankan))
             :rinshan (and tsumo (deal-rinshan deal))
             :chankan chankan
             :tenhou (and tsumo first-turn dealer)
             :chiihou (and tsumo first-turn (not dealer))
             :renhou (and (not tsumo) (not chankan) first-turn (not dealer))
             :dealer dealer
             :dora-indicators (wall-dora-indicators wall)
             :ura-indicators (wall-ura-indicators wall)
             :tiles (append tiles (loop for meld in (player-melds player)
                                        append (meld-tiles meld))))
            (hand-counts tileset tiles)
            (player-melds player))))

(defun best-win (game deal seat tile &key tsumo chankan (honba 0) (sticks 0))
  "The best score of SEAT winning on TILE, or NIL when the hand is not a winning hand with a yaku."
  (multiple-value-bind (context counts melds) (win-setup game deal seat tile
                                                         :tsumo tsumo :chankan chankan)
    (let ((variant (game-variant game)))
      (best-score (evaluate-score (variant-scoring variant) (game-tileset game)
                                  (variant-shapes variant) (variant-yaku variant)
                                  counts melds context
                                  :options (variant-options variant)
                                  :honba honba :riichi-sticks sticks
                                  :players (game-seats game))))))

(defun winning-kinds (game counts melds)
  "Kind ids that complete the hand of COUNTS and MELDS."
  ;; TODO: one decomposition per tile kind; derive waits from the shanten search (#19)
  (let ((tileset (game-tileset game))
        (shapes (variant-shapes (game-variant game)))
        (counts (copy-counts counts)))
    (loop for kind across (tileset-kinds tileset)
          for id = (kind-id kind)
          when (and (not (eq (kind-category kind) :bonus))
                    (< (held-count counts melds id) (kind-copies kind))
                    (progn (incf (aref counts id))
                           (prog1 (complete-p tileset shapes counts melds)
                             (decf (aref counts id)))))
            collect id)))

(defun furiten-p (game deal seat)
  (let ((player (deal-player deal seat)))
    (or (player-temp-furiten player)
        (player-riichi-furiten player)
        (let ((waits (winning-kinds game (hand-counts (game-tileset game) (player-tiles player))
                                    (player-melds player))))
          (some (lambda (id) (member id waits))
                (tile-kind-ids game (player-discards player)))))))

;;; Points

(defun apply-deltas (points deltas)
  (let ((new (copy-seq points)))
    (loop for (seat . delta) in deltas do (incf (aref new seat) delta))
    new))

(defun win-deltas (game deal score winner from)
  "Alist of (seat . points) for a win; FROM is the discarder, or NIL for a tsumo."
  (let ((payments (score-payments score))
        (dealer (deal-dealer deal)))
    (cons (cons winner (score-points score))
          (if from
              (list (cons from (- (second payments))))
              (loop for seat below (game-seats game)
                    unless (= seat winner)
                      collect (cons seat (- (if (and (/= winner dealer) (= seat dealer))
                                                (getf payments :dealer)
                                                (getf payments :other)))))))))

(defun merge-deltas (&rest lists)
  (let ((totals '()))
    (dolist (list lists)
      (loop for (seat . delta) in list
            do (let ((entry (assoc seat totals)))
                 (if entry (incf (cdr entry) delta) (push (cons seat delta) totals)))))
    (sort totals #'< :key #'car)))

;;; Ending a deal

(defun standings (game points sticks)
  "Plist (:order seats best first :points points with the table's STICKS given to first :scores final
scores with uma and oka)."
  (let* ((seats (game-seats game))
         (order (stable-sort (loop for seat below seats collect seat) #'>
                             :key (lambda (seat) (aref points seat))))
         (points (copy-seq points))
         (uma (opt game :uma))
         (target (opt game :target-points))
         (oka (and (opt game :oka)
                   (* seats (- target (opt game :start-points)))))
         (scores (make-array seats)))
    (incf (aref points (first order)) (* sticks (opt game :riichi-cost)))
    (loop for seat in order
          for place from 0
          do (setf (aref scores seat)
                   (+ (- (aref points seat) target)
                      (* 1000 (or (nth place uma) 0))
                      (if (and oka (zerop place)) oka 0))))
    (list :order order :points points :scores scores)))

(defun game-ends-p (game points kyoku renchan yame)
  "Whether the game is over after a deal that left POINTS and goes on at KYOKU."
  (let* ((seats (game-seats game))
         (regular (* seats (ecase (opt game :length) (:tonpuu 1) (:hanchan 2))))
         (limit (if (opt game :west-extension) (+ regular seats) regular))
         (target (opt game :target-points))
         (top (reduce #'max points))
         (dealer (mod kyoku seats)))
    (cond ((and (opt game :tobi) (some #'minusp points)) t)
          ((>= kyoku limit) t)
          ((and (>= kyoku regular) (>= top target)) t)
          ((and renchan yame (= kyoku (1- regular)) (>= top target)
                (= dealer (position top points))
                (if (eq yame :agari) (opt game :agari-yame) (opt game :tenpai-yame)))
           t))))

(defun conclude-deal (game deal type deltas &key renchan reset-honba collect yame detail)
  "Finish DEAL: apply DELTAS, rotate the dealer unless RENCHAN, and end the game when its rules say so."
  (let* ((points (apply-deltas (deal-points deal) deltas))
         (sticks (if collect 0 (deal-sticks deal)))
         (kyoku (if renchan (game-kyoku game) (1+ (game-kyoku game))))
         (honba (if reset-honba 0 (1+ (game-honba game))))
         (over (game-ends-p game points kyoku renchan yame))
         (finished (copy-deal deal))
         (result (list* :type type :deltas deltas :kyoku (game-kyoku game) detail))
         (new (copy-game game)))
    (setf (deal-phase finished) :ended
          (deal-claim finished) nil
          (deal-points finished) points
          (deal-sticks finished) sticks
          (game-deal new) finished
          (game-scores new) points
          (game-table-sticks new) sticks
          (game-kyoku new) kyoku
          (game-honba new) honba
          (game-result new) result
          (game-phase new) (if over :game-over :deal-over))
    (emit :deal-end :type type :deltas deltas :points points)
    (when over
      (setf (game-final new) (standings game points sticks))
      (emit :game-end :final (game-final new)))
    new))

(defun conclude-win (game deal winners score-table)
  "WINNERS are seats in payout order; SCORE-TABLE maps each to (score . discarder)."
  (let ((deltas (apply #'merge-deltas
                       (loop for winner in winners
                             for (score . from) = (cdr (assoc winner score-table))
                             collect (win-deltas game deal score winner from))))
        (dealer-wins (member (deal-dealer deal) winners)))
    (dolist (winner winners)
      (let ((score (cadr (assoc winner score-table))))
        (emit :win :seat winner :from (cddr (assoc winner score-table))
                   :han (score-han score) :fu (score-fu score) :limit (score-limit score)
                   :points (score-points score) :yaku (reading-yaku (score-reading score))
                   :score score)))
    (conclude-deal game deal (if (cddr (assoc (first winners) score-table)) :ron :tsumo) deltas
                   :renchan dealer-wins :reset-honba (not dealer-wins) :collect t :yame :agari
                   :detail (list :winners winners))))

(defun noten-deltas (game tenpai)
  (let* ((seats (game-seats game))
         (count (length tenpai))
         (pot (opt game :noten-payment)))
    (when (< 0 count seats)
      (loop for seat below seats
            collect (cons seat (if (member seat tenpai)
                                   (floor pot count)
                                   (- (floor pot (- seats count)))))))))

(defun nagashi-seat-p (game player)
  (let ((tileset (game-tileset game)))
    (and (player-discards player)
         (zerop (player-called-away player))
         (every (lambda (tile)
                  (let ((kind (tile-kind tileset tile)))
                    (or (kind-honor-p kind) (terminal-p tileset kind))))
                (player-discards player)))))

(defun nagashi-deltas (game deal seats)
  (let ((scoring (variant-scoring (game-variant game))))
    (apply #'merge-deltas
           (loop for seat in seats
                 collect (let ((dealer (= seat (deal-dealer deal))))
                           (multiple-value-bind (payments total)
                               (limit-payments scoring (opt game :nagashi-mangan)
                                               :dealer dealer :players (game-seats game))
                             (cons (cons seat total)
                                   (loop for other below (game-seats game)
                                         unless (= other seat)
                                           collect (cons other
                                                         (- (if (and (not dealer)
                                                                     (= other (deal-dealer deal)))
                                                                (getf payments :dealer)
                                                                (getf payments :other))))))))))))

(defun tenpai-p (game player)
  (let ((variant (game-variant game)))
    (eql 0 (shanten (game-tileset game) (variant-shapes variant)
                    (hand-counts (game-tileset game) (player-tiles player))
                    (player-melds player)))))

(defun exhaustive-draw (game)
  (let* ((deal (game-deal game))
         (seats (game-seats game))
         (tenpai (loop for seat below seats
                       when (tenpai-p game (deal-player deal seat)) collect seat))
         (nagashi (and (opt game :nagashi-mangan)
                       (loop for seat below seats
                             when (nagashi-seat-p game (deal-player deal seat)) collect seat)))
         (renchan (and (eq (opt game :renchan) :tenpai) (member (deal-dealer deal) tenpai) t)))
    (conclude-deal game deal :exhaustive
                   (if nagashi (nagashi-deltas game deal nagashi) (noten-deltas game tenpai))
                   :renchan renchan :yame :tenpai
                   :detail (list :tenpai tenpai :nagashi nagashi))))

(defun abort-deal (game reason)
  (conclude-deal game (game-deal game) :abortive nil :renchan t :detail (list :reason reason)))
