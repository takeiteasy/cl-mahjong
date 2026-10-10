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
    :uma (15 5 -5 -15) :oka t :score-rounding nil :initial-dora 1
    :bonus-tiles nil :bonus-rinshan t :bonus-win nil :bonus-rob nil :riichi t :furiten t
    :nukidora nil :kita-ippatsu nil :kita-interrupts t :kita-rinshan t :kita-ron t
    :winds (:east :south :west :north))
  "Game-flow options a variant's :options may override.")

(defparameter *hand-size* 13)

(defun variant-option (variant key)
  (let ((value (getf (variant-options variant) key '%missing)))
    (if (eq value '%missing) (getf *game-defaults* key) value)))

;;; State

(defstruct (player (:constructor make-player (tiles)))
  tiles melds discards nuki bonus
  (called-away 0)
  riichi double-riichi ippatsu temp-furiten riichi-furiten)

(defstruct claim kind tile from options responses)

(defstruct deal
  wall players dealer turn phase drawn rinshan kan-from forbidden claim
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
    (when (and (not (variant-option variant :bonus-tiles))
               (find :bonus (tileset-kinds (variant-tileset variant)) :key #'kind-category))
      (illegal "The tileset has bonus tiles; the variant needs the :bonus-tiles option"))
    (let ((game (begin-deal game)))
      (values game (reverse *events*)))))

;;; Bonus tiles

(defun bonus-tile-p (game tile)
  (and (opt game :bonus-tiles)
       (eq (kind-category (tile-kind (game-tileset game) tile)) :bonus)))

(defun settle-deal-bonus (game wall hands dealer)
  "(values wall bonuses events): each seat from DEALER sets aside its bonus tiles, replacing them in turn.
HANDS is changed to the tiles kept; BONUSES holds the set-aside tiles of each seat."
  ;; TODO: :bonus-win and :bonus-rob are not checked while dealing (https://github.com/takeiteasy/cl-mahjong/issues/26)
  (let ((bonuses (make-array (game-seats game) :initial-element '()))
        (events '()))
    (dotimes (step (game-seats game))
      (let* ((seat (seat-after game dealer step))
             (queue (svref hands seat))
             (kept '()))
        (loop while queue
              do (let ((tile (pop queue)))
                   (cond ((bonus-tile-p game tile)
                          (push tile (svref bonuses seat))
                          (push (list :bonus :seat seat :tile tile) events)
                          (multiple-value-bind (new next) (wall-draw-nuki wall)
                            (setf wall next)
                            (when new
                              (push (list :draw :seat seat :tile new :bonus t) events)
                              (push new queue))))
                         (t (push tile kept)))))
        (setf (svref hands seat) kept
              (svref bonuses seat) (nreverse (svref bonuses seat)))))
    (values wall bonuses (nreverse events))))

(defun begin-deal (game)
  (let* ((dealer (mod (game-kyoku game) (game-seats game)))
         (stacked (game-walls game))
         (wall (or (first stacked) (make-wall (game-tileset game) :seed (wall-seed game)))))
    (multiple-value-bind (wall hands) (deal-hands game wall dealer)
      (multiple-value-bind (wall bonuses bonus-events)
          (if (opt game :bonus-tiles)
              (settle-deal-bonus game wall hands dealer)
              (values wall (make-array (game-seats game) :initial-element '()) '()))
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
                         :players (coerce (loop for tiles across hands
                                                for bonus across bonuses
                                                collect (let ((player (make-player (sorted-tiles tiles))))
                                                          (setf (player-bonus player) bonus)
                                                          player))
                                          'simple-vector)))
        (emit :deal-start :kyoku (game-kyoku game) :dealer dealer :honba (game-honba game)
                          :dora (wall-dora-indicators wall))
        (dolist (event bonus-events)
          (push event *events*))
        (draw-tile game dealer))))))

(defun next-deal (game)
  "(values game events): start the deal that follows a finished one."
  (unless (eq (game-phase game) :deal-over)
    (illegal "The game is not between deals"))
  (let ((*events* '()))
    (let ((game (begin-deal game)))
      (values game (reverse *events*)))))

;;; Draws

(defun take-tile (game seat tile wall rinshan)
  (if (bonus-tile-p game tile)
      (set-aside-bonus game seat tile wall rinshan)
      (take-live-tile game seat tile wall rinshan)))

(defun set-aside-bonus (game seat tile wall rinshan)
  "SEAT sets aside the bonus TILE it drew and draws its replacement."
  (emit :bonus :seat seat :tile tile)
  (let ((game (updating-deal (deal game)
                (let ((deal (updating-player (player deal seat)
                              (setf (player-bonus player)
                                    (append (player-bonus player) (list tile))))))
                  (setf (deal-wall deal) wall)
                  deal))))
    (let ((winner (bonus-winner game seat)))
      (if winner
          (conclude-bonus-win game winner (and (/= winner seat) seat))
          (multiple-value-bind (new next) (wall-draw-nuki (deal-wall (game-deal game)))
            (if (null new)
                (exhaustive-draw game)
                (let ((rinshan (or rinshan (and (opt game :bonus-rinshan) :bonus))))
                  (emit :draw :seat seat :tile new :bonus t :rinshan (and rinshan t))
                  (take-tile game seat new next rinshan))))))))

(defun bonus-winner (game seat)
  "The seat that wins at once on the bonus tile SEAT just set aside, or NIL: SEAT with enough tiles, else
with :bonus-rob another seat one tile short."
  (let ((count (first (opt game :bonus-win)))
        (deal (game-deal game)))
    (when count
      (flet ((held (other) (length (player-bonus (deal-player deal other)))))
        (cond ((>= (held seat) count) seat)
              ((opt game :bonus-rob)
               (loop for other below (game-seats game)
                     when (and (/= other seat) (= (held other) (1- count))) return other)))))))

(defun conclude-bonus-win (game winner from)
  "WINNER wins the limit of :bonus-win with no hand, as a tsumo or, paid by FROM, a ron."
  (let* ((deal (game-deal game))
         (limit (second (opt game :bonus-win)))
         (scoring (resolve-scoring (variant-scoring (game-variant game)))))
    (multiple-value-bind (payments total)
        (limit-payments scoring limit :dealer (= winner (deal-dealer deal))
                                      :players (game-seats game) :tsumo (null from))
      (let ((reading (make-reading nil nil nil (list (cons :bonus-win 1)) 0 1 nil nil)))
        (conclude-win game deal (list winner)
                      (list (list* winner
                                   (make-score reading 0 0 nil limit
                                               (second (assoc limit (scoring-limits scoring)))
                                               total payments)
                                   from)))))))

(defun take-live-tile (game seat tile wall rinshan)
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

(defun nuki-draw (game seat)
  "SEAT's replacement draw for a tile set aside as dora."
  (multiple-value-bind (tile wall) (wall-draw-nuki (deal-wall (game-deal game)))
    (emit :draw :seat seat :tile tile :kita t :rinshan (opt game :kita-rinshan))
    (take-tile game seat tile wall (opt game :kita-rinshan))))

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
             :kan-from (and (eq (deal-rinshan deal) t) (deal-kan-from deal))
             :nuki (player-nuki player)
             :bonus (player-bonus player)
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

(defun winning-kinds (game counts melds &key nuki concealed-only)
  "Kind ids that complete the hand of COUNTS and MELDS.
NUKI are the player's own tiles set aside. Copies in the hand, melds and NUKI are used up, or with
CONCEALED-ONLY just those in COUNTS and NUKI, so a tile whose other copies are in a meld still waits."
  ;; TODO: one decomposition per tile kind; derive waits from the shanten search (https://github.com/takeiteasy/cl-mahjong/issues/8)
  (let* ((tileset (game-tileset game))
         (shapes (variant-shapes (game-variant game)))
         (counts (copy-counts counts))
         (nuki-ids (tile-kind-ids game nuki))
         (*check-copies* (not concealed-only)))
    (loop for kind across (tileset-kinds tileset)
          for id = (kind-id kind)
          when (and (not (eq (kind-category kind) :bonus))
                    (< (+ (if concealed-only (aref counts id) (held-count counts melds id))
                          (count id nuki-ids))
                       (kind-copies kind))
                    (progn (incf (aref counts id))
                           (prog1 (complete-p tileset shapes counts melds)
                             (decf (aref counts id)))))
            collect id)))

(defun furiten-p (game deal seat)
  (let ((player (deal-player deal seat)))
    (and (opt game :furiten)
         (or (player-temp-furiten player)
             (player-riichi-furiten player)
             (let ((waits (winning-kinds game (hand-counts (game-tileset game) (player-tiles player))
                                         (player-melds player) :nuki (player-nuki player))))
               (some (lambda (id) (member id waits))
                     (tile-kind-ids game (player-discards player))))))))

;;; Points

(defun apply-deltas (points deltas)
  (let ((new (copy-seq points)))
    (loop for (seat . delta) in deltas do (incf (aref new seat) delta))
    new))

(defun liable-seats (payments)
  "(seat . amount) for each seat liable under PAYMENTS: yakuman pao, or the whole payment of a rinshan pao."
  (or (getf (cddr payments) :pao)
      (let ((giver (getf (cddr payments) :from)))
        (and giver (list (cons giver (second payments)))))))

(defun win-deltas (game deal score winner from)
  "Alist of (seat . points) for a win; FROM is the discarder, or NIL for a tsumo.
A score whose payments name a :from seat is paid whole by that seat."
  (let* ((payments (score-payments score))
         (dealer (deal-dealer deal))
         (from (or from (getf (cddr payments) :from))))
    (cons (cons winner (score-points score))
          (append (if from
                      (cons (cons from (- (second payments)))
                            (let ((others (getf (cddr payments) :others)))
                              (and others
                                   (loop for seat below (game-seats game)
                                         unless (or (= seat winner) (= seat from))
                                           collect (cons seat (- others))))))
                      (loop for seat below (game-seats game)
                            unless (= seat winner)
                              collect (cons seat (- (if (and (/= winner dealer) (= seat dealer))
                                                        (getf payments :dealer)
                                                        (getf payments :other))))))
                  (loop for (seat . amount) in (getf payments :pao)
                        collect (cons seat (- amount)))))))

(defun merge-deltas (&rest lists)
  (let ((totals '()))
    (dolist (list lists)
      (loop for (seat . delta) in list
            do (let ((entry (assoc seat totals)))
                 (if entry (incf (cdr entry) delta) (push (cons seat delta) totals)))))
    (sort totals #'< :key #'car)))

;;; Ending a deal

(defun round-half-away (points)
  "POINTS rounded to thousands, a half going away from zero."
  (* (signum points) (floor (+ (abs points) 500) 1000)))

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
         (rounded (eq (opt game :score-rounding) :half-away))
         (scores (make-array seats)))
    (incf (aref points (first order)) (* sticks (opt game :riichi-cost)))
    (loop for seat in order
          for place from 0
          do (setf (aref scores seat)
                   (if rounded
                       (+ (- (* 1000 (round-half-away (aref points seat))) target)
                          (* 1000 (or (nth place uma) 0)))
                       (+ (- (aref points seat) target)
                          (* 1000 (or (nth place uma) 0))
                          (if (and oka (zerop place)) oka 0)))))
    (when rounded
      (setf (aref scores (first order))
            (- (loop for seat in (rest order) sum (aref scores seat)))))
    (list :order order :points points :scores scores)))

(defun game-ends-p (game points kyoku renchan yame)
  "Whether the game is over after a deal that left POINTS and goes on at KYOKU."
  (let* ((seats (game-seats game))
         (regular (* seats (ecase (opt game :length) (:tonpuu 1) (:hanchan 2) (:full 4))))
         (limit (if (opt game :west-extension) (+ regular seats) regular))
         (target (opt game :target-points))
         (top (reduce #'max points))
         (dealer (mod kyoku seats)))
    (cond ((and (opt game :tobi) (some #'minusp points)) t)
          ((>= kyoku limit) t)
          ((and (>= kyoku regular) (not renchan) (>= top target)) t)
          ((and renchan yame (>= kyoku (1- regular)) (>= top target)
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
                   :pao (liable-seats (score-payments score)) :score score)))
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
  "True when PLAYER is one tile from a win and a winning kind is not held in full in the concealed hand."
  (let* ((tileset (game-tileset game))
         (counts (hand-counts tileset (player-tiles player))))
    (and (eql 0 (shanten tileset (variant-shapes (game-variant game)) counts (player-melds player)))
         (winning-kinds game counts (player-melds player)
                        :nuki (player-nuki player) :concealed-only t)
         t)))

(defun exhaustive-draw (game)
  (let* ((deal (game-deal game))
         (seats (game-seats game))
         (tenpai (loop for seat below seats
                       when (tenpai-p game (deal-player deal seat)) collect seat))
         (nagashi (and (opt game :nagashi-mangan)
                       (loop for seat below seats
                             when (nagashi-seat-p game (deal-player deal seat)) collect seat)))
         (renchan (or (eq (opt game :renchan) :draw)
                      (and (eq (opt game :renchan) :tenpai) (member (deal-dealer deal) tenpai) t))))
    (conclude-deal game deal :exhaustive
                   (if nagashi (nagashi-deltas game deal nagashi) (noten-deltas game tenpai))
                   :renchan renchan :yame :tenpai
                   :detail (list :tenpai tenpai :nagashi nagashi))))

(defun abort-deal (game reason)
  (conclude-deal game (game-deal game) :abortive nil :renchan t :detail (list :reason reason)))
