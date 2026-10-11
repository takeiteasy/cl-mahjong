(in-package #:cl-mahjong)

;;; What a seat sees of another

(defun shown-tiles (game viewer seat)
  "The concealed tiles of SEAT that VIEWER sees: the hand of a seat in open riichi."
  (let ((player (deal-player (game-deal game) seat)))
    (and (/= viewer seat)
         (eq (player-riichi player) :active)
         (player-open-riichi player)
         (player-tiles player))))

(defstruct (seen-seat (:conc-name seen-))
  discards melds nuki bonus hand-size riichi open-riichi dead shown)

(defun seen-seats (game viewer)
  "A vector with the `seen-seat' of every seat, as VIEWER sees it."
  (let ((deal (game-deal game)))
    (coerce (loop for seat below (game-seats game)
                  collect (let ((player (deal-player deal seat)))
                            (make-seen-seat :discards (player-discards player)
                                            :melds (player-melds player)
                                            :nuki (player-nuki player)
                                            :bonus (player-bonus player)
                                            :hand-size (length (player-tiles player))
                                            :riichi (player-riichi player)
                                            :open-riichi (player-open-riichi player)
                                            :dead (player-dead player)
                                            :shown (shown-tiles game viewer seat))))
            'simple-vector)))

;;; View

(defstruct (seat-view (:conc-name view-))
  seat variant phase deal-phase dealer turn kyoku honba round-wind seat-wind points sticks dora
  wall-left claim result final hand drawn forbidden furiten actions seats)

(defun view-tileset (view) (variant-tileset (view-variant view)))
(defun view-opt (view key) (variant-option (view-variant view) key))
(defun view-own (view) (svref (view-seats view) (view-seat view)))

(defun game-view (game seat)
  "A `seat-view' of GAME: only what SEAT may see. It holds no deal, wall, claim or player."
  (let* ((deal (game-deal game))
         (claim (deal-claim deal))
         (turn (and (eq (game-phase game) :playing) (eq (deal-phase deal) :turn)
                    (= seat (deal-turn deal))))
         (seats (game-seats game))
         (winds (opt game :winds)))
    (make-seat-view
     :seat seat
     :variant (game-variant game)
     :phase (game-phase game)
     :deal-phase (deal-phase deal)
     :dealer (deal-dealer deal)
     :turn (deal-turn deal)
     :kyoku (game-kyoku game)
     :honba (game-honba game)
     :round-wind (nth (mod (floor (game-kyoku game) seats) (length winds)) winds)
     :seat-wind (nth (mod (- seat (deal-dealer deal)) seats) winds)
     :points (copy-seq (game-points game))
     :sticks (game-sticks game)
     :dora (wall-dora-indicators (deal-wall deal))
     :wall-left (wall-live-count (deal-wall deal))
     :claim (and claim (list :kind (claim-kind claim) :tile (claim-tile claim)
                             :from (claim-from claim)))
     :result (game-result game)
     :final (game-final game)
     :hand (copy-list (player-tiles (deal-player deal seat)))
     :drawn (and turn (deal-drawn deal))
     :forbidden (and turn (copy-list (deal-forbidden deal)))
     :furiten (and (furiten-p game deal seat) t)
     :actions (legal-actions game seat)
     :seats (seen-seats game seat))))

;;; Events

(defun seat-events (seat events)
  "EVENTS as SEAT may see them: the tile of another seat's draw is hidden."
  (mapcar (lambda (event)
            (if (and (eq (first event) :draw) (/= seat (getf (rest event) :seat)))
                (cons :draw (loop for (key value) on (rest event) by #'cddr
                                  unless (eq key :tile) append (list key value)))
                event))
          events))
