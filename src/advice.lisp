(in-package #:cl-mahjong)

;;; Visible tiles

(defun called-tiles (deal)
  "Tile ids taken from a discard into a meld; they stay in the discarder's discards."
  (loop for player across (deal-players deal)
        append (loop for meld in (player-melds player)
                     when (and (meld-from meld) (meld-tiles meld))
                       collect (first (meld-tiles meld)))))

(defun seat-visible (game seat)
  "Kind-count vector of the tiles SEAT sees outside its own hand and melds, for the :visible key of
`shanten' and `ukeire'. Discards, other seats' melds, every seat's nuki, dora indicators and the
revealed hand of an open riichi seat count once each."
  (let* ((deal (game-deal game))
         (tileset (game-tileset game))
         (counts (make-array (tileset-kind-count tileset) :element-type 'fixnum :initial-element 0))
         (called (called-tiles deal)))
    (flet ((see (tiles)
             (dolist (tile tiles)
               (let ((id (tile-kind-id tileset tile)))
                 (unless (eq (kind-category (tileset-kind tileset id)) :bonus)
                   (incf (aref counts id)))))))
      (dotimes (other (game-seats game))
        (let ((player (deal-player deal other)))
          (see (remove-if (lambda (tile) (member tile called)) (player-discards player)))
          (see (player-nuki player))
          (unless (= other seat)
            (see (loop for meld in (player-melds player) append (meld-tiles meld)))
            (when (and (eq (player-riichi player) :active) (player-open-riichi player))
              (see (player-tiles player))))))
      (see (wall-dora-indicators (deal-wall deal))))
    counts))

;;; Waits

(defun waiting-kind-ids (game seat discard)
  "Kind ids that complete SEAT's hand once DISCARD, a tile or NIL, has left it."
  (let* ((player (deal-player (game-deal game) seat))
         (tiles (if discard (remove discard (player-tiles player)) (player-tiles player))))
    (unless (= 1 (mod (+ (length tiles) (* 3 (length (player-melds player)))) 3))
      (bad-hand "Waits need a hand of 3n+1 tiles; pass :discard for a hand holding a drawn tile"))
    (winning-kinds game (hand-counts (game-tileset game) tiles) (player-melds player)
                   :nuki (player-nuki player))))

(defun tenpai-waits (game seat &key discard)
  "(kind . remaining) for each kind that completes SEAT's hand, or NIL when it is not tenpai.
DISCARD, a tile, is taken out first, so a hand holding a drawn tile can be asked. Remaining copies
exclude those in the hand, its melds and everything SEAT sees, and may be zero."
  (let* ((player (deal-player (game-deal game) seat))
         (tileset (game-tileset game))
         (ids (waiting-kind-ids game seat discard))
         (tiles (if discard (remove discard (player-tiles player)) (player-tiles player)))
         (remaining (remaining-copies tileset (hand-counts tileset tiles) (player-melds player)
                                      (seat-visible game seat))))
    (loop for id in ids
          collect (cons (tileset-kind tileset id) (aref remaining id)))))

(defun seat-furiten-p (game seat &key discard)
  "Whether SEAT is furiten. With DISCARD, a tile, whether it would be after discarding it."
  (let ((deal (game-deal game)))
    (and (if (null discard)
             (furiten-p game deal seat)
             (let ((waits (waiting-kind-ids game seat discard)))
               (and (opt game :furiten)
                    (some (lambda (id) (member id waits))
                          (tile-kind-ids game (append (player-discards (deal-player deal seat))
                                                      (list discard)))))))
         t)))

;;; Discards

(defstruct (discard-option (:type list))
  tile shanten total ukeire)

(defun discard-options (game seat &key live)
  "Each distinct legal discard of SEAT as a `discard-option': the tile, the shanten after it, and
the ukeire of the hand it leaves with its total. Best first: lowest shanten, then most ukeire,
then kind, a plain tile before a red one. With LIVE the copies SEAT sees are taken off each kind's
count, and a kind with none left is dropped."
  (let* ((deal (game-deal game))
         (player (deal-player deal seat))
         (tileset (game-tileset game))
         (shapes (variant-shapes (game-variant game)))
         (visible (and live (seat-visible game seat)))
         (tiles (sort (loop for (type tile) in (legal-actions game seat)
                            when (eq type :discard) collect tile)
                      #'< :key (lambda (tile)
                                 (+ (* 2 (tile-kind-id tileset tile))
                                    (if (tile-red-p tileset tile) 1 0)))))
         (options '()))
    (dolist (tile (mapcar #'first (distinct-tile-sets game (mapcar #'list tiles))))
      (let* ((counts (hand-counts tileset (remove tile (player-tiles player))))
             (ukeire (ukeire tileset shapes counts (player-melds player))))
        (when visible
          (setf ukeire (loop for (kind . left) in ukeire
                             for unseen = (- left (aref visible (kind-id kind)))
                             when (plusp unseen) collect (cons kind unseen))))
        (push (make-discard-option
               :tile tile
               :shanten (or (shanten tileset shapes counts (player-melds player)) 99)
               :total (reduce #'+ ukeire :key #'cdr)
               :ukeire ukeire)
              options)))
    (stable-sort (nreverse options)
                 (lambda (a b)
                   (or (< (discard-option-shanten a) (discard-option-shanten b))
                       (and (= (discard-option-shanten a) (discard-option-shanten b))
                            (> (discard-option-total a) (discard-option-total b))))))))

;;; Safety

(defun suji-p (game tile discards)
  "Whether the kinds three ranks either side of TILE, which a sequence wait on it needs, are all in DISCARDS."
  (let* ((tileset (game-tileset game))
         (kind (tile-kind tileset tile))
         (low (car (tileset-ranks tileset)))
         (high (cdr (tileset-ranks tileset)))
         (rank (kind-rank kind))
         (parents (remove-if-not (lambda (parent) (<= low parent high))
                                 (list (- rank 3) (+ rank 3)))))
    (and parents
         (every (lambda (parent)
                  (let ((other (find-kind tileset (kind-suit kind) parent)))
                    (and other (member (kind-id other) discards))))
                parents))))

(defun tile-safety-against (game seat tile other visible)
  (let* ((tileset (game-tileset game))
         (kind (tile-kind tileset tile))
         (discards (tile-kind-ids game (player-discards (deal-player (game-deal game) other))))
         (own (count (kind-id kind) (tile-kind-ids game (player-tiles
                                                         (deal-player (game-deal game) seat))))))
    (cond ((member (kind-id kind) discards) 4)
          ((and (kind-honor-p kind)
                (>= (+ (aref visible (kind-id kind)) (1- own)) (1- (kind-copies kind))))
           3)
          ((and (eq (kind-category kind) :suit) (suji-p game tile discards)) 2)
          ((or (kind-honor-p kind) (terminal-p tileset kind)) 1)
          (t 0))))

;; TODO: only the riichi seat's own discards are genbutsu; tiles passed since the declaration, kabe and open hands are not read (https://github.com/takeiteasy/cl-mahjong/issues/41)
(defun tile-safety (game seat tile)
  "How safe TILE is for SEAT to discard against the seats in riichi, 0 (dangerous) to 4 (a tile that
seat discarded). 4 when nobody is in riichi."
  (let ((visible (seat-visible game seat))
        (deal (game-deal game)))
    (reduce #'min (loop for other below (game-seats game)
                        when (and (/= other seat) (player-riichi (deal-player deal other)))
                          collect (tile-safety-against game seat tile other visible))
            :initial-value 4)))
