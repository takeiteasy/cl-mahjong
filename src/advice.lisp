(in-package #:cl-mahjong)

;;; Visible tiles

(defun called-tiles (seen)
  "Tile ids taken from a discard into a meld; they stay in the discarder's discards."
  (loop for seat across seen
        append (loop for meld in (seen-melds seat)
                     when (and (meld-from meld) (meld-tiles meld))
                       collect (first (meld-tiles meld)))))

(defun view-kind-ids (view tiles)
  (let ((tileset (view-tileset view)))
    (mapcar (lambda (tile) (tile-kind-id tileset tile)) tiles)))

(defun seat-visible (view)
  "Kind-count vector of the tiles the seat of VIEW sees outside its own hand and melds, for the :visible
key of `shanten' and `ukeire'. Discards, other seats' melds, every seat's nuki, dora indicators and the
`shown-tiles' of other seats count once each."
  (let* ((tileset (view-tileset view))
         (counts (make-array (tileset-kind-count tileset) :element-type 'fixnum :initial-element 0))
         (seen (view-seats view))
         (called (called-tiles seen)))
    (flet ((see (tiles)
             (dolist (tile tiles)
               (let ((id (tile-kind-id tileset tile)))
                 (unless (eq (kind-category (tileset-kind tileset id)) :bonus)
                   (incf (aref counts id)))))))
      (loop for other across seen
            for index from 0
            do (see (remove-if (lambda (tile) (member tile called)) (seen-discards other)))
               (see (seen-nuki other))
               (unless (= index (view-seat view))
                 (see (loop for meld in (seen-melds other) append (meld-tiles meld)))
                 (see (seen-shown other))))
      (see (view-dora view)))
    counts))

;;; Waits

(defun waiting-kind-ids (view discard)
  "Kind ids that complete the hand of VIEW once DISCARD, a tile or NIL, has left it."
  (let* ((own (view-own view))
         (tiles (if discard (remove discard (view-hand view)) (view-hand view))))
    (unless (= 1 (mod (+ (length tiles) (* 3 (length (seen-melds own)))) 3))
      (bad-hand "Waits need a hand of 3n+1 tiles; pass :discard for a hand holding a drawn tile"))
    (winning-kinds (view-variant view) (hand-counts (view-tileset view) tiles) (seen-melds own)
                   :nuki (seen-nuki own))))

(defun tenpai-waits (view &key discard)
  "(kind . remaining) for each kind that completes the hand of VIEW, or NIL when it is not tenpai.
DISCARD, a tile, is taken out first, so a hand holding a drawn tile can be asked. Remaining copies
exclude those in the hand, its melds and everything the seat sees, and may be zero."
  (let* ((tileset (view-tileset view))
         (ids (waiting-kind-ids view discard))
         (tiles (if discard (remove discard (view-hand view)) (view-hand view)))
         (remaining (remaining-copies tileset (hand-counts tileset tiles) (seen-melds (view-own view))
                                      (seat-visible view))))
    (loop for id in ids
          collect (cons (tileset-kind tileset id) (aref remaining id)))))

(defun seat-furiten-p (view &key discard)
  "Whether the seat of VIEW is furiten. With DISCARD, a tile, whether it would be after discarding it."
  (and (if (null discard)
           (view-furiten view)
           (let ((waits (waiting-kind-ids view discard)))
             (and (view-opt view :furiten)
                  (some (lambda (id) (member id waits))
                        (view-kind-ids view (append (seen-discards (view-own view))
                                                    (list discard)))))))
       t))

;;; Discards

(defstruct (discard-option (:type list))
  tile shanten total ukeire)

(defun discard-options (view &key live)
  "Each distinct legal discard of the seat of VIEW as a `discard-option': the tile, the shanten after
it, and the ukeire of the hand it leaves with its total. Best first: lowest shanten, then most ukeire,
then kind, a plain tile before a red one. With LIVE the copies the seat sees are taken off each kind's
count, and a kind with none left is dropped."
  (let* ((tileset (view-tileset view))
         (melds (seen-melds (view-own view)))
         (shapes (variant-shapes (view-variant view)))
         (visible (and live (seat-visible view)))
         (tiles (sort (loop for (type tile) in (view-actions view)
                            when (eq type :discard) collect tile)
                      #'< :key (lambda (tile)
                                 (+ (* 2 (tile-kind-id tileset tile))
                                    (if (tile-red-p tileset tile) 1 0)))))
         (options '()))
    (dolist (tile (mapcar #'first (distinct-tile-sets tileset (mapcar #'list tiles))))
      (let* ((counts (hand-counts tileset (remove tile (view-hand view))))
             (ukeire (ukeire tileset shapes counts melds)))
        (when visible
          (setf ukeire (loop for (kind . left) in ukeire
                             for unseen = (- left (aref visible (kind-id kind)))
                             when (plusp unseen) collect (cons kind unseen))))
        (push (make-discard-option
               :tile tile
               :shanten (or (shanten tileset shapes counts melds) 99)
               :total (reduce #'+ ukeire :key #'cdr)
               :ukeire ukeire)
              options)))
    (stable-sort (nreverse options)
                 (lambda (a b)
                   (or (< (discard-option-shanten a) (discard-option-shanten b))
                       (and (= (discard-option-shanten a) (discard-option-shanten b))
                            (> (discard-option-total a) (discard-option-total b))))))))

;;; Safety

(defun suji-p (tileset tile discards)
  "Whether the kinds three ranks either side of TILE, which a sequence wait on it needs, are all in DISCARDS."
  (let* ((kind (tile-kind tileset tile))
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

(defun tile-safety-against (view tile other visible)
  (let* ((tileset (view-tileset view))
         (kind (tile-kind tileset tile))
         (discards (view-kind-ids view (seen-discards other)))
         (own (count (kind-id kind) (view-kind-ids view (view-hand view)))))
    (cond ((member (kind-id kind) discards) 4)
          ((and (kind-honor-p kind)
                (>= (+ (aref visible (kind-id kind)) (1- own)) (1- (kind-copies kind))))
           3)
          ((and (eq (kind-category kind) :suit) (suji-p tileset tile discards)) 2)
          ((or (kind-honor-p kind) (terminal-p tileset kind)) 1)
          (t 0))))

;; TODO: only the riichi seat's own discards are genbutsu; tiles passed since the declaration, kabe and open hands are not read (https://github.com/takeiteasy/cl-mahjong/issues/41)
(defun tile-safety (view tile)
  "How safe TILE is for the seat of VIEW to discard against the seats in riichi, 0 (dangerous) to 4
(a tile that seat discarded). 4 when nobody is in riichi."
  (let ((visible (seat-visible view)))
    (reduce #'min (loop for other across (view-seats view)
                        for index from 0
                        when (and (/= index (view-seat view)) (seen-riichi other))
                          collect (tile-safety-against view tile other visible))
            :initial-value 4)))
