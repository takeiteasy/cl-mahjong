(in-package #:cl-mahjong)

;; seat-wind and round-wind are honor kind names such as :east.
;; dora-indicators and ura-indicators are tile ids; tiles are the tile ids of the whole hand,
;; melds and the winning tile included, so red fives can be counted. nuki are the tile ids set
;; aside as dora. paarenchan is the number of consecutive dealer wins, or NIL.
(defstruct win-context
  win-tile
  tsumo riichi double-riichi ippatsu
  seat-wind round-wind
  last-tile rinshan chankan
  tenhou chiihou renhou
  open-riichi paarenchan
  dealer
  dora-indicators ura-indicators
  tiles nuki)

(defun check-win-context (context melds)
  "Signal `invalid-hand' when CONTEXT describes a win that cannot happen with MELDS.
The first-turn wins only check the seat when a seat wind is given."
  (flet ((fail (message)
           (bad-hand "Impossible win: ~a" message)))
    (let ((seated (win-context-seat-wind context))
          (dealer (win-context-dealer context))
          (tsumo (win-context-tsumo context))
          (riichi (or (win-context-riichi context) (win-context-double-riichi context))))
      (when (and riichi (not (hand-closed-p melds)))
        (fail "riichi on an open hand"))
      (when (and (win-context-ippatsu context) (not riichi))
        (fail "ippatsu without riichi"))
      (when (and (win-context-chankan context) tsumo)
        (fail "chankan with tsumo"))
      (when (and (win-context-rinshan context) (not tsumo))
        (fail "rinshan without tsumo"))
      (when (and (win-context-last-tile context) (win-context-rinshan context))
        (fail "haitei with rinshan"))
      (when (and (win-context-last-tile context) (win-context-chankan context))
        (fail "houtei with chankan"))
      (when (win-context-tenhou context)
        (when (and seated (not dealer)) (fail "tenhou as a non-dealer"))
        (unless tsumo (fail "tenhou without tsumo"))
        (when melds (fail "tenhou with a meld")))
      (when (win-context-chiihou context)
        (when (and seated dealer) (fail "chiihou as the dealer"))
        (unless tsumo (fail "chiihou without tsumo"))
        (when melds (fail "chiihou with a meld")))
      (when (win-context-renhou context)
        (when (and seated dealer) (fail "renhou as the dealer"))
        (when tsumo (fail "renhou with tsumo"))
        (when melds (fail "renhou with a meld"))))))

(defstruct (win-hand (:constructor make-win-hand
                         (tileset decomposition counts melds context options
                          win-kind win-group wait)))
  tileset decomposition counts melds context options
  win-kind win-group wait)

(defun win-hand-closed-p (hand)
  (hand-closed-p (win-hand-melds hand)))

(defun win-hand-pre-win-counts (hand)
  "Kind counts of the hand before the winning tile."
  (let ((counts (copy-counts (win-hand-counts hand))))
    (decf (aref counts (win-hand-win-kind hand)))
    counts))

(defun group-concealed-p (hand group)
  "A group is concealed unless it is an open meld or a triplet completed by ron."
  (and (not (group-open-p group))
       (not (and (eq group (win-hand-win-group hand))
                 (eq (group-type group) :triplet)
                 (not (win-context-tsumo (win-hand-context hand)))))))

(defun sequence-wait (tileset group win-kind)
  (destructuring-bind (low middle high) (group-kinds group)
    (let ((ranks (tileset-ranks tileset)))
      (flet ((rank (id) (kind-rank (tileset-kind tileset id))))
        (cond ((= win-kind middle) :kanchan)
              ((and (= win-kind high) (eql (rank low) (car ranks))) :penchan)
              ((and (= win-kind low) (eql (rank high) (cdr ranks))) :penchan)
              (t :ryanmen))))))

(defun group-wait (tileset group win-kind)
  (ecase (group-type group)
    (:pair :tanki)
    (:triplet :shanpon)
    (:sequence (sequence-wait tileset group win-kind))))

(defun sets-placements (tileset decomposition melds win-kind)
  "Each distinct concealed group holding the winning kind, with its wait."
  (let ((seen '())
        (placements '()))
    (dolist (group (nthcdr (length melds) (decomposition-groups decomposition)))
      (let ((key (cons (group-type group) (group-kinds group))))
        (when (and (member win-kind (group-kinds group))
                   (not (member key seen :test #'equal)))
          (push key seen)
          (push (cons group (group-wait tileset group win-kind)) placements))))
    (nreverse placements)))

(defun each-wait (tileset shape counts win-kind)
  (let ((pre-win (copy-counts counts)))
    (decf (aref pre-win win-kind))
    (if (every (lambda (id) (plusp (aref pre-win id)))
               (class-kind-ids tileset (shape-classes shape)))
        :thirteen-sided
        :tanki)))

(defun expand-wins (tileset shape decomposition counts melds context options)
  "One WIN-HAND for each way the winning tile can sit in DECOMPOSITION."
  (let ((win-kind (tile-kind-id tileset (win-context-win-tile context))))
    (flet ((win-hand (group wait)
             (make-win-hand tileset decomposition counts melds context options
                            win-kind group wait)))
      (ecase (shape-family shape)
        (:sets (mapcar (lambda (placement) (win-hand (car placement) (cdr placement)))
                       (sets-placements tileset decomposition melds win-kind)))
        (:pairs (list (win-hand nil :tanki)))
        (:each (list (win-hand nil (each-wait tileset shape counts win-kind))))
        (:lambda (list (win-hand nil nil)))))))
