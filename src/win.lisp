(in-package #:cl-mahjong)

;; seat-wind and round-wind are honor kind names such as :east.
;; dora-indicators and ura-indicators are tile ids; tiles are the tile ids of the whole hand,
;; melds and the winning tile included, so red fives can be counted. nuki are the tile ids set
;; aside as dora.
(defstruct win-context
  win-tile
  tsumo riichi double-riichi ippatsu
  seat-wind round-wind
  last-tile rinshan chankan
  tenhou chiihou renhou
  dealer
  dora-indicators ura-indicators
  tiles nuki)

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
