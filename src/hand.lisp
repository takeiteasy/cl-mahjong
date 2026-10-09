(in-package #:cl-mahjong)

(define-condition invalid-hand (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream) (write-string (error-message c) stream))))

(defun bad-hand (control &rest args)
  (error 'invalid-hand :message (apply #'format nil control args)))

(defun hand-counts (tileset tiles)
  "Kind-count vector for a sequence of tile ids."
  (let ((counts (make-array (tileset-kind-count tileset) :element-type 'fixnum
                                                         :initial-element 0)))
    (map nil (lambda (tile)
               (let ((kind (tile-kind tileset tile)))
                 (when (eq (kind-category kind) :bonus)
                   (bad-hand "Bonus tile ~(~a~) cannot be in a hand" (kind-name kind)))
                 (incf (aref counts (kind-id kind)))))
         tiles)
    counts))

(defstruct (meld (:constructor %make-meld (type kinds open-p from tiles)))
  type kinds open-p from tiles)

(defun make-meld (type kinds &key open from tiles)
  "TYPE is :sequence, :triplet or :quad; KINDS lists the kind id of every tile.
FROM is the seat the called tile came from and TILES the tile ids; scoring ignores both."
  (unless (member type '(:sequence :triplet :quad))
    (bad-hand "Unknown meld type ~s" type))
  (unless (= (length kinds) (if (eq type :quad) 4 3))
    (bad-hand "A ~(~a~) meld has ~a tiles, got ~s" type (if (eq type :quad) 4 3) kinds))
  (%make-meld type kinds (and open t) from tiles))

(defun hand-closed-p (melds)
  "A hand is closed when no meld is open; a closed kan keeps it closed."
  (notany #'meld-open-p melds))
