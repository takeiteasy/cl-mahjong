(in-package #:cl-mahjong)

(defstruct (kind (:constructor make-kind (name category suit rank group copies)))
  (id 0 :type fixnum)
  (first 0 :type fixnum)
  name category suit rank group copies)

(defstruct (wall-layout (:constructor make-wall-layout (dead-wall replacement dora ura nuki)))
  (dead-wall 0 :type fixnum)
  replacement dora ura
  (nuki 0 :type fixnum))

(defstruct (tileset (:constructor make-tileset
                        (spec ranks kinds index tile-kinds tile-attrs wall-layout)))
  spec ranks kinds index tile-kinds tile-attrs wall-layout)

(defmacro deftileset (name &body body)
  (multiple-value-bind (parent clauses) (parse-definition-body body)
    `(register-definition :tileset ',name ',parent ',clauses)))

(defun find-tileset (name)
  (find-definition :tileset name))

(defmethod compile-definition ((kind (eql :tileset)) parent clauses)
  (build-tileset (merge-spec (and parent (tileset-spec parent)) clauses)))

(defun merge-spec (parent clauses)
  "Child clauses replace the parent's, except :remove and :add, which accumulate."
  (let ((spec (copy-list parent)))
    (dolist (clause clauses spec)
      (unless (consp clause)
        (invalid "Tileset clause must be a list, got ~s" clause))
      (destructuring-bind (key . args) clause
        (case key
          ((:suits :ranks :honors :bonus :copies :red :wall)
           (setf (getf spec key) args))
          ((:remove :add)
           (setf (getf spec key) (append (getf spec key) args)))
          (t (invalid "Unknown tileset clause ~s" key)))))))

(defun suit-kind-name (suit rank)
  (keyword-of (format nil "~a-~a" suit rank)))

(defun rank-bounds (spec)
  (destructuring-bind (&optional (low 1) (high 9)) (getf spec :ranks)
    (cons low high)))

(defun bonus-kinds (entry)
  "ENTRY is (NAME [COPIES]), or (GROUP NAME...) whose kinds take the rank of their position."
  (destructuring-bind (head &rest more) entry
    (if (and more (every #'symbolp more))
        (loop for name in more
              for rank from 1
              collect (make-kind (keyword-of name) :bonus nil rank (keyword-of head) 1))
        (list (make-kind (keyword-of head) :bonus nil nil :bonus (or (first more) 1))))))

(defun draft-kinds (spec)
  (let ((copies (or (first (getf spec :copies)) 1))
        (bounds (rank-bounds spec)))
    (append
     (loop for suit in (getf spec :suits)
           for suit-key = (keyword-of suit)
           append (loop for rank from (car bounds) to (cdr bounds)
                        collect (make-kind (suit-kind-name suit-key rank)
                                           :suit suit-key rank nil copies)))
     (loop for (group . names) in (getf spec :honors)
           append (loop for name in names
                        collect (make-kind (keyword-of name) :honor nil nil
                                           (keyword-of group) copies)))
     (loop for entry in (append (getf spec :bonus) (getf spec :add))
           append (bonus-kinds entry)))))

(defun removal-matches-p (kind form)
  "FORM is NAME, (SUIT), (SUIT RANK), (SUIT LOW HIGH), (GROUP) or (NAME)."
  (destructuring-bind (head &optional low (high low)) (if (listp form) form (list form))
    (let ((head (keyword-of head)))
      (if (eq (kind-category kind) :suit)
          (and (eq (kind-suit kind) head)
               (or (null low) (<= low (kind-rank kind) high)))
          (and (null low)
               (or (eq (kind-name kind) head) (eq (kind-group kind) head)))))))

(defun red-counts (spec drafts)
  (let ((counts (make-hash-table :test 'eq)))
    (loop for (suit rank count) in (getf spec :red)
          for kind = (find-if (lambda (k)
                                (and (eq (kind-suit k) (keyword-of suit))
                                     (eql (kind-rank k) rank)))
                              drafts)
          do (unless kind
               (invalid ":red names a tile that does not exist: ~s" (list suit rank)))
             (unless (<= 0 count (kind-copies kind))
               (invalid ":red count ~a exceeds the ~a copies of ~(~a~)"
                        count (kind-copies kind) (kind-name kind)))
             (setf (gethash (kind-name kind) counts) count))
    counts))

(defun build-wall-layout (spec tile-count)
  (let ((clauses (getf spec :wall)))
    (dolist (clause clauses)
      (unless (member (first clause) '(:dead-wall :replacement :dora :ura :nuki))
        (invalid "Unknown :wall clause ~s" (first clause))))
    (let ((dead (or (second (assoc :dead-wall clauses)) 0))
          (replacement (rest (assoc :replacement clauses)))
          (dora (rest (assoc :dora clauses)))
          (ura (rest (assoc :ura clauses)))
          (nuki (or (second (assoc :nuki clauses)) 0)))
      (unless (and (integerp dead) (<= 0 dead) (< dead tile-count))
        (invalid ":dead-wall must be between 0 and ~a, got ~s" (1- tile-count) dead))
      (let ((positions (append replacement dora ura)))
        (unless (every (lambda (p) (and (integerp p) (< -1 p dead))) positions)
          (invalid "Wall positions must lie inside the ~a-tile dead wall: ~s" dead positions))
        (unless (= (length positions) (length (remove-duplicates positions)))
          (invalid "Wall positions overlap: ~s" positions)))
      (unless (and (integerp nuki) (<= 0 nuki))
        (invalid ":nuki must be a non-negative integer, got ~s" nuki))
      (when (and (plusp nuki) (null replacement))
        (invalid ":nuki needs :replacement slots to draw from"))
      (make-wall-layout dead replacement dora ura nuki))))

(defun build-tileset (spec)
  (let* ((drafts (draft-kinds spec))
         (red (red-counts spec drafts))
         (removals (getf spec :remove))
         (kinds (coerce (remove-if (lambda (kind)
                                     (some (lambda (form) (removal-matches-p kind form))
                                           removals))
                                   drafts)
                        'simple-vector))
         (tile-count (reduce #'+ kinds :key #'kind-copies))
         (tile-kinds (make-array tile-count :element-type 'fixnum))
         (tile-attrs (make-array tile-count :initial-element nil))
         (index (make-hash-table :test 'eq))
         (next 0))
    (loop for kind across kinds
          for id from 0
          do (when (gethash (kind-name kind) index)
               (invalid "Duplicate tile kind ~(~a~)" (kind-name kind)))
             (setf (kind-id kind) id
                   (kind-first kind) next
                   (gethash (kind-name kind) index) kind)
             (dotimes (copy (kind-copies kind))
               (setf (aref tile-kinds next) id)
               (when (< copy (gethash (kind-name kind) red 0))
                 (setf (aref tile-attrs next) '(:red)))
               (incf next)))
    (make-tileset spec (rank-bounds spec) kinds index tile-kinds tile-attrs
                  (build-wall-layout spec tile-count))))

(defun tileset-tile-count (tileset)
  (length (tileset-tile-kinds tileset)))

(defun tileset-kind-count (tileset)
  (length (tileset-kinds tileset)))

(defun tileset-kind (tileset id)
  (svref (tileset-kinds tileset) id))

(defun find-kind (tileset suit-or-name &optional rank)
  "(find-kind ts 'man 5) or (find-kind ts 'east)."
  (gethash (if rank
               (suit-kind-name (keyword-of suit-or-name) rank)
               (keyword-of suit-or-name))
           (tileset-index tileset)))

(defun kind-honor-p (kind)
  (eq (kind-category kind) :honor))

(defun terminal-p (tileset kind)
  (and (eq (kind-category kind) :suit)
       (or (eql (kind-rank kind) (car (tileset-ranks tileset)))
           (eql (kind-rank kind) (cdr (tileset-ranks tileset))))))

(defun tile-kind-id (tileset tile)
  (aref (tileset-tile-kinds tileset) tile))

(defun tile-kind (tileset tile)
  (tileset-kind tileset (tile-kind-id tileset tile)))

(defun tile-attrs (tileset tile)
  (svref (tileset-tile-attrs tileset) tile))

(defun tile-red-p (tileset tile)
  (and (member :red (tile-attrs tileset tile)) t))

(defun tiles-of-kind (tileset kind)
  (declare (ignore tileset))
  (loop for tile from (kind-first kind) repeat (kind-copies kind) collect tile))
