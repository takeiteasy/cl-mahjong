(in-package #:cl-mahjong)

(defstruct (variant (:constructor make-variant (spec name players tileset shapes yaku scoring options)))
  spec name players tileset shapes yaku scoring options)

(defparameter *variant-clauses* '(:name :players :tileset :shapes :yaku :scoring :options))

(defmacro defvariant (name &body body)
  (multiple-value-bind (parent clauses) (parse-definition-body body)
    `(register-definition :variant ',name ',parent ',(cons (list :name name) clauses))))

(defun find-variant (name)
  (find-definition :variant name))

(defmethod compile-definition ((kind (eql :variant)) parent clauses)
  (build-variant (merge-variant-spec (and parent (variant-spec parent)) clauses)))

(defun merge-variant-spec (parent clauses)
  "Child clauses replace the parent's clause with the same key."
  (let ((spec (copy-list parent)))
    (dolist (clause clauses spec)
      (unless (and (consp clause) (member (car clause) *variant-clauses*))
        (invalid "Unknown variant clause ~s" clause))
      (setf (getf spec (car clause)) (cdr clause)))))

(defun single-arg (spec key)
  (let ((args (getf spec key)))
    (unless (and (consp args) (null (cdr args)))
      (invalid "Variant clause ~s takes one value, got ~s" key args))
    (first args)))

(defun build-variant (spec)
  (let ((players (single-arg spec :players))
        (shapes (getf spec :shapes))
        (yaku (getf spec :yaku))
        (options (getf spec :options)))
    (unless (and (integerp players) (> players 1))
      (invalid "Variant :players must be an integer above 1, got ~s" players))
    (unless (and shapes (every #'symbolp shapes))
      (invalid "Variant :shapes needs shape names, got ~s" shapes))
    (unless (and yaku (every #'symbolp yaku))
      (invalid "Variant :yaku needs yaku names, got ~s" yaku))
    (unless (evenp (length options))
      (invalid "Variant :options needs keyword/value pairs, got ~s" options))
    (let ((scoring (single-arg spec :scoring)))
      (find-scoring scoring)
      (mapc #'find-shape shapes)
      (mapc #'find-yaku yaku)
      (make-variant spec (keyword-of (single-arg spec :name)) players
                    (find-tileset (single-arg spec :tileset))
                    shapes yaku scoring options))))
