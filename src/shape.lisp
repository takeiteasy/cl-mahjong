(in-package #:cl-mahjong)

(defstruct shape
  spec name family
  (sets 0) (pair 0) set-types
  (pairs 0) distinct
  classes (extra 0) extra-classes
  complete-fn shanten-fn)

(defparameter *shape-clauses*
  '((:sets . :sets) (:pair . :sets)
    (:pairs . :pairs)
    (:each . :each) (:extra . :each)
    (:lambda . :lambda)))

(defparameter *tile-classes* '(:terminal :honor :simple))

(defmacro defshape (name &body body)
  (multiple-value-bind (parent clauses) (parse-definition-body body)
    `(register-definition :shape ',name ',parent ',(cons (list :name name) clauses))))

(defun find-shape (name)
  (find-definition :shape name))

(defmethod compile-definition ((kind (eql :shape)) parent clauses)
  (build-shape (merge-shape-spec (and parent (shape-spec parent)) clauses)))

(defun merge-shape-spec (parent clauses)
  "Child clauses replace the parent's clause with the same key."
  (let ((spec (copy-list parent)))
    (dolist (clause clauses spec)
      (unless (and (consp clause)
                   (or (eq (car clause) :name) (assoc (car clause) *shape-clauses*)))
        (invalid "Unknown shape clause ~s" clause))
      (setf (getf spec (car clause)) (cdr clause)))))

(defun clause-present-p (spec key)
  (loop for (k) on spec by #'cddr thereis (eq k key)))

(defun count-arg (value what minimum)
  (unless (and (integerp value) (>= value minimum))
    (invalid "~a needs an integer of at least ~a, got ~s" what minimum value))
  value)

(defun clause-options (options what allowed)
  (unless (and (evenp (length options))
               (loop for (key) on options by #'cddr always (member key allowed)))
    (invalid "~a takes only ~{~s~^, ~} options, got ~s" what allowed options))
  options)

(defun keyword-list (designators allowed what)
  (unless (and (listp designators) designators)
    (invalid "~a needs a non-empty list, got ~s" what designators))
  (mapcar (lambda (designator)
            (let ((key (keyword-of designator)))
              (unless (member key allowed)
                (invalid "~a: ~(~a~) is not one of ~{~(~a~)~^, ~}" what designator allowed))
              key))
          designators))

(defun resolve-function (form what)
  "FORM is a function, the name of one, or a form evaluated in the null environment."
  (let ((fn (cond ((functionp form) form)
                  ((and form (symbolp form)) (and (fboundp form) (fdefinition form)))
                  (t (eval form)))))
    (unless (functionp fn)
      (invalid "~a must be a function, got ~s" what form))
    fn))

(defun build-shape (spec)
  (let* ((name (keyword-of (first (getf spec :name))))
         (families (remove-duplicates
                    (loop for (key) on spec by #'cddr
                          for family = (cdr (assoc key *shape-clauses*))
                          when family collect family))))
    (case (length families)
      (0 (invalid "Shape ~(~a~) has no clauses" name))
      (1 (ecase (first families)
           (:sets (build-sets-shape spec name))
           (:pairs (build-pairs-shape spec name))
           (:each (build-each-shape spec name))
           (:lambda (build-lambda-shape spec name))))
      (t (invalid "Shape ~(~a~) mixes ~{~(~s~)~^ and ~} clauses" name families)))))

(defun build-sets-shape (spec name)
  (unless (clause-present-p spec :sets)
    (invalid "Shape ~(~a~) needs a :sets clause" name))
  (let* ((args (getf spec :sets))
         (options (clause-options (rest args) ":sets" '(:of)))
         (types (keyword-list (or (getf options :of) '(sequence triplet))
                              '(:sequence :triplet) ":sets :of"))
         (pair-args (getf spec :pair)))
    (when (and (clause-present-p spec :pair) (/= 1 (length pair-args)))
      (invalid ":pair takes one count, got ~s" pair-args))
    (make-shape :spec spec :name name :family :sets
                :sets (count-arg (first args) ":sets" 1)
                :pair (if pair-args (count-arg (first pair-args) ":pair" 0) 0)
                :set-types types)))

(defun build-pairs-shape (spec name)
  (let* ((args (getf spec :pairs))
         (options (clause-options (rest args) ":pairs" '(:distinct))))
    (make-shape :spec spec :name name :family :pairs
                :pairs (count-arg (first args) ":pairs" 1)
                :distinct (and (getf options :distinct) t))))

(defun build-each-shape (spec name)
  (unless (clause-present-p spec :each)
    (invalid "Shape ~(~a~) needs an :each clause" name))
  (let* ((classes (keyword-list (getf spec :each) *tile-classes* ":each"))
         (extra-args (getf spec :extra))
         (options (clause-options (rest extra-args) ":extra" '(:of))))
    (make-shape :spec spec :name name :family :each
                :classes classes
                :extra (if extra-args (count-arg (first extra-args) ":extra" 0) 0)
                :extra-classes (if (getf options :of)
                                   (keyword-list (getf options :of) *tile-classes* ":extra :of")
                                   classes))))

(defun build-lambda-shape (spec name)
  (let* ((options (clause-options (getf spec :lambda) ":lambda" '(:complete :shanten)))
         (complete (getf options :complete))
         (shanten (getf options :shanten)))
    (unless complete
      (invalid "Shape ~(~a~) needs :lambda :complete" name))
    (make-shape :spec spec :name name :family :lambda
                :complete-fn (resolve-function complete ":complete")
                :shanten-fn (and shanten (resolve-function shanten ":shanten")))))
