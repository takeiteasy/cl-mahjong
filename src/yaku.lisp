(in-package #:cl-mahjong)

(defstruct yaku
  spec name
  han open-han yakuman closed-only
  double-when patterns implies excludes)

(defstruct (reading (:constructor make-reading (decomposition win-group wait yaku han yakuman)))
  decomposition win-group wait yaku han yakuman)

(defparameter *yaku-clauses* '(:name :han :yakuman :when :implies :excludes))

(defmacro defyaku (name &body body)
  (multiple-value-bind (parent clauses) (parse-definition-body body)
    `(register-definition :yaku ',name ',parent ',(cons (list :name name) clauses))))

(defun find-yaku (name)
  (find-definition :yaku name))

(defmethod compile-definition ((kind (eql :yaku)) parent clauses)
  (build-yaku (merge-yaku-spec (and parent (yaku-spec parent)) clauses)))

(defun merge-yaku-spec (parent clauses)
  "Child clauses replace the parent's clause with the same key; :han and :yakuman replace each other."
  (dolist (clause clauses)
    (unless (and (consp clause) (member (car clause) *yaku-clauses*))
      (invalid "Unknown yaku clause ~s" clause)))
  (when (and (assoc :han clauses) (assoc :yakuman clauses))
    (invalid "A yaku takes :han or :yakuman, not both"))
  (let ((spec (copy-list parent)))
    (dolist (clause clauses spec)
      (case (car clause)
        (:han (remf spec :yakuman))
        (:yakuman (remf spec :han)))
      (setf (getf spec (car clause)) (cdr clause)))))

(defun name-list (names what)
  (unless (every #'symbolp names)
    (invalid "~a needs yaku names, got ~s" what names))
  (mapcar #'keyword-of names))

(defun build-yaku (spec)
  (let* ((name (keyword-of (first (getf spec :name))))
         (han-args (getf spec :han))
         (yakuman-args (getf spec :yakuman))
         (args (or han-args yakuman-args))
         (options (clause-options (rest args) (if han-args ":han" ":yakuman")
                                  (if han-args
                                      '(:open :closed-only)
                                      '(:closed-only :double-when)))))
    (unless (xor (clause-present-p spec :han) (clause-present-p spec :yakuman))
      (invalid "Yaku ~(~a~) needs exactly one of :han and :yakuman" name))
    (unless (getf spec :when)
      (invalid "Yaku ~(~a~) needs a :when clause" name))
    (let ((value (count-arg (first args) (if han-args ":han" ":yakuman") 1)))
      (make-yaku :spec spec :name name
                 :han (and han-args value)
                 :open-han (and han-args
                                (if (clause-present-p options :open)
                                    (count-arg (getf options :open) ":han :open" 0)
                                    value))
                 :yakuman (and yakuman-args value)
                 :closed-only (and (getf options :closed-only) t)
                 :double-when (and (getf options :double-when)
                                   (compile-pattern (getf options :double-when)))
                 :patterns (compile-patterns (getf spec :when))
                 :implies (name-list (getf spec :implies) ":implies")
                 :excludes (name-list (getf spec :excludes) ":excludes")))))

(defun xor (a b)
  (if a (not b) b))

(defun resolve-yaku (yaku)
  (mapcar (lambda (item) (if (yaku-p item) item (find-yaku item))) yaku))

;;; Evaluation

(defun yaku-value (yaku hand)
  (cond ((yaku-yakuman yaku)
         (if (and (getf (win-hand-options hand) :double-yakuman)
                  (yaku-double-when yaku)
                  (funcall (yaku-double-when yaku) hand))
             (* 2 (yaku-yakuman yaku))
             (yaku-yakuman yaku)))
        ((win-hand-closed-p hand) (yaku-han yaku))
        (t (yaku-open-han yaku))))

(defun award (hand yaku)
  "(yaku . value) when YAKU scores for HAND."
  (when (and (or (win-hand-closed-p hand) (not (yaku-closed-only yaku)))
             (every (lambda (pattern) (funcall pattern hand)) (yaku-patterns yaku)))
    (let ((value (yaku-value yaku hand)))
      (when (plusp value)
        (cons yaku value)))))

(defun drop-implied (awards)
  (let ((implied (loop for (yaku) in awards append (yaku-implies yaku))))
    (remove-if (lambda (award) (member (yaku-name (car award)) implied)) awards)))

(defun drop-excluded (awards)
  "Of two excluding yaku the lower value goes; on a tie the later one."
  (let ((losers '()))
    (loop for (first . rest) on awards
          do (dolist (second rest)
               (when (or (member (yaku-name (car second)) (yaku-excludes (car first)))
                         (member (yaku-name (car first)) (yaku-excludes (car second))))
                 (push (if (< (cdr first) (cdr second)) first second) losers))))
    (remove-if (lambda (award) (member award losers)) awards)))

(defun settle-awards (awards)
  "Yakuman replace every other yaku, then :implies and :excludes drop the rest."
  (let ((yakuman (remove-if-not (lambda (award) (yaku-yakuman (car award))) awards)))
    (drop-excluded (drop-implied (or yakuman awards)))))

(defun read-win-hand (hand yaku)
  (let* ((awards (settle-awards (remove nil (mapcar (lambda (item) (award hand item)) yaku))))
         (yakuman (loop for (item . value) in awards
                        when (yaku-yakuman item) sum value)))
    (make-reading (win-hand-decomposition hand) (win-hand-win-group hand) (win-hand-wait hand)
                  (mapcar (lambda (award) (cons (yaku-name (car award)) (cdr award))) awards)
                  (if (plusp yakuman) 0 (reduce #'+ awards :key #'cdr))
                  yakuman)))

(defun evaluate-yaku (tileset shapes yaku counts melds context &key options)
  "Every reading of a winning hand: each decomposition at each winning-tile placement.
COUNTS include the winning tile named by CONTEXT. A reading's yaku are (name . value) pairs,
value being han, or the yakuman multiple when `reading-yakuman' is positive."
  (let ((shapes (resolve-shapes shapes))
        (yaku (resolve-yaku yaku)))
    (unless (and (win-context-win-tile context)
                 (plusp (aref counts (tile-kind-id tileset (win-context-win-tile context)))))
      (bad-hand "The winning tile must be in the hand"))
    (loop for decomposition in (decompose tileset shapes counts melds)
          for shape = (find (decomposition-shape decomposition) shapes :key #'shape-name)
          append (mapcar (lambda (hand) (read-win-hand hand yaku))
                         (expand-wins tileset shape decomposition counts melds context options)))))

(defun reading-better-p (a b)
  (or (> (reading-yakuman a) (reading-yakuman b))
      (and (= (reading-yakuman a) (reading-yakuman b))
           (> (reading-han a) (reading-han b)))))

(defun best-reading (readings)
  "The reading with the most yakuman, then han; the first of equals."
  ;; TODO: ties are not broken by fu; tracked in #4
  (let (best)
    (dolist (reading readings best)
      (when (or (null best) (reading-better-p reading best))
        (setf best reading)))))
