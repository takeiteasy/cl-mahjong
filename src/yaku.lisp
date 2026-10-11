(in-package #:cl-mahjong)

(defstruct yaku
  spec name
  han open-han yakuman closed-only
  double-when needs-yaku per pao patterns implies excludes option-keys)

;; pao is an alist of (liable-seat . yakuman) for the yakuman the liable seats fed.
(defstruct (reading (:constructor make-reading (decomposition win-group wait yaku han yakuman hand
                                                pao)))
  decomposition win-group wait yaku han yakuman hand pao)

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

(defun yakuman-function (form)
  (unless (eq :lambda (keyword-of (first form)))
    (invalid ":yakuman takes a number or (:lambda (hand) ...), got ~s" form))
  (unless (and (rest form) (listp (second form)))
    (invalid ":yakuman :lambda needs a parameter list, got ~s" form))
  (resolve-function (cons 'lambda (rest form)) ":yakuman :lambda"))

(defun build-yaku (spec)
  (let* ((name (keyword-of (first (getf spec :name))))
         (han-args (getf spec :han))
         (yakuman-args (getf spec :yakuman))
         (args (or han-args yakuman-args))
         (options (clause-options (rest args) (if han-args ":han" ":yakuman")
                                  (if han-args
                                      '(:open :closed-only :needs-yaku :per)
                                      '(:closed-only :double-when :needs-yaku :pao)))))
    (unless (xor (clause-present-p spec :han) (clause-present-p spec :yakuman))
      (invalid "Yaku ~(~a~) needs exactly one of :han and :yakuman" name))
    (unless (getf spec :when)
      (invalid "Yaku ~(~a~) needs a :when clause" name))
    (let* ((*option-keys* (list nil))
           (value (if (and yakuman-args (consp (first args)))
                     (yakuman-function (first args))
                     (count-arg (first args) (if han-args ":han" ":yakuman") 1))))
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
                 :needs-yaku (and (getf options :needs-yaku)
                                  (compile-pattern (getf options :needs-yaku)))
                 :per (and (getf options :per) (compile-counter (getf options :per)))
                 :pao (pao-selector (getf options :pao))
                 :patterns (compile-patterns (getf spec :when))
                 :implies (name-list (getf spec :implies) ":implies")
                 :excludes (name-list (getf spec :excludes) ":excludes")
                 :option-keys (car *option-keys*)))))

(defun pao-selector (form)
  "FORM is a tile selector, or `kan' for the seat that fed the latest daiminkan."
  (cond ((null form) nil)
        ((and (symbolp form) (eq (keyword-of form) :kan)) :kan)
        (t (compile-selector form))))

(defun xor (a b)
  (if a (not b) b))

(defun resolve-yaku (yaku)
  (mapcar (lambda (item) (if (yaku-p item) item (find-yaku item))) yaku))

;;; Evaluation

(defun yaku-value (yaku hand)
  (cond ((yaku-yakuman yaku)
         (let ((multiple (if (functionp (yaku-yakuman yaku))
                             (funcall (yaku-yakuman yaku) hand)
                             (yaku-yakuman yaku))))
           (if (and (getf (win-hand-options hand) :double-yakuman)
                    (yaku-double-when yaku)
                    (funcall (yaku-double-when yaku) hand))
               (* 2 multiple)
               multiple)))
        (t (* (if (win-hand-closed-p hand) (yaku-han yaku) (yaku-open-han yaku))
              (if (yaku-per yaku) (funcall (yaku-per yaku) hand) 1)))))

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

(defun drop-lone-needing (hand awards)
  "Drop each award whose yaku needs another yaku while it is the only award."
  (remove-if (lambda (award)
               (let ((needs (yaku-needs-yaku (car award))))
                 (and needs (funcall needs hand) (null (remove award awards)))))
             awards))

(defun settle-awards (hand awards stack-yakuman)
  "Yakuman replace every other yaku unless STACK-YAKUMAN, then :implies and :excludes drop the rest."
  (let* ((awards (drop-lone-needing hand awards))
         (yakuman (remove-if-not (lambda (award) (yaku-yakuman (car award))) awards)))
    (drop-excluded (drop-implied (if stack-yakuman awards (or yakuman awards))))))

(defun pao-seat (yaku hand)
  (let ((selector (yaku-pao yaku)))
    (cond ((null selector) nil)
          ((eq selector :kan) (win-context-kan-from (win-hand-context hand)))
          (t (selected-pao-seat selector hand)))))

(defun selected-pao-seat (selector hand)
  "The seat that fed the last open meld matching SELECTOR, or NIL.
Every matching set made or completed after that meld must be a kind its caller already held as a
triplet, which the option `pao-held' allows; without it no such set may exist."
  (flet ((matches (group)
           (and (member (group-type group) '(:triplet :quad))
                (funcall selector hand (kind-of-id hand (first (group-kinds group)))))))
    (let* ((melds (win-hand-melds hand))
           (last (find-if (lambda (meld) (and (meld-open-p meld) (matches (meld-group meld))))
                          melds :from-end t)))
      (and last
           (every (lambda (group)
                    (and (getf (win-hand-options hand) :pao-held)
                         (member (first (group-kinds group)) (meld-held last))))
                  (remove-if-not #'matches
                                 (append (mapcar #'meld-group (rest (member last melds)))
                                         (nthcdr (length melds) (hand-groups hand)))))
           (meld-from last)))))

(defun read-win-hand (hand yaku stack-yakuman)
  (let* ((awards (settle-awards hand (remove nil (mapcar (lambda (item) (award hand item)) yaku))
                                stack-yakuman))
         (yakuman (loop for (item . value) in awards
                        when (yaku-yakuman item) sum value)))
    (make-reading (win-hand-decomposition hand) (win-hand-win-group hand) (win-hand-wait hand)
                  (mapcar (lambda (award) (cons (yaku-name (car award)) (cdr award))) awards)
                  (loop for (item . value) in awards
                        unless (yaku-yakuman item) sum value)
                  yakuman
                  hand
                  (loop for (item . value) in awards
                        for seat = (and (yaku-yakuman item) (pao-seat item hand))
                        when seat collect (cons seat value)))))

(defun evaluate-yaku (tileset shapes yaku counts melds context &key options stack-yakuman)
  "Every reading of a winning hand: each decomposition at each winning-tile placement.
COUNTS include the winning tile named by CONTEXT. A reading's yaku are (name . value) pairs,
value being han, or the yakuman multiple when `reading-yakuman' is positive.
STACK-YAKUMAN keeps the other yaku of a yakuman reading, and `reading-han' counts them."
  (evaluate-yaku-as context tileset shapes yaku counts melds context
                    :options options :stack-yakuman stack-yakuman))

(defun evaluate-yaku-as (read-context tileset shapes yaku counts melds context
                         &key options stack-yakuman)
  "`evaluate-yaku' for CONTEXT, with the hand read under READ-CONTEXT once CONTEXT is checked."
  (let ((shapes (resolve-shapes shapes))
        (yaku (resolve-yaku yaku)))
    (unless (and (win-context-win-tile context)
                 (plusp (aref counts (tile-kind-id tileset (win-context-win-tile context)))))
      (bad-hand "The winning tile must be in the hand"))
    (check-win-context context melds)
    (loop for decomposition in (decompose tileset shapes counts melds)
          for shape = (find (decomposition-shape decomposition) shapes :key #'shape-name)
          append (mapcar (lambda (hand) (read-win-hand hand yaku stack-yakuman))
                         (expand-wins tileset shape decomposition counts melds read-context
                                      options)))))
