(in-package #:cl-mahjong)

(defstruct scoring
  spec name fu limits floors kiriage indicator payments)

(defstruct fu-table
  (base 0) (round 1) (closed-ron 0) (tsumo 0) (open-minimum 0) (concealed 1) (quad 1)
  groups pairs waits flats no-tsumo-fu fn)

;; ron and tsumo are (dealer . other) multipliers of the base points.
(defstruct payment-table
  ron tsumo (round 1) (honba 0) (stick 0))

(defstruct (score (:constructor make-score (reading fu han dora limit base points payments)))
  reading fu han dora limit base points payments)

(defparameter *scoring-clauses* '(:name :fu :limits :floor :kiriage :dora :payments))

(defparameter *scoring-containers* '(:fu :limits :dora :payments))

(defmacro defscoring (name &body body)
  (multiple-value-bind (parent clauses) (parse-definition-body body)
    `(register-definition :scoring ',name ',parent ',(cons (list :name name) clauses))))

(defun find-scoring (name)
  (find-definition :scoring name))

(defmethod compile-definition ((kind (eql :scoring)) parent clauses)
  (build-scoring (merge-scoring-spec (and parent (scoring-spec parent)) clauses)))

;;; Spec merging

(defun scoring-entries (clauses)
  "Flatten CLAUSES into entries (TOP . ARGS); a container clause gives one entry per sub-clause."
  (loop for clause in clauses
        do (unless (and (consp clause) (member (car clause) *scoring-clauses*))
             (invalid "Unknown scoring clause ~s" clause))
        append (if (member (car clause) *scoring-containers*)
                   (mapcar (lambda (entry)
                             (unless (consp entry)
                               (invalid "~s takes lists, got ~s" (car clause) entry))
                             (cons (car clause) entry))
                           (cdr clause))
                   (list clause))))

(defun key-form (form)
  (cond ((and form (symbolp form)) (keyword-of form))
        ((consp form) (mapcar #'key-form form))
        (t form)))

(defun entry-key (entry)
  "Entries with the same key replace each other when a scoring extends another."
  (key-form (if (and (eq (first entry) :fu) (member (second entry) '(:group :pair :wait :flat)))
                (butlast entry)
                (case (first entry)
                  ((:fu :limits :dora :payments :floor) (list (first entry) (second entry)))
                  (t (list (first entry)))))))

(defun merge-scoring-spec (parent clauses)
  (let ((entries (copy-list parent)))
    (dolist (entry (scoring-entries clauses) entries)
      (let ((position (position (entry-key entry) entries :key #'entry-key :test #'equal)))
        (if position
            (setf (nth position entries) entry)
            (setf entries (append entries (list entry))))))))

(defun entries-of (spec top)
  (loop for entry in spec when (eq (first entry) top) collect (rest entry)))

;;; Building

(defun amount-arg (args what minimum)
  (unless (= 1 (length args))
    (invalid "~a takes one number, got ~s" what args))
  (count-arg (first args) what minimum))

(defun names-arg (form what)
  (keyword-args (if (listp form) form (list form)) what))

(defun build-group-rule (args)
  (when (< (length args) 2)
    (invalid ":group needs group types and an amount, got ~s" args))
  (let* ((types (group-types-arg (first args)))
         (types (if (member :triplet types) (adjoin :quad types) types)))
    (cons (compile-group-test '(:group) (cons types (butlast (rest args))))
          (count-arg (car (last args)) ":group" 0))))

(defun build-fu (entries name)
  (when (null entries)
    (invalid "Scoring ~(~a~) needs a :fu clause" name))
  (let ((table (make-fu-table)))
    (dolist (entry entries table)
      (destructuring-bind (key &rest args) entry
        (case key
          (:base (setf (fu-table-base table) (amount-arg args ":base" 0)))
          (:round (setf (fu-table-round table) (amount-arg args ":round" 1)))
          (:closed-ron (setf (fu-table-closed-ron table) (amount-arg args ":closed-ron" 0)))
          (:tsumo (setf (fu-table-tsumo table) (amount-arg args ":tsumo" 0)))
          (:open-minimum (setf (fu-table-open-minimum table) (amount-arg args ":open-minimum" 0)))
          (:concealed (setf (fu-table-concealed table) (amount-arg args ":concealed" 1)))
          (:quad (setf (fu-table-quad table) (amount-arg args ":quad" 1)))
          (:group (push (build-group-rule args) (fu-table-groups table)))
          (:pair (unless (= 2 (length args))
                   (invalid ":pair takes a selector and an amount, got ~s" args))
           (push (cons (compile-selector (first args)) (count-arg (second args) ":pair" 0))
                 (fu-table-pairs table)))
          (:wait (unless (= 2 (length args))
                   (invalid ":wait takes wait names and an amount, got ~s" args))
           (push (cons (names-arg (first args) ":wait") (count-arg (second args) ":wait" 0))
                 (fu-table-waits table)))
          (:flat (unless (= 2 (length args))
                   (invalid ":flat takes a yaku and an amount, got ~s" args))
           (push (cons (keyword-of (first args)) (count-arg (second args) ":flat" 0))
                 (fu-table-flats table)))
          (:no-tsumo-fu (setf (fu-table-no-tsumo-fu table) (names-arg args ":no-tsumo-fu")))
          (:lambda (unless (and args (listp (first args)))
                     (invalid ":fu :lambda needs a parameter list, got ~s" args))
           (setf (fu-table-fn table) (resolve-function (cons 'lambda args) ":fu :lambda")))
          (t (invalid "Unknown :fu entry ~s" entry)))))
    (setf (fu-table-groups table) (nreverse (fu-table-groups table))
          (fu-table-pairs table) (nreverse (fu-table-pairs table))
          (fu-table-waits table) (nreverse (fu-table-waits table))
          (fu-table-flats table) (nreverse (fu-table-flats table)))
    table))

(defun build-limits (entries name)
  "Limits as (name base han) sorted by base."
  (let ((limits (mapcar (lambda (entry)
                          (unless (and (symbolp (first entry)) (rest entry))
                            (invalid ":limits takes (name base [:han n]), got ~s" entry))
                          (let ((options (clause-options (cddr entry) ":limits" '(:han))))
                            (list (keyword-of (first entry))
                                  (count-arg (second entry) ":limits" 1)
                                  (and (getf options :han) (count-arg (getf options :han) ":limits :han" 1)))))
                        entries)))
    (unless (assoc :yakuman limits)
      (invalid "Scoring ~(~a~) needs a yakuman limit" name))
    (stable-sort limits #'< :key #'second)))

(defun build-floors (entries limits)
  (mapcar (lambda (entry)
            (unless (= 2 (length entry))
              (invalid ":floor takes a yaku and a limit, got ~s" entry))
            (let ((limit (keyword-of (second entry))))
              (unless (assoc limit limits)
                (invalid ":floor names an unknown limit ~(~a~)" limit))
              (cons (keyword-of (first entry)) limit)))
          entries))

(defun build-indicator (entries)
  (let ((entry (find :indicator entries :key #'first)))
    (cond ((null entry) #'dora-kind-id)
          ((and (= 2 (length entry)) (eq (keyword-of (second entry)) :next)) #'dora-kind-id)
          (t (resolve-function (second entry) ":dora :indicator")))))

(defun multiplier-p (form)
  (and (integerp form) (plusp form)))

(defun payment-pair (args what valid-other)
  (let ((options (clause-options args what '(:dealer :other))))
    (unless (and (multiplier-p (getf options :dealer)) (funcall valid-other (getf options :other)))
      (invalid "~a needs a :dealer and an :other multiplier, got ~s" what args))
    (cons (getf options :dealer) (getf options :other))))

(defun build-payments (entries name)
  (let ((table (make-payment-table)))
    (dolist (entry entries)
      (destructuring-bind (key &rest args) entry
        (case key
          (:ron (setf (payment-table-ron table) (payment-pair args ":ron" #'multiplier-p)))
          (:tsumo (setf (payment-table-tsumo table)
                        (payment-pair args ":tsumo"
                                      (lambda (other)
                                        (or (multiplier-p other)
                                            (and (listp other) (= 2 (length other))
                                                 (every #'multiplier-p other)))))))
          (:round (setf (payment-table-round table) (amount-arg args ":round" 1)))
          (:honba (setf (payment-table-honba table) (amount-arg args ":honba" 0)))
          (:riichi-stick (setf (payment-table-stick table) (amount-arg args ":riichi-stick" 0)))
          (t (invalid "Unknown :payments entry ~s" entry)))))
    (unless (and (payment-table-ron table) (payment-table-tsumo table))
      (invalid "Scoring ~(~a~) needs :ron and :tsumo payments" name))
    table))

(defun build-scoring (spec)
  (let* ((name (keyword-of (second (assoc :name spec))))
         (kiriage (entries-of spec :kiriage))
         (limits (build-limits (entries-of spec :limits) name)))
    (make-scoring :spec spec :name name
                  :fu (build-fu (entries-of spec :fu) name)
                  :limits limits
                  :floors (build-floors (entries-of spec :floor) limits)
                  :kiriage (and kiriage (amount-arg (first kiriage) ":kiriage" 1))
                  :indicator (build-indicator (entries-of spec :dora))
                  :payments (build-payments (entries-of spec :payments) name))))

(defun resolve-scoring (scoring)
  (if (scoring-p scoring) scoring (find-scoring scoring)))

;;; Fu

(defun round-up (number unit)
  (* unit (ceiling number unit)))

(defun group-fu (table hand group)
  (let ((rule (find-if (lambda (rule) (funcall (car rule) hand group)) (fu-table-groups table))))
    (if rule
        (* (cdr rule)
           (if (group-concealed-p hand group) (fu-table-concealed table) 1)
           (if (eq (group-type group) :quad) (fu-table-quad table) 1))
        0)))

(defun pair-fu (table hand)
  (let ((total (loop for group in (hand-groups hand)
                     when (eq (group-type group) :pair)
                       sum (loop with kind = (kind-of-id hand (first (group-kinds group)))
                                 for (selector . fu) in (fu-table-pairs table)
                                 when (funcall selector hand kind) sum fu)))
        (cap (getf (win-hand-options hand) :double-wind-fu)))
    (if cap (min total cap) total)))

(defun wait-fu (table hand)
  (let ((rule (find-if (lambda (rule) (member (win-hand-wait hand) (car rule)))
                       (fu-table-waits table))))
    (if rule (cdr rule) 0)))

(defun tsumo-fu (table hand names)
  (let ((context (win-hand-context hand)))
    (if (and (win-context-tsumo context)
             (notany (lambda (name) (member name names)) (fu-table-no-tsumo-fu table))
             (or (not (win-context-rinshan context))
                 (getf (win-hand-options hand) :rinshan-tsumo-fu)))
        (fu-table-tsumo table)
        0)))

(defun reading-fu (table reading)
  (let* ((hand (reading-hand reading))
         (names (mapcar #'car (reading-yaku reading)))
         (flat (find-if (lambda (flat) (member (car flat) names)) (fu-table-flats table)))
         (closed (win-hand-closed-p hand)))
    (cond ((fu-table-fn table) (funcall (fu-table-fn table) hand reading))
          (flat (cdr flat))
          (t (let ((fu (round-up (+ (fu-table-base table)
                                    (if (and closed (not (win-context-tsumo (win-hand-context hand))))
                                        (fu-table-closed-ron table)
                                        0)
                                    (tsumo-fu table hand names)
                                    (if (win-hand-win-group hand)
                                        (loop for group in (hand-groups hand)
                                              sum (group-fu table hand group))
                                        0)
                                    (pair-fu table hand)
                                    (wait-fu table hand))
                                 (fu-table-round table))))
               (if closed fu (max fu (fu-table-open-minimum table))))))))

;;; Dora

(defun dora-kind-id (tileset kind)
  "Kind id of the dora an indicator of KIND points to: the next kind of its suit or honor group, wrapping round."
  (unless (eq (kind-category kind) :bonus)
    (let* ((family (loop for other across (tileset-kinds tileset)
                         when (if (eq (kind-category kind) :suit)
                                  (eq (kind-suit other) (kind-suit kind))
                                  (eq (kind-group other) (kind-group kind)))
                           collect (kind-id other)))
           (following (rest (member (kind-id kind) family))))
      (first (or following family)))))

(defun hand-kind-count (hand id)
  (+ (aref (win-hand-counts hand) id)
     (loop for meld in (win-hand-melds hand) sum (count id (meld-kinds meld)))))

(defun count-dora (scoring hand indicators)
  (let ((tileset (win-hand-tileset hand)))
    (loop for tile in indicators
          for id = (funcall (scoring-indicator scoring) tileset (tile-kind tileset tile))
          when id sum (hand-kind-count hand id))))

(defun count-aka (hand)
  (let ((tileset (win-hand-tileset hand)))
    (count-if (lambda (tile) (tile-red-p tileset tile))
              (win-context-tiles (win-hand-context hand)))))

;;; Base points

(defun base-points (scoring han fu yakuman options)
  "(values base limit-name)."
  (let* ((limits (scoring-limits scoring))
         (yakuman-limit (assoc :yakuman limits)))
    (if (plusp yakuman)
        (values (* yakuman (second yakuman-limit)) :yakuman)
        (let* ((usable (if (getf options :kazoe-yakuman) limits (remove yakuman-limit limits)))
               (reached (find-if (lambda (limit) (and (third limit) (>= han (third limit))))
                                 usable :from-end t))
               (formula (* fu (expt 2 (+ han 2))))
               (lowest (first limits)))
          (cond (reached (values (second reached) (first reached)))
                ((or (>= formula (second lowest))
                     (and (getf options :kiriage) (eql formula (scoring-kiriage scoring))))
                 (values (second lowest) (first lowest)))
                (t (values formula nil)))))))

(defun apply-floors (scoring names base limit)
  "The higher of the hand's own score and the floors of the yaku it holds."
  (let ((floor (loop for (name . floor-limit) in (scoring-floors scoring)
                     when (member name names)
                       maximize (second (assoc floor-limit (scoring-limits scoring))))))
    (if (and floor (> floor base))
        (values floor (first (find floor (scoring-limits scoring) :key #'second)))
        (values base limit))))

;;; Payments

(defun payments-for (table base dealer tsumo honba players)
  "(values payments total): PAYMENTS is (:ron n) or (:dealer n :other n), :dealer NIL when the winner deals."
  (let ((honba-points (* honba (payment-table-honba table))))
    (flet ((share (multiplier extra)
             (+ (round-up (* base multiplier) (payment-table-round table)) extra)))
      (cond ((not tsumo)
             (let* ((ron (payment-table-ron table))
                    (paid (share (if dealer (car ron) (cdr ron)) honba-points)))
               (values (list :ron paid) paid)))
            (t
             (let ((extra (ceiling honba-points (1- players)))
                   (tsumo (payment-table-tsumo table)))
               (if dealer
                   (let ((paid (share (car tsumo) extra)))
                     (values (list :dealer nil :other paid) (* paid (1- players))))
                   (destructuring-bind (from-dealer from-other) (cdr tsumo)
                     (let ((dealer-paid (share from-dealer extra))
                           (other-paid (share from-other extra)))
                       (values (list :dealer dealer-paid :other other-paid)
                               (+ dealer-paid (* other-paid (- players 2)))))))))))))

;;; Scoring

(defun reading-score (scoring reading &key (honba 0) (riichi-sticks 0) (players 4))
  "A SCORE for READING, or NIL when it has no yaku. Dora never make a yaku."
  (let ((scoring (resolve-scoring scoring)))
    (when (reading-yaku reading)
      (let* ((hand (reading-hand reading))
             (context (win-hand-context hand))
             (options (win-hand-options hand))
             (names (mapcar #'car (reading-yaku reading)))
             (floored (mapcar #'car (scoring-floors scoring)))
             (yakuman (reading-yakuman reading))
             (dora (count-dora scoring hand (win-context-dora-indicators context)))
             (ura (if (or (win-context-riichi context) (win-context-double-riichi context))
                      (count-dora scoring hand (win-context-ura-indicators context))
                      0))
             (aka (count-aka hand))
             (han (if (plusp yakuman)
                      0
                      (+ dora ura aka
                         (loop for (name . value) in (reading-yaku reading)
                               unless (member name floored) sum value))))
             (fu (reading-fu (scoring-fu scoring) reading)))
        (multiple-value-bind (base limit) (base-points scoring han fu yakuman options)
          (multiple-value-bind (base limit) (apply-floors scoring names base limit)
            (multiple-value-bind (payments total)
                (payments-for (scoring-payments scoring) base (win-context-dealer context)
                              (win-context-tsumo context) honba players)
              (make-score reading fu han (list :dora dora :ura ura :aka aka) limit base
                          (+ total (* riichi-sticks (payment-table-stick (scoring-payments scoring))))
                          payments))))))))

(defun evaluate-score (scoring tileset shapes yaku counts melds context
                       &key options (honba 0) (riichi-sticks 0) (players 4))
  "The score of every reading of a winning hand that has a yaku; see `evaluate-yaku' for the arguments.
HONBA and RIICHI-STICKS are counts; PLAYERS decides who pays a tsumo."
  (let ((scoring (resolve-scoring scoring)))
    (loop for reading in (evaluate-yaku tileset shapes yaku counts melds context :options options)
          for score = (reading-score scoring reading :honba honba :riichi-sticks riichi-sticks
                                                     :players players)
          when score collect score)))

(defun score-better-p (a b)
  (loop for (x y) in `((,(score-points a) ,(score-points b))
                       (,(score-han a) ,(score-han b))
                       (,(score-fu a) ,(score-fu b)))
        when (> x y) return t
        when (< x y) return nil))

(defun best-score (scores)
  "The score with the most points, then han, then fu; the first of equals. NIL when SCORES is empty."
  (let (best)
    (dolist (score scores best)
      (when (or (null best) (score-better-p score best))
        (setf best score)))))
