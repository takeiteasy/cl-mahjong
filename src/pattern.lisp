(in-package #:cl-mahjong)

(defparameter *context-flags*
  '((:tsumo . win-context-tsumo)
    (:riichi . win-context-riichi)
    (:double-riichi . win-context-double-riichi)
    (:ippatsu . win-context-ippatsu)
    (:last-tile . win-context-last-tile)
    (:rinshan . win-context-rinshan)
    (:chankan . win-context-chankan)
    (:tenhou . win-context-tenhou)
    (:chiihou . win-context-chiihou)
    (:renhou . win-context-renhou)
    (:open-riichi . win-context-open-riichi)
    (:paarenchan . win-context-paarenchan)))

(defparameter *group-types* '(:sequence :triplet :quad :pair :single))

;;; Selectors: predicates over (hand kind)

(defun seat-wind-p (hand kind)
  (eq (kind-name kind) (win-context-seat-wind (win-hand-context hand))))

(defun round-wind-p (hand kind)
  (eq (kind-name kind) (win-context-round-wind (win-hand-context hand))))

(defun seat-bonus-p (hand kind)
  "A bonus kind whose rank is the seat's number, east being 1."
  (let* ((tileset (win-hand-tileset hand))
         (winds (loop for other across (tileset-kinds tileset)
                      when (eq (kind-group other) :wind) collect (kind-name other)))
         (seat (position (win-context-seat-wind (win-hand-context hand)) winds)))
    (and seat
         (eq (kind-category kind) :bonus)
         (eql (kind-rank kind) (1+ seat)))))

(defun bonus-count (hand selector)
  "How many of the set-aside bonus tiles match SELECTOR."
  (let ((tileset (win-hand-tileset hand)))
    (count-if (lambda (tile) (funcall selector hand (tile-kind tileset tile)))
              (win-context-bonus (win-hand-context hand)))))

(defun compile-selector (form)
  "FORM is a symbol, a list of them meaning any, or (and ...) meaning every one."
  (if (consp form)
      (let* ((every (eq :and (keyword-of (first form))))
             (selectors (mapcar #'compile-selector (if every (rest form) form))))
        (lambda (hand kind)
          (if every
              (every (lambda (selector) (funcall selector hand kind)) selectors)
              (some (lambda (selector) (funcall selector hand kind)) selectors))))
      (let ((key (keyword-of form)))
        (case key
          (:simple (lambda (hand kind)
                     (and (eq (kind-category kind) :suit)
                          (not (terminal-p (win-hand-tileset hand) kind)))))
          (:terminal (lambda (hand kind) (terminal-p (win-hand-tileset hand) kind)))
          (:honor (lambda (hand kind) (declare (ignore hand)) (kind-honor-p kind)))
          (:suit (lambda (hand kind) (declare (ignore hand)) (eq (kind-category kind) :suit)))
          (:wind (lambda (hand kind) (declare (ignore hand)) (eq (kind-group kind) :wind)))
          (:dragon (lambda (hand kind) (declare (ignore hand)) (eq (kind-group kind) :dragon)))
          (:seat-bonus #'seat-bonus-p)
          (:seat-wind #'seat-wind-p)
          (:round-wind #'round-wind-p)
          (:value (lambda (hand kind)
                    (or (eq (kind-group kind) :dragon)
                        (seat-wind-p hand kind)
                        (round-wind-p hand kind))))
          ;; TODO: a misspelt name never matches; checking names against the tileset is tracked in https://github.com/takeiteasy/cl-mahjong/issues/7
          (t (lambda (hand kind)
               (declare (ignore hand))
               (or (eq (kind-suit kind) key) (eq (kind-name kind) key)
                   (eq (kind-group kind) key))))))))

;;; Hand accessors

(defun hand-kind-ids (hand)
  "Kind ids of every tile in the hand, melds included."
  (let ((ids (loop for count across (win-hand-counts hand)
                   for id from 0
                   when (plusp count) collect id)))
    (dolist (meld (win-hand-melds hand) ids)
      (dolist (id (meld-kinds meld))
        (pushnew id ids)))))

(defun hand-groups (hand)
  (decomposition-groups (win-hand-decomposition hand)))

(defun kind-of-id (hand id)
  (tileset-kind (win-hand-tileset hand) id))

(defun group-start (group)
  (reduce #'min (group-kinds group)))

(defun tileset-suits (tileset)
  (remove-duplicates (loop for kind across (tileset-kinds tileset)
                           when (eq (kind-category kind) :suit) collect (kind-suit kind))))

(defun group-starts (hand types)
  "(suit . rank) of the lowest tile of every group of TYPES."
  (loop for group in (hand-groups hand)
        when (member (group-type group) types)
          collect (let ((kind (kind-of-id hand (group-start group))))
                    (cons (kind-suit kind) (kind-rank kind)))))

;;; Pattern arguments

(defun check-arity (form minimum maximum)
  (let ((count (length (rest form))))
    (unless (and (>= count minimum) (or (null maximum) (<= count maximum)))
      (invalid "Pattern ~s takes ~a~@[ to ~a~] arguments" form minimum maximum))))

(defun keyword-args (args what)
  (unless (and args (every #'symbolp args))
    (invalid "~a needs one or more names, got ~s" what args))
  (mapcar #'keyword-of args))

(defun group-types-arg (form)
  (let ((names (keyword-list (if (listp form) form (list form))
                             (cons :any *group-types*) "Group types")))
    (if (member :any names) *group-types* names)))

(defun compile-group-test (form args)
  "Predicate (hand group) from group arguments: TYPES [SELECTOR] [:concealed t]."
  (when (null args)
    (invalid "Pattern ~s needs group types" form))
  (destructuring-bind (types &rest more) args
    (let* ((selector (unless (keywordp (first more)) (pop more)))
           (options (clause-options more (string (first form)) '(:concealed)))
           (types (group-types-arg types))
           (selector (and selector (compile-selector selector)))
           (concealed (getf options :concealed)))
      (lambda (hand group)
        (and (member (group-type group) types)
             (or (not concealed) (group-concealed-p hand group))
             (or (null selector)
                 (some (lambda (id) (funcall selector hand (kind-of-id hand id)))
                       (group-kinds group))))))))

(defun count-matching-groups (hand test)
  (count-if (lambda (group) (funcall test hand group)) (hand-groups hand)))

;;; Pattern compiler

(defun twin-sequence-pairs (hand)
  (let ((sequences (loop for group in (hand-groups hand)
                         when (and (eq (group-type group) :sequence)
                                   (group-concealed-p hand group))
                           collect (group-kinds group))))
    (loop for kinds in (remove-duplicates sequences :test #'equal)
          sum (floor (count kinds sequences :test #'equal) 2))))

(defun full-straight-p (hand)
  (let ((starts (group-starts hand '(:sequence)))
        (low (car (tileset-ranks (win-hand-tileset hand)))))
    (some (lambda (suit)
            (every (lambda (rank) (member (cons suit rank) starts :test #'equal))
                   (list low (+ low 3) (+ low 6))))
          (tileset-suits (win-hand-tileset hand)))))

(defun across-suits-p (hand types)
  (let ((starts (group-starts hand types))
        (suits (tileset-suits (win-hand-tileset hand))))
    (some (lambda (rank)
            (every (lambda (suit) (member (cons suit rank) starts :test #'equal)) suits))
          (remove-duplicates (mapcar #'cdr starts)))))

(defun one-suit-p (hand)
  (let ((suits (remove-duplicates
                (loop for id in (hand-kind-ids hand)
                      for kind = (kind-of-id hand id)
                      when (eq (kind-category kind) :suit) collect (kind-suit kind)))))
    (= 1 (length suits))))

(defun compile-patterns (forms)
  (mapcar #'compile-pattern forms))

(defun compile-pattern (form)
  "Compile FORM into a closure over a WIN-HAND."
  (cond
    ((eq form t) (constantly t))
    ((not (and (consp form) (symbolp (first form))))
     (invalid "A pattern is a list starting with a name, got ~s" form))
    (t (compile-pattern-form (keyword-of (first form)) form (rest form)))))

(defun compile-pattern-form (op form args)
  (case op
     (:and (let ((patterns (compile-patterns args)))
             (lambda (hand) (every (lambda (pattern) (funcall pattern hand)) patterns))))
     (:or (let ((patterns (compile-patterns args)))
            (lambda (hand) (some (lambda (pattern) (funcall pattern hand)) patterns))))
     (:not (check-arity form 1 1)
      (let ((pattern (compile-pattern (first args))))
        (lambda (hand) (not (funcall pattern hand)))))
     (:shape (let ((names (keyword-args args "shape")))
               (lambda (hand)
                 (and (member (decomposition-shape (win-hand-decomposition hand)) names) t))))
     (:closed (check-arity form 0 0)
      #'win-hand-closed-p)
     (:context (check-arity form 1 1)
      (let ((reader (cdr (assoc (keyword-of (first args)) *context-flags*))))
        (unless reader
          (invalid "Unknown context flag ~s" (first args)))
        (lambda (hand) (funcall reader (win-hand-context hand)))))
     (:option (check-arity form 1 2)
      (let ((key (keyword-of (first args))))
        (if (rest args)
            (let ((value (second args)))
              (lambda (hand) (eql (getf (win-hand-options hand) key) value)))
            (lambda (hand) (getf (win-hand-options hand) key)))))
     (:every-tile (check-arity form 1 1)
      (let ((selector (compile-selector (first args))))
        (lambda (hand)
          (every (lambda (id) (funcall selector hand (kind-of-id hand id)))
                 (hand-kind-ids hand)))))
     (:some-tile (check-arity form 1 1)
      (let ((selector (compile-selector (first args))))
        (lambda (hand)
          (some (lambda (id) (funcall selector hand (kind-of-id hand id)))
                (hand-kind-ids hand)))))
     (:bonus (check-arity form 1 2)
      (let ((needed (count-arg (first args) "bonus" 0))
            (selector (if (rest args) (compile-selector (second args)) (constantly t))))
        (lambda (hand) (>= (bonus-count hand selector) needed))))
     (:one-suit (check-arity form 0 0)
      #'one-suit-p)
     (:every-group (let ((test (compile-group-test form args)))
                     (lambda (hand)
                       (let ((groups (hand-groups hand)))
                         (and groups
                              (= (length groups) (count-matching-groups hand test)))))))
     (:some-group (let ((test (compile-group-test form args)))
                    (lambda (hand) (plusp (count-matching-groups hand test)))))
     (:count-groups (unless (and args (integerp (first args)))
                      (invalid "Pattern ~s needs a group count" form))
      (let ((test (compile-group-test form (rest args)))
            (needed (first args)))
        (lambda (hand) (>= (count-matching-groups hand test) needed))))
     (:pair (check-arity form 1 1)
      (let ((selector (compile-selector (first args))))
        (lambda (hand)
          (some (lambda (group)
                  (and (eq (group-type group) :pair)
                       (funcall selector hand (kind-of-id hand (first (group-kinds group))))))
                (hand-groups hand)))))
     (:wait (let ((types (keyword-args args "wait")))
              (lambda (hand) (and (member (win-hand-wait hand) types) t))))
     (:across-suits (check-arity form 1 1)
      (let ((types (group-types-arg (first args))))
        (lambda (hand) (across-suits-p hand types))))
     (:full-straight (check-arity form 0 0)
      #'full-straight-p)
     (:twin-sequences (check-arity form 1 1)
      (let ((needed (count-arg (first args) "twin-sequences" 1)))
        (lambda (hand) (>= (twin-sequence-pairs hand) needed))))
     (:lambda (unless (and args (listp (first args)))
                (invalid "Pattern ~s needs a parameter list" form))
      (resolve-function (cons 'lambda args) ":lambda pattern"))
     (t (invalid "Unknown pattern ~s" form))))
