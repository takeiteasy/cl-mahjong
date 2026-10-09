(in-package #:cl-mahjong)

(defstruct (group (:constructor make-group (type kinds &optional open-p)))
  type kinds open-p)

(defstruct (decomposition (:constructor make-decomposition (shape groups)))
  shape groups)

;; next/next2: kind id one/two ranks up in the same suit, or NIL.
(defstruct (analysis (:constructor make-analysis (tileset next next2)))
  tileset next next2)

(defun analysis-for (tileset)
  (let* ((size (tileset-kind-count tileset))
         (next (make-array size :initial-element nil))
         (next2 (make-array size :initial-element nil)))
    (loop for kind across (tileset-kinds tileset)
          when (eq (kind-category kind) :suit)
            do (let* ((suit (kind-suit kind))
                      (rank (kind-rank kind))
                      (one (find-kind tileset suit (1+ rank)))
                      (two (and one (find-kind tileset suit (+ rank 2)))))
                 (setf (svref next (kind-id kind)) (and one (kind-id one))
                       (svref next2 (kind-id kind)) (and two (kind-id two)))))
    (make-analysis tileset next next2)))

(defun resolve-shapes (shapes)
  (mapcar (lambda (shape) (if (shape-p shape) shape (find-shape shape))) shapes))

(defun copy-counts (counts)
  (replace (make-array (length counts) :element-type 'fixnum) counts))

(defun hand-size (counts melds)
  (+ (reduce #'+ counts) (* 3 (length melds))))

(defun check-hand (analysis counts melds)
  "Signal INVALID-HAND unless the hand has 3n+1 or 3n+2 tiles; returns the size."
  (unless (and (vectorp counts)
               (= (length counts) (tileset-kind-count (analysis-tileset analysis))))
    (bad-hand "Counts must be a vector with one entry per tile kind"))
  (let ((size (hand-size counts melds)))
    (unless (member (mod size 3) '(1 2))
      (bad-hand "A hand has 3n+1 or 3n+2 tiles, got ~a" size))
    size))

(defun kind-in-class-p (tileset kind class)
  (ecase class
    (:honor (kind-honor-p kind))
    (:terminal (terminal-p tileset kind))
    (:simple (and (eq (kind-category kind) :suit) (not (terminal-p tileset kind))))))

(defun class-kind-ids (tileset classes)
  (loop for kind across (tileset-kinds tileset)
        when (some (lambda (class) (kind-in-class-p tileset kind class)) classes)
          collect (kind-id kind)))

(defun meld-set-type (meld)
  (if (eq (meld-type meld) :sequence) :sequence :triplet))

(defun meld-group (meld)
  (make-group (meld-type meld) (meld-kinds meld) (meld-open-p meld)))

(defun sets-needed (shape melds)
  "Sets still to build from concealed tiles, or NIL when MELDS don't fit SHAPE."
  (when (and (<= (length melds) (shape-sets shape))
             (every (lambda (meld) (member (meld-set-type meld) (shape-set-types shape)))
                    melds))
    (- (shape-sets shape) (length melds))))

(defmacro without ((&rest ids) &body body)
  "Run BODY with one tile of each of IDS taken out of COUNTS."
  `(progn ,@(mapcar (lambda (id) `(decf (aref counts ,id))) ids)
          (prog1 (progn ,@body)
            ,@(mapcar (lambda (id) `(incf (aref counts ,id))) ids))))

;;; Decomposition

(defun decompose-sets (analysis shape counts melds)
  (let ((needed (sets-needed shape melds)))
    (when (and needed
               (= (reduce #'+ counts) (+ (* 3 needed) (* 2 (shape-pair shape)))))
      (let* ((counts (copy-counts counts))
             (next (analysis-next analysis))
             (next2 (analysis-next2 analysis))
             (end (length counts))
             (types (shape-set-types shape))
             (name (shape-name shape))
             (melded (mapcar #'meld-group melds))
             (results '()))
        ;; The lowest remaining kind always starts its group, so each split is found once.
        (labels ((walk (i sets pairs trail)
                   (loop while (and (< i end) (zerop (aref counts i))) do (incf i))
                   (if (= i end)
                       (push (make-decomposition name (append melded (reverse trail))) results)
                       (let ((one (svref next i))
                             (two (svref next2 i)))
                         (when (and (member :triplet types) (>= (aref counts i) 3) (plusp sets))
                           (without (i i i)
                             (walk i (1- sets) pairs (cons (make-group :triplet (list i i i)) trail))))
                         (when (and (member :sequence types) two (plusp sets)
                                    (plusp (aref counts one)) (plusp (aref counts two)))
                           (without (i one two)
                             (walk i (1- sets) pairs
                                   (cons (make-group :sequence (list i one two)) trail))))
                         (when (and (>= (aref counts i) 2) (plusp pairs))
                           (without (i i)
                             (walk i sets (1- pairs) (cons (make-group :pair (list i i)) trail))))))))
          (walk 0 needed (shape-pair shape) '()))
        (nreverse results)))))

(defun decompose-pairs (shape counts melds)
  (when (null melds)
    (let ((distinct (shape-distinct shape))
          (groups '()))
      (loop for count across counts
            for id from 0
            do (unless (and (evenp count) (or (not distinct) (<= count 2)))
                 (return-from decompose-pairs nil))
               (dotimes (n (floor count 2))
                 (push (make-group :pair (list id id)) groups)))
      (when (= (length groups) (shape-pairs shape))
        (list (make-decomposition (shape-name shape) (nreverse groups)))))))

(defun decompose-each (analysis shape counts melds)
  (when (null melds)
    (let* ((tileset (analysis-tileset analysis))
           (required (class-kind-ids tileset (shape-classes shape)))
           (extras (class-kind-ids tileset (shape-extra-classes shape))))
      (when (and (= (reduce #'+ counts) (+ (length required) (shape-extra shape)))
                 (every (lambda (id) (plusp (aref counts id))) required)
                 (loop for count across counts
                       for id from 0
                       always (or (member id extras)
                                  (<= count (if (member id required) 1 0)))))
        (list (make-decomposition
               (shape-name shape)
               (loop for count across counts
                     for id from 0
                     when (plusp count)
                       collect (make-group (ecase count (1 :single) (2 :pair) (3 :triplet) (4 :quad))
                                           (make-list count :initial-element id)))))))))

(defun lambda-complete-p (analysis shape counts melds)
  (and (funcall (shape-complete-fn shape) (analysis-tileset analysis) counts melds) t))

(defun decompose-shape (analysis shape counts melds)
  (ecase (shape-family shape)
    (:sets (decompose-sets analysis shape counts melds))
    (:pairs (decompose-pairs shape counts melds))
    (:each (decompose-each analysis shape counts melds))
    (:lambda (when (lambda-complete-p analysis shape counts melds)
               (list (make-decomposition (shape-name shape) '()))))))

(defun decompose (tileset shapes counts melds)
  "Every way COUNTS and MELDS split into the groups of one of SHAPES."
  (let ((analysis (analysis-for tileset)))
    (check-hand analysis counts melds)
    (loop for shape in (resolve-shapes shapes)
          append (decompose-shape analysis shape counts melds))))

(defun complete-p (tileset shapes counts melds)
  (and (decompose tileset shapes counts melds) t))

;;; Shanten

(defun sets-shanten (analysis shape counts needed)
  ;; TODO: generic block search is exponential in the worst case; a table-driven fast path for the standard shape is tracked in #15
  (let* ((counts (copy-counts counts))
         (next (analysis-next analysis))
         (next2 (analysis-next2 analysis))
         (end (length counts))
         (sequences (member :sequence (shape-set-types shape)))
         (triplets (member :triplet (shape-set-types shape)))
         (heads (shape-pair shape))
         (target (+ (* 2 needed) heads -1))
         (best target))
    (labels ((leaf (sets partials head)
               (setf best (min best (- target (+ (* 2 sets)
                                                 (min partials (- needed sets))
                                                 head)))))
             (walk (i sets partials head)
               (loop while (and (< i end) (zerop (aref counts i))) do (incf i))
               (if (= i end)
                   (leaf sets partials head)
                   (let ((count (aref counts i))
                         (one (svref next i))
                         (two (svref next2 i)))
                     (when (and triplets (>= count 3) (< sets needed))
                       (without (i i i) (walk i (1+ sets) partials head)))
                     (when (and sequences two (< sets needed)
                                (plusp (aref counts one)) (plusp (aref counts two)))
                       (without (i one two) (walk i (1+ sets) partials head)))
                     (when (and (>= count 2) (< head heads))
                       (without (i i) (walk i sets partials (1+ head))))
                     (when (< (+ sets partials) needed)
                       (when (and triplets (>= count 2))
                         (without (i i) (walk i sets (1+ partials) head)))
                       (when (and sequences one (plusp (aref counts one)))
                         (without (i one) (walk i sets (1+ partials) head)))
                       (when (and sequences two (plusp (aref counts two)))
                         (without (i two) (walk i sets (1+ partials) head))))
                     (without (i) (walk i sets partials head))))))
      (walk 0 0 0 0)
      best)))

(defun pairs-shanten (shape counts)
  (let ((target (shape-pairs shape))
        (distinct (shape-distinct shape))
        (pairs 0)
        (kinds 0))
    (loop for count across counts
          do (when (plusp count) (incf kinds))
             (incf pairs (if distinct (if (>= count 2) 1 0) (floor count 2))))
    (+ (- target 1 (min pairs target))
       (if distinct (max 0 (- target kinds)) 0))))

(defun each-shanten (analysis shape counts)
  (let* ((tileset (analysis-tileset analysis))
         (required (class-kind-ids tileset (shape-classes shape)))
         (extras (class-kind-ids tileset (shape-extra-classes shape)))
         (present (count-if (lambda (id) (plusp (aref counts id))) required))
         (spare (loop for id in extras
                      sum (max 0 (- (aref counts id) (if (member id required) 1 0))))))
    (- (+ (length required) (shape-extra shape) -1) present (min (shape-extra shape) spare))))

(defun tenpai-by-adding-p (analysis shape counts melds)
  "True when adding one tile of some kind completes the lambda SHAPE."
  (loop for kind across (tileset-kinds (analysis-tileset analysis))
        for id = (kind-id kind)
        thereis (and (not (eq (kind-category kind) :bonus))
                     (progn (incf (aref counts id))
                            (prog1 (lambda-complete-p analysis shape counts melds)
                              (decf (aref counts id)))))))

(defun lambda-shanten (analysis shape counts melds)
  "Without :shanten a lambda shape reports -1, 0 or NIL (unknown)."
  (let ((custom (shape-shanten-fn shape))
        (counts (copy-counts counts)))
    (cond (custom (funcall custom (analysis-tileset analysis) counts melds))
          ((= 1 (mod (hand-size counts melds) 3))
           (and (tenpai-by-adding-p analysis shape counts melds) 0))
          ((lambda-complete-p analysis shape counts melds) -1)
          ((loop for id below (length counts)
                 thereis (and (plusp (aref counts id))
                              (without (id)
                                (tenpai-by-adding-p analysis shape counts melds))))
           0))))

(defun shape-shanten (analysis shape counts melds)
  "Shanten of one shape, or NIL when the shape cannot apply to the hand."
  (ecase (shape-family shape)
    (:sets (let ((needed (sets-needed shape melds)))
             (and needed (sets-shanten analysis shape counts needed))))
    (:pairs (and (null melds) (pairs-shanten shape counts)))
    (:each (and (null melds) (each-shanten analysis shape counts)))
    (:lambda (lambda-shanten analysis shape counts melds))))

(defun best-shanten (analysis shapes counts melds)
  (let (best name)
    (dolist (shape shapes (values best name))
      (let ((value (shape-shanten analysis shape counts melds)))
        (when (and value (or (null best) (< value best)))
          (setf best value
                name (shape-name shape)))))))

(defun shanten (tileset shapes counts melds)
  "(values shanten shape-name) for the closest shape; -1 is a complete hand.
NIL when no shape applies."
  (let ((analysis (analysis-for tileset)))
    (check-hand analysis counts melds)
    (best-shanten analysis (resolve-shapes shapes) counts melds)))

(defun ukeire (tileset shapes counts melds &key visible)
  "For a 3n+1 hand, (kind . remaining) for each kind that lowers the shanten.
Remaining copies are the kind's copies minus those in the hand and in VISIBLE, a kind-count vector."
  (let ((analysis (analysis-for tileset))
        (shapes (resolve-shapes shapes)))
    (unless (= 1 (mod (check-hand analysis counts melds) 3))
      (bad-hand "Ukeire needs a hand of 3n+1 tiles"))
    (let ((counts (copy-counts counts))
          (current (best-shanten analysis shapes counts melds)))
      (when current
        (loop for kind across (tileset-kinds tileset)
              for id = (kind-id kind)
              for remaining = (- (kind-copies kind) (aref counts id)
                                 (if visible (aref visible id) 0))
              when (and (not (eq (kind-category kind) :bonus))
                        (plusp remaining)
                        (let ((new (progn (incf (aref counts id))
                                          (prog1 (best-shanten analysis shapes counts melds)
                                            (decf (aref counts id))))))
                          (and new (< new current))))
                collect (cons kind remaining))))))
