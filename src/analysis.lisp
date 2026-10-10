(in-package #:cl-mahjong)

(defstruct (group (:constructor make-group (type kinds &optional open-p)))
  type kinds open-p)

(defstruct (decomposition (:constructor make-decomposition (shape groups)))
  shape groups)

;; prev/next/next2: kind id one rank down, one/two ranks up in the same suit, or NIL.
(defstruct (analysis (:constructor make-analysis (tileset prev next next2)))
  tileset prev next next2)

(defun analysis-for (tileset)
  (let* ((size (tileset-kind-count tileset))
         (prev (make-array size :initial-element nil))
         (next (make-array size :initial-element nil))
         (next2 (make-array size :initial-element nil)))
    (loop for kind across (tileset-kinds tileset)
          when (eq (kind-category kind) :suit)
            do (let* ((suit (kind-suit kind))
                      (rank (kind-rank kind))
                      (one (find-kind tileset suit (1+ rank)))
                      (two (and one (find-kind tileset suit (+ rank 2)))))
                 (setf (svref next (kind-id kind)) (and one (kind-id one))
                       (svref next2 (kind-id kind)) (and two (kind-id two)))
                 (when one
                   (setf (svref prev (kind-id one)) (kind-id kind)))))
    (make-analysis tileset prev next next2)))

(defun resolve-shapes (shapes)
  (mapcar (lambda (shape) (if (shape-p shape) shape (find-shape shape))) shapes))

(defun copy-counts (counts)
  (replace (make-array (length counts) :element-type 'fixnum) counts))

(defun hand-size (counts melds)
  (+ (reduce #'+ counts) (* 3 (length melds))))

(defun held-count (counts melds id)
  "Tiles of kind ID in COUNTS and MELDS together."
  (+ (aref counts id) (loop for meld in melds sum (count id (meld-kinds meld)))))

(defvar *check-copies* t
  "Whether a hand holding more copies of a kind than the tileset has is invalid.")

(defun check-copies (tileset counts melds)
  (loop for id below (length counts)
        for held = (held-count counts melds id)
        for kind = (tileset-kind tileset id)
        when (and *check-copies* (> held (kind-copies kind)))
          do (bad-hand "A hand holds ~a ~(~a~), the tileset has ~a" held (kind-name kind)
                       (kind-copies kind))))

(defun check-hand (analysis counts melds)
  "Signal INVALID-HAND unless the hand has 3n+1 or 3n+2 tiles within the tileset's copies; returns the size."
  (unless (and (vectorp counts)
               (= (length counts) (tileset-kind-count (analysis-tileset analysis))))
    (bad-hand "Counts must be a vector with one entry per tile kind"))
  (let ((size (hand-size counts melds)))
    (unless (member (mod size 3) '(1 2))
      (bad-hand "A hand has 3n+1 or 3n+2 tiles, got ~a" size))
    (check-copies (analysis-tileset analysis) counts melds)
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
        ;; The lowest remaining kind always starts its group. Groups starting at the same kind are
        ;; taken triplet, sequence, pair (FLOOR is the rank of the last), so each split is found once.
        (labels ((walk (i sets pairs trail floor)
                   (let ((start i))
                     (loop while (and (< i end) (zerop (aref counts i))) do (incf i))
                     (unless (= i start)
                       (setf floor 0)))
                   (if (= i end)
                       (push (make-decomposition name (append melded (reverse trail))) results)
                       (let ((one (svref next i))
                             (two (svref next2 i)))
                         (when (and (member :triplet types) (>= (aref counts i) 3) (plusp sets)
                                    (<= floor 0))
                           (without (i i i)
                             (walk i (1- sets) pairs (cons (make-group :triplet (list i i i)) trail)
                                   0)))
                         (when (and (member :sequence types) two (plusp sets)
                                    (plusp (aref counts one)) (plusp (aref counts two))
                                    (<= floor 1))
                           (without (i one two)
                             (walk i (1- sets) pairs
                                   (cons (make-group :sequence (list i one two)) trail) 1)))
                         (when (and (>= (aref counts i) 2) (plusp pairs))
                           (without (i i)
                             (walk i sets (1- pairs) (cons (make-group :pair (list i i)) trail)
                                   2)))))))
          (walk 0 needed (shape-pair shape) '() 0))
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

(defvar *remaining* nil
  "Copies of each kind not yet in the hand or seen, or NIL when shanten ignores them.")

(defun live-p (id)
  (or (null *remaining*) (and id (plusp (aref *remaining* id)))))

(defun remaining-copies (tileset counts melds visible)
  (let ((remaining (make-array (length counts) :element-type 'fixnum)))
    (dotimes (id (length counts) remaining)
      (setf (aref remaining id)
            (- (kind-copies (tileset-kind tileset id)) (held-count counts melds id)
               (if visible (aref visible id) 0))))))

;; A lone tile can start a group from a copy of its own kind or of a neighbour that would make a
;; sequence partial; only own-kind copies start a head. Entries are (set-kinds . head-kinds).
(defun seed-table (analysis)
  (let ((prev (analysis-prev analysis))
        (next (analysis-next analysis))
        (next2 (analysis-next2 analysis))
        (table (make-array (tileset-kind-count (analysis-tileset analysis)))))
    (dotimes (id (length table) table)
      (let ((own (and (live-p id) (list id)))
            (near (remove-if-not #'live-p
                                 (list (svref prev id)
                                       (and (svref prev id) (svref prev (svref prev id)))
                                       (svref next id) (svref next2 id)))))
        (setf (svref table id) (cons (append own near) own))))))

(defun note-wait (waits &rest ids)
  "WAITS with the live kinds of IDS added as one partial's waits, when copies are tracked."
  (if *remaining*
      (cons (remove-if-not #'live-p ids) waits)
      waits))

(defun share-copies (copies open-sets open-heads waits seeds table bound)
  "Most the partials (WAITS, worth 2) and lone tiles (SEEDS, worth 1) can fill of the open set and
head slots when each takes its own copy from COPIES; stops early at BOUND."
  (let ((best 0))
    (block search
      (labels ((score (gain)
                 (when (> gain best)
                   (setf best gain)
                   (when (>= best bound) (return-from search))))
               (claim (id fn)
                 (when (plusp (aref copies id))
                   (decf (aref copies id))
                   (funcall fn)
                   (incf (aref copies id))))
               (seed (rest sets heads gain)
                 (score gain)
                 (when (and rest (or (plusp sets) (plusp heads)))
                   (destructuring-bind (set-kinds . head-kinds) (svref table (car rest))
                     (when (plusp sets)
                       (dolist (id set-kinds)
                         (claim id (lambda () (seed (cdr rest) (1- sets) heads (1+ gain))))))
                     (when (plusp heads)
                       (dolist (id head-kinds)
                         (claim id (lambda () (seed (cdr rest) sets (1- heads) (1+ gain))))))
                     (seed (cdr rest) sets heads gain))))
               (partial (rest sets gain)
                 (if (and rest (plusp sets))
                     (progn
                       (dolist (id (car rest))
                         (claim id (lambda () (partial (cdr rest) (1- sets) (+ gain 2)))))
                       (partial (cdr rest) sets gain))
                     (seed seeds sets open-heads gain))))
        (partial waits open-sets 0)))
    best))

(defun live-leaf (copies needed sets heads head waits seeds table)
  "Open slots left after the groups taken, less what the partials and lone tiles can fill from COPIES."
  (let* ((open-sets (- needed sets))
         (open-heads (- heads head))
         (partials (min (length waits) open-sets))
         (set-seeds (count-if (lambda (id) (car (svref table id))) seeds))
         (head-seeds (count-if (lambda (id) (cdr (svref table id))) seeds))
         (rest-sets (- open-sets partials))
         (bound (+ (* 2 partials)
                   (min (+ rest-sets open-heads) set-seeds
                        (+ rest-sets (min open-heads head-seeds))))))
    (- (+ open-sets open-heads)
       (share-copies (replace copies *remaining*) open-sets open-heads waits seeds table bound))))

(defun sets-shanten (analysis shape counts needed)
  ;; TODO: generic block search is exponential in the worst case; a table-driven fast path for the standard shape is tracked in https://github.com/takeiteasy/cl-mahjong/issues/6
  (let* ((counts (copy-counts counts))
         (prev (analysis-prev analysis))
         (next (analysis-next analysis))
         (next2 (analysis-next2 analysis))
         (end (length counts))
         (sequences (member :sequence (shape-set-types shape)))
         (triplets (member :triplet (shape-set-types shape)))
         (heads (shape-pair shape))
         (target (+ (* 2 needed) heads -1))
         (best target)
         (table (and *remaining* (seed-table analysis)))
         (copies (and *remaining* (copy-counts *remaining*))))
    (labels ((leaf (sets partials head waits seeds)
               (setf best (min best
                               (if *remaining*
                                   (+ target (* -2 sets) (- head)
                                      (live-leaf copies needed sets heads head waits seeds table))
                                   (- target (+ (* 2 sets) (min partials (- needed sets)) head))))))
             (walk (i sets partials head waits seeds)
               (loop while (and (< i end) (zerop (aref counts i))) do (incf i))
               (if (= i end)
                   (leaf sets partials head waits seeds)
                   (let ((count (aref counts i))
                         (one (svref next i))
                         (two (svref next2 i)))
                     (when (and triplets (>= count 3) (< sets needed))
                       (without (i i i) (walk i (1+ sets) partials head waits seeds)))
                     (when (and sequences two (< sets needed)
                                (plusp (aref counts one)) (plusp (aref counts two)))
                       (without (i one two) (walk i (1+ sets) partials head waits seeds)))
                     (when (and (>= count 2) (< head heads))
                       (without (i i) (walk i sets partials (1+ head) waits seeds)))
                     (when (< (+ sets partials) needed)
                       (when (and triplets (>= count 2) (live-p i))
                         (without (i i) (walk i sets (1+ partials) head (note-wait waits i) seeds)))
                       (when (and sequences one (plusp (aref counts one))
                                  (or (live-p (svref prev i)) (live-p (svref next2 i))))
                         (without (i one)
                           (walk i sets (1+ partials) head
                                 (note-wait waits (svref prev i) (svref next2 i)) seeds)))
                       (when (and sequences two (plusp (aref counts two)) (live-p one))
                         (without (i two)
                           (walk i sets (1+ partials) head (note-wait waits one) seeds))))
                     (without (i)
                       (walk i sets partials head waits
                             (if (and table (car (svref table i))) (cons i seeds) seeds)))))))
      (walk 0 0 0 0 '() '())
      best)))

(defun pairs-possible-p (analysis shape counts)
  "Whether enough kinds keep two copies reachable to make the pairs of SHAPE."
  (or (null *remaining*)
      (not (shape-distinct shape))
      (<= (shape-pairs shape)
          (loop for kind across (tileset-kinds (analysis-tileset analysis))
                for id = (kind-id kind)
                count (and (not (eq (kind-category kind) :bonus))
                           (>= (+ (aref counts id) (aref *remaining* id)) 2))))))

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
    (unless (some (lambda (id) (and (zerop (aref counts id)) (not (live-p id)))) required)
      (- (+ (length required) (shape-extra shape) -1) present (min (shape-extra shape) spare)))))

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

(defun live-tenpai-p (analysis shape counts melds)
  "True when a kind with copies left completes the 3n+1 COUNTS."
  (loop for kind across (tileset-kinds (analysis-tileset analysis))
        for id = (kind-id kind)
        thereis (and (not (eq (kind-category kind) :bonus))
                     (plusp (aref *remaining* id))
                     (progn (incf (aref counts id))
                            (prog1 (decompose-shape analysis shape counts melds)
                              (decf (aref counts id)))))))

(defun live-shanten (analysis shape counts melds value)
  "VALUE, with a tenpai whose every wait is out of copies raised to 1."
  (cond ((or (null *remaining*) (null value) (/= value 0)) value)
        ((if (= 1 (mod (hand-size counts melds) 3))
             (live-tenpai-p analysis shape counts melds)
             (loop for id below (length counts)
                   thereis (and (plusp (aref counts id))
                                (without (id) (live-tenpai-p analysis shape counts melds)))))
         0)
        (t 1)))

(defun shape-shanten (analysis shape counts melds)
  "Shanten of one shape, or NIL when the shape cannot apply to the hand."
  (live-shanten
   analysis shape counts melds
   (ecase (shape-family shape)
     (:sets (let ((needed (sets-needed shape melds)))
              (and needed (sets-shanten analysis shape counts needed))))
     (:pairs (and (null melds) (pairs-possible-p analysis shape counts)
                  (pairs-shanten shape counts)))
     (:each (and (null melds) (each-shanten analysis shape counts)))
     (:lambda (lambda-shanten analysis shape counts melds)))))

(defun best-shanten (analysis shapes counts melds)
  (let (best name)
    (dolist (shape shapes (values best name))
      (let ((value (shape-shanten analysis shape counts melds)))
        (when (and value (or (null best) (< value best)))
          (setf best value
                name (shape-name shape)))))))

(defun shanten (tileset shapes counts melds &key live visible)
  "(values shanten shape-name) for the closest shape; -1 is a complete hand.
NIL when no shape applies. With LIVE or VISIBLE, a kind-count vector of other tiles seen, only
kinds with copies left in the tileset count as partial groups or waits."
  (let ((analysis (analysis-for tileset)))
    (check-hand analysis counts melds)
    (let ((*remaining* (and (or live visible) (remaining-copies tileset counts melds visible))))
      (best-shanten analysis (resolve-shapes shapes) counts melds))))

(defun ukeire (tileset shapes counts melds &key live visible)
  "For a 3n+1 hand, (kind . remaining) for each kind that lowers the shanten.
Remaining copies are the kind's copies minus those in the hand, its melds and VISIBLE, a kind-count
vector. With LIVE or VISIBLE the shanten is live, and a draw only counts if it lowers that."
  (let ((analysis (analysis-for tileset))
        (shapes (resolve-shapes shapes)))
    (unless (= 1 (mod (check-hand analysis counts melds) 3))
      (bad-hand "Ukeire needs a hand of 3n+1 tiles"))
    (let* ((counts (copy-counts counts))
           (remaining (remaining-copies tileset counts melds visible))
           (*remaining* (and (or live visible) remaining))
           (current (best-shanten analysis shapes counts melds)))
      (when current
        (loop for kind across (tileset-kinds tileset)
              for id = (kind-id kind)
              when (and (not (eq (kind-category kind) :bonus))
                        (plusp (aref remaining id))
                        (let ((new (progn (incf (aref counts id))
                                          (when *remaining* (decf (aref *remaining* id)))
                                          (prog1 (best-shanten analysis shapes counts melds)
                                            (decf (aref counts id))
                                            (when *remaining* (incf (aref *remaining* id)))))))
                          (and new (< new current))))
                collect (cons kind (aref remaining id)))))))
