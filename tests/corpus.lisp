(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *corpus-file*
    (asdf:system-relative-pathname "cl-mahjong" "tests/corpus/riichi.sexp"))

  (defun read-corpus ()
    (with-open-file (in *corpus-file*)
      (let ((*read-eval* nil)
            (*package* (find-package '#:cl-mahjong/tests)))
        (read in))))

  (defun corpus-file-of (case)
    (subseq (getf case :id) 0 (search "::" (getf case :id)))))

(defparameter *corpus* (read-corpus))

(defparameter *known-failures*
  '(("aotenjou::test_aotenjou_kokushi_tsumo#1" . "upstream drops menzen-tsumo from kokushi")
    ("aotenjou::test_aotenjou_kokushi_with_ura_dora#1" . "upstream drops riichi from kokushi")
    ("hand_dividing::test_fix_not_correct_kan_handling#1"
     . "upstream's hand holds only three of the kan's four tiles, so it finds no win; the hand left here is complete"))
  "Alist of case id to why this implementation deliberately differs.")

;;; Building inputs

(defun corpus-tiles (tileset notation)
  "Distinct tile ids for NOTATION, where 0 is a red five and 5 a plain one."
  (let ((used '()))
    (loop for (letter rank red) in (notation-entries notation)
          for kind = (cl-mahjong:tileset-kind tileset (entry-kind-id tileset letter rank))
          for tiles = (cl-mahjong:tiles-of-kind tileset kind)
          for matching = (remove-if-not (lambda (tile)
                                          (eq red (and (cl-mahjong:tile-red-p tileset tile) t)))
                                        tiles)
          for tile = (or (find-if-not (lambda (tile) (member tile used)) matching)
                         (first matching)
                         (first tiles))
          do (push tile used)
          collect tile)))

(defun corpus-melds (tileset specs)
  (loop for (type notation nil open) in specs
        collect (cl-mahjong:make-meld type (kind-ids tileset notation) :open open)))

(defun corpus-variant (case)
  (cl-mahjong:find-variant (getf case :variant)))

(defun corpus-limits (kazoe cap)
  "The yakuman limit entry for a kazoe mode (:limited, :sanbaiman or :multiple) and a yakuman cap."
  `(:limits (yakuman 8000 ,@(case kazoe
                              (:limited '(:han 13))
                              (:multiple '(:han 13 :per 13)))
                     ,@(and cap (list :max cap)))))

(defun corpus-scoring (case)
  "The scoring of the case's variant with the case's kazoe mode and yakuman cap."
  (let ((base (cl-mahjong:variant-scoring (corpus-variant case)))
        (kazoe (getf case :kazoe))
        (cap (getf case :cap)))
    (if (null kazoe)
        base
        (let ((name (format nil "CORPUS-~a-~a-~a" base kazoe cap)))
          (unless (nth-value 1 (gethash (cl-mahjong::definition-key :scoring name)
                                        cl-mahjong::*definitions*))
            (cl-mahjong::register-definition :scoring name base
                                             (list (list :name name) (corpus-limits kazoe cap))))
          name))))

(defun north-tiles (tileset count)
  (subseq (cl-mahjong:tiles-of-kind tileset (cl-mahjong:find-kind tileset 'north)) 0 count))

(defun hand-notation (case)
  (apply #'concatenate 'string (getf case :hand) (mapcar #'second (getf case :melds))))

(defun corpus-scores (case)
  (let* ((variant (corpus-variant case))
         (tileset (cl-mahjong:variant-tileset variant))
         (context (apply #'cl-mahjong:make-win-context
                         :win-tile (first (corpus-tiles tileset (getf case :win)))
                         :dora-indicators (corpus-tiles tileset (getf case :dora))
                         :ura-indicators (corpus-tiles tileset (getf case :ura))
                         :tiles (and (getf case :aka) (corpus-tiles tileset (hand-notation case)))
                         :nuki (north-tiles tileset (or (getf (getf case :context) :nuki) 0))
                         (loop for (key value) on (getf case :context) by #'cddr
                               unless (eq key :nuki) append (list key value)))))
    (cl-mahjong:evaluate-score (corpus-scoring case) tileset
                               (cl-mahjong:variant-shapes variant) (cl-mahjong:variant-yaku variant)
                               (hand tileset (getf case :hand))
                               (corpus-melds tileset (getf case :melds)) context
                               :options (getf case :options) :honba (getf case :honba)
                               :riichi-sticks (getf case :sticks) :players (getf case :players))))

;;; Comparing results

(defun score-yaku-names (score)
  "Yaku names of SCORE; the house-rules suukantsu-pao is upstream's suukantsu."
  (sort (mapcar (lambda (entry)
                  (let ((name (string-downcase (symbol-name (car entry)))))
                    (if (string= name "suukantsu-pao") "suukantsu" name)))
                (cl-mahjong:reading-yaku (cl-mahjong:score-reading score)))
        #'string<))

(defun differences (&rest triples)
  "The (field expected actual) triples whose expected value is given and differs."
  (loop for (field expected actual) in triples
        unless (or (null expected) (equal expected actual))
          collect (list field expected actual)))

(defun expected-names (names)
  (sort (mapcar (lambda (name) (string-downcase (string name))) names) #'string<))

(defun payment-differences (expect points payments limit)
  (differences (list :points (getf expect :points) points)
               (list :payments (getf expect :payments) payments)
               (list :limit (getf expect :limit) limit)))

(defun check-invalid-hand (case)
  (handler-case (let ((best (cl-mahjong:best-score (corpus-scores case))))
                  (list (list :invalid-hand (getf (getf case :expect) :invalid)
                              (and best (summary best)))))
    (cl-mahjong:invalid-hand () nil)))

(defun check-hand (case)
  (let* ((expect (getf case :expect))
         (best (and (not (getf expect :invalid))
                    (handler-case (cl-mahjong:best-score (corpus-scores case))
                      (cl-mahjong:invalid-hand (condition)
                        (if (getf expect :none) nil (error condition)))))))
    (cond ((getf expect :invalid) (check-invalid-hand case))
          ((getf expect :none) (and best (list (list :score nil (summary best)))))
          ((null best) (list (list :score expect nil)))
          (t (append (differences (list :han (getf expect :han) (cl-mahjong:score-han best))
                                  (list :fu (getf expect :fu) (cl-mahjong:score-fu best))
                                  (list :yaku (expected-names (getf expect :yaku)) (score-yaku-names best)))
                     (payment-differences expect (cl-mahjong:score-points best)
                                          (cl-mahjong:score-payments best)
                                          (cl-mahjong:score-limit best)))))))

(defun check-scores (case)
  (let* ((scoring (cl-mahjong::resolve-scoring (corpus-scoring case)))
         (context (getf case :context))
         (han (getf case :han)))
    (multiple-value-bind (base limit)
        (cl-mahjong::base-points scoring han (getf case :fu)
                                 (if (getf case :yakuman) (max 1 (floor han 13)) 0)
                                 (getf case :options))
      (multiple-value-bind (payments total)
          (cl-mahjong::payments-for (cl-mahjong::scoring-payments scoring) base
                                    (getf context :dealer) (getf context :tsumo)
                                    (getf case :honba) (getf case :players))
        (payment-differences
         (getf case :expect)
         (+ total (* (getf case :sticks)
                     (cl-mahjong::payment-table-stick (cl-mahjong::scoring-payments scoring))))
         payments limit)))))

(defun implied-melds (counts count)
  "COUNT open triplets of kinds absent from COUNTS, standing in for the sets a short hand implies."
  (loop for id below (length counts)
        when (zerop (aref counts id))
          collect (cl-mahjong:make-meld :triplet (list id id id) :open t) into melds
        finally (return (subseq melds 0 count))))

(defun check-shanten (case)
  (let* ((tileset (cl-mahjong:variant-tileset (cl-mahjong:find-variant (if (= (getf case :players) 3) 'sanma 'riichi))))
         (expected (getf case :expect))
         (actual (handler-case (let ((counts (hand tileset (getf case :hand))))
                                 (cl-mahjong:shanten tileset (getf case :shapes) counts
                                                     (implied-melds counts (getf case :pad))
                                                     :live t))
                   (error () :error))))
    (and (not (equal expected actual)) (list (list :shanten expected actual)))))

(defun decompositions-or-none (tileset shapes case)
  "Every split of the case's hand; an invalid hand has none."
  (handler-case (cl-mahjong:decompose tileset shapes (hand tileset (getf case :hand))
                                      (corpus-melds tileset (getf case :melds)))
    (cl-mahjong:invalid-hand () '())))

(defun check-agari (case)
  (let* ((tileset (cl-mahjong:find-tileset 'riichi-136))
         (actual (and (decompositions-or-none tileset *riichi-shapes* case) t)))
    (and (not (eq (getf case :expect) actual)) (list (list :agari (getf case :expect) actual)))))

(defun list< (a b)
  (loop for x in a
        for y in b
        when (< x y) return t
        when (> x y) return nil
        finally (return (< (length a) (length b)))))

(defun canonical-divisions (divisions)
  (sort (mapcar (lambda (blocks)
                  (sort (mapcar (lambda (block) (sort (copy-list block) #'<)) blocks) #'list<))
                divisions)
        #'list< :key (lambda (blocks) (apply #'append blocks))))

(defun check-divide (case)
  (let* ((tileset (cl-mahjong:find-tileset 'riichi-136))
         (actual (canonical-divisions
                  (mapcar (lambda (decomposition)
                            (mapcar #'cl-mahjong:group-kinds
                                    (cl-mahjong:decomposition-groups decomposition)))
                          (decompositions-or-none tileset '(standard seven-pairs) case))))
         (expected (canonical-divisions (getf case :expect))))
    (and (not (equal expected actual)) (list (list :divisions expected actual)))))

(defun check-case (case)
  (handler-case (ecase (getf case :kind)
                  (:hand (check-hand case))
                  (:scores (check-scores case))
                  (:shanten (check-shanten case))
                  (:agari (check-agari case))
                  (:divide (check-divide case)))
    (error (condition) (list (list :error nil (princ-to-string condition))))))

;;; Tests

(defun check-corpus-file (file)
  (let ((failures '())
        (checked 0))
    (dolist (case *corpus*)
      (when (string= file (corpus-file-of case))
        (let ((known (cdr (assoc (getf case :id) *known-failures* :test #'string=))))
          (cond ((getf case :skip) (skip (format nil "~a: ~a" (getf case :id) (getf case :skip))))
                (known (skip (format nil "~a: known difference: ~a" (getf case :id) known)))
                (t (incf checked)
                   (let ((differences (check-case case)))
                     (when differences
                       (push (format nil "~a~{~%    ~s~}" (getf case :id) differences) failures))))))))
    (is (null failures) "~a of ~a cases failed:~{~%  ~a~}"
        (length failures) checked (reverse failures))))

(defmacro define-corpus-tests ()
  `(progn
     ,@(loop for file in (remove-duplicates (mapcar #'corpus-file-of (read-corpus)) :test #'string=)
             collect `(test ,(intern (format nil "CORPUS-~:@(~a~)" file))
                        (check-corpus-file ,file)))))

(define-corpus-tests)
