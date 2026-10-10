(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defparameter *riichi-shapes* '(standard seven-pairs kokushi))

(defun notation-entries (notation)
  "(suit-letter rank red-p) for each tile of NOTATION; 0 is a red five."
  (let ((digits '())
        (entries '()))
    (loop for char across notation
          do (cond ((digit-char-p char) (push (digit-char-p char) digits))
                   ((char= char #\Space))
                   (t (dolist (digit (nreverse digits))
                        (push (list char (if (zerop digit) 5 digit) (zerop digit)) entries))
                      (setf digits '()))))
    (nreverse entries)))

(defun entry-kind-id (tileset letter rank)
  (cl-mahjong:kind-id
   (if (char= letter #\z)
       (cl-mahjong:find-kind tileset (nth (1- rank) '(east south west north white green red)))
       (cl-mahjong:find-kind tileset (ecase letter (#\m 'man) (#\p 'pin) (#\s 'sou)) rank))))

(defun hand (tileset notation)
  "Kind counts from notation such as \"123m456p11z\" (z is east south west north white green red)."
  (let ((counts (make-array (cl-mahjong:tileset-kind-count tileset)
                            :element-type 'fixnum :initial-element 0)))
    (loop for (letter rank) in (notation-entries notation)
          do (incf (aref counts (entry-kind-id tileset letter rank))))
    counts))

(defun kind-of (tileset name &optional rank)
  (cl-mahjong:kind-id (cl-mahjong:find-kind tileset name rank)))

(defun shanten-of (tileset shapes notation &optional melds)
  (cl-mahjong:shanten tileset shapes (hand tileset notation) melds))

(defun group-summary (decomposition)
  (mapcar #'cl-mahjong:group-type (cl-mahjong:decomposition-groups decomposition)))

(defun decompositions-of (tileset shape notation &optional melds)
  (cl-mahjong:decompose tileset (list shape) (hand tileset notation) melds))

(defmacro with-riichi ((ts) &body body)
  `(let ((,ts (cl-mahjong:find-tileset 'riichi-136)))
     ,@body))

;;; Shanten

(test standard-shanten
  (with-riichi (ts)
    (is (= -1 (shanten-of ts '(standard) "123m456m789m123p11p")))
    (is (= 0 (shanten-of ts '(standard) "123m456m789m123p1p")))
    (is (= 0 (shanten-of ts '(standard) "123m456m789m12p55p")))
    (is (= 1 (shanten-of ts '(standard) "123m456m135p77s99s")))
    (is (= 1 (shanten-of ts '(standard) "123m456p12s45s77z1z")))
    (is (= 2 (shanten-of ts '(standard) "123m456p12s4s8s77z1z")))
    (is (= 4 (shanten-of ts '(standard) "123m456p1s4s7s1234z")))))

(test seven-pairs-shanten
  (with-riichi (ts)
    (is (= 0 (shanten-of ts '(seven-pairs) "1122m3344p5566s7z")))
    (is (= -1 (shanten-of ts '(seven-pairs) "1122m3344p5566s77z")))
    (is (= 1 (shanten-of ts '(seven-pairs) "1122m3344p55s6s7s1z")))
    (is (= 4 (shanten-of ts '(seven-pairs) "111222333444m5p")))))

(test seven-pairs-does-not-count-a-quad-as-two
  (with-riichi (ts)
    (let ((counts (hand ts "1111m2233p4455s66z")))
      (is (= 1 (cl-mahjong:shanten ts '(seven-pairs) counts '())))
      (is (not (cl-mahjong:complete-p ts '(seven-pairs) counts '()))))))

(test kokushi-shanten
  (with-riichi (ts)
    (is (= 0 (shanten-of ts '(kokushi) "19m19p19s1234567z")))
    (is (= -1 (shanten-of ts '(kokushi) "19m19p19s11234567z")))
    (is (= 1 (shanten-of ts '(kokushi) "19m19p19s1122345z")))
    (is (= 7 (shanten-of ts '(kokushi) "19m23456p15s5z111z")))))

(test shanten-reports-the-closest-shape
  (with-riichi (ts)
    (multiple-value-bind (value name)
        (cl-mahjong:shanten ts *riichi-shapes* (hand ts "1122m3344p5566s7z") '())
      (is (= 0 value))
      (is (eq :seven-pairs name)))
    (multiple-value-bind (value name)
        (cl-mahjong:shanten ts *riichi-shapes* (hand ts "19m19p19s1234567z") '())
      (is (= 0 value))
      (is (eq :kokushi name)))))

(test hand-size-is-checked
  (with-riichi (ts)
    (signals cl-mahjong:invalid-hand (shanten-of ts '(standard) "123m456m"))
    (signals cl-mahjong:invalid-hand
      (cl-mahjong:shanten ts '(standard) (make-array 3) '()))))

(test hand-copies-are-checked
  (with-riichi (ts)
    (signals cl-mahjong:invalid-hand (shanten-of ts '(standard) "11111m234p567s11z"))
    (signals cl-mahjong:invalid-hand
      (shanten-of ts '(standard) "1111m234p567s1z" (list (meld ts :triplet "111m"))))
    (finishes (shanten-of ts '(standard) "1111m234p567s111z"))))

;;; Sanma

(test sanma-sequences-follow-rank-not-id
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (is (not (cl-mahjong:complete-p ts '(standard) (hand ts "19m1p456p789p123s11z") '())))
    (is (cl-mahjong:complete-p ts '(standard) (hand ts "111m999m456p789p11p") '()))))

(test sanma-kokushi
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (is (= -1 (shanten-of ts '(kokushi) "19m19p19s11234567z")))
    (is (= 0 (shanten-of ts '(kokushi) "19m19p19s1234567z")))))

;;; Decomposition

(test decomposition-enumerates-every-split
  (with-riichi (ts)
    (let ((results (decompositions-of ts 'standard "111222333m456p11p")))
      (is (= 2 (length results)))
      (is (member '(:sequence :sequence :sequence :pair :sequence)
                  (mapcar #'group-summary results) :test #'equal))
      (is (member '(:triplet :triplet :triplet :pair :sequence)
                  (mapcar #'group-summary results) :test #'equal)))))

(test decomposition-has-no-duplicates
  (with-riichi (ts)
    (let ((results (decompositions-of ts 'standard "112233m112233p77s")))
      (is (= 1 (length results)))
      (is (equal '(:sequence :sequence :sequence :sequence :pair)
                 (group-summary (first results)))))))

(test groups-starting-on-the-same-kind-are-found-once
  (with-riichi (ts)
    (is (= 1 (length (decompositions-of ts 'standard "111123666789s11z"))))
    (is (= 2 (length (decompositions-of ts 'standard "11122233388899m"))))
    (is (= 1 (length (decompositions-of ts 'standard "22234m234s567s666z"))))))

(test ryanpeikou-is-standard-and-seven-pairs
  (with-riichi (ts)
    (let ((results (cl-mahjong:decompose ts *riichi-shapes* (hand ts "112233m112233p77s") '())))
      (is (equal '(:standard :seven-pairs)
                 (mapcar #'cl-mahjong:decomposition-shape results))))))

(test decomposition-of-incomplete-hand-is-empty
  (with-riichi (ts)
    (is (null (cl-mahjong:decompose ts *riichi-shapes* (hand ts "123m456m789m123p1p") '())))
    (is (not (cl-mahjong:complete-p ts *riichi-shapes* (hand ts "123m456m789m123p1p") '())))))

(test kokushi-decomposition
  (with-riichi (ts)
    (let ((results (decompositions-of ts 'kokushi "19m19p19s11234567z")))
      (is (= 1 (length results)))
      (is (= 12 (count :single (group-summary (first results)))))
      (is (equal '(:pair) (remove :single (group-summary (first results))))))))

;;; Melds

(test open-pon-reduces-the-sets-needed
  (with-riichi (ts)
    (let* ((east (kind-of ts 'east))
           (pon (cl-mahjong:make-meld :triplet (list east east east) :open t)))
      (is (= -1 (shanten-of ts '(standard) "123m456m789m11p" (list pon))))
      (is (= 1 (length (decompositions-of ts 'standard "123m456m789m11p" (list pon)))))
      (is (not (cl-mahjong:hand-closed-p (list pon)))))))

(test melds-appear-in-decompositions
  (with-riichi (ts)
    (let* ((east (kind-of ts 'east))
           (pon (cl-mahjong:make-meld :triplet (list east east east) :open t))
           (groups (cl-mahjong:decomposition-groups
                    (first (decompositions-of ts 'standard "123m456m789m11p" (list pon))))))
      (is (equal (list east east east) (cl-mahjong:group-kinds (first groups))))
      (is (cl-mahjong:group-open-p (first groups)))
      (is (not (cl-mahjong:group-open-p (second groups)))))))

(test melds-rule-out-other-shapes
  (with-riichi (ts)
    (let* ((east (kind-of ts 'east))
           (pon (cl-mahjong:make-meld :triplet (list east east east) :open t))
           (counts (hand ts "123m456m789m11p")))
      (is (null (cl-mahjong:shanten ts '(seven-pairs) counts (list pon))))
      (is (null (cl-mahjong:shanten ts '(kokushi) counts (list pon))))
      (is (eq :standard (nth-value 1 (cl-mahjong:shanten ts *riichi-shapes* counts (list pon))))))))

(test closed-kan-stays-closed
  (with-riichi (ts)
    (let* ((east (kind-of ts 'east))
           (kan (cl-mahjong:make-meld :quad (list east east east east))))
      (is (cl-mahjong:hand-closed-p (list kan)))
      (is (= -1 (shanten-of ts '(standard) "123m456m789m11p" (list kan)))))))

(test sequence-meld-must-be-allowed-by-the-shape
  (with-riichi (ts)
    (cl-mahjong:defshape anl-triplets-only :extends standard (:sets 4 :of (triplet)))
    (let ((chi (cl-mahjong:make-meld :sequence (list (kind-of ts 'man 1) (kind-of ts 'man 2)
                                                     (kind-of ts 'man 3))
                                     :open t)))
      (is (null (cl-mahjong:shanten ts '(anl-triplets-only) (hand ts "111p222p333p1z") (list chi))))
      (is (= 0 (cl-mahjong:shanten ts '(standard) (hand ts "111p222p333p1z") (list chi)))))))

;;; Live shanten

(test live-shanten-discounts-tiles-held-in-full
  (with-riichi (ts)
    (let ((counts (hand ts "123456789p1111z")))
      (is (= 0 (cl-mahjong:shanten ts '(standard) counts '())))
      (is (= 1 (cl-mahjong:shanten ts '(standard) counts '() :live t))))))

(test live-shanten-counts-tiles-in-melds
  (with-riichi (ts)
    (let ((pon (cl-mahjong:make-meld :triplet (list (kind-of ts 'east) (kind-of ts 'east)
                                                    (kind-of ts 'east))
                                     :open t)))
      (is (= 0 (shanten-of ts '(standard) "123m456m789m1z" (list pon))))
      (is (= 1 (cl-mahjong:shanten ts '(standard) (hand ts "123m456m789m1z") (list pon) :live t))))))

(test live-shanten-visible-tiles-turn-live-mode-on
  (with-riichi (ts)
    (let ((counts (hand ts "123m456m789m11p23s")))
      (is (= 0 (cl-mahjong:shanten ts '(standard) counts '())))
      (is (= 0 (cl-mahjong:shanten ts '(standard) counts '() :live t)))
      (is (= 0 (cl-mahjong:shanten ts '(standard) counts '() :visible (hand ts "1s4s"))))
      (is (= 1 (cl-mahjong:shanten ts '(standard) counts '() :visible (hand ts "1111s4444s")))))))

(test live-shanten-seven-pairs-needs-a-live-wait
  (with-riichi (ts)
    (let ((counts (hand ts "1122m3344p5566s7z")))
      (is (= 0 (cl-mahjong:shanten ts '(seven-pairs) counts '() :live t)))
      (is (= 1 (cl-mahjong:shanten ts '(seven-pairs) counts '() :visible (hand ts "777z")))))))

(test live-shanten-kokushi-needs-every-kind-reachable
  (with-riichi (ts)
    (let ((counts (hand ts "19m19p19s1123456z")))
      (is (= 0 (cl-mahjong:shanten ts '(kokushi) counts '() :live t)))
      (is (null (cl-mahjong:shanten ts '(kokushi) counts '() :visible (hand ts "7777z")))))))

(test live-shanten-sanma-nuki-count-as-seen
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (is (= 0 (cl-mahjong:shanten ts '(standard) (hand ts "123p456p789p123s4z") '() :live t)))
    (is (= 1 (cl-mahjong:shanten ts '(standard) (hand ts "123p456p789p123s4z") '()
                                 :visible (hand ts "444z"))))))

;;; Ukeire

(test ukeire-counts-remaining-copies
  (with-riichi (ts)
    (let* ((counts (hand ts "123m456m789m12p55p"))
           (result (cl-mahjong:ukeire ts '(standard) counts '())))
      (is (equal '((:pin-3 . 4))
                 (mapcar (lambda (entry) (cons (cl-mahjong:kind-name (car entry)) (cdr entry)))
                         result))))))

(test ukeire-subtracts-visible-tiles
  (with-riichi (ts)
    (let* ((counts (hand ts "123m456m789m12p55p"))
           (visible (hand ts "33p"))
           (result (cl-mahjong:ukeire ts '(standard) counts '() :visible visible)))
      (is (equal '(2) (mapcar #'cdr result))))
    (let* ((counts (hand ts "123m456m789m12p55p"))
           (visible (hand ts "3333p")))
      (is (not (member :pin-3 (cl-mahjong:ukeire ts '(standard) counts '() :visible visible)
                       :key (lambda (entry) (cl-mahjong:kind-name (car entry)))))))))

(test ukeire-visible-drops-draws-that-only-complete-a-dead-partial
  (with-riichi (ts)
    (let* ((counts (hand ts "123m456m789p1m55s9s"))
           (names (lambda (visible)
                    (mapcar (lambda (entry) (cl-mahjong:kind-name (car entry)))
                            (cl-mahjong:ukeire ts '(standard) counts '() :visible visible)))))
      (is (member :man-2 (funcall names (hand ts "3m"))))
      (is (not (member :man-2 (funcall names (hand ts "333m"))))))))

(test ukeire-live-needs-no-visible-tiles
  (with-riichi (ts)
    (let ((counts (hand ts "123m456m789m12p55s")))
      (is (equal (cl-mahjong:ukeire ts '(standard) counts '())
                 (cl-mahjong:ukeire ts '(standard) counts '() :live t))))))

(test ukeire-counts-tiles-in-melds
  (with-riichi (ts)
    (let ((chi (cl-mahjong:make-meld :sequence (list (kind-of ts 'pin 4) (kind-of ts 'pin 5)
                                                     (kind-of ts 'pin 6))
                                     :open t)))
      (is (equal '(3) (mapcar #'cdr (cl-mahjong:ukeire ts '(standard) (hand ts "123m789m46p55s")
                                                       (list chi))))))))

(test ukeire-skips-kinds-that-do-not-lower-shanten
  (with-riichi (ts)
    (let ((names (mapcar (lambda (entry) (cl-mahjong:kind-name (car entry)))
                         (cl-mahjong:ukeire ts '(standard) (hand ts "123m456m789m13p55p") '()))))
      (is (equal '(:pin-2) names)))))

(test ukeire-needs-a-3n+1-hand
  (with-riichi (ts)
    (signals cl-mahjong:invalid-hand
      (cl-mahjong:ukeire ts '(standard) (hand ts "123m456m789m123p11p") '()))))

;;; Lambda shapes

(test lambda-shape-without-shanten
  (with-riichi (ts)
    (cl-mahjong:defshape anl-all-man
      (:lambda :complete (lambda (tileset counts melds)
                           (declare (ignore melds))
                           (loop for count across counts
                                 for id from 0
                                 always (or (zerop count)
                                            (eq :man (cl-mahjong:kind-suit
                                                      (cl-mahjong:tileset-kind tileset id))))))))
    (is (= -1 (shanten-of ts '(anl-all-man) "1112345678999m1m")))
    (is (= 0 (shanten-of ts '(anl-all-man) "1112345678999m")))
    (is (= 0 (shanten-of ts '(anl-all-man) "1112345678999m1p")))
    (is (null (shanten-of ts '(anl-all-man) "1112345678999p1z")))
    (is (= 1 (length (decompositions-of ts 'anl-all-man "1112345678999m1m"))))))

(test lambda-shape-with-shanten
  (with-riichi (ts)
    (cl-mahjong:defshape anl-fixed
      (:lambda :complete (lambda (tileset counts melds)
                           (declare (ignore tileset counts melds))
                           nil)
               :shanten (lambda (tileset counts melds)
                          (declare (ignore tileset counts melds))
                          7)))
    (is (= 7 (shanten-of ts '(anl-fixed) "123m456m789m123p1p")))
    (is (= 7 (nth-value 0 (shanten-of ts '(anl-fixed standard) "1m4m7m1p4p7p1s4s7s1z2z3z4z"))))))
