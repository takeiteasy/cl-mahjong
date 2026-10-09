(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test splitmix64-reference-value
  (is (= #xE220A8397B1DCDAF (cl-mahjong:rng-next (cl-mahjong:make-rng 0)))))

(test rng-below-stays-in-range
  (let ((rng (cl-mahjong:make-rng 7)))
    (is (loop repeat 1000 always (< -1 (cl-mahjong:rng-below rng 5) 5)))))

(test shuffle-is-a-seeded-permutation
  (let* ((source (coerce (loop for i below 50 collect i) 'vector))
         (a (cl-mahjong:shuffle-vector source (cl-mahjong:make-rng 1)))
         (b (cl-mahjong:shuffle-vector source (cl-mahjong:make-rng 1)))
         (c (cl-mahjong:shuffle-vector source (cl-mahjong:make-rng 2))))
    (is (equalp a b))
    (is (not (equalp a c)))
    (is (equalp source (sort (copy-seq a) #'<)))))
