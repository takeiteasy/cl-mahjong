(in-package #:cl-mahjong)

(defstruct (rng (:constructor %make-rng (state)))
  (state 0 :type (unsigned-byte 64)))

(defun make-rng (&optional seed)
  (%make-rng (ldb (byte 64 0) (or seed (random most-positive-fixnum)))))

(defun rng-next (rng)
  "Next 64-bit value of splitmix64."
  (let ((z (setf (rng-state rng)
                 (ldb (byte 64 0) (+ (rng-state rng) #x9E3779B97F4A7C15)))))
    (setf z (ldb (byte 64 0) (* (logxor z (ash z -30)) #xBF58476D1CE4E5B9))
          z (ldb (byte 64 0) (* (logxor z (ash z -27)) #x94D049BB133111EB)))
    (logxor z (ash z -31))))

(defun rng-below (rng n)
  "Unbiased integer in [0, N)."
  (let ((limit (- (ash 1 64) (mod (ash 1 64) n))))
    (loop for x = (rng-next rng)
          when (< x limit) return (mod x n))))

(defun shuffle-vector (vector rng)
  "Fisher-Yates shuffle of a copy of VECTOR."
  (let ((result (copy-seq vector)))
    (loop for i from (1- (length result)) downto 1
          do (rotatef (aref result i) (aref result (rng-below rng (1+ i)))))
    result))
