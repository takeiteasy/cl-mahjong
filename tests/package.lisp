(defpackage #:cl-mahjong/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:cl-mahjong/tests)

(def-suite :cl-mahjong)

(defun run-tests ()
  (run! :cl-mahjong))
