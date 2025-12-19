;; t/package.lisp

(defpackage #:cl-mahjong/test
  (:use #:cl #:cl-mahjong #:fiveam)
  (:export #:cl-mahjong-tests
           #:yakuman-tests
           #:scoring-tests
           #:integration-tests
           #:call-mechanics-tests
           #:sanma-tests))

(in-package #:cl-mahjong/test)

;; Define main test suite
(def-suite cl-mahjong-tests
  :description "Master test suite for cl-mahjong")

;; Define sub-suites
(def-suite yakuman-tests
  :in cl-mahjong-tests
  :description "Tests for yakuman detection")

(def-suite scoring-tests
  :in cl-mahjong-tests
  :description "Tests for scoring and fu calculation")

(def-suite integration-tests
  :in cl-mahjong-tests
  :description "Tests for game integration (furiten, payments, events)")

(def-suite call-mechanics-tests
  :in cl-mahjong-tests
  :description "Tests for call mechanics (pon, chi, kan, pass)")

(def-suite sanma-tests
  :in cl-mahjong-tests
  :description "Tests for 3-player (sanma) mahjong")
