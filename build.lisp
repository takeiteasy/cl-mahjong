;; Build script for cli-mahjong
;; Usage: sbcl --load build.lisp

(require :asdf)
(load "~/.sbclrc")

(format t "~%Loading cli-mahjong system...~%")
(ql:quickload :cl-mahjong/cli :silent t)

(format t "Building executable...~%")
(asdf:make :cl-mahjong/cli)
