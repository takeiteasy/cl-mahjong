;; cl-mahjong-tests.asd

(asdf:defsystem #:cl-mahjong-tests
  :description "Test suite for cl-mahjong"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-mahjong
               #:fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "test-yakuman")
                             (:file "test-scoring")
                             (:file "test-integration")
                             (:file "test-calls"))))
  :perform (asdf:test-op (o c)
                         (symbol-call :fiveam :run!
                                      (find-symbol* :cl-mahjong-tests
                                                    :cl-mahjong-tests))))
