;;;; cl-mahjong.asd

(asdf:defsystem "cl-mahjong"
  :description "Mahjong rules engine where every variant is defined through a DSL"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "src/registry")
               (:file "src/random")
               (:file "src/tileset")
               (:file "src/wall")
               (:file "src/hand")
               (:file "src/shape")
               (:file "src/analysis")
               (:file "src/win")
               (:file "src/pattern")
               (:file "src/yaku")
               (:file "src/scoring")
               (:file "src/variant")
               (:file "src/game")
               (:file "src/play")
               (:file "variants/riichi")
               (:file "variants/sanma")
               (:file "variants/tenhou")
               (:file "variants/events"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-mahjong/tests"))))

(asdf:defsystem "cl-mahjong/tests"
  :depends-on ("cl-mahjong" "fiveam")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/smoke")
               (:file "tests/registry")
               (:file "tests/random")
               (:file "tests/tileset")
               (:file "tests/wall")
               (:file "tests/shape")
               (:file "tests/analysis")
               (:file "tests/win")
               (:file "tests/yaku")
               (:file "tests/scoring")
               (:file "tests/variant")
               (:file "tests/game")
               (:file "tests/sanma")
               (:file "tests/events")
               (:file "tests/tenhou")
               (:file "tests/corpus")
               (:file "tests/replay"))
  :perform (asdf:test-op (op component)
             (declare (ignore op component))
             (unless (uiop:symbol-call :cl-mahjong/tests :run-tests)
               (error "cl-mahjong tests failed"))))
