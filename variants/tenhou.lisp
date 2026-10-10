(in-package #:cl-mahjong)

;; Tenhou ranked hanchan: composite yakuman stack, a single hand never scores double.
(defvariant tenhou :extends riichi
  (:set :double-yakuman nil :uma (20 10 -10 -20)))
