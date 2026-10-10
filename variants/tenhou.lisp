(in-package #:cl-mahjong)

;; Tenhou ranked hanchan: composite yakuman stack, a single hand never scores double.
(defvariant tenhou :extends riichi
  (:set :double-yakuman nil :uma (20 10 -10 -20)
        :tenpai-yame t :kan-dora :on-discard))

;; Until January 2022 Tenhou rounded each seat's final score to whole thousands.
(defvariant tenhou-2021 :extends tenhou
  (:set :score-rounding :half-away))

;; Tenhou sanma hanchan: the tenhou rules on the sanma deltas.
(defvariant tenhou-sanma :extends sanma
  (:set :double-yakuman nil :uma (20 0 -20)
        :tenpai-yame t :kan-dora :on-discard))
