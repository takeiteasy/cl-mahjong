(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test tenhou-scores-a-single-hand-yakuman-once
  (with-riichi (ts)
    (let ((variant (cl-mahjong:find-variant 'tenhou)))
      (is (= 1 (cl-mahjong:reading-yakuman
                (best ts "111m222p333s444z55z" "5z"
                      :yaku (cl-mahjong:variant-yaku variant)
                      :options (cl-mahjong:variant-options variant))))))))

(test tenhou-standings-use-tenhou-uma
  (let* ((game (cl-mahjong:start-game 'tenhou :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(40000 30000 20000 10000) 0)))
    (is (equal '(50000 10000 -20000 -40000) (coerce (getf standings :scores) 'list)))))
