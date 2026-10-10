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

(test tenhou-2021-rounds-final-scores-to-thousands
  (let* ((game (cl-mahjong:start-game 'tenhou-2021 :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(18700 25700 4500 51100) 0)))
    (is (equal '(-21000 6000 -45000 60000) (coerce (getf standings :scores) 'list)))))

(test tenhou-ends-at-the-last-hand-when-the-tenpai-dealer-leads
  (is (eq t (cl-mahjong:variant-option (cl-mahjong:find-variant 'tenhou) :tenpai-yame))))

(test tenhou-keeps-final-scores-exact
  (let* ((game (cl-mahjong:start-game 'tenhou :walls (list (stacked))))
         (standings (cl-mahjong::standings game #(18700 25700 4500 51100) 0)))
    (is (equal '(-21300 5700 -45500 61100) (coerce (getf standings :scores) 'list)))))

(test tenhou-sanma-is-sanma-with-tenhou-rules
  (let ((variant (cl-mahjong:find-variant 'tenhou-sanma)))
    (is (= 3 (cl-mahjong:variant-players variant)))
    (is (null (cl-mahjong:variant-option variant :double-yakuman)))
    (is (eq t (cl-mahjong:variant-option variant :tenpai-yame)))
    (is (eq t (cl-mahjong:variant-option variant :nukidora)))))

(test tenhou-sanma-standings-use-sanma-uma
  (let* ((game (cl-mahjong:start-game 'tenhou-sanma :seed 1))
         (standings (cl-mahjong::standings game #(42000 39800 23200) 0)))
    (is (equal '(37000 -200 -36800) (coerce (getf standings :scores) 'list)))))
