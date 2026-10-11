(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun wrc-readings (notation win &rest context-args)
  (let* ((variant (cl-mahjong:find-variant 'wrc))
         (ts (cl-mahjong:variant-tileset variant)))
    (readings ts notation win :yaku (cl-mahjong:variant-yaku variant)
                              :options (cl-mahjong:variant-options variant)
                              :context-args context-args)))

(test wrc-has-no-red-fives
  (let ((ts (cl-mahjong:variant-tileset (cl-mahjong:find-variant 'wrc))))
    (is (= 136 (cl-mahjong:tileset-tile-count ts)))
    (is (notany (lambda (id) (cl-mahjong:tile-red-p ts id))
                (loop for id below (cl-mahjong:tileset-tile-count ts) collect id)))))

(test wrc-plays-30000-points-with-its-uma-and-no-oka
  (let* ((game (cl-mahjong:start-game 'wrc :walls (list (new-wrc-wall))))
         (standings (cl-mahjong::standings game #(45000 30000 25000 20000) 0)))
    (is (equal '(30000 30000 30000 30000)
               (coerce (cl-mahjong:game-points game) 'list)))
    (is (equal '(30000 5000 -10000 -25000)
               (coerce (getf standings :scores) 'list)))))

(defun new-wrc-wall ()
  (stacked :tileset (cl-mahjong:variant-tileset (cl-mahjong:find-variant 'wrc))))

(test wrc-turns-off-every-abortive-draw-and-early-end
  (let ((variant (cl-mahjong:find-variant 'wrc)))
    (dolist (option '(:kyuushu :suufon-renda :suucha-riichi :suukaikan :sanchahou
                      :tobi :agari-yame :west-extension :nagashi-mangan))
      (is (null (cl-mahjong:variant-option variant option)) "~s" option))
    (is (eq :head-bump (cl-mahjong:variant-option variant :multi-ron)))))

(test wrc-kazoe-stops-at-sanbaiman
  (let* ((variant (cl-mahjong:find-variant 'wrc))
         (ts (cl-mahjong:variant-tileset variant))
         (score (cl-mahjong:best-score
                 (cl-mahjong:evaluate-score
                  (cl-mahjong:variant-scoring variant) ts (cl-mahjong:variant-shapes variant)
                  (cl-mahjong:variant-yaku variant) (hand ts *sanshoku-hand*) nil
                  (context ts "7m" :dora-indicators (dora-rich ts 9))
                  :options (cl-mahjong:variant-options variant)))))
    (is (eq :sanbaiman (cl-mahjong:score-limit score)))
    (is (= 24000 (cl-mahjong:score-points score)))))

(test wrc-stacks-distinct-yakuman
  (let ((reading (best-reading (wrc-readings "555z666z777z111z22z" "2z"))))
    (is (equal '(:daisangen :suuankou :tsuuiisou) (names reading)))
    (is (= 3 (cl-mahjong:reading-yakuman reading)))))

(test wrc-scores-a-double-yakuman-wait-once
  (is (= 1 (cl-mahjong:reading-yakuman
            (best-reading (wrc-readings "119m19p19s1234567z" "1m"))))))

(test wrc-four-kan-liability
  (let ((yaku (cl-mahjong:variant-yaku (cl-mahjong:find-variant 'wrc))))
    (is (member 'cl-mahjong::suukantsu-pao yaku :key #'string-upcase-symbol :test #'string=))
    (is (not (member "SUUKANTSU" yaku :key #'string-upcase-symbol :test #'string=)))))

(defun string-upcase-symbol (name)
  (string-upcase (string name)))

(test wrc-continues-after-four-kans
  (let ((game (new-game :variant 'wrc
                        :hands '("111m222p333s4445z" "555s666p777m6677z" "248m348p248s2345z"
                                 "368m378p368s1177z")
                        :draws '("1m" "5s") :rinshan '("2p" "9s" "6p" "9m"))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "1m")))))
    (setf game (cl-mahjong:apply-action game 0 (list :ankan (first (kind-ids (tileset-of game) "2p")))))
    (setf game (pass-all (cl-mahjong:apply-action game 0 (list :discard (drawn-tile game)))))
    (setf game (cl-mahjong:apply-action game 1 (list :ankan (first (kind-ids (tileset-of game) "5s")))))
    (setf game (cl-mahjong:apply-action game 1 (list :ankan (first (kind-ids (tileset-of game) "6p")))))
    (setf game (pass-all (cl-mahjong:apply-action game 1 (list :discard (drawn-tile game)))))
    (is (eq :playing (cl-mahjong:game-phase game)))))

(test wrc-double-ron-pays-the-nearest-winner-only
  (let ((game (new-game :variant 'wrc
                        :hands (list (first *multi-ron-hands*) (second *multi-ron-hands*)
                                     (third *multi-ron-hands*) "357m357p357s3344z")
                        :draws '("6s"))))
    (setf game (act game 0 :discard "6s"))
    (dolist (seat (cl-mahjong:deciding-seats game))
      (setf game (cl-mahjong:apply-action game seat '(:ron))))
    (is (equal '(1) (result-of game :winners)))))
