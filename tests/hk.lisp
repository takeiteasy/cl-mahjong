(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun hk-score (notation win &key context-args (scoring 'hk))
  (let* ((variant (cl-mahjong:find-variant 'hk))
         (ts (cl-mahjong:variant-tileset variant)))
    (cl-mahjong:best-score
     (cl-mahjong:evaluate-score scoring ts (cl-mahjong:variant-shapes variant)
                                (cl-mahjong:variant-yaku variant) (hand ts notation) nil
                                (apply #'context ts win context-args)
                                :options (cl-mahjong:variant-options variant)))))

(defun hk-bonus (&rest names)
  (let ((ts (cl-mahjong:variant-tileset (cl-mahjong:find-variant 'hk))))
    (list :bonus (mapcar (lambda (name) (bonus-tile ts name)) names))))

(defun hk-names (score)
  (names (cl-mahjong:score-reading score)))

(test hk-is-a-144-tile-fan-game
  (let ((variant (cl-mahjong:find-variant 'hk)))
    (is (= 144 (cl-mahjong:tileset-tile-count (cl-mahjong:variant-tileset variant))))
    (is (not (cl-mahjong:tile-red-p (cl-mahjong:variant-tileset variant) 52)))
    (is (cl-mahjong:variant-option variant :bonus-tiles))))

(test all-pungs-with-a-concealed-hand-is-four-fan
  (let ((score (hk-score "222m444p666s11z999s" "9s" :context-args (hk-bonus 'plum))))
    (is (equal '(:all-pungs :concealed-hand :own-flower) (hk-names score)))
    (is (= 0 (cl-mahjong:score-fu score)))
    (is (= 5 (cl-mahjong:score-han score)))
    (is (equal '(:ron 24 :others 12) (cl-mahjong:score-payments score)))
    (is (= 48 (cl-mahjong:score-points score)))))

(test three-fan-are-needed-to-win
  (let ((other-flower (hk-bonus 'orchid)))
    (is (null (hk-score "234m567p345s678s66p" "8s" :context-args other-flower)))
    (is (= 24 (cl-mahjong:score-points
               (hk-score "234m567p345s678s66p" "8s"
                         :context-args (append '(:tsumo t) other-flower)))))))

(test own-flowers-and-no-flowers-score-a-fan-each
  (is (equal '(:common-hand :concealed-hand :no-flowers)
             (hk-names (hk-score "234m567p345s678s66p" "8s"))))
  (is (equal '(:common-hand :concealed-hand :own-flower :own-season)
             (hk-names (hk-score "234m567p345s678s66p" "8s"
                                 :context-args (hk-bonus 'plum 'spring)))))
  (is (equal '(:all-flowers :common-hand :concealed-hand :own-flower)
             (hk-names (hk-score "234m567p345s678s66p" "8s"
                                 :context-args (hk-bonus 'plum 'orchid 'chrysanthemum 'bamboo))))))

(test the-seat-decides-which-flowers-are-own
  (is (member :own-flower (hk-names (hk-score "234m567p345s678s66p" "8s"
                                              :context-args (append '(:seat-wind :west)
                                                                    (hk-bonus 'chrysanthemum)))))))

(test a-limit-hand-pays-the-top-of-the-table
  (is (= 128 (cl-mahjong:score-base (hk-score "19m19p19s12345677z" "7z")))))

(test full-shooter-makes-the-discarder-pay-for-everyone
  (let ((score (hk-score "222m444p666s11z999s" "9s" :scoring 'hk-full-shooter
                                                    :context-args (hk-bonus 'plum))))
    (is (equal '(:ron 48) (cl-mahjong:score-payments score)))
    (is (= 48 (cl-mahjong:score-points score)))))

(test hk-deals-conserve-points
  (dolist (variant '(hk hk-full-shooter))
    (let ((game (cl-mahjong:start-game variant :seed 1)))
      (dotimes (deal 4)
        (setf game (play-deal game))
        (is (eq :deal-over (cl-mahjong:game-phase game)))
        (is (= 0 (reduce #'+ (cl-mahjong:game-points game))))
        (setf game (cl-mahjong:next-deal game))))))

(defun hk-game-holding (&rest holdings)
  "A new hk game where each seat holds the bonus kinds in HOLDINGS, a list per seat."
  (let* ((game (cl-mahjong:start-game 'hk :seed 1))
         (ts (tileset-of game))
         (deal (cl-mahjong:game-deal game)))
    (loop for names in holdings
          for seat from 0
          do (let ((player (copy-structure (cl-mahjong::deal-player deal seat))))
               (setf (cl-mahjong:player-bonus player)
                     (mapcar (lambda (name) (bonus-tile ts name)) names)
                     deal (cl-mahjong::set-player deal seat player))))
    (cl-mahjong::replace-deal game deal)))

(defparameter *flowers* '(plum orchid chrysanthemum bamboo spring summer autumn winter))

(test eight-flowers-win-at-once
  (let* ((game (hk-game-holding (subseq *flowers* 0 7) '() '() '()))
         (cl-mahjong::*events* '())
         (won (cl-mahjong::set-aside-bonus game 0 (bonus-tile (tileset-of game) 'winter)
                                           (cl-mahjong::deal-wall (cl-mahjong:game-deal game)) nil)))
    (is (eq :deal-over (cl-mahjong:game-phase won)))
    (is (eq :tsumo (result-of won :type)))
    (is (equal '(384 -128 -128 -128) (coerce (cl-mahjong:game-points won) 'list)))
    (is (eq :yakuman (getf (rest (assoc :win (reverse cl-mahjong::*events*))) :limit)))))

(test seven-flowers-rob-the-eighth
  (let* ((game (hk-game-holding '() '() (subseq *flowers* 0 7) '()))
         (cl-mahjong::*events* '())
         (won (cl-mahjong::set-aside-bonus game 1 (bonus-tile (tileset-of game) 'winter)
                                           (cl-mahjong::deal-wall (cl-mahjong:game-deal game)) nil)))
    (is (eq :ron (result-of won :type)))
    (is (equal '(2) (result-of won :winners)))
    (is (equal '(-64 -128 256 -64) (coerce (cl-mahjong:game-points won) 'list)))))

(test seven-flowers-do-not-win-without-the-eighth-being-set-aside
  (let* ((game (hk-game-holding '() '() (subseq *flowers* 0 6) '()))
         (cl-mahjong::*events* '())
         (next (cl-mahjong::set-aside-bonus game 1 (bonus-tile (tileset-of game) 'winter)
                                            (cl-mahjong::deal-wall (cl-mahjong:game-deal game)) nil)))
    (is (eq :playing (cl-mahjong:game-phase next)))))

(test per-counts-bonus-tiles
  (let* ((variant (cl-mahjong:find-variant 'hk))
         (ts (cl-mahjong:variant-tileset variant)))
    (cl-mahjong:defyaku test-flower-count (:han 1 :per (bonus flower))
      (:when t))
    (flet ((han (&rest names)
             (cl-mahjong:reading-han
              (best-reading
               (readings ts "234m567p345s678s66p" "8s" :yaku '(test-flower-count)
                         :options (cl-mahjong:variant-options variant)
                         :context-args (apply #'hk-bonus names))))))
      (is (= 2 (han 'plum 'orchid 'spring)))
      (is (= 3 (han 'plum 'orchid 'bamboo))))))
