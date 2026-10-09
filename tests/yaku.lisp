(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun score (reading)
  (list (names reading)
        (cl-mahjong:reading-han reading)
        (cl-mahjong:reading-yakuman reading)))

(defmacro is-score (expected (ts notation win &rest args))
  `(is (equal ,expected (score (best ,ts ,notation ,win ,@args)))))

;;; Hand-based yaku

(test tanyao-and-pinfu
  (with-riichi (ts)
    (is-score '((:pinfu :tanyao) 2 0) (ts "234m567p345s678s66p" "8s"))
    (is-score '((:tanyao) 1 0) (ts "234m567p345s678s66p" "3m"))
    (is-score '((:pinfu) 1 0) (ts "234m567p345s678s11p" "8s"))
    (is-score '(() 0 0) (ts "234m567p345s678s55z" "8s"))))

(test open-tanyao-follows-kuitan
  (with-riichi (ts)
    (let ((melds (list (meld ts :sequence "345s" :open t))))
      (is-score '((:tanyao) 1 0) (ts "234m567p678s66p" "8s" :melds melds))
      (is-score '(() 0 0) (ts "234m567p678s66p" "8s" :melds melds :options '(:kuitan nil))))))

(test riichi-family
  (with-riichi (ts)
    (is-score '((:ippatsu :menzen-tsumo :pinfu :riichi :tanyao) 5 0)
              (ts "234m567p345s678s66p" "8s" :context-args '(:riichi t :ippatsu t :tsumo t)))
    (is-score '((:double-riichi :pinfu :tanyao) 4 0)
              (ts "234m567p345s678s66p" "8s" :context-args '(:double-riichi t)))
    (is-score '((:tanyao) 1 0)
              (ts "234m567p678s66p" "8s"
                  :melds (list (meld ts :sequence "345s" :open t))
                  :context-args '(:riichi t)))))

(test yakuhai
  (with-riichi (ts)
    (is-score '((:haku) 1 0) (ts "123m456p789s555z11z" "5z"))
    (is-score '((:bakaze :jikaze) 2 0) (ts "123m456p789s111z55p" "5p"))
    (is-score '((:bakaze) 1 0) (ts "123m456p789s111z55p" "5p" :context-args '(:seat-wind :south)))
    (is-score '((:chun) 1 0) (ts "123m456p789s777z22z" "2z"))))

(test situational-yaku
  (with-riichi (ts)
    (is (member :haitei (names (best ts "234m567p345s678s66p" "8s"
                                     :context-args '(:last-tile t :tsumo t)))))
    (is (member :houtei (names (best ts "234m567p345s678s66p" "8s" :context-args '(:last-tile t)))))
    (is (member :rinshan (names (best ts "234m567p345s678s66p" "8s"
                                      :context-args '(:rinshan t :tsumo t)))))
    (is (member :chankan (names (best ts "234m567p345s678s66p" "8s" :context-args '(:chankan t)))))
    (is (not (member :haitei (names (best ts "234m567p345s678s66p" "8s"
                                          :context-args '(:last-tile t))))))))

(test sequences
  (with-riichi (ts)
    (is-score '((:pinfu :sanshoku :tanyao) 4 0) (ts "234m234p234s567m66p" "7m"))
    (is-score '((:sanshoku :tanyao) 2 0)
              (ts "234p234s567m66p" "7m" :melds (list (meld ts :sequence "234m" :open t))))
    (is-score '((:ittsu) 2 0) (ts "123456789m234p55s" "5s"))
    (is-score '((:iipeikou) 1 0) (ts "112233m456p789s55p" "5p"))))

(test across-suits-and-straights-need-every-suit
  (with-riichi (ts)
    (is (member :sanshoku-doukou (names (best ts "222m222p222s456m11z" "6m"))))
    (is (not (member :sanshoku-doukou (names (best ts "222m222p333s456m11z" "6m")))))
    (is (not (member :sanshoku (names (best ts "234m234p345s567m66p" "7m")))))
    (is (not (member :ittsu (names (best ts "123m456p789s234m55s" "5s")))))))

(test peikou-and-seven-pairs-compete
  (with-riichi (ts)
    (let ((readings (readings ts "112233m445566p77s" "7s")))
      (is (equal '(:standard :seven-pairs)
                 (mapcar #'cl-mahjong:decomposition-shape
                         (mapcar #'cl-mahjong:reading-decomposition readings))))
      (is (equal '((:ryanpeikou) 3 0) (score (best-reading readings)))))
    (is-score '((:chiitoitsu) 2 0) (ts "1122m3344p5566s77z" "7z"))))

(test best-reading-picks-the-best-split
  (with-riichi (ts)
    (let ((readings (readings ts "111222333m456p77s" "7s")))
      (is (= 2 (length readings)))
      (is (equal '((:sanankou) 2 0) (score (best-reading readings)))))))

(test triplet-yaku
  (with-riichi (ts)
    (is-score '((:toitoi) 2 0)
              (ts "333s444z55z" "5z"
                  :melds (list (meld ts :triplet "111m" :open t) (meld ts :triplet "222p" :open t))))
    (is-score '((:sankantsu) 2 0)
              (ts "456m77p" "7p"
                  :melds (list (meld ts :quad "1111m") (meld ts :quad "2222p" :open t)
                               (meld ts :quad "3333s" :open t))))
    (is-score '((:haku :hatsu :shousangen) 4 0) (ts "123m456p555z666z77z" "3m"))
    (is-score '((:sanankou :toitoi) 4 0) (ts "111m222p333s444z55z" "4z"))))

(test terminal-yaku
  (with-riichi (ts)
    (is-score '((:honroutou :toitoi) 4 0)
              (ts "111p999p11z" "1z"
                  :melds (list (meld ts :triplet "111m" :open t) (meld ts :triplet "999m" :open t))))
    (is-score '((:chiitoitsu :honroutou) 4 0) (ts "1199m1199p1199s11z" "1z"))
    (is-score '((:junchan) 3 0) (ts "123m789m123p789p11s" "1s"))
    (is-score '((:chanta) 2 0) (ts "123m789m123p789p11z" "1z"))
    (is-score '((:junchan) 2 0)
              (ts "789m123p789p11s" "1s" :melds (list (meld ts :sequence "123m" :open t))))
    (is-score '((:chanta) 1 0)
              (ts "789m123p789p11z" "1z" :melds (list (meld ts :sequence "123m" :open t))))))

(test flush-yaku
  (with-riichi (ts)
    (is-score '((:haku :honitsu) 4 0) (ts "234m567m789m555z66m" "6m"))
    (is-score '((:haku :honitsu) 3 0)
              (ts "567m789m555z66m" "6m" :melds (list (meld ts :sequence "234m" :open t))))
    (is-score '((:chinitsu :iipeikou :pinfu) 8 0) (ts "234m567m567m789m88m" "8m"))
    (is-score '((:chinitsu) 5 0)
              (ts "567m567m789m88m" "8m" :melds (list (meld ts :sequence "234m" :open t))))))

;;; Yakuman

(test yakuman-supersede-other-yaku
  (with-riichi (ts)
    (is-score '((:daisangen) 0 1) (ts "555z666z777z123m11p" "3m"))
    (is-score '((:shousuushii) 0 1) (ts "111z222z333z789m44z" "9m"))
    (is-score '((:tsuuiisou) 0 1) (ts "111z222z333z555z66z" "5z"))
    (is-score '((:chinroutou) 0 1) (ts "111m999m111p999p11s" "1m"))
    (is-score '((:ryuuiisou) 0 1) (ts "234s234s666s888s66z" "6z"))
    (is-score '((:suukantsu) 0 1)
              (ts "77p" "7p"
                  :melds (list (meld ts :quad "1111m") (meld ts :quad "2222p" :open t)
                               (meld ts :quad "3333s" :open t) (meld ts :quad "4444z" :open t))))))

(test heavenly-hands
  (with-riichi (ts)
    (is-score '((:tenhou) 0 1)
              (ts "234m567p345s678s66p" "8s" :context-args '(:tenhou t :tsumo t)))
    (is-score '((:chiihou) 0 1)
              (ts "234m567p345s678s66p" "8s" :context-args '(:chiihou t :tsumo t)))
    (is-score '((:pinfu :tanyao) 2 0) (ts "234m567p345s678s66p" "8s" :context-args '(:renhou t)))
    (is-score '((:renhou) 0 1)
              (ts "234m567p345s678s66p" "8s" :context-args '(:renhou t)
                                             :options '(:renhou :yakuman)))))

(test double-yakuman
  (with-riichi (ts)
    (let ((single '(:double-yakuman nil)))
      (is-score '((:kokushi) 0 2) (ts "19m19p19s1234567z1m" "1m"))
      (is-score '((:kokushi) 0 1) (ts "19m19p19s1234567z1m" "1m" :options single))
      (is-score '((:kokushi) 0 1) (ts "19m19p19s11234567z" "7z"))
      (is-score '((:suuankou) 0 2) (ts "111m222p333s444z55z" "5z"))
      (is-score '((:suuankou) 0 1)
                (ts "111m222p333s444z55z" "4z" :context-args '(:tsumo t)))
      (is-score '((:suuankou) 0 1) (ts "111m222p333s444z55z" "5z" :options single))
      (is-score '((:daisuushii) 0 2) (ts "111z222z333z444z55m" "4z"))
      (is-score '((:daisuushii) 0 1) (ts "111z222z333z444z55m" "4z" :options single))
      (is-score '((:chuuren) 0 2) (ts "11123455678999m" "5m"))
      (is-score '((:chuuren) 0 1) (ts "11123455678999m" "2m"))
      (is-score '((:chuuren) 0 1) (ts "11123455678999m" "5m" :options single)))))

;;; Engine rules

(test yaku-definitions-inherit
  (with-riichi (ts)
    (cl-mahjong:defyaku test-big-tanyao :extends tanyao (:han 3))
    (is-score '((:test-big-tanyao) 3 0)
              (ts "234m567p345s678s66p" "3m" :yaku '(test-big-tanyao)))))

(test excludes-drops-the-lower-value
  (with-riichi (ts)
    (cl-mahjong:defyaku test-low (:han 1) (:when t))
    (cl-mahjong:defyaku test-high (:han 2) (:excludes test-low) (:when t))
    (cl-mahjong:defyaku test-twin (:han 2) (:when t))
    (cl-mahjong:defyaku test-twin-excluder (:han 2) (:excludes test-twin) (:when t))
    (flet ((names-for (yaku)
             (names (best ts "234m567p345s678s66p" "8s" :yaku yaku))))
      (is (equal '(:test-high) (names-for '(test-low test-high))))
      (is (equal '(:test-twin) (names-for '(test-twin test-twin-excluder))))
      (is (equal '(:test-twin-excluder) (names-for '(test-twin-excluder test-twin)))))))

(test lambda-pattern
  (with-riichi (ts)
    (cl-mahjong:defyaku test-lambda (:han 1)
      (:when (:lambda (hand) (cl-mahjong:win-hand-closed-p hand))))
    (is-score '((:test-lambda) 1 0) (ts "234m567p345s678s66p" "8s" :yaku '(test-lambda)))
    (is-score '(() 0 0)
              (ts "234m567p678s66p" "8s" :yaku '(test-lambda)
                                         :melds (list (meld ts :sequence "345s" :open t))))))

(test closed-only-yaku-do-not-score-open
  (with-riichi (ts)
    (is-score '(() 0 0)
              (ts "234m567p678s66p" "8s" :yaku '(riichi pinfu)
                                         :context-args '(:riichi t)
                                         :melds (list (meld ts :sequence "345s" :open t))))))

(test invalid-yaku-definitions
  (flet ((invalid (name &rest clauses)
           (eval `(cl-mahjong:defyaku ,name ,@clauses))
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-yaku name))))
    (invalid 'test-bad-pattern '(:han 1) '(:when (no-such-pattern)))
    (invalid 'test-bad-arity '(:han 1) '(:when (not)))
    (invalid 'test-bad-flag '(:han 1) '(:when (context no-such-flag)))
    (invalid 'test-bad-types '(:han 1) '(:when (some-group triangle)))
    (invalid 'test-bad-option '(:han 1) '(:when (some-group sequence :colour red)))
    (invalid 'test-no-when '(:han 1))
    (invalid 'test-no-value '(:when t))
    (invalid 'test-both-values '(:han 1) '(:yakuman 1) '(:when t))
    (invalid 'test-bad-han '(:han 0) '(:when t))
    (invalid 'test-bad-clause '(:han 1) '(:when t) '(:colour red))
    (invalid 'test-stray-clause '(:han 1) 'bogus '(:when t))))
