(in-package #:cl-mahjong)

;;; Hong Kong mahjong: 144 tiles, fan scoring without fu, three fan to win

(deftileset hk-144 :extends riichi-136
  (:red)
  (:bonus (flower plum orchid chrysanthemum bamboo)
          (season spring summer autumn winter))
  (:wall (:dead-wall 16) (:replacement 0 1 2 3) (:nuki 8)))

;;; Yaku

(defyaku common-hand (:han 1)
  (:when (shape standard) (every-group (sequence pair)) (every-tile suit)))
(defyaku concealed-hand (:han 1)
  (:when (closed)))
(defyaku self-drawn (:han 1)
  (:when (context tsumo)))
(defyaku own-flower (:han 1)
  (:when (bonus 1 (and flower seat-bonus))))
(defyaku own-season (:han 1)
  (:when (bonus 1 (and season seat-bonus))))
(defyaku no-flowers (:han 1)
  (:when (not (bonus 1))))
(defyaku all-flowers (:han 2)
  (:when (bonus 4 flower)))
(defyaku all-seasons (:han 2)
  (:when (bonus 4 season)))
(defyaku all-pungs (:han 3)
  (:when (count-groups 4 (triplet quad))))
(defyaku mixed-one-suit (:han 3)
  (:when (one-suit) (some-tile honor)))
(defyaku seven-pairs (:han 4 :closed-only t)
  (:when (shape seven-pairs)))
(defyaku small-three-dragons (:han 5)
  (:implies haku hatsu chun)
  (:when (count-groups 2 (triplet quad) dragon) (pair dragon)))
(defyaku small-four-winds (:han 6)
  (:when (count-groups 3 (triplet quad) wind) (pair wind)))
(defyaku pure-one-suit (:han 7)
  (:implies mixed-one-suit)
  (:when (one-suit) (every-tile suit)))
(defyaku mixed-terminals (:han 7)
  (:implies all-pungs)
  (:when (shape standard) (every-tile (terminal honor)) (some-tile honor) (some-tile terminal)))
(defyaku great-three-dragons (:han 8)
  (:implies haku hatsu chun)
  (:when (count-groups 3 (triplet quad) dragon)))
(defyaku great-four-winds (:yakuman 1)
  (:implies all-pungs)
  (:when (count-groups 4 (triplet quad) wind)))

;;; Scoring: the fan table gives the payment; the limit is ten fan

(defscoring hk
  (:points (0 1) (1 2) (2 4) (3 8) (4 16) (5 24) (6 32) (7 48) (8 64) (9 96) (10 128))
  (:minimum 3)
  (:limits (yakuman 128 :max 1))
  (:payments (:ron :dealer 1 :other 1 :others 1/2)
             (:tsumo :dealer 1 :other (1 1))))

(defscoring hk-full-shooter :extends hk
  (:payments (:ron :dealer 2 :other 2)))

;;; Variants

(defvariant hk
  (:players 4)
  (:tileset hk-144)
  (:shapes standard seven-pairs kokushi)
  (:yaku
   common-hand concealed-hand self-drawn own-flower own-season no-flowers all-flowers all-seasons
   haku hatsu chun jikaze bakaze haitei houtei rinshan chankan
   all-pungs mixed-one-suit seven-pairs small-three-dragons small-four-winds
   pure-one-suit mixed-terminals great-three-dragons
   kokushi chuuren suuankou tsuuiisou chinroutou suukantsu great-four-winds tenhou chiihou)
  (:scoring hk)
  (:options :bonus-tiles t :bonus-win (8 :yakuman) :bonus-rob t :riichi nil :furiten nil :renchan :draw :length :full
            :west-extension nil :agari-yame nil :tobi nil
            :start-points 0 :target-points 0 :uma (0 0 0 0) :oka nil
            :noten-payment 0 :nagashi-mangan nil
            :kyuushu nil :suufon-renda nil :suucha-riichi nil :suukaikan nil
            :kuikae nil :ankan-chankan nil :multi-ron :head-bump :sanchahou nil
            :initial-dora 0 :double-yakuman nil))

(defvariant hk-full-shooter :extends hk
  (:scoring hk-full-shooter))
