(in-package #:cl-mahjong)

(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8)))

(defscoring sanma :extends riichi
  (:payments (:honba 200)))

;; Tsumo-son: the missing north's share is not paid.
(defvariant sanma :extends riichi
  (:players 3)
  (:tileset sanma-108)
  (:scoring sanma)
  (:set :chi nil :winds (:east :south :west)
        :start-points 35000 :target-points 40000 :uma (15 0 -15)
        :noten-payment 2000 :suufon-renda nil :suucha-riichi nil))
