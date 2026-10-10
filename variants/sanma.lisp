(in-package #:cl-mahjong)

(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8))
  (:wall (:dead-wall 14)
         (:replacement 0 1 2 3)
         (:dora 4 6 8 10 12)
         (:ura 5 7 9 11 13)
         (:nuki 4)))

(defscoring sanma :extends riichi
  (:payments (:honba 200)))

;; The missing north's share is split between the payers.
(defscoring sanma-split :extends sanma
  (:payments (:absent split)))

;; Tsumo-son: the missing north's share is not paid.
(defvariant sanma :extends riichi
  (:players 3)
  (:tileset sanma-108)
  (:scoring sanma)
  (:set :chi nil :winds (:east :south :west)
        :start-points 35000 :target-points 40000 :uma (15 0 -15)
        :noten-payment 2000 :suufon-renda nil :suucha-riichi nil
        :riichi-min-tiles 3 :nukidora t))

(defvariant sanma-split :extends sanma
  (:scoring sanma-split))
