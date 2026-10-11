(in-package #:cl-mahjong)

;;; World Riichi Championship rules: riichi without red fives, abortive draws, tobi or agari-yame

(deftileset wrc-136 :extends riichi-136
  (:red))

;; No kazoe yakuman: thirteen han without a yakuman stay a sanbaiman, distinct yakuman stack
(defscoring wrc :extends riichi
  (:limits (yakuman 8000)))

;; Four-kan liability applies in WRC 2022
(defyaku suukantsu-pao :extends suukantsu
  (:yakuman 1 :pao kan))

(defvariant wrc :extends riichi
  (:tileset wrc-136)
  (:scoring wrc)
  (:remove :yaku suukantsu)
  (:add :yaku suukantsu-pao)
  (:set :start-points 30000 :target-points 30000 :uma (15 5 -5 -15)
        :west-extension nil :tobi nil :agari-yame nil
        :multi-ron :head-bump :sanchahou nil
        :kyuushu nil :suufon-renda nil :suucha-riichi nil :suukaikan nil
        :nagashi-mangan nil :double-yakuman nil))
