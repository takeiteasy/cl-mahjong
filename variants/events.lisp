(in-package #:cl-mahjong)

;;; Mahjong-Soul-style event rulesets

(deftileset riichi-136-akadora :extends riichi-136
  (:red (man 5 2) (pin 5 2) (sou 5 2)))

(defvariant akadora-frenzy :extends riichi
  (:tileset riichi-136-akadora))

(defyaku daisharin (:yakuman 1 :closed-only t)
  (:when (shape seven-pairs)
         (every-tile (pin-2 pin-3 pin-4 pin-5 pin-6 pin-7 pin-8))))

(defvariant yaku-festival :extends riichi
  (:add :yaku daisharin)
  (:remove :yaku renhou-mangan)
  (:set :renhou :yakuman))

(defvariant sanma-tonpuu :extends sanma
  (:set :length :tonpuu :start-points 40000 :target-points 45000))

(defvariant dora-bomb :extends riichi
  (:set :initial-dora 3 :kan-dora :immediate))
