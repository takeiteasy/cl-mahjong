(in-package #:cl-mahjong)

;;; Mahjong-Soul-style event rulesets

(deftileset riichi-136-akadora :extends riichi-136
  (:red (man 5 2) (pin 5 2) (sou 5 2)))

(defvariant akadora-frenzy :extends riichi
  (:tileset riichi-136-akadora))

;;; House-rule yaku, each switched on by an option

(defyaku daisharin (:yakuman 1 :closed-only t)
  (:implies chinitsu pinfu tanyao ryanpeikou chiitoitsu)
  (:when (option daisharin)
         (shape seven-pairs)
         (every-tile (pin-2 pin-3 pin-4 pin-5 pin-6 pin-7 pin-8))))

(defyaku daisuurin (:yakuman 1 :closed-only t)
  (:implies chinitsu pinfu tanyao ryanpeikou chiitoitsu)
  (:when (option daisharin :all)
         (shape seven-pairs)
         (every-tile (man-2 man-3 man-4 man-5 man-6 man-7 man-8))))

(defyaku daichikurin (:yakuman 1 :closed-only t)
  (:implies chinitsu pinfu tanyao ryanpeikou chiitoitsu)
  (:when (option daisharin :all)
         (shape seven-pairs)
         (every-tile (sou-2 sou-3 sou-4 sou-5 sou-6 sou-7 sou-8))))

(defyaku daichisei (:yakuman 1 :closed-only t)
  (:when (option daichisei) (shape seven-pairs) (every-tile honor)))

(defyaku open-riichi (:han 2 :closed-only t)
  (:implies riichi)
  (:when (context open-riichi) (context riichi) (not (context double-riichi))))

(defyaku double-open-riichi (:han 3 :closed-only t)
  (:implies riichi double-riichi)
  (:when (context open-riichi) (context double-riichi)))

(defyaku sashikomi (:yakuman 1 :closed-only t)
  (:when (option sashikomi)
         (context open-riichi) (or (context riichi) (context double-riichi))
         (not (context tsumo)) (not (context from-riichi))))

;; One yakuman per consecutive dealer win.
(defyaku paarenchan (:yakuman (:lambda (hand) (or (win-context-paarenchan (win-hand-context hand)) 0))
                              :needs-yaku (not (option paarenchan-without-yaku)))
  (:when (context paarenchan)))

(defyaku suukantsu-pao :extends suukantsu
  (:yakuman 1 :pao kan))

(defscoring house-rules :extends riichi
  (:payments (:pao :ron split :honba discarder :rinshan tsumo)))

(defvariant house-rules :extends riichi
  (:scoring house-rules)
  (:add :yaku open-riichi double-open-riichi sashikomi paarenchan
        daisharin daisuurin daichikurin daichisei suukantsu-pao)
  (:remove :yaku suukantsu)
  (:set :open-riichi t :sashikomi t :daisharin :all :daichisei t :pao-held t))

;;; Aotenjou: no limits, each yakuman is 13 han and stacks with the other yaku
(defscoring aotenjou :extends house-rules
  (:aotenjou 13)
  (:fu (:flat kokushi 40 :tsumo 30))
  (:payments (:honba 0) (:riichi-stick 0)))

(defvariant aotenjou :extends house-rules
  (:scoring aotenjou))

(defvariant yaku-festival :extends riichi
  (:add :yaku daisharin)
  (:remove :yaku renhou-mangan renhou-han)
  (:set :renhou :yakuman :daisharin t))

(defvariant sanma-tonpuu :extends sanma
  (:set :length :tonpuu :start-points 40000 :target-points 45000))

(defvariant dora-bomb :extends riichi
  (:set :initial-dora 3 :kan-dora :immediate))
