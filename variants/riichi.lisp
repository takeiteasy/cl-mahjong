(in-package #:cl-mahjong)

(deftileset riichi-136
  (:suits man pin sou)
  (:honors (wind east south west north)
           (dragon white green red))
  (:copies 4)
  (:red (man 5 1) (pin 5 1) (sou 5 1))
  (:wall (:dead-wall 14)
         (:replacement 0 1 2 3)
         (:dora 4 6 8 10 12)
         (:ura 5 7 9 11 13)))

(defshape standard
  (:sets 4 :of (sequence triplet))
  (:pair 1))

(defshape seven-pairs
  (:pairs 7 :distinct t))

(defshape kokushi
  (:each terminal honor)
  (:extra 1 :of (terminal honor)))

;;; Yaku

(defun chuuren-counts-p (tileset counts exact)
  "One suit holding the low and high ranks three times and each rank between at least once, or exactly once with EXACT."
  (let ((by-rank (make-hash-table))
        (suit nil)
        (low (car (tileset-ranks tileset)))
        (high (cdr (tileset-ranks tileset))))
    (loop for count across counts
          for id from 0
          when (plusp count)
            do (let ((kind (tileset-kind tileset id)))
                 (unless (and (eq (kind-category kind) :suit)
                              (or (null suit) (eq suit (kind-suit kind))))
                   (return-from chuuren-counts-p nil))
                 (setf suit (kind-suit kind)
                       (gethash (kind-rank kind) by-rank) count)))
    (flet ((at (rank) (gethash rank by-rank 0)))
      (and suit
           (if exact
               (and (= (at low) 3) (= (at high) 3))
               (and (>= (at low) 3) (>= (at high) 3)))
           (loop for rank from (1+ low) below high
                 always (if exact (= (at rank) 1) (>= (at rank) 1)))))))

(defyaku riichi (:han 1 :closed-only t)
  (:when (or (context riichi) (context double-riichi))))
(defyaku double-riichi (:han 2 :closed-only t)
  (:implies riichi)
  (:when (context double-riichi)))
(defyaku ippatsu (:han 1 :closed-only t)
  (:when (or (context riichi) (context double-riichi)) (context ippatsu)))
(defyaku menzen-tsumo (:han 1 :closed-only t)
  (:when (context tsumo)))
(defyaku pinfu (:han 1 :closed-only t)
  (:when (shape standard) (every-group (sequence pair)) (not (pair value)) (wait ryanmen)))
(defyaku iipeikou (:han 1 :closed-only t)
  (:when (twin-sequences 1)))
(defyaku ryanpeikou (:han 3 :closed-only t)
  (:implies iipeikou)
  (:when (twin-sequences 2)))
(defyaku tanyao (:han 1)
  (:when (every-tile simple) (or (closed) (option kuitan))))
(defyaku haku (:han 1)
  (:when (some-group (triplet quad) white)))
(defyaku hatsu (:han 1)
  (:when (some-group (triplet quad) green)))
(defyaku chun (:han 1)
  (:when (some-group (triplet quad) red)))
(defyaku jikaze (:han 1)
  (:when (some-group (triplet quad) seat-wind)))
(defyaku bakaze (:han 1)
  (:when (some-group (triplet quad) round-wind)))
(defyaku haitei (:han 1)
  (:when (context last-tile) (context tsumo)))
(defyaku houtei (:han 1)
  (:when (context last-tile) (not (context tsumo))))
(defyaku rinshan (:han 1)
  (:when (context rinshan)))
(defyaku chankan (:han 1)
  (:when (context chankan)))
(defyaku chiitoitsu (:han 2 :closed-only t)
  (:when (shape seven-pairs)))
(defyaku chanta (:han 2 :open 1)
  (:when (every-group any (terminal honor)) (some-group sequence)))
(defyaku junchan (:han 3 :open 2)
  (:implies chanta)
  (:when (every-group any terminal) (some-group sequence)))
(defyaku ittsu (:han 2 :open 1)
  (:when (full-straight)))
(defyaku sanshoku (:han 2 :open 1)
  (:when (across-suits sequence)))
(defyaku sanshoku-doukou (:han 2)
  (:when (across-suits (triplet quad))))
(defyaku sanankou (:han 2)
  (:when (count-groups 3 (triplet quad) :concealed t)))
(defyaku sankantsu (:han 2)
  (:when (count-groups 3 quad)))
(defyaku toitoi (:han 2)
  (:when (count-groups 4 (triplet quad))))
(defyaku shousangen (:han 2)
  (:when (count-groups 2 (triplet quad) dragon) (pair dragon)))
(defyaku honroutou (:han 2)
  (:when (every-tile (terminal honor)) (some-tile honor) (some-tile terminal)))
(defyaku honitsu (:han 3 :open 2)
  (:when (one-suit) (some-tile honor)))
(defyaku chinitsu (:han 6 :open 5)
  (:when (one-suit) (every-tile suit)))

(defyaku kokushi (:yakuman 1 :closed-only t :double-when (wait thirteen-sided))
  (:when (shape kokushi)))
(defyaku suuankou (:yakuman 1 :closed-only t :double-when (wait tanki))
  (:when (count-groups 4 (triplet quad) :concealed t)))
(defyaku chuuren (:yakuman 1 :closed-only t
                  :double-when (:lambda (hand)
                                 (chuuren-counts-p (win-hand-tileset hand)
                                                   (win-hand-pre-win-counts hand) t)))
  (:when (:lambda (hand)
           (chuuren-counts-p (win-hand-tileset hand) (win-hand-counts hand) nil))))
(defyaku daisangen (:yakuman 1)
  (:when (count-groups 3 (triplet quad) dragon)))
(defyaku shousuushii (:yakuman 1)
  (:when (count-groups 3 (triplet quad) wind) (pair wind)))
(defyaku daisuushii (:yakuman 1 :double-when t)
  (:when (count-groups 4 (triplet quad) wind)))
(defyaku tsuuiisou (:yakuman 1)
  (:when (every-tile honor)))
(defyaku chinroutou (:yakuman 1)
  (:when (every-tile terminal)))
(defyaku ryuuiisou (:yakuman 1)
  (:when (every-tile (sou-2 sou-3 sou-4 sou-6 sou-8 green))))
(defyaku suukantsu (:yakuman 1)
  (:when (count-groups 4 quad)))
(defyaku tenhou (:yakuman 1 :closed-only t)
  (:when (context tenhou)))
(defyaku chiihou (:yakuman 1 :closed-only t)
  (:when (context chiihou)))
(defyaku renhou (:yakuman 1 :closed-only t)
  (:when (context renhou) (option renhou :yakuman)))
(defyaku renhou-mangan (:han 5 :closed-only t)
  (:when (context renhou) (option renhou :mangan)))


;;; Scoring

(defscoring riichi
  (:fu (:base 20) (:round 10) (:closed-ron 10) (:tsumo 2) (:open-minimum 30)
       (:group triplet simple 2) (:group triplet (terminal honor) 4)
       (:concealed 2) (:quad 4)
       (:pair seat-wind 2) (:pair round-wind 2) (:pair dragon 2)
       (:wait (kanchan penchan tanki) 2)
       (:flat chiitoitsu 25)
       (:no-tsumo-fu pinfu))
  (:limits (mangan 2000 :han 5) (haneman 3000 :han 6) (baiman 4000 :han 8)
           (sanbaiman 6000 :han 11) (yakuman 8000 :han 13))
  (:floor renhou-mangan mangan)
  (:kiriage 1920)
  (:dora (:indicator next))
  (:payments (:ron :dealer 6 :other 4) (:tsumo :dealer 2 :other (2 1))
             (:round 100) (:honba 300) (:riichi-stick 1000)))

;;; Variant

(defvariant riichi
  (:players 4)
  (:tileset riichi-136)
  (:shapes standard seven-pairs kokushi)
  (:yaku
   riichi double-riichi ippatsu menzen-tsumo pinfu iipeikou ryanpeikou tanyao
   haku hatsu chun jikaze bakaze haitei houtei rinshan chankan
   chiitoitsu chanta junchan ittsu sanshoku sanshoku-doukou sanankou sankantsu
   toitoi shousangen honroutou honitsu chinitsu
   kokushi suuankou chuuren daisangen shousuushii daisuushii tsuuiisou chinroutou
   ryuuiisou suukantsu tenhou chiihou renhou renhou-mangan)
  (:scoring riichi)
  (:options :kuitan t :double-yakuman t :renhou nil
            :kiriage nil :kazoe-yakuman t :double-wind-fu 4 :rinshan-tsumo-fu t))

(defparameter *riichi-yaku* (variant-yaku (find-variant 'riichi)))
(defparameter *riichi-options* (variant-options (find-variant 'riichi)))
