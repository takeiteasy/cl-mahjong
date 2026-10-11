(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun tile-ids (tileset notation &key plain)
  "Distinct tile ids for NOTATION; the first copy of a kind is the red one unless PLAIN."
  (let ((used (make-hash-table)))
    (mapcar (lambda (id)
              (let* ((tiles (cl-mahjong:tiles-of-kind tileset (cl-mahjong:tileset-kind tileset id)))
                     (n (1- (incf (gethash id used 0)))))
                (nth (if plain (- (length tiles) 1 n) n) tiles)))
            (kind-ids tileset notation))))

(defun scores (tileset notation win &key melds (options cl-mahjong:*riichi-options*)
                                         context-args honba riichi-sticks (players 4)
                                         (scoring 'riichi))
  (cl-mahjong:evaluate-score scoring tileset *riichi-shapes* cl-mahjong:*riichi-yaku*
                             (hand tileset notation) melds
                             (apply #'context tileset win context-args)
                             :options options :honba (or honba 0)
                             :riichi-sticks (or riichi-sticks 0) :players players))

(defun best-scored (&rest args)
  (cl-mahjong:best-score (apply #'scores args)))

(defun summary (score)
  (list (cl-mahjong:score-fu score) (cl-mahjong:score-han score) (cl-mahjong:score-points score)))

(defmacro is-points (expected (ts notation win &rest args))
  `(is (equal ,expected (summary (best-scored ,ts ,notation ,win ,@args)))))

(defun dora-of (score)
  (cl-mahjong:score-dora score))

;;; Fu

(test pinfu-fu
  (with-riichi (ts)
    (is-points '(20 3 2700) (ts "234m567p345s678s66p" "8s" :context-args '(:tsumo t)))
    (is-points '(30 2 2000) (ts "234m567p345s678s66p" "8s"))))

(test pinfu-fu-rules-are-options
  (with-riichi (ts)
    (flet ((fu (options notation win &rest args)
             (cl-mahjong:score-fu
              (apply #'best-scored ts notation win
                     :options (append options cl-mahjong:*riichi-options*) args))))
      (is (= 20 (fu '() "234m567p345s678s66p" "8s" :context-args '(:tsumo t))))
      (is (= 30 (fu '(:pinfu-tsumo-fu t) "234m567p345s678s66p" "8s" :context-args '(:tsumo t))))
      (let ((melds (list (meld ts :sequence "234m" :open t))))
        (is (= 30 (fu '() "567p345s678s66p" "8s" :melds melds)))
        (is (= 20 (fu '(:open-pinfu-fu nil) "567p345s678s66p" "8s" :melds melds)))))))

(test chiitoitsu-is-25-fu
  (with-riichi (ts)
    (is-points '(25 2 1600) (ts "1122m3344p5566s77z" "7z"))))

(test triplets-are-concealed-unless-completed-by-ron
  (with-riichi (ts)
    (is-points '(40 1 1300) (ts "222m345p678p456s88s" "2m"))
    (is-points '(30 2 2000) (ts "222m345p678p456s88s" "2m" :context-args '(:tsumo t)))
    (is-points '(30 1 1000)
               (ts "345p678p456s88s" "4s" :melds (list (meld ts :triplet "222m" :open t))))))

(test quad-fu
  (with-riichi (ts)
    (is-points '(70 1 2300)
               (ts "234p567p456s99s" "4s" :melds (list (meld ts :quad "1111m"))
                                           :context-args '(:riichi t)))))

(test wait-fu
  (with-riichi (ts)
    (is-points '(40 1 1300) (ts "234m567p345s678s66p" "3m"))
    (is-points '(40 1 1300) (ts "123m456p789s234s66p" "3m" :context-args '(:riichi t)))
    (is-points '(40 1 1300) (ts "234m567p345s678s66p" "6p"))))

(test value-pairs
  (with-riichi (ts)
    (is-points '(40 1 1300) (ts "234m567p345s678s55z" "8s" :context-args '(:riichi t)))
    (is-points '(50 1 1600) (ts "111m234p567s789s11z" "4p" :context-args '(:riichi t)))
    (is-points '(40 1 1300)
               (ts "111m234p567s789s11z" "4p" :context-args '(:riichi t)
                                              :options (list* :double-wind-fu 2
                                                              cl-mahjong:*riichi-options*)))))

(test rinshan-tsumo-fu-is-an-option
  (with-riichi (ts)
    (flet ((fu (options)
             (cl-mahjong:score-fu
              (best-scored ts "111m234p567s789s22p" "3p"
                           :context-args '(:riichi t :tsumo t :rinshan t) :options options))))
      (is (= 40 (fu cl-mahjong:*riichi-options*)))
      (is (= 30 (fu (list* :rinshan-tsumo-fu nil cl-mahjong:*riichi-options*)))))))

;;; Limits

(defparameter *sanshoku-hand* "234m234p234s567m66p")

(defun dora-rich (ts count)
  "Indicators giving COUNT dora in the sanshoku hand."
  (tile-ids ts (subseq "1m1p1s2m2p2s3m3p3s" 0 (* 2 count))))

(test limits
  (with-riichi (ts)
    (flet ((limit (dora)
             (let ((score (best-scored ts *sanshoku-hand* "7m"
                                       :context-args (list :dora-indicators (dora-rich ts dora)))))
               (list (cl-mahjong:score-han score) (cl-mahjong:score-limit score)
                     (cl-mahjong:score-points score)))))
      (is (equal '(4 nil 7700) (limit 0)))
      (is (equal '(5 :mangan 8000) (limit 1)))
      (is (equal '(6 :haneman 12000) (limit 2)))
      (is (equal '(8 :baiman 16000) (limit 4)))
      (is (equal '(11 :sanbaiman 24000) (limit 7)))
      (is (equal '(13 :yakuman 32000) (limit 9))))))

(cl-mahjong:defscoring test-kazoe-sanbaiman :extends riichi
  (:limits (yakuman 8000)))

(cl-mahjong:defscoring test-kazoe-multiple :extends riichi
  (:limits (yakuman 8000 :han 13 :per 13 :max 3)))

(test kazoe-stops-at-sanbaiman-without-a-yakuman-han
  (with-riichi (ts)
    (let ((score (best-scored ts *sanshoku-hand* "7m" :scoring 'test-kazoe-sanbaiman
                              :context-args (list :dora-indicators (dora-rich ts 9)))))
      (is (eq :sanbaiman (cl-mahjong:score-limit score)))
      (is (= 24000 (cl-mahjong:score-points score))))))

(test kazoe-multiples-and-the-yakuman-cap
  (flet ((base (scoring han yakuman)
           (cl-mahjong::base-points (cl-mahjong::find-scoring scoring) han 30 yakuman nil)))
    (is (equal '(8000 :yakuman) (multiple-value-list (base 'riichi 13 0))))
    (is (equal '(8000 :yakuman) (multiple-value-list (base 'riichi 40 0))))
    (is (equal '(6000 :sanbaiman) (multiple-value-list (base 'test-kazoe-sanbaiman 40 0))))
    (is (= 16000 (base 'test-kazoe-multiple 26 0)))
    (is (= 16000 (base 'test-kazoe-multiple 38 0)))
    (is (= 24000 (base 'test-kazoe-multiple 39 0)))
    (is (= 24000 (base 'test-kazoe-multiple 100 0)))
    (is (= 24000 (base 'test-kazoe-multiple 13 7)))
    (is (= 48000 (base 'riichi 13 6)))
    (is (= 48000 (base 'riichi 13 7)))))

(test kiriage-is-an-option
  (with-riichi (ts)
    (is (= 7700 (cl-mahjong:score-points (best-scored ts *sanshoku-hand* "7m"))))
    (let ((score (best-scored ts *sanshoku-hand* "7m"
                              :options (list* :kiriage t cl-mahjong:*riichi-options*))))
      (is (eq :mangan (cl-mahjong:score-limit score)))
      (is (= 8000 (cl-mahjong:score-points score))))))

(test yakuman-multiples
  (with-riichi (ts)
    (let ((score (best-scored ts "19m19p19s1234567z1m" "1m")))
      (is (eq :yakuman (cl-mahjong:score-limit score)))
      (is (= 64000 (cl-mahjong:score-points score))))
    (is (= 96000 (cl-mahjong:score-points
                  (best-scored ts "19m19p19s1234567z1m" "1m" :context-args '(:dealer t)))))
    (is (= 64000 (cl-mahjong:score-points
                  (best-scored ts "111m222p333s444z55z" "5z"))))))

;;; Dora

(defun dora-counts (score)
  (list (getf (dora-of score) :dora) (getf (dora-of score) :ura) (getf (dora-of score) :aka)))

(test indicators-wrap-round
  (with-riichi (ts)
    (flet ((dora (notation win indicator)
             (first (dora-counts
                     (best-scored ts notation win
                                  :context-args (list :riichi t
                                                      :dora-indicators (tile-ids ts indicator)))))))
      (is (= 3 (dora "111m234p567s789s22p" "4p" "9m")))
      (is (= 2 (dora "234m567p345s678s11z" "8s" "4z")))
      (is (= 3 (dora "123m456p789s555z22z" "3m" "7z")))
      (is (= 1 (dora "234m567p345s678s66p" "8s" "1m"))))))

(test dora-count-quads
  (with-riichi (ts)
    (is (= 4 (first (dora-counts
                     (best-scored ts "234p567p456s99s" "4s"
                                  :melds (list (meld ts :quad "1111m"))
                                  :context-args (list :riichi t
                                                      :dora-indicators (tile-ids ts "9m")))))))))

(test kan-dora-are-more-indicators
  (with-riichi (ts)
    (is (= 2 (first (dora-counts
                     (best-scored ts "234m567p345s678s66p" "8s"
                                  :context-args (list :dora-indicators (tile-ids ts "1m2s")))))))))

(test ura-needs-riichi
  (with-riichi (ts)
    (flet ((counts (&rest context-args)
             (dora-counts (best-scored ts "234m567p345s678s66p" "8s"
                                       :context-args (list* :ura-indicators (tile-ids ts "1m")
                                                            context-args)))))
      (is (equal '(0 0 0) (counts)))
      (is (equal '(0 1 0) (counts :riichi t))))))

(test dora-never-make-a-yaku
  (with-riichi (ts)
    (is (null (scores ts "234m567p345s678s55z" "8s"
                      :context-args (list :dora-indicators (tile-ids ts "1m")))))))

(test red-fives
  (with-riichi (ts)
    (flet ((aka (plain)
             (third (dora-counts
                     (best-scored ts "234m567p345s678s66p" "8s"
                                  :context-args (list :tiles (tile-ids ts "234m567p345s678s66p8s"
                                                                       :plain plain)))))))
      (is (= 2 (aka nil)))
      (is (= 0 (aka t))))))

(test sanma-indicators-skip-the-removed-ranks
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (flet ((dora (indicator)
             (first (dora-counts
                     (best-scored ts "111m999m234p567s11z" "4p"
                                  :context-args (list :riichi t
                                                      :dora-indicators (tile-ids ts indicator)))))))
      (is (= 3 (dora "1m")))
      (is (= 3 (dora "9m"))))))

;;; Payments

(test tsumo-payments
  (with-riichi (ts)
    (let ((score (best-scored ts "234m567p345s678s66p" "8s" :context-args '(:tsumo t))))
      (is (equal '(:dealer 1300 :other 700) (cl-mahjong:score-payments score)))
      (is (= 2700 (cl-mahjong:score-points score))))
    (let ((score (best-scored ts "234m567p345s678s66p" "8s" :context-args '(:tsumo t :dealer t))))
      (is (equal '(:dealer nil :other 1300) (cl-mahjong:score-payments score)))
      (is (= 3900 (cl-mahjong:score-points score))))))

(test honba-and-riichi-sticks
  (with-riichi (ts)
    (let ((ron (best-scored ts "234m567p345s678s66p" "8s" :honba 3 :riichi-sticks 1)))
      (is (equal '(:ron 2900) (cl-mahjong:score-payments ron)))
      (is (= 3900 (cl-mahjong:score-points ron))))
    (let ((tsumo (best-scored ts "234m567p345s678s66p" "8s" :context-args '(:tsumo t)
                                                              :honba 2 :riichi-sticks 2)))
      (is (equal '(:dealer 1500 :other 900) (cl-mahjong:score-payments tsumo)))
      (is (= 5300 (cl-mahjong:score-points tsumo))))))

(test dealer-limits-pay-more
  (with-riichi (ts)
    (is (= 12000 (cl-mahjong:score-points
                  (best-scored ts *sanshoku-hand* "7m"
                               :context-args (list :dealer t :dora-indicators (dora-rich ts 1))))))))

;;; Ranking

(test best-score-ranks-points-then-han-then-fu
  (flet ((score (fu han points) (cl-mahjong::make-score nil fu han nil nil 0 points nil)))
    (is (= 4 (cl-mahjong:score-han
              (cl-mahjong:best-score (list (score 30 3 1000) (score 30 4 1000) (score 30 2 1000))))))
    (is (= 40 (cl-mahjong:score-fu
               (cl-mahjong:best-score (list (score 30 2 1000) (score 40 2 1000))))))
    (is (= 2000 (cl-mahjong:score-points
                 (cl-mahjong:best-score (list (score 30 4 1000) (score 60 1 2000))))))
    (is (null (cl-mahjong:best-score '())))))

(test best-score-picks-the-better-split
  (with-riichi (ts)
    (is (= 2 (length (scores ts "111222333m456p77s" "7s"))))
    (is-points '(50 2 3200) (ts "111222333m456p77s" "7s"))))

;;; Renhou

(test renhou-mangan-is-a-floor
  (with-riichi (ts)
    (flet ((points (renhou &rest context-args)
             (cl-mahjong:score-points
              (best-scored ts *sanshoku-hand* "7m"
                           :context-args (list* :renhou t context-args)
                           :options (list* :renhou renhou cl-mahjong:*riichi-options*)))))
      (is (= 7700 (points nil)))
      (is (= 8000 (points :mangan)))
      (is (= 12000 (points :mangan :dora-indicators (dora-rich ts 2))))
      (is (= 32000 (points :yakuman))))
    (is (eq :mangan (cl-mahjong:score-limit
                     (best-scored ts "234m567p345s678s66p" "8s" :context-args '(:renhou t)
                                  :options '(:renhou :mangan)))))))

(test renhou-han-stacks
  (with-riichi (ts)
    (flet ((best (renhou)
             (best-scored ts *sanshoku-hand* "7m" :context-args '(:renhou t)
                          :options (list* :renhou renhou cl-mahjong:*riichi-options*))))
      (is (= 9 (cl-mahjong:score-han (best :han))))
      (is (eq :baiman (cl-mahjong:score-limit (best :han)))))))

;;; Definitions

(test scoring-extends-and-lambda-fu
  (cl-mahjong::register-definition
   :scoring 'test-base-30 'riichi '((:name test-base-30) (:fu (:base 30))))
  (cl-mahjong::register-definition
   :scoring 'test-lambda-fu 'riichi
   '((:name test-lambda-fu) (:fu (:lambda (hand reading) (declare (ignore hand reading)) 99))))
  (with-riichi (ts)
    (is-points '(40 2 2600) (ts "234m567p345s678s66p" "8s" :scoring 'test-base-30))
    (is (= 99 (cl-mahjong:score-fu
               (best-scored ts "234m567p345s678s66p" "8s" :scoring 'test-lambda-fu))))))

(test invalid-scoring-definitions
  (flet ((invalid (name &rest clauses)
           (cl-mahjong::register-definition :scoring name nil (cons (list :name name) clauses))
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-scoring name))))
    (invalid 'test-no-fu '(:limits (yakuman 8000)))
    (invalid 'test-no-yakuman '(:fu (:base 20)) '(:limits (mangan 2000 :han 5)))
    (invalid 'test-bad-clause '(:colour red))
    (invalid 'test-bad-fu '(:fu (:colour 2)) '(:limits (yakuman 8000)))
    (invalid 'test-bad-floor '(:fu (:base 20)) '(:limits (yakuman 8000)) '(:floor x mangan))
    (invalid 'test-no-payments '(:fu (:base 20)) '(:limits (yakuman 8000)))))

;;; Pao

(defun pao-score (ts &key tsumo dealer honba (scoring 'riichi) (givers '(1 2 3))
                       (notation "234m11p") (win "2m") melds)
  (best-scored ts notation win
               :melds (or melds
                          (loop for dragon in '("555z" "666z" "777z")
                                for from in givers
                                collect (meld ts :triplet dragon :open t :from from)))
               :context-args (list :tsumo tsumo :dealer dealer)
               :honba honba :scoring scoring))

(defun pao-scoring (honba ron)
  (let ((name (intern (format nil "TEST-PAO-~a-~a" ron honba) :cl-mahjong/tests)))
    (cl-mahjong::register-definition
     :scoring name 'riichi `((:name ,name) (:payments (:pao :ron ,ron :honba ,honba))))
    name))

(test pao-ron-is-split-with-the-discarder
  (with-riichi (ts)
    (let ((score (pao-score ts)))
      (is (equal '(:ron 16000 :pao ((3 . 16000))) (cl-mahjong:score-payments score)))
      (is (= 32000 (cl-mahjong:score-points score))))
    (let ((score (pao-score ts :dealer t)))
      (is (equal '(:ron 24000 :pao ((3 . 24000))) (cl-mahjong:score-payments score)))
      (is (= 48000 (cl-mahjong:score-points score))))))

(test pao-tsumo-is-paid-by-the-liable-seat
  (with-riichi (ts)
    (let ((score (pao-score ts :tsumo t :honba 2)))
      (is (equal '(:dealer 0 :other 0 :pao ((3 . 32600))) (cl-mahjong:score-payments score)))
      (is (= 32600 (cl-mahjong:score-points score))))
    (is (equal '(:dealer nil :other 0 :pao ((3 . 48000)))
               (cl-mahjong:score-payments (pao-score ts :tsumo t :dealer t))))))

(defun rinshan-pao-scoring (mode)
  (let ((name (intern (format nil "TEST-RINSHAN-PAO-~a" mode) :cl-mahjong/tests)))
    (cl-mahjong::register-definition
     :scoring name 'riichi `((:name ,name) (:payments (:pao :rinshan ,mode))))
    name))

(defun rinshan-pao-score (ts mode &key dealer (honba 0) (kan-from 2) (rinshan t))
  (best-scored ts "234m567p345s11p" "2m"
               :melds (list (meld ts :quad "5555z" :open t :from 2))
               :context-args (list :tsumo t :rinshan rinshan :kan-from kan-from :dealer dealer)
               :honba honba :scoring (rinshan-pao-scoring mode)))

(test rinshan-pao-makes-the-daiminkan-giver-pay-the-whole-tsumo
  (with-riichi (ts)
    (let ((score (rinshan-pao-score ts :tsumo)))
      (is (equal '(:ron 2700 :from 2) (cl-mahjong:score-payments score)))
      (is (= 2700 (cl-mahjong:score-points score))))
    (is (equal '(:ron 4500 :from 2)
               (cl-mahjong:score-payments (rinshan-pao-score ts :tsumo :dealer t :honba 2))))))

(test rinshan-pao-ron-scores-the-win-as-a-ron
  (with-riichi (ts)
    (let ((score (rinshan-pao-score ts :ron)))
      (is (equal '(:ron 2600 :from 2) (cl-mahjong:score-payments score)))
      (is (member :rinshan (mapcar #'car (cl-mahjong:reading-yaku (cl-mahjong:score-reading score))))))
    (is (equal '(:ron 4500 :from 2)
               (cl-mahjong:score-payments (rinshan-pao-score ts :ron :dealer t :honba 2))))))

(test rinshan-pao-needs-a-rinshan-win-on-a-daiminkan
  (with-riichi (ts)
    (is (equal '(:dealer 1300 :other 700)
               (cl-mahjong:score-payments (rinshan-pao-score ts :none))))
    (dolist (mode '(:tsumo :ron))
      (is (equal '(:dealer 1300 :other 700)
                 (cl-mahjong:score-payments (rinshan-pao-score ts mode :kan-from nil))))
      (is (equal '(:dealer 700 :other 400)
                 (cl-mahjong:score-payments (rinshan-pao-score ts mode :rinshan nil)))))))

(test pao-honba-follows-the-scoring
  (with-riichi (ts)
    (flet ((payments (scoring)
             (cl-mahjong:score-payments (pao-score ts :honba 3 :scoring scoring))))
      (is (equal '(:ron 16900 :pao ((3 . 16000))) (payments 'riichi)))
      (is (equal '(:ron 16000 :pao ((3 . 16900))) (payments (pao-scoring 'liable 'split))))
      (is (equal '(:ron 16450 :pao ((3 . 16450))) (payments (pao-scoring 'split 'split))))
      (is (equal '(:ron 0 :pao ((3 . 32900))) (payments (pao-scoring 'liable 'liable)))))))

(test pao-covers-only-its-own-yakuman
  (with-riichi (ts)
    (let ((score (pao-score ts :tsumo t :notation "11z222z" :win "2z")))
      (is (equal '(:dealer 16000 :other 8000 :pao ((3 . 32000)))
                 (cl-mahjong:score-payments score)))
      (is (= 64000 (cl-mahjong:score-points score))))
    (let ((score (pao-score ts :notation "11z222z" :win "2z")))
      (is (equal '(:ron 48000 :pao ((3 . 16000))) (cl-mahjong:score-payments score))))))

(test double-pao-yakuman-is-split-as-two
  (with-riichi (ts)
    (let ((score (pao-score ts :notation "11p" :win "1p"
                               :melds (list (meld ts :triplet "111z" :open t :from 0)
                                            (meld ts :triplet "222z" :open t :from 2)
                                            (meld ts :triplet "333z" :open t :from 3)
                                            (meld ts :triplet "444z" :open t :from 1)))))
      (is (equal '(:ron 32000 :pao ((1 . 32000))) (cl-mahjong:score-payments score)))
      (is (= 64000 (cl-mahjong:score-points score))))))

(test no-pao-leaves-payments-alone
  (with-riichi (ts)
    (is (equal '(:ron 32000)
               (cl-mahjong:score-payments
                (pao-score ts :melds (list (meld ts :triplet "555z" :open t)
                                           (meld ts :triplet "666z" :open t)
                                           (meld ts :triplet "777z" :open t))))))))

(test invalid-pao-payments
  (flet ((invalid (name &rest entry)
           (cl-mahjong::register-definition
            :scoring name 'riichi `((:name ,name) (:payments (:pao ,@entry))))
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-scoring name))))
    (invalid 'test-pao-bad-ron :ron 'half)
    (invalid 'test-pao-bad-honba :honba 'nobody)
    (invalid 'test-pao-bad-key :colour 'red)
    (invalid 'test-pao-bad-value :ron 3)))

(test aotenjou-scores-without-limits-and-stacks-yakuman
  (with-riichi (ts)
    (let ((options (list* :double-yakuman nil cl-mahjong:*riichi-options*)))
      (is-points '(40 13 5242900)
                 (ts "19m19p119s1234567z" "1s" :scoring 'aotenjou :options options))
      (is-points '(40 13 7864400)
                 (ts "19m19p119s1234567z" "1s" :scoring 'aotenjou :options options
                                                :context-args '(:dealer t)))
      (is (= 30 (cl-mahjong:score-fu
                 (best-scored ts "19m19p119s1234567z" "1s" :scoring 'aotenjou :options options
                                                           :context-args '(:tsumo t))))))))

(test aotenjou-pays-no-honba-or-sticks
  (with-riichi (ts)
    (let ((options (list* :double-yakuman nil cl-mahjong:*riichi-options*)))
      (is (= 5242900
             (cl-mahjong:score-points
              (best-scored ts "19m19p119s1234567z" "1s" :scoring 'aotenjou :options options
                                                        :honba 3 :riichi-sticks 2)))))))

;;; Fan table

(cl-mahjong:defscoring test-fan
  (:points (0 1) (1 2) (2 4) (3 8) (4 16) (5 24) (6 32))
  (:minimum 2)
  (:limits (yakuman 128 :max 1))
  (:payments (:ron :dealer 1 :other 1) (:tsumo :dealer 1 :other (1 1))))

(test fan-table-scores-without-fu
  (with-riichi (ts)
    (let ((score (best-scored ts "234m567p345s678s66p" "8s" :scoring 'test-fan)))
      (is (equal '(0 2 4) (summary score)))
      (is (equal '(:ron 4) (cl-mahjong:score-payments score))))))

(test fan-table-stops-at-its-last-row
  (with-riichi (ts)
    (let ((score (best-scored ts "112233m445566m77m" "7m" :scoring 'test-fan)))
      (is (> (cl-mahjong:score-han score) 6))
      (is (= 32 (cl-mahjong:score-points score))))))

(test minimum-han-rejects-a-hand
  (with-riichi (ts)
    (is (null (best-scored ts "234m567p345s678s66p" "3m" :scoring 'test-fan)))
    (is (best-scored ts "234m567p345s678s66p" "3m" :scoring 'test-fan
                     :context-args '(:tsumo t)))))

(test yakuman-ignores-the-minimum
  (with-riichi (ts)
    (is (= 128 (cl-mahjong:score-points
                (best-scored ts "19m19p19s12345677z" "7z" :scoring 'test-fan))))))

(cl-mahjong:defscoring test-fan-excluding :extends test-fan
  (:minimum 2 :excluding menzen-tsumo))

(test minimum-does-not-count-excluded-yaku
  (with-riichi (ts)
    (is (null (best-scored ts "234m567p345s678s66p" "3m" :scoring 'test-fan-excluding
                                                        :context-args '(:tsumo t))))
    (let ((score (best-scored ts "234m567p345s678s66p" "3m" :scoring 'test-fan-excluding
                                                            :context-args '(:riichi t))))
      (is (= 2 (cl-mahjong:score-han score))))))

(test excluded-yaku-still-score
  (with-riichi (ts)
    (let ((score (best-scored ts "234m567p345s678s66p" "3m" :scoring 'test-fan-excluding
                                                            :context-args '(:riichi t :tsumo t))))
      (is (= 3 (cl-mahjong:score-han score))))))

(test minimum-takes-a-number-and-excluded-names
  (dolist (clause '((:minimum) (:minimum 2 :excluding) (:minimum 2 :without riichi)
                    (:minimum 2 :excluding 3)))
    (cl-mahjong::register-definition :scoring 'test-bad-minimum nil
                                     (list '(:name test-bad-minimum) clause))
    (signals cl-mahjong:invalid-definition (cl-mahjong:find-scoring 'test-bad-minimum))))

;;; Ron paid by every seat

(cl-mahjong:defscoring test-half-shooter :extends test-fan
  (:payments (:ron :dealer 1 :other 1 :others 1/2)))

(test others-pay-a-share-of-a-ron
  (with-riichi (ts)
    (let ((score (best-scored ts "234m567p345s678s66p" "8s" :scoring 'test-half-shooter)))
      (is (equal '(:ron 4 :others 2) (cl-mahjong:score-payments score)))
      (is (= 8 (cl-mahjong:score-points score)))
      (let ((game (cl-mahjong:start-game 'riichi :seed 1)))
        (is (equal '((0 . 8) (1 . -2) (2 . -4) (3 . -2))
                   (sort (cl-mahjong::win-deltas game (cl-mahjong:game-deal game) score 0 2)
                         #'< :key #'car)))))))

(test others-do-not-pay-a-tsumo-share
  (with-riichi (ts)
    (let ((score (best-scored ts "234m567p345s678s66p" "8s" :scoring 'test-half-shooter
                                                           :context-args '(:tsumo t))))
      (is (null (getf (cddr (cl-mahjong:score-payments score)) :others))))))
