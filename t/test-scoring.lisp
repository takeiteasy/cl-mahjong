;; t/test-scoring.lisp

(in-package #:cl-mahjong/test)
(in-suite scoring-tests)

(test riichi-tsumo
  "Test Riichi + Menzen Tsumo scoring (2 han 30 fu)"
  (let* ((hand '(:2m :3m :4m :5p :6p :7p :2s :3s :4s :6s :7s :8s :white :white))
         (melds nil)
         (winning-tile :white)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :south
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :south
                   :riichi-declared-p t
                   :riichi-win-p t
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (= (score-result-total-han result) 2) "Should have 2 han")
      (is (= (score-result-fu result) 30) "Should have 30 fu"))))

(test tanyao-pinfu-ron
  "Test Tanyao + Pinfu scoring (2 han 20 fu) - Ron"
  (let* ((hand '(:2m :3m :4m :3p :4p :5p :4s :5s :6s :5s :6s :7s :6p :6p))
         (melds nil)
         (winning-tile :6p)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :west
                   :loser-seat :south
                   :dealer-p nil
                   :loser-is-dealer-p nil
                   :round-wind :east
                   :seat-wind :west
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (= (score-result-total-han result) 2) "Should have 2 han")
      (is (member :tanyao (mapcar #'first (score-result-yaku-list result)))
          "Should detect tanyao")
      (is (member :pinfu (mapcar #'first (score-result-yaku-list result)))
          "Should detect pinfu"))))

(test honitsu-mangan
  "Test Honitsu + Yakuhai = Mangan"
  (let* ((hand '(:2m :3m :4m :6m :7m :8m :white :white :white :green :green :green :red :red))
         (melds nil)
         (winning-tile :red)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :north
                   :loser-seat :east
                   :dealer-p nil
                   :loser-is-dealer-p t
                   :round-wind :east
                   :seat-wind :north
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (>= (score-result-total-han result) 5) "Should be mangan (5+ han)")
      (is (eq (score-result-limit-hand result) :mangan) "Should be mangan limit hand"))))

(test chinitsu-haneman
  "Test Chinitsu (Haneman) - Dealer Tsumo"
  (let* ((hand '(:1m :2m :3m :4m :5m :6m :7m :8m :9m :2m :3m :4m :5m :5m))
         (melds nil)
         (winning-tile :5m)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :east
                   :dealer-p t
                   :round-wind :east
                   :seat-wind :east
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (= (score-result-total-han result) 6) "Chinitsu should be 6 han")
      (is (eq (score-result-limit-hand result) :haneman) "Should be haneman limit hand")
      (is (member :chinitsu (mapcar #'first (score-result-yaku-list result)))
          "Should detect chinitsu"))))

(test daisangen-yakuman
  "Test Daisangen yakuman (13 han)"
  (let* ((hand '(:2m :3m :4m :white :white :white :green :green :green :red :red :red :5p :5p))
         (melds nil)
         (winning-tile :5p)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :south
                   :loser-seat :west
                   :dealer-p nil
                   :loser-is-dealer-p nil
                   :round-wind :east
                   :seat-wind :south
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (= (score-result-total-han result) 13) "Daisangen should be 13 han")
      (is (eq (score-result-limit-hand result) :yakuman) "Should be yakuman limit hand"))))

(test riichi-with-dora
  "Test Riichi + Tsumo + Dora (3 han 30 fu)"
  (let* ((hand '(:2m :3m :4m :5p :6p :7p :2s :3s :4s :6s :7s :8s :white :white))
         (melds nil)
         (winning-tile :white)
         (dora-indicators '(:1m))
         (dora-tiles (calculate-dora dora-indicators))
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :south
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :south
                   :riichi-declared-p t
                   :riichi-win-p t
                   :dora-tiles dora-tiles
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (= (score-result-total-han result) 3) "Should have 3 han (2 yaku + 1 dora)")
      (is (= (score-result-dora-count result) 1) "Should have 1 dora"))))

(test honba-riichi-sticks
  "Test payment with honba and riichi sticks"
  (let* ((hand '(:2m :3m :4m :5p :6p :7p :2s :3s :4s :6s :7s :8s :white :white))
         (melds nil)
         (winning-tile :white)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :south
                   :loser-seat :north
                   :dealer-p nil
                   :loser-is-dealer-p nil
                   :round-wind :east
                   :seat-wind :south
                   :riichi-declared-p t
                   :riichi-win-p t
                   :honba 3
                   :riichi-sticks 2
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (let ((payment (score-result-payment result)))
        (is (not (null payment)) "Payment should not be nil")
        ;; Honba adds 300 per stick, riichi sticks add 1000 each
        (is (> (payment-split-winner-receives payment) 0)
            "Winner should receive points")))))

(test multiple-yakuman
  "Test multiple yakuman (Suuankou tanki + Chinroutou)"
  (let* ((hand '(:1m :1m :1m :9m :9m :9m :1p :1p :1p :9p :9p :9p :1s :1s))
         (melds nil)
         (winning-tile :1s)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :east
                   :dealer-p t
                   :round-wind :east
                   :seat-wind :east
                   :dora-tiles nil
                   :ura-dora-tiles nil
                   :aka-dora-enabled-p nil)))
    (let ((result (calculate-score hand melds winning-tile context)))
      (is (not (null result)) "Score result should not be nil")
      (is (>= (score-result-total-han result) 13) "Should be at least yakuman"))))
