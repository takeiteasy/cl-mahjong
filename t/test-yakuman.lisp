;;;; t/test-yakuman.lisp
;;;; Tests for yakuman detection

(in-package #:cl-mahjong-tests)
(in-suite yakuman-tests)

(test kokushi-musou
  "Test Kokushi Musou (13 orphans) detection"
  (let* ((hand '(:1m :9m :1p :9p :1s :9s :east :south :west :north :white :green :red :red))
         (melds nil)
         (winning-tile :red)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :south
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :south)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Kokushi should be 13 han")
      (is (member :kokushi (mapcar #'first yaku-list))
          "Should detect kokushi yaku"))))

(test suuankou
  "Test Suuankou (four concealed triplets) detection"
  (let* ((hand '(:2m :2m :2m :5p :5p :5p :white :white :white :east :east :east :9s :9s))
         (melds nil)
         (winning-tile :9s)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :west
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :west)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Suuankou should be 13 han")
      (is (member :suuankou (mapcar #'first yaku-list))
          "Should detect suuankou yaku"))))

(test daisangen
  "Test Daisangen (big three dragons) detection"
  (let* ((hand '(:2m :3m :4m :white :white :white :green :green :green :red :red :red :5p :5p))
         (melds nil)
         (winning-tile :5p)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :north
                   :loser-seat :east
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :north)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Daisangen should be 13 han")
      (is (member :daisangen (mapcar #'first yaku-list))
          "Should detect daisangen yaku"))))

(test shousuushii
  "Test Shousuushii (little four winds) detection"
  (let* ((hand '(:2m :3m :4m :east :east :east :south :south :south :west :west :west :north :north))
         (melds nil)
         (winning-tile :north)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :south
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :south)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Shousuushii should be 13 han")
      (is (member :shousuushii (mapcar #'first yaku-list))
          "Should detect shousuushii yaku"))))

(test tsuuiisou
  "Test Tsuuiisou (all honors) detection"
  (let* ((hand '(:east :east :east :south :south :south :west :west :west :white :white :white :green :green))
         (melds nil)
         (winning-tile :green)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :west
                   :loser-seat :north
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :west)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Tsuuiisou should be 13 han")
      (is (member :tsuuiisou (mapcar #'first yaku-list))
          "Should detect tsuuiisou yaku"))))

(test chinroutou
  "Test Chinroutou (all terminals) detection"
  (let* ((hand '(:1m :1m :1m :9m :9m :9m :1p :1p :1p :9p :9p :9p :1s :1s))
         (melds nil)
         (winning-tile :1s)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :east
                   :dealer-p t
                   :round-wind :east
                   :seat-wind :east)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Chinroutou should be 13 han")
      (is (member :chinroutou (mapcar #'first yaku-list))
          "Should detect chinroutou yaku"))))

(test ryuuiisou
  "Test Ryuuiisou (all green) detection"
  (let* ((hand '(:2s :3s :4s :2s :3s :4s :6s :6s :6s :8s :8s :8s :green :green))
         (melds nil)
         (winning-tile :green)
         (context (make-winning-context
                   :tsumo-p nil
                   :winning-tile winning-tile
                   :winner-seat :south
                   :loser-seat :west
                   :dealer-p nil
                   :round-wind :east
                   :seat-wind :south)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Ryuuiisou should be 13 han")
      (is (member :ryuuiisou (mapcar #'first yaku-list))
          "Should detect ryuuiisou yaku"))))

(test tenhou
  "Test Tenhou (heavenly hand) detection"
  (let* ((hand '(:2m :3m :4m :5p :6p :7p :2s :3s :4s :6s :7s :8s :white :white))
         (melds nil)
         (winning-tile :white)
         (context (make-winning-context
                   :tsumo-p t
                   :winning-tile winning-tile
                   :winner-seat :east
                   :dealer-p t
                   :round-wind :east
                   :seat-wind :east
                   :tenhou-p t)))
    (multiple-value-bind (yaku-list total-han)
        (detect-yaku hand melds winning-tile context)
      (is (= total-han 13) "Tenhou should be 13 han")
      (is (member :tenhou (mapcar #'first yaku-list))
          "Should detect tenhou yaku"))))
