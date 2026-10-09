(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test riichi-variant
  (let ((variant (cl-mahjong:find-variant 'riichi)))
    (is (eq :riichi (cl-mahjong:variant-name variant)))
    (is (= 4 (cl-mahjong:variant-players variant)))
    (is (eq (cl-mahjong:find-tileset 'riichi-136) (cl-mahjong:variant-tileset variant)))
    (is (equal cl-mahjong:*riichi-yaku* (cl-mahjong:variant-yaku variant)))
    (is (eq t (getf (cl-mahjong:variant-options variant) :kuitan)))))

(test variant-extends-replaces-clauses
  (cl-mahjong:defvariant variant-child :extends riichi
    (:players 3) (:tileset sanma-108))
  (let ((child (cl-mahjong:find-variant 'variant-child)))
    (is (= 3 (cl-mahjong:variant-players child)))
    (is (= 108 (cl-mahjong:tileset-tile-count (cl-mahjong:variant-tileset child))))
    (is (equal cl-mahjong:*riichi-yaku* (cl-mahjong:variant-yaku child)))))

(test invalid-variant-definitions
  (cl-mahjong:defvariant variant-bad-clause :extends riichi (:bogus 1))
  (signals cl-mahjong:invalid-definition (cl-mahjong:find-variant 'variant-bad-clause))
  (cl-mahjong:defvariant variant-bad-players :extends riichi (:players 1))
  (signals cl-mahjong:invalid-definition (cl-mahjong:find-variant 'variant-bad-players))
  (cl-mahjong:defvariant variant-bad-options :extends riichi (:options :kuitan))
  (signals cl-mahjong:invalid-definition (cl-mahjong:find-variant 'variant-bad-options))
  (cl-mahjong:defvariant variant-bad-yaku :extends riichi (:yaku no-such-yaku))
  (signals cl-mahjong:unknown-definition (cl-mahjong:find-variant 'variant-bad-yaku)))
