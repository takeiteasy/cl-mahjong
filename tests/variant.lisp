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

;;; Deltas

(defun variant-yaku-of (name) (cl-mahjong:variant-yaku (cl-mahjong:find-variant name)))
(defun option-of (name key) (cl-mahjong:variant-option (cl-mahjong:find-variant name) key))

(test set-replaces-and-adds-options
  (cl-mahjong:defvariant delta-set :extends riichi
    (:set :kuitan nil :initial-dora 2))
  (is (null (option-of 'delta-set :kuitan)))
  (is (= 2 (option-of 'delta-set :initial-dora)))
  (is (eq t (option-of 'riichi :kuitan)))
  (is (= 1 (option-of 'riichi :initial-dora))))

(test add-and-remove-edit-yaku-and-shapes
  (cl-mahjong:defvariant delta-edit :extends riichi
    (:remove :yaku renhou-mangan tanyao)
    (:remove :shapes seven-pairs)
    (:add :shapes seven-pairs))
  (let ((yaku (variant-yaku-of 'delta-edit)))
    (is (not (member :renhou-mangan yaku :key #'cl-mahjong::keyword-of)))
    (is (= (- (length cl-mahjong:*riichi-yaku*) 2) (length yaku))))
  (is (equal '(:standard :kokushi :seven-pairs)
             (mapcar #'cl-mahjong::keyword-of
                     (cl-mahjong:variant-shapes (cl-mahjong:find-variant 'delta-edit))))))

(test remove-option-falls-back-to-the-game-default
  (cl-mahjong:defvariant delta-no-chi :extends riichi
    (:set :chi nil))
  (cl-mahjong:defvariant delta-chi-again :extends delta-no-chi
    (:remove :options :chi))
  (is (null (option-of 'delta-no-chi :chi)))
  (is (eq t (option-of 'delta-chi-again :chi))))

(test deltas-apply-eagerly-down-a-chain
  (cl-mahjong:defvariant delta-parent :extends riichi
    (:remove :yaku renhou-mangan))
  (cl-mahjong:defvariant delta-child :extends delta-parent
    (:set :kuitan nil))
  (cl-mahjong:defvariant delta-grandchild :extends delta-child
    (:yaku riichi tanyao))
  (is (not (member :renhou-mangan (variant-yaku-of 'delta-child) :key #'cl-mahjong::keyword-of)))
  (is (null (option-of 'delta-child :kuitan)))
  (is (equal '(:riichi :tanyao) (mapcar #'cl-mahjong::keyword-of (variant-yaku-of 'delta-grandchild))))
  (is (null (option-of 'delta-grandchild :kuitan))))

(test redefining-a-parent-recompiles-the-delta-child
  (cl-mahjong:defvariant delta-base :extends riichi (:set :kiriage nil))
  (cl-mahjong:defvariant delta-leaf :extends delta-base (:set :kuitan nil))
  (is (null (option-of 'delta-leaf :kiriage)))
  (cl-mahjong:defvariant delta-base :extends riichi (:set :kiriage t))
  (is (eq t (option-of 'delta-leaf :kiriage))))

(test invalid-deltas
  (flet ((bad (name &rest clauses)
           (eval `(cl-mahjong:defvariant ,name :extends riichi ,@clauses))
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-variant name))))
    (bad 'delta-bad-set '(:set :kuitan))
    (bad 'delta-bad-target '(:add :options kuitan))
    (bad 'delta-no-target '(:remove))
    (bad 'delta-missing-name '(:remove :yaku no-such-yaku))
    (bad 'delta-missing-option '(:remove :options :no-such-option))
    (bad 'delta-duplicate '(:add :yaku tanyao))))
