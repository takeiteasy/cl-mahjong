(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test riichi-shapes-are-registered
  (is (eq :sets (cl-mahjong:shape-family (cl-mahjong:find-shape 'standard))))
  (is (eq :pairs (cl-mahjong:shape-family (cl-mahjong:find-shape 'seven-pairs))))
  (is (eq :each (cl-mahjong:shape-family (cl-mahjong:find-shape 'kokushi))))
  (is (eq :kokushi (cl-mahjong:shape-name (cl-mahjong:find-shape :kokushi)))))

(test shape-inheritance
  (cl-mahjong:defshape shp-small :extends standard (:sets 2))
  (cl-mahjong:defshape shp-small-child :extends shp-small (:sets 2 :of (triplet)))
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (is (cl-mahjong:complete-p ts '(shp-small) (hand ts "123m456m11p") '()))
    (is (not (cl-mahjong:complete-p ts '(standard) (hand ts "123m456m11p") '())))
    (is (cl-mahjong:complete-p ts '(shp-small-child) (hand ts "111m222m11p") '()))
    (is (not (cl-mahjong:complete-p ts '(shp-small-child) (hand ts "123m456m11p") '())))
    (is (eq :shp-small-child
            (cl-mahjong:shape-name (cl-mahjong:find-shape 'shp-small-child))))))

(test redefining-a-shape-recompiles-children
  (cl-mahjong:defshape shp-parent (:sets 1) (:pair 1))
  (cl-mahjong:defshape shp-kid :extends shp-parent (:pair 1))
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (is (cl-mahjong:complete-p ts '(shp-kid) (hand ts "123m11p") '()))
    (cl-mahjong:defshape shp-parent (:sets 2) (:pair 1))
    (is (not (cl-mahjong:complete-p ts '(shp-kid) (hand ts "123m11p") '())))))

(test invalid-shapes
  (flet ((bad (name)
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-shape name))))
    (cl-mahjong:defshape shp-empty)
    (bad 'shp-empty)
    (cl-mahjong:defshape shp-unknown-clause (:bogus 1))
    (bad 'shp-unknown-clause)
    (cl-mahjong:defshape shp-mixed (:sets 4) (:pairs 7))
    (bad 'shp-mixed)
    (cl-mahjong:defshape shp-pair-only (:pair 1))
    (bad 'shp-pair-only)
    (cl-mahjong:defshape shp-zero-sets (:sets 0))
    (bad 'shp-zero-sets)
    (cl-mahjong:defshape shp-bad-set-type (:sets 4 :of (quad)))
    (bad 'shp-bad-set-type)
    (cl-mahjong:defshape shp-bad-option (:sets 4 :bogus t))
    (bad 'shp-bad-option)
    (cl-mahjong:defshape shp-bad-class (:each rare))
    (bad 'shp-bad-class)
    (cl-mahjong:defshape shp-no-complete (:lambda :shanten (lambda (ts counts melds) 0)))
    (bad 'shp-no-complete)
    (cl-mahjong:defshape shp-not-a-function (:lambda :complete 42))
    (bad 'shp-not-a-function)))

(test meld-validation
  (signals cl-mahjong:invalid-hand (cl-mahjong:make-meld :bogus '(0 0 0)))
  (signals cl-mahjong:invalid-hand (cl-mahjong:make-meld :quad '(0 0 0)))
  (is (cl-mahjong:hand-closed-p '()))
  (is (cl-mahjong:hand-closed-p (list (cl-mahjong:make-meld :quad '(0 0 0 0)))))
  (is (not (cl-mahjong:hand-closed-p
            (list (cl-mahjong:make-meld :quad '(0 0 0 0))
                  (cl-mahjong:make-meld :triplet '(1 1 1) :open t))))))

(test bonus-tiles-stay-out-of-hands
  (cl-mahjong:deftileset shp-flowers :extends riichi-136 (:bonus (plum 1)))
  (let* ((ts (cl-mahjong:find-tileset 'shp-flowers))
         (plum (first (cl-mahjong:tiles-of-kind ts (cl-mahjong:find-kind ts 'plum)))))
    (signals cl-mahjong:invalid-hand (cl-mahjong:hand-counts ts (list plum)))))
