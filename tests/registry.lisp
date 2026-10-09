(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(test lookup-is-cached
  (cl-mahjong:deftileset reg-cached (:suits man))
  (is (eq (cl-mahjong:find-tileset 'reg-cached)
          (cl-mahjong:find-tileset 'reg-cached))))

(test names-ignore-package
  (is (eq (cl-mahjong:find-tileset 'riichi-136)
          (cl-mahjong:find-tileset :riichi-136))))

(test unknown-definition
  (signals cl-mahjong:unknown-definition
    (cl-mahjong:find-tileset 'no-such-tileset))
  (cl-mahjong:deftileset reg-orphan :extends no-such-parent (:suits man))
  (signals cl-mahjong:unknown-definition
    (cl-mahjong:find-tileset 'reg-orphan)))

(test circular-extends
  (cl-mahjong:deftileset reg-loop-a :extends reg-loop-b (:suits man))
  (cl-mahjong:deftileset reg-loop-b :extends reg-loop-a (:suits man))
  (signals cl-mahjong:circular-extends
    (cl-mahjong:find-tileset 'reg-loop-a)))

(test redefining-parent-recompiles-child
  (cl-mahjong:deftileset reg-parent (:suits man) (:copies 2))
  (cl-mahjong:deftileset reg-child :extends reg-parent (:remove (man 2 9)))
  (is (= 2 (cl-mahjong:tileset-tile-count (cl-mahjong:find-tileset 'reg-child))))
  (cl-mahjong:deftileset reg-parent (:suits man) (:copies 3))
  (is (= 3 (cl-mahjong:tileset-tile-count (cl-mahjong:find-tileset 'reg-child)))))
