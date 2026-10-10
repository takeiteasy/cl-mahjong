(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun red-tiles (tileset)
  (loop for tile below (cl-mahjong:tileset-tile-count tileset)
        when (cl-mahjong:tile-red-p tileset tile) collect tile))

(test riichi-136-counts
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (is (= 34 (cl-mahjong:tileset-kind-count ts)))
    (is (= 136 (cl-mahjong:tileset-tile-count ts)))
    (is (= 3 (length (red-tiles ts))))))

(test red-fives-are-fives
  (let* ((ts (cl-mahjong:find-tileset 'riichi-136))
         (five (cl-mahjong:find-kind ts 'pin 5)))
    (is (= 4 (length (cl-mahjong:tiles-of-kind ts five))))
    (is (= 1 (count-if (lambda (tile) (cl-mahjong:tile-red-p ts tile))
                       (cl-mahjong:tiles-of-kind ts five))))
    (is (every (lambda (tile) (eq five (cl-mahjong:tile-kind ts tile)))
               (cl-mahjong:tiles-of-kind ts five)))))

(test kind-lookup-and-classes
  (let ((ts (cl-mahjong:find-tileset 'riichi-136)))
    (is (cl-mahjong:kind-honor-p (cl-mahjong:find-kind ts 'east)))
    (is (cl-mahjong:terminal-p ts (cl-mahjong:find-kind ts 'sou 9)))
    (is (not (cl-mahjong:terminal-p ts (cl-mahjong:find-kind ts 'sou 5))))
    (is (not (cl-mahjong:terminal-p ts (cl-mahjong:find-kind ts 'red))))
    (is (null (cl-mahjong:find-kind ts 'man 10)))))

(test sanma-108-counts
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (is (= 27 (cl-mahjong:tileset-kind-count ts)))
    (is (= 108 (cl-mahjong:tileset-tile-count ts)))
    (is (= 2 (length (red-tiles ts))))
    (is (null (cl-mahjong:find-kind ts 'man 5)))
    (is (cl-mahjong:find-kind ts 'man 1))
    (is (cl-mahjong:find-kind ts 'man 9))))

(test kind-ids-are-dense
  (let ((ts (cl-mahjong:find-tileset 'sanma-108)))
    (is (equal (loop for i below 27 collect i)
               (loop for i below 27 collect (cl-mahjong:kind-id (cl-mahjong:tileset-kind ts i)))))))

(test remove-by-group
  (cl-mahjong:deftileset ts-no-dragons :extends riichi-136 (:remove (dragon)))
  (is (= 31 (cl-mahjong:tileset-kind-count (cl-mahjong:find-tileset 'ts-no-dragons)))))

(test bonus-and-add
  (cl-mahjong:deftileset ts-flowers :extends riichi-136
    (:bonus (plum 1) (orchid 1))
    (:add (bamboo 1)))
  (let ((ts (cl-mahjong:find-tileset 'ts-flowers)))
    (is (= 37 (cl-mahjong:tileset-kind-count ts)))
    (is (= 139 (cl-mahjong:tileset-tile-count ts)))
    (is (eq :bonus (cl-mahjong:kind-category (cl-mahjong:find-kind ts 'orchid))))))

(test child-clause-replaces-parent
  (cl-mahjong:deftileset ts-three-copies :extends riichi-136
    (:copies 3) (:red))
  (let ((ts (cl-mahjong:find-tileset 'ts-three-copies)))
    (is (= 102 (cl-mahjong:tileset-tile-count ts)))
    (is (null (red-tiles ts)))))

(test invalid-tilesets
  (flet ((bad (name)
           (signals cl-mahjong:invalid-definition (cl-mahjong:find-tileset name))))
    (cl-mahjong:deftileset bad-clause (:suits man) (:bogus 1))
    (bad 'bad-clause)
    (cl-mahjong:deftileset bad-red (:suits man) (:copies 4) (:red (pin 5 1)))
    (bad 'bad-red)
    (cl-mahjong:deftileset bad-red-count (:suits man) (:copies 1) (:red (man 5 2)))
    (bad 'bad-red-count)
    (cl-mahjong:deftileset bad-dead-wall (:suits man) (:copies 1) (:wall (:dead-wall 9)))
    (bad 'bad-dead-wall)
    (cl-mahjong:deftileset bad-position (:suits man) (:copies 4)
      (:wall (:dead-wall 4) (:dora 4)))
    (bad 'bad-position)
    (cl-mahjong:deftileset bad-overlap (:suits man) (:copies 4)
      (:wall (:dead-wall 4) (:dora 1) (:ura 1)))
    (bad 'bad-overlap)
    (cl-mahjong:deftileset bad-nuki-count (:suits man) (:copies 4)
      (:wall (:dead-wall 4) (:replacement 0) (:nuki -1)))
    (bad 'bad-nuki-count)
    (cl-mahjong:deftileset bad-nuki-slots (:suits man) (:copies 4)
      (:wall (:dead-wall 4) (:nuki 1)))
    (bad 'bad-nuki-slots)
    (cl-mahjong:deftileset bad-duplicate-kind (:suits man) (:copies 1)
      (:honors (wind east east)))
    (bad 'bad-duplicate-kind)))

(test removing-a-red-fives-suit-is-fine
  (cl-mahjong:deftileset ts-no-man :extends riichi-136 (:remove (man)))
  (is (= 2 (length (red-tiles (cl-mahjong:find-tileset 'ts-no-man))))))
