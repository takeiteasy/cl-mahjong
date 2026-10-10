(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

(defun kind-ids (tileset notation)
  "One kind id per tile of NOTATION."
  (loop for count across (hand tileset notation)
        for id from 0
        append (make-list count :initial-element id)))

(defun meld (tileset type notation &key open from held)
  (cl-mahjong:make-meld type (kind-ids tileset notation) :open open :from from
                        :held (and held (remove-duplicates (kind-ids tileset held)))))

(defun context (tileset win &rest args)
  "A win context for the tile WIN (notation such as \"4m\"); both winds are east unless given."
  (let ((kind (cl-mahjong:tileset-kind tileset (first (kind-ids tileset win)))))
    (apply #'cl-mahjong:make-win-context
           :win-tile (first (cl-mahjong:tiles-of-kind tileset kind))
           (append args '(:seat-wind :east :round-wind :east)))))

(defun readings (tileset notation win &key melds (options cl-mahjong:*riichi-options*)
                                           (yaku cl-mahjong:*riichi-yaku*) (shapes *riichi-shapes*)
                                           context-args)
  (cl-mahjong:evaluate-yaku tileset shapes yaku (hand tileset notation) melds
                            (apply #'context tileset win context-args) :options options))

(defun waits (tileset notation win &rest args)
  (sort (mapcar #'cl-mahjong:reading-wait (apply #'readings tileset notation win :yaku '() args))
        #'string<))

(test sequence-waits
  (with-riichi (ts)
    (is (equal '(:ryanmen) (waits ts "234m456p789s123s11z" "4m")))
    (is (equal '(:kanchan) (waits ts "234m456p789s123s11z" "3m")))
    (is (equal '(:penchan) (waits ts "123m456p789s123s11z" "3m")))
    (is (equal '(:penchan) (waits ts "123m456p789s123s11z" "7s")))
    (is (equal '(:ryanmen) (waits ts "123m456p789s123s11z" "1m")))))

(test pair-and-triplet-waits
  (with-riichi (ts)
    (is (equal '(:tanki) (waits ts "123m456p789s123s11z" "1z")))
    (is (equal '(:shanpon) (waits ts "123m456p789s11z222z" "2z")))))

(test each-placement-gives-a-reading
  (with-riichi (ts)
    (is (equal '(:kanchan :ryanmen) (waits ts "233445m456p789s11z" "3m")))))

(test identical-groups-are-one-placement
  (with-riichi (ts)
    (is (equal '(:ryanmen) (waits ts "223344m456p789s11z" "2m")))))

(test pairs-and-each-waits
  (with-riichi (ts)
    (is (equal '(:tanki) (waits ts "1122m3344p5566s77z" "7z")))
    (is (equal '(:thirteen-sided) (waits ts "19m19p19s1234567z1m" "1m")))
    (is (equal '(:thirteen-sided) (waits ts "19m19p19s11234567z" "1z")))
    (is (equal '(:tanki) (waits ts "19m19p19s11234567z" "7z")))))

(test win-tile-must-be-in-the-hand
  (with-riichi (ts)
    (signals cl-mahjong:invalid-hand (waits ts "123m456p789s123s11z" "5m"))))

(test lambda-shapes-have-no-placement
  (with-riichi (ts)
    (cl-mahjong:defshape test-all-man
      (:lambda :complete (lambda (tileset counts melds)
                           (declare (ignore melds))
                           (loop for count across counts
                                 for id from 0
                                 always (or (zerop count)
                                            (eq :man (cl-mahjong:kind-suit
                                                      (cl-mahjong:tileset-kind tileset id))))))))
    (cl-mahjong:defyaku test-group-wait (:han 1)
      (:when (or (some-group any) (wait tanki ryanmen))))
    (let ((readings (readings ts "123456789m11122m" "2m" :shapes '(test-all-man)
                                                         :yaku '(test-group-wait))))
      (is (= 1 (length readings)))
      (is (null (cl-mahjong:reading-wait (first readings))))
      (is (null (cl-mahjong:reading-yaku (first readings)))))))

;;; Concealment

(defun names (reading)
  (sort (mapcar #'car (cl-mahjong:reading-yaku reading)) #'string<))

(defun best-reading (readings)
  "The reading with the most yakuman, then han; the first of equals."
  (let (best)
    (dolist (reading readings best)
      (when (or (null best)
                (> (cl-mahjong:reading-yakuman reading) (cl-mahjong:reading-yakuman best))
                (and (= (cl-mahjong:reading-yakuman reading) (cl-mahjong:reading-yakuman best))
                     (> (cl-mahjong:reading-han reading) (cl-mahjong:reading-han best))))
        (setf best reading)))))

(defun best (tileset notation win &rest args)
  (best-reading (apply #'readings tileset notation win args)))

(test ron-triplet-is-not-concealed-but-the-hand-stays-closed
  (with-riichi (ts)
    (let ((ron (best ts "111m222p333s456s11z" "1m" :context-args '(:riichi t)))
          (tsumo (best ts "111m222p333s456s11z" "1m" :context-args '(:tsumo t))))
      (is (not (member :sanankou (names ron))))
      (is (member :riichi (names ron)))
      (is (member :sanankou (names tsumo))))))

(test impossible-win-contexts-are-rejected
  (with-riichi (ts)
    (flet ((rejected (context-args &optional melds)
             (signals cl-mahjong:invalid-hand
               (readings ts (if melds "567p345s678s66p" "234m567p345s678s66p") "8s"
                         :melds melds :context-args context-args))))
      (let ((open-meld (list (meld ts :sequence "234m" :open t)))
            (closed-kan (list (meld ts :quad "1111m"))))
        (rejected '(:riichi t) open-meld)
        (rejected '(:double-riichi t) open-meld)
        (is (readings ts "567p345s678s66p" "8s" :melds closed-kan :context-args '(:riichi t)))
        (rejected '(:ippatsu t))
        (rejected '(:chankan t :tsumo t))
        (rejected '(:rinshan t))
        (rejected '(:last-tile t :tsumo t :rinshan t))
        (rejected '(:last-tile t :chankan t))
        (rejected '(:tenhou t))
        (rejected '(:tenhou t :tsumo t :dealer t) closed-kan)
        (rejected '(:tenhou t :tsumo t))
        (rejected '(:chiihou t :tsumo t :dealer t))
        (rejected '(:chiihou t))
        (rejected '(:chiihou t :tsumo t) closed-kan)
        (rejected '(:renhou t :dealer t))
        (rejected '(:renhou t :tsumo t))
        (rejected '(:renhou t) open-meld)))))

(test first-turn-seat-checks-need-a-seat-wind
  (with-riichi (ts)
    (let ((context (cl-mahjong:make-win-context
                    :win-tile (first (cl-mahjong:tiles-of-kind
                                      ts (cl-mahjong:tileset-kind ts (first (kind-ids ts "8s")))))
                    :tsumo t :tenhou t)))
      (is (cl-mahjong:evaluate-yaku ts *riichi-shapes* cl-mahjong:*riichi-yaku*
                                    (hand ts "234m567p345s678s66p") '() context)))))
