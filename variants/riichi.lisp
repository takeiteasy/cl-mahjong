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
