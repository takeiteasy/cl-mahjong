(in-package #:cl-mahjong)

(defun game-log (game)
  "The plist that replays GAME: its start (:seed :walls :points :kyoku :honba :sticks) and :actions in order."
  (append (game-start game) (list :actions (reverse (game-actions game)))))

(defun replay-game (variant log)
  "(values game events): the game that LOG, from `game-log', leads to under VARIANT, with all its events."
  (let* ((variant (if (variant-p variant) variant (find-variant variant)))
         (tileset (variant-tileset variant))
         (batches '()))
    (let ((game (multiple-value-bind (game events)
                    (start-game variant :seed (getf log :seed)
                                        :walls (mapcar (lambda (tiles) (make-wall tileset :tiles tiles))
                                                       (getf log :walls))
                                        :points (getf log :points) :kyoku (getf log :kyoku)
                                        :honba (getf log :honba) :sticks (getf log :sticks))
                  (push events batches)
                  game)))
      (dolist (entry (getf log :actions))
        (multiple-value-bind (next events) (if (eq (first entry) :next-deal)
                                               (next-deal game)
                                               (apply-action game (first entry) (second entry)))
          (push events batches)
          (setf game next)))
      (values game (apply #'append (nreverse batches))))))

(defun game-record (game)
  "A readable plist that `restore-game' turns back into GAME. The variant must be registered under its name."
  (let* ((variant (game-variant game))
         (registered (handler-case (find-variant (variant-name variant))
                       (definition-error () nil))))
    (unless (eq registered variant)
      (invalid "Variant ~(~a~) is not the registered one, so a record cannot name it" (variant-name variant)))
    (list* :variant (variant-name variant) (game-log game))))

(defun restore-game (record)
  "(values game events) for a RECORD from `game-record'."
  (replay-game (getf record :variant) record))

(defun write-game (game stream)
  (with-standard-io-syntax
    (prin1 (game-record game) stream))
  game)

(defun read-game (stream)
  "(values game events) for the record read from STREAM."
  (restore-game (with-standard-io-syntax
                  (let ((*read-eval* nil))
                    (read stream)))))
