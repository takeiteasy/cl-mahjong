(in-package #:cl-mahjong/tests)

(in-suite :cl-mahjong)

;;; Failures

(define-condition replay-mismatch (error)
  ((message :initarg :message :reader replay-message))
  (:report (lambda (condition stream) (write-string (replay-message condition) stream))))

(defvar *replay-context* "")

(defun diverge (control &rest args)
  (error 'replay-mismatch :message (format nil "~a: ~?" *replay-context* control args)))

(defun expect (what expected actual)
  (unless (equal expected actual)
    (diverge "~a: log has ~s, engine has ~s" what expected actual)))

;;; Tiles

(defun tile-sig (game tile)
  "The kind and red flag of TILE: the log and the engine agree on these, not on tile ids."
  (let ((tileset (tileset-of game)))
    (+ (* 2 (cl-mahjong:tile-kind-id tileset tile))
       (if (cl-mahjong:tile-red-p tileset tile) 1 0))))

(defun tile-sigs (game tiles)
  (sort (mapcar (lambda (tile) (tile-sig game tile)) tiles) #'<))

(defun log-wall (deal)
  (stack-wall :hands (getf deal :hands) :draws (getf deal :draws) :rinshan (getf deal :rinshan)
              :dora (cons (getf deal :dora) (getf deal :dora-added)) :ura (getf deal :ura)
              :dealer (getf deal :dealer)))

;;; Matching logged actions to legal ones

(defun legal-tile-action (game seat type tile)
  (let ((sig (tile-sig game tile)))
    (or (find-if (lambda (action)
                   (and (eq (first action) type) (= sig (tile-sig game (second action)))))
                 (cl-mahjong:legal-actions game seat))
        (diverge "seat ~a has no legal ~s of tile ~a; legal: ~s" seat type tile
                  (cl-mahjong:legal-actions game seat)))))

(defun legal-ankan (game seat tile)
  (let ((kind (cl-mahjong:tile-kind-id (tileset-of game) tile)))
    (or (find (list :ankan kind) (cl-mahjong:legal-actions game seat) :test #'equal)
        (diverge "seat ~a cannot ankan kind ~a; legal: ~s" seat kind
                  (cl-mahjong:legal-actions game seat)))))

(defun legal-call (game seat type tiles claimed)
  "The legal call of TYPE using TILES, a meld that includes the CLAIMED discard."
  (let ((own (tile-sigs game (remove (tile-sig game claimed) tiles :key (lambda (tile) (tile-sig game tile))
                                                                  :count 1))))
    (or (find-if (lambda (action)
                   (and (eq (first action) type) (equal own (tile-sigs game (rest action)))))
                 (cl-mahjong:legal-actions game seat))
        (diverge "seat ~a has no legal ~s of ~s; legal: ~s" seat type tiles
                  (cl-mahjong:legal-actions game seat)))))

(defun perform (game seat action)
  (handler-case (cl-mahjong:apply-action game seat action)
    (cl-mahjong::illegal-action ()
      (diverge "seat ~a cannot ~s; legal: ~s" seat action (cl-mahjong:legal-actions game seat)))))

(defun call-event-p (event &rest types)
  (and (eq (first event) :call) (member (third event) types)))

(defun ron-seats (result)
  (cond ((eq (getf result :type) :ron) (mapcar (lambda (win) (getf win :seat)) (getf result :wins)))
        ((eq (getf result :reason) :sanchahou) (getf result :seats))))

;;; Replaying a deal

(defun turn-action (game seat events result)
  "(values action consumed-event-p last-discard) for SEAT to act."
  (let ((event (first events)))
    (cond ((and event (eq (first event) :discard) (= seat (second event)))
           (values (legal-tile-action game seat (if (fourth event) :riichi :discard) (third event))
                   t (third event)))
          ((and event (call-event-p event :ankan) (= seat (second event)))
           (values (legal-ankan game seat (fourth event)) t))
          ((and event (call-event-p event :shouminkan) (= seat (second event)))
           (values (legal-tile-action game seat :shouminkan (fourth event)) t))
          ((and (null event) (eq (getf result :type) :tsumo)) (list :tsumo))
          ((and (null event) (eq (getf result :reason) :kyuushu)) (list :kyuushu))
          (t (diverge "seat ~a is to act, the log has ~s and then ~s ~s"
                       seat event (getf result :type) (getf result :reason))))))

(defun claim-action (game seat events result claimed)
  "(values action consumed-event-p) for SEAT answering a claim on CLAIMED."
  (let ((event (first events)))
    (cond ((and event (call-event-p event :pon :chi :daiminkan) (= seat (second event)))
           (values (legal-call game seat (third event) (nthcdr 3 event) claimed) t))
          ((and (null event) (member seat (ron-seats result))) (list :ron))
          (t (list :pass)))))

(defun replay-deal (game deal)
  "(values game engine-events): play the logged events of DEAL until the engine ends it."
  (let ((events (getf deal :events))
        (result (getf deal :result))
        (claimed nil)
        (log '()))
    (flet ((step-seat (seat action)
             (multiple-value-bind (next engine-events) (perform game seat action)
               (setf game next)
               (setf log (append log engine-events)))))
      (loop while (eq (cl-mahjong:game-phase game) :playing)
            do (if (eq (cl-mahjong:deal-phase (cl-mahjong:game-deal game)) :turn)
                   (let ((seat (first (cl-mahjong:deciding-seats game))))
                     (multiple-value-bind (action consumed discard) (turn-action game seat events result)
                       (when consumed (pop events))
                       (when discard (setf claimed discard))
                       (step-seat seat action)))
                   (let ((consumed nil)
                         (deciders (cl-mahjong:deciding-seats game)))
                     (when (and (first events) (call-event-p (first events) :pon :chi :daiminkan)
                                (not (member (second (first events)) deciders)))
                       (diverge "the log has ~s but seat ~a was offered no claim"
                                 (first events) (second (first events))))
                     (dolist (seat deciders)
                       (multiple-value-bind (action used) (claim-action game seat events result claimed)
                         (when used (setf consumed t))
                         (step-seat seat action)))
                     (when consumed (pop events))))))
    (values game log)))

;;; Checks

(defun delta-list (seats alist)
  (loop for seat below seats collect (or (cdr (assoc seat alist)) 0)))

(defun sum-lists (lists)
  (apply #'mapcar #'+ lists))

(defun engine-wins (log)
  (loop for (type . plist) in log when (eq type :win) collect plist))

(defun yaku-strings (names)
  (sort (mapcar (lambda (name) (string-downcase (string name))) names) #'string<))

(defun verify-win (expected engine)
  (let ((*replay-context* (format nil "~a, seat ~a's win" *replay-context* (getf expected :seat))))
    (expect "discarder" (getf expected :from) (getf engine :from))
    (expect "limit" (getf expected :limit) (getf engine :limit))
    (when (getf expected :han)
      (expect "han" (getf expected :han) (getf engine :han)))
    (when (and (getf expected :han) (not (getf expected :limit)))
      (expect "fu" (getf expected :fu) (getf engine :fu)))
    (expect "yaku" (yaku-strings (getf expected :yaku))
            (yaku-strings (mapcar #'car (getf engine :yaku))))
    (expect "liable seats" (and (getf expected :pao) (list (getf expected :pao)))
            (mapcar #'car (getf engine :pao)))))

(defun verify-result (game log deal)
  (let* ((result (getf deal :result))
         (engine (cl-mahjong:game-result game))
         (seats (cl-mahjong:variant-players (cl-mahjong:game-variant game))))
    (expect "result" (getf result :type) (getf engine :type))
    (ecase (getf result :type)
      ((:tsumo :ron)
       (let ((wins (engine-wins log)))
         (expect "winners" (mapcar (lambda (win) (getf win :seat)) (getf result :wins))
                 (mapcar (lambda (win) (getf win :seat)) wins))
         (loop for expected in (getf result :wins)
               for actual in wins
               do (verify-win expected actual))))
      (:abortive (expect "abortive draw" (getf result :reason) (getf engine :reason)))
      (:exhaustive
       (if (eq (getf result :reason) :nagashi)
           (expect "nagashi mangan" t (and (getf engine :nagashi) t))
           (expect "tenpai seats" (getf result :tenpai) (getf engine :tenpai)))))
    (expect "point changes"
            (if (member (getf result :type) '(:tsumo :ron))
                (sum-lists (mapcar (lambda (win) (getf win :deltas)) (getf result :wins)))
                (getf result :deltas))
            (delta-list seats (getf engine :deltas)))))

(defun verify-deal-start (game deal events)
  (expect "kyoku" (getf deal :kyoku) (cl-mahjong:game-kyoku game))
  (expect "honba" (getf deal :honba) (cl-mahjong:game-honba game))
  (expect "riichi sticks" (getf deal :sticks) (cl-mahjong:game-sticks game))
  (expect "dealer" (getf deal :dealer) (cl-mahjong:deal-dealer (cl-mahjong:game-deal game)))
  (expect "points" (getf deal :points) (coerce (cl-mahjong:game-points game) 'list))
  (expect "dora indicator" (list (tile-sig game (getf deal :dora)))
          (mapcar (lambda (tile) (tile-sig game tile))
                  (getf (rest (find :deal-start events :key #'first)) :dora)))
  (loop for hand in (getf deal :hands)
        for seat from 0
        for held = (if (= seat (getf deal :dealer)) (append hand (list (first (getf deal :draws)))) hand)
        do (expect (format nil "seat ~a's tiles" seat) (tile-sigs game held)
                   (tile-sigs game (cl-mahjong:player-tiles
                                    (cl-mahjong::deal-player (cl-mahjong:game-deal game) seat))))))

(defun verify-final (game data)
  (let ((final (cl-mahjong:game-final game))
        (owari (getf data :owari)))
    (expect "final points" (getf owari :points) (coerce (getf final :points) 'list))
    (loop for expected in (getf owari :scores)
          for actual across (getf final :scores)
          for seat from 0
          unless (= expected actual)
            do (diverge "final score of seat ~a: log has ~s, engine has ~s" seat expected actual))))

;;; Replaying a game

(defun replay-game (data &optional (variant (or (getf data :variant) 'tenhou)))
  "NIL when the engine reproduces the logged game, else a description of the first divergence."
  (handler-case
      (let* ((deals (getf data :deals))
             (opening (first deals))
             (game nil)
             (events nil))
        (setf (values game events)
              (cl-mahjong:start-game variant :walls (mapcar #'log-wall deals)
                                             :points (getf opening :points) :kyoku (getf opening :kyoku)
                                             :honba (getf opening :honba) :sticks (getf opening :sticks)))
        (loop for (deal . more) on deals
              for number from 1
              do (let ((*replay-context* (format nil "~a deal ~a (kyoku ~a, honba ~a)" (getf data :id) number
                                                 (getf deal :kyoku) (getf deal :honba))))
                   (verify-deal-start game deal events)
                   (multiple-value-bind (next log) (replay-deal game deal)
                     (setf game next)
                     (verify-result game log deal))
                   (cond (more (unless (eq (cl-mahjong:game-phase game) :deal-over)
                                 (diverge "the engine ended the game but the log goes on"))
                               (setf (values game events) (cl-mahjong:next-deal game)))
                         (t (unless (cl-mahjong:game-over-p game)
                              (diverge "the log ends but the engine's game goes on"))
                            (verify-final game data)))))
        nil)
    (replay-mismatch (condition) (replay-message condition))))

;;; Real logs

(defparameter *replay-known-failures*
  '(("2012121117gm-00a9-0000-bf2db132&tw=1" . "#27: tenpai hand holds all four tiles of its wait")
    ("2021032012gm-0029-0000-e0b9e6c1&tw=0" . "#27: tenpai hand holds all four tiles of its wait")
    ("2025102709gm-00a9-0000-af75dfc4&tw=3" . "#27: tenpai hand holds all four tiles of its wait"))
  "Alist of log id to the ticket that tracks its divergence.")

(defun replay-log-files ()
  "The converted logs in CL_MAHJONG_REPLAY_DIR, thinned to CL_MAHJONG_REPLAY_LIMIT evenly spaced ones."
  (let* ((dir (uiop:getenv "CL_MAHJONG_REPLAY_DIR"))
         (files (and dir (directory (merge-pathnames "*.sexp" (uiop:ensure-directory-pathname dir)))))
         (limit (parse-integer (or (uiop:getenv "CL_MAHJONG_REPLAY_LIMIT") "") :junk-allowed t)))
    (if (and limit (< 0 limit (length files)))
        (loop for index below limit
              collect (nth (floor (* index (length files)) limit) files))
        files)))

(defun read-replay-file (path)
  (with-open-file (in path)
    (let ((*read-eval* nil)
          (*package* (find-package '#:cl-mahjong/tests)))
      (read in))))

(defun replay-threads ()
  (or (parse-integer (or (uiop:getenv "CL_MAHJONG_REPLAY_THREADS") "") :junk-allowed t) 4))

(defun replay-file (path)
  "(id . problem) for the converted log at PATH; a Lisp error counts as a problem."
  (let ((data (read-replay-file path)))
    (cons (getf data :id)
          (handler-case (replay-game data)
            (error (condition) (format nil "~a: ~a" (getf data :id) condition))))))

(defun replay-files (files)
  "A vector of (id . problem) in the order of FILES, replayed on REPLAY-THREADS threads."
  (let* ((files (coerce files 'vector))
         (results (make-array (length files)))
         (next (list 0)))
    ;; Workers only read the registry once the variants are compiled.
    (dolist (name '(tenhou tenhou-2021))
      (cl-mahjong:find-variant name))
    (flet ((work ()
             (loop for index = (sb-ext:atomic-incf (car next))
                   while (< index (length files))
                   do (setf (aref results index) (replay-file (aref files index))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (max 1 (replay-threads)) collect (sb-thread:make-thread #'work))))
    results))

(test tenhou-logs-replay
  (let ((files (replay-log-files)))
    (if (null files)
        (skip "set CL_MAHJONG_REPLAY_DIR to a directory of converted Tenhou logs (docs/replay.md)")
        (loop for (id . problem) across (replay-files files)
              for known = (assoc id *replay-known-failures* :test #'string=)
              do (cond ((and problem known) (skip "~a: ~a" id (cdr known)))
                       (problem (fail "~a" problem))
                       (known (fail "~a passes now; drop it from *replay-known-failures*" id))
                       (t (pass)))))))

;;; Self-play round trip: checks the replay machinery, not the rules

(defun best-discard (game seat actions rng)
  "The discard that leaves the lowest shanten; ties are broken at random."
  (let* ((tileset (tileset-of game))
         (variant (cl-mahjong:game-variant game))
         (player (cl-mahjong::deal-player (cl-mahjong:game-deal game) seat))
         (scored (mapcar (lambda (action)
                           (cons (cl-mahjong:shanten
                                  tileset (cl-mahjong:variant-shapes variant)
                                  (cl-mahjong::hand-counts tileset (remove (second action)
                                                                           (cl-mahjong:player-tiles player)))
                                  (cl-mahjong:player-melds player))
                                 action))
                         actions))
         (best (reduce #'min scored :key #'car))
         (ties (remove best scored :key #'car :test-not #'eql)))
    (cdr (nth (cl-mahjong::rng-below rng (length ties)) ties))))

(defvar *greedy* nil
  "Whether random-action discards for the lowest shanten.")

(defun random-action (game seat rng)
  (let ((actions (cl-mahjong:legal-actions game seat)))
    (flet ((of (&rest types) (remove-if-not (lambda (action) (member (first action) types)) actions))
           (choose (list) (nth (cl-mahjong::rng-below rng (length list)) list))
           (chance (n) (zerop (cl-mahjong::rng-below rng n))))
      (cond ((of :tsumo :ron) (first (of :tsumo :ron)))
            ((and (of :riichi) (chance 2)) (choose (of :riichi)))
            ((and (of :pon :chi :daiminkan :ankan :shouminkan :kyuushu) (chance 3))
             (choose (of :pon :chi :daiminkan :ankan :shouminkan :kyuushu)))
            ((and (of :discard) *greedy*) (best-discard game seat (of :discard) rng))
            ((of :discard) (choose (of :discard)))
            (t (list :pass))))))

(defun recorded-event (event)
  (destructuring-bind (type &key seat tile riichi tiles &allow-other-keys) event
    (ecase type
      (:discard (list :discard seat tile riichi))
      (:call (let ((call (getf (rest event) :type)))
               (case call
                 (:shouminkan (list :call seat call (car (last tiles))))
                 (t (list* :call seat call tiles))))))))

(defun recorded-win (win deltas)
  (list :seat (getf win :seat) :from (getf win :from) :fu (getf win :fu) :han (getf win :han)
        :limit (getf win :limit)
        :yaku (mapcar (lambda (entry) (intern (string-upcase (string (car entry))) :keyword))
                      (getf win :yaku))
        :pao (car (first (getf win :pao))) :deltas deltas))

(defun recorded-result (game events seats signalled)
  (let* ((result (cl-mahjong:game-result game))
         (type (getf result :type))
         (deltas (delta-list seats (getf result :deltas))))
    (case type
      ((:tsumo :ron)
       (list :type type
             :wins (loop for win in (engine-wins events)
                         for first = t then nil
                         collect (recorded-win win (if first deltas (make-list seats :initial-element 0))))))
      (:abortive (list :type type :reason (getf result :reason)
                       :seats signalled
                       :deltas deltas))
      (t (list :type type :reason (and (getf result :nagashi) :nagashi)
               :tenpai (getf result :tenpai) :deltas deltas)))))

(defun record-deal (game start-events rng)
  "(values game deal): play one deal with random actions and describe it in the log format."
  (let* ((deal (cl-mahjong:game-deal game))
         (seats (cl-mahjong:variant-players (cl-mahjong:game-variant game)))
         (dealer (cl-mahjong:deal-dealer deal))
         (first-draw (getf (rest (find :draw start-events :key #'first)) :tile))
         (hands (loop for seat below seats
                      collect (let ((tiles (cl-mahjong:player-tiles (cl-mahjong::deal-player deal seat))))
                                (if (= seat dealer) (remove first-draw tiles) tiles))))
         (header (list :kyoku (cl-mahjong:game-kyoku game) :honba (cl-mahjong:game-honba game)
                       :sticks (cl-mahjong:game-sticks game) :dealer dealer
                       :points (coerce (cl-mahjong:game-points game) 'list)
                       :dora (first (getf (rest (find :deal-start start-events :key #'first)) :dora))
                       :hands hands))
         (events (copy-list start-events))
         (signalled '()))
    (loop while (eq (cl-mahjong:game-phase game) :playing)
          do (let* ((seat (first (cl-mahjong:deciding-seats game)))
                    (action (random-action game seat rng)))
               (when (member (first action) '(:ron :kyuushu)) (push seat signalled))
               (multiple-value-bind (next new) (cl-mahjong:apply-action game seat action)
                 (setf game next
                       events (append events new)))))
    (flet ((tiles-of (type &optional rinshan)
             (loop for (kind . plist) in events
                   when (and (eq kind type) (eq (and (getf plist :rinshan) t) rinshan))
                     collect (getf plist :tile))))
      (values game
              (append header
                      (list :draws (tiles-of :draw) :rinshan (tiles-of :draw t)
                            :dora-added (tiles-of :dora)
                            :ura (cl-mahjong:wall-ura-indicators (cl-mahjong::deal-wall (cl-mahjong:game-deal game)))
                            :events (loop for event in events
                                          when (member (first event) '(:discard :call))
                                            collect (recorded-event event))
                            :result (recorded-result game events seats (reverse signalled))))))))

(defun record-game (variant seed)
  "A seeded self-play game in the log format."
  (let ((rng (cl-mahjong::make-rng seed))
        (deals '()))
    (multiple-value-bind (game events) (cl-mahjong:start-game variant :seed seed)
      (loop
        (multiple-value-bind (next deal) (record-deal game events rng)
          (push deal deals)
          (setf game next))
        (when (cl-mahjong:game-over-p game) (return))
        (setf (values game events) (cl-mahjong:next-deal game)))
      (let ((final (cl-mahjong:game-final game)))
        (list :id (format nil "self-~a" seed) :deals (reverse deals)
              :owari (list :points (coerce (getf final :points) 'list)
                           :scores (coerce (getf final :scores) 'list)))))))

(test self-play-games-replay
  (dolist (seed '(1 2 3 4))
    (dolist (*greedy* '(nil t))
      (let ((data (record-game 'tenhou seed)))
        (is (null (replay-game data)) "seed ~a greedy ~a: ~a" seed *greedy* (replay-game data))))))

(test replay-reports-the-first-divergence
  (let* ((data (record-game 'tenhou 1))
         (deal (first (getf data :deals))))
    (setf (getf deal :points) (list 1 2 3 4)
          (first (getf data :deals)) deal)
    (is (search "points" (replay-game data)))))
