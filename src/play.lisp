(in-package #:cl-mahjong)

;;; Combinations

(defun combinations (list n)
  (cond ((zerop n) '(()))
        ((null list) '())
        (t (append (mapcar (lambda (rest) (cons (first list) rest))
                           (combinations (rest list) (1- n)))
                   (combinations (rest list) n)))))

(defun distinct-tile-sets (game sets)
  "SETS of tile ids without those that differ only by tile ids of the same kind and attributes."
  (let ((tileset (game-tileset game)))
    (remove-duplicates sets :from-end t :test #'equal
                            :key (lambda (set)
                                   (mapcar (lambda (tile)
                                             (cons (tile-kind-id tileset tile)
                                                   (tile-attrs tileset tile)))
                                           set)))))

(defun tiles-of-kind-in (game tiles kind-id)
  (remove kind-id tiles :key (lambda (tile) (tile-kind-id (game-tileset game) tile))
                        :test-not #'eql))

;;; Calls

(defun chi-pairs (game tile)
  "Kind id pairs that form a sequence with the kind of TILE."
  (let* ((tileset (game-tileset game))
         (kind (tile-kind tileset tile)))
    (when (eq (kind-category kind) :suit)
      (flet ((id (offset)
               (let ((other (find-kind tileset (kind-suit kind) (+ (kind-rank kind) offset))))
                 (and other (kind-id other)))))
        (loop for (a b) in '((-2 -1) (-1 1) (1 2))
              for first = (id a)
              for second = (id b)
              when (and first second) collect (list first second))))))

(defun kuikae-trap-p (game player type tile own-tiles)
  "Whether every tile left after the call would be forbidden to discard."
  (let ((forbidden (kuikae-kinds game type tile (sorted-kind-ids game (cons tile own-tiles)))))
    (and forbidden
         (every (lambda (kept) (member (tile-kind-id (game-tileset game) kept) forbidden))
                (remove-tiles (player-tiles player) own-tiles)))))

(defun open-call-options (game player tile)
  (let* ((tileset (game-tileset game))
         (id (tile-kind-id tileset tile))
         (same (tiles-of-kind-in game (player-tiles player) id))
         (wall-live (not (wall-exhausted-p (deal-wall (game-deal game)))))
         (options '()))
    (when (and wall-live (>= (length same) 2))
      (dolist (pair (distinct-tile-sets game (combinations same 2)))
        (unless (kuikae-trap-p game player :triplet tile pair)
          (push (cons :pon pair) options))))
    (when (and wall-live (>= (length same) 3) (kan-possible-p game))
      (push (cons :daiminkan (subseq same 0 3)) options))
    (nreverse options)))

(defun chi-options (game player tile)
  (when (and (opt game :chi) (not (wall-exhausted-p (deal-wall (game-deal game)))))
    (let ((options '()))
      (dolist (pair (chi-pairs game tile))
        (dolist (set (distinct-tile-sets
                      game
                      (loop for a in (tiles-of-kind-in game (player-tiles player) (first pair))
                            append (loop for b in (tiles-of-kind-in game (player-tiles player)
                                                                    (second pair))
                                         collect (list a b)))))
          (unless (kuikae-trap-p game player :sequence tile set)
            (push (cons :chi set) options))))
      (nreverse options))))

(defun kan-possible-p (game)
  "A kan needs a replacement tile and a live wall tile to refill it."
  (let ((wall (deal-wall (game-deal game))))
    (and (not (wall-exhausted-p wall))
         (wall-draw-replacement wall)
         t)))

(defun ron-option-p (game deal seat tile &key chankan kokushi-only)
  (let ((player (deal-player deal seat)))
    (and (not (eq (player-riichi player) :pending))
         (let ((counts (hand-counts (game-tileset game) (cons tile (player-tiles player)))))
           (complete-p (game-tileset game) (variant-shapes (game-variant game))
                       counts (player-melds player)))
         (not (furiten-p game deal seat))
         (let ((score (best-win game deal seat tile :chankan chankan)))
           (and score
                (or (not kokushi-only)
                    (find :kokushi (reading-yaku (score-reading score)) :key #'car)))))))

(defun seat-claim-options (game seat tile from &key kan ankan kita)
  "Legal responses of SEAT to TILE discarded (or added to a kan, or set aside as KITA) by FROM, or NIL when it has none.
A closed kan, ANKAN, can only be robbed for a kokushi win."
  (let* ((deal (game-deal game))
         (player (deal-player deal seat))
         (options '()))
    (when (ron-option-p game deal seat tile :chankan kan :kokushi-only ankan)
      (push (list :ron) options))
    (unless (or kan kita (eq (player-riichi player) :active))
      (setf options (append (reverse (open-call-options game player tile)) options))
      (when (= seat (seat-after game from))
        (setf options (append (reverse (chi-options game player tile)) options))))
    (when options
      (append (nreverse options) (list (list :pass))))))

;;; Legal actions

(defun nuki-possible-p (game)
  "A set-aside tile needs a live wall tile and a nuki slot for its replacement."
  (let ((wall (deal-wall (game-deal game))))
    (and (opt game :nukidora)
         (not (wall-exhausted-p wall))
         (wall-draw-nuki wall)
         t)))

(defun kita-tile (game deal seat)
  "The north SEAT would set aside: the drawn one if it is north, else any in hand. NIL when it may not."
  (let* ((player (deal-player deal seat))
         (north (find-kind (game-tileset game) 'north))
         (drawn (deal-drawn deal)))
    (when (and north (nuki-possible-p game))
      (let ((norths (tiles-of-kind-in game (player-tiles player) (kind-id north))))
        (cond ((member drawn norths) drawn)
              ((eq (player-riichi player) :active) nil)
              (t (first norths)))))))

(defun forbidden-discard-p (game deal tile)
  (member (tile-kind-id (game-tileset game) tile) (deal-forbidden deal)))

(defun discard-candidates (game deal seat)
  (let* ((player (deal-player deal seat))
         (tiles (player-tiles player)))
    (if (eq (player-riichi player) :active)
        (and (deal-drawn deal) (list (deal-drawn deal)))
        (remove-if (lambda (tile) (forbidden-discard-p game deal tile)) tiles))))

(defun riichi-candidates (game deal seat candidates)
  (let* ((player (deal-player deal seat))
         (variant (game-variant game))
         (tileset (game-tileset game))
         (cache '()))
    (when (and (deal-drawn deal)
               (null (player-riichi player))
               (player-closed-p player)
               (>= (aref (deal-points deal) seat) (opt game :riichi-cost))
               (>= (wall-live-count (deal-wall deal)) (opt game :riichi-min-tiles)))
      (remove-if-not
       (lambda (tile)
         (let ((id (tile-kind-id tileset tile)))
           (unless (assoc id cache)
             (let ((counts (hand-counts tileset (remove tile (player-tiles player)))))
               (push (cons id (eql 0 (shanten tileset (variant-shapes variant) counts
                                              (player-melds player))))
                     cache)))
           (cdr (assoc id cache))))
       candidates))))

(defun ankan-kinds (game deal seat)
  (let* ((player (deal-player deal seat))
         (tileset (game-tileset game))
         (drawn (deal-drawn deal))
         (counts (hand-counts tileset (player-tiles player))))
    (when (kan-possible-p game)
      (loop for kind across (tileset-kinds tileset)
            for id = (kind-id kind)
            when (and (= (aref counts id) 4)
                      (or (not (eq (player-riichi player) :active))
                          (riichi-kan-keeps-waits-p game player drawn id)))
              collect id))))

(defun riichi-kan-keeps-waits-p (game player drawn id)
  (let* ((tileset (game-tileset game))
         (before (hand-counts tileset (remove drawn (player-tiles player))))
         (after (hand-counts tileset (remove id (player-tiles player)
                                             :key (lambda (tile) (tile-kind-id tileset tile))))))
    (and (= id (tile-kind-id tileset drawn))
         (let ((quad (make-meld :quad (make-list 4 :initial-element id))))
           (equal (winning-kinds game before (player-melds player) :nuki (player-nuki player))
                  (winning-kinds game after (append (player-melds player) (list quad))
                                 :nuki (player-nuki player)))))))

(defun shouminkan-tiles (game deal seat)
  (let* ((player (deal-player deal seat))
         (tileset (game-tileset game)))
    (when (kan-possible-p game)
      (loop for tile in (player-tiles player)
            when (find-if (lambda (meld)
                            (and (eq (meld-type meld) :triplet) (meld-open-p meld)
                                 (eql (first (meld-kinds meld)) (tile-kind-id tileset tile))))
                          (player-melds player))
              collect tile))))

(defun turn-actions (game seat)
  (let* ((deal (game-deal game))
         (player (deal-player deal seat))
         (drawn (deal-drawn deal))
         (candidates (discard-candidates game deal seat))
         (actions (mapcar (lambda (tile) (list :discard tile)) candidates)))
    (when drawn
      (when (best-win game deal seat drawn :tsumo t)
        (push (list :tsumo) actions))
      (setf actions
            (append actions
                    (mapcar (lambda (tile) (list :riichi tile))
                            (riichi-candidates game deal seat candidates))
                    (and (kyuushu-p game deal seat) (list (list :kyuushu)))
                    (and (kita-tile game deal seat) (list (list :kita)))
                    (mapcar (lambda (id) (list :ankan id)) (ankan-kinds game deal seat))
                    (unless (eq (player-riichi player) :active)
                      (mapcar (lambda (tile) (list :shouminkan tile))
                              (shouminkan-tiles game deal seat))))))
    actions))

(defun legal-actions (game seat)
  "Every action SEAT may take now, or NIL when it is not SEAT's decision."
  (let ((deal (game-deal game)))
    (when (eq (game-phase game) :playing)
      (case (deal-phase deal)
        (:turn (when (= seat (deal-turn deal)) (turn-actions game seat)))
        ((:claim :chankan)
         (let ((claim (deal-claim deal)))
           (unless (assoc seat (claim-responses claim))
             (cdr (assoc seat (claim-options claim))))))))))

(defun deciding-seats (game)
  "Seats that still owe an action."
  (let ((deal (game-deal game)))
    (when (eq (game-phase game) :playing)
      (case (deal-phase deal)
        (:turn (list (deal-turn deal)))
        ((:claim :chankan)
         (let ((claim (deal-claim deal)))
           (loop for (seat) in (claim-options claim)
                 unless (assoc seat (claim-responses claim)) collect seat)))))))

;;; Applying actions

(defun apply-action (game seat action)
  "(values game events). Signals ILLEGAL-ACTION unless ACTION is one of `legal-actions' for SEAT."
  ;; TODO: recomputes every legal action to validate one; check the action directly (https://github.com/takeiteasy/cl-mahjong/issues/8)
  (unless (member action (legal-actions game seat) :test #'equal)
    (illegal "~s is not a legal action for seat ~a" action seat))
  (let* ((*events* '())
         (deal (game-deal game))
         (new (if (eq (deal-phase deal) :turn)
                  (ecase (first action)
                    (:discard (discard-tile game seat (second action) nil))
                    (:riichi (discard-tile game seat (second action) t))
                    (:tsumo (declare-tsumo game seat))
                    (:kyuushu (abort-deal game :kyuushu))
                    (:kita (declare-kita game seat))
                    (:ankan (declare-ankan game seat (second action)))
                    (:shouminkan (declare-shouminkan game seat (second action))))
                  (respond game seat action))))
    (values new (reverse *events*))))

(defun declare-tsumo (game seat)
  (let* ((deal (game-deal game))
         (tile (deal-drawn deal))
         (score (best-win game deal seat tile :tsumo t
                          :honba (game-honba game) :sticks (deal-sticks deal))))
    (conclude-win game deal (list seat) (list (list* seat score nil)))))

;;; Discards

(defun discard-tile (game seat tile riichi)
  (let* ((deal (game-deal game))
         (first-turn (first-turn-p deal (deal-player deal seat)))
         (game (updating-deal (deal game)
                 (let ((deal (updating-player (player deal seat)
                               (setf (player-tiles player) (remove tile (player-tiles player))
                                     (player-discards player) (append (player-discards player)
                                                                      (list tile))
                                     (player-temp-furiten player) nil)
                               (when (eq (player-riichi player) :active)
                                 (setf (player-ippatsu player) nil))
                               (when riichi
                                 (setf (player-riichi player) :pending
                                       (player-double-riichi player) first-turn)))))
                   (setf (deal-drawn deal) nil
                         (deal-forbidden deal) nil
                         (deal-rinshan deal) nil)
                   deal))))
    (emit :discard :seat seat :tile tile :riichi riichi)
    (open-claim (if (eq (opt game :kan-dora) :on-discard) (reveal-pending-dora game) game)
                tile seat)))

(defun open-claim (game tile from)
  (let ((options (loop for seat below (game-seats game)
                       unless (= seat from)
                         append (let ((options (seat-claim-options game seat tile from)))
                                  (and options (list (cons seat options)))))))
    (if (null options)
        (after-discard game from)
        (updating-deal (deal game)
          (let ((deal (copy-deal deal)))
            (setf (deal-phase deal) :claim
                  (deal-claim deal) (make-claim :kind :discard :tile tile :from from
                                                :options options))
            deal)))))

(defun establish-riichi (game seat)
  (let ((player (deal-player (game-deal game) seat)))
    (if (eq (player-riichi player) :pending)
        (let ((game (updating-deal (deal game)
                      (let ((deal (updating-player (player deal seat)
                                    (setf (player-riichi player) :active
                                          (player-ippatsu player) t))))
                        (setf (deal-points deal) (apply-deltas (deal-points deal)
                                                               (list (cons seat (- (opt game :riichi-cost)))))
                              (deal-sticks deal) (1+ (deal-sticks deal)))
                        deal))))
          (emit :riichi :seat seat)
          game)
        game)))

(defun reveal-doras (game count)
  (updating-deal (deal game)
    (let ((deal (copy-deal deal)))
      (dotimes (i count)
        (let ((wall (wall-reveal-dora (deal-wall deal))))
          (when wall
            (setf (deal-wall deal) wall)
            (emit :dora :tile (car (last (wall-dora-indicators wall)))))))
      deal)))

(defun reveal-pending-dora (game)
  (let ((count (deal-pending-dora (game-deal game))))
    (if (zerop count)
        game
        (reveal-doras (updating-deal (deal game)
                        (let ((deal (copy-deal deal)))
                          (setf (deal-pending-dora deal) 0)
                          deal))
                      count))))

(defun suufon-renda-p (game deal)
  (let ((discards (loop for player across (deal-players deal) append (player-discards player)))
        (tileset (game-tileset game)))
    (and (not (deal-interrupted deal))
         (= (length discards) (game-seats game))
         (let ((kinds (tile-kind-ids game discards)))
           (and (every (lambda (id) (= id (first kinds))) kinds)
                (eq :wind (kind-group (tileset-kind tileset (first kinds)))))))))

(defun suucha-riichi-p (deal)
  (every (lambda (player) (eq (player-riichi player) :active)) (deal-players deal)))

(defun suukaikan-p (deal)
  (and (= (deal-kans deal) 4)
       (> (count-if (lambda (player) (find :quad (player-melds player) :key #'meld-type))
                    (deal-players deal))
          1)))

(defun riichi-abort-reason (game)
  (let ((deal (game-deal game)))
    (and (opt game :suucha-riichi) (suucha-riichi-p deal) :suucha-riichi)))

(defun abortive-reason (game)
  "Why the deal is abandoned after a discard that nobody called, or NIL."
  (let ((deal (game-deal game)))
    (or (riichi-abort-reason game)
        (and (opt game :suufon-renda) (suufon-renda-p game deal) :suufon-renda)
        (and (opt game :suukaikan) (suukaikan-p deal) :suukaikan))))

(defun after-discard (game from)
  "The discard of FROM was not called or won on."
  (let* ((game (reveal-pending-dora (establish-riichi game from)))
         (reason (abortive-reason game)))
    (if reason
        (abort-deal game reason)
        (draw-tile game (seat-after game from)))))

(defun kyuushu-p (game deal seat)
  (let* ((player (deal-player deal seat))
         (tileset (game-tileset game)))
    (and (opt game :kyuushu)
         (first-turn-p deal player)
         (>= (length (remove-duplicates
                      (remove-if-not (lambda (id)
                                       (let ((kind (tileset-kind tileset id)))
                                         (or (kind-honor-p kind) (terminal-p tileset kind))))
                                     (tile-kind-ids game (player-tiles player)))))
             9))))

;;; Claims

(defun respond (game seat action)
  (let ((game (updating-deal (deal game)
                (let ((deal (copy-deal deal))
                      (claim (copy-claim (deal-claim deal))))
                  (push (cons seat action) (claim-responses claim))
                  (setf (deal-claim deal) claim)
                  deal))))
    (let ((claim (deal-claim (game-deal game))))
      (if (every (lambda (entry) (assoc (car entry) (claim-responses claim)))
                 (claim-options claim))
          (resolve-claim game)
          game))))

(defun passed-on-ron-p (claim seat)
  (and (member (list :ron) (cdr (assoc seat (claim-options claim))) :test #'equal)
       (not (equal (list :ron) (cdr (assoc seat (claim-responses claim)))))))

(defun mark-passed-rons (game claim)
  "A seat that passed on a ron it could make is furiten: for a turn, or for good in riichi."
  (let ((game game))
    (loop for (seat) in (claim-options claim)
          when (passed-on-ron-p claim seat)
            do (setf game (updating-deal (deal game)
                            (updating-player (player deal seat)
                              (setf (player-temp-furiten player) t)
                              (when (eq (player-riichi player) :active)
                                (setf (player-riichi-furiten player) t))))))
    game))

(defun ron-seats (game claim)
  "Seats that claimed ron, nearest to the discarder first."
  (sort (loop for (seat . action) in (claim-responses claim)
              when (equal action (list :ron)) collect seat)
        #'< :key (lambda (seat) (seat-distance game (claim-from claim) seat))))

(defun find-call (claim)
  (let ((calls (remove-if-not (lambda (entry) (member (first (cdr entry)) '(:pon :daiminkan :chi)))
                              (claim-responses claim))))
    (or (find-if (lambda (entry) (member (first (cdr entry)) '(:pon :daiminkan))) calls)
        (first calls))))

(defun resolve-claim (game)
  (let* ((claim (deal-claim (game-deal game)))
         (game (mark-passed-rons game claim))
         (rons (ron-seats game claim)))
    (cond (rons (resolve-ron game claim rons))
          ((eq (claim-kind claim) :kan)
           (finish-shouminkan game (claim-from claim) (claim-tile claim)))
          ((eq (claim-kind claim) :ankan)
           (finish-ankan game (claim-from claim) (claim-tile claim)))
          ((eq (claim-kind claim) :kita)
           (finish-kita game (claim-from claim) (claim-tile claim)))
          (t (let ((call (find-call claim)))
               (if call
                   (let* ((game (establish-riichi game (claim-from claim)))
                          (reason (riichi-abort-reason game)))
                     (if reason
                         (abort-deal game reason)
                         (execute-call (reveal-pending-dora game) (car call) (cdr call))))
                   (after-discard game (claim-from claim))))))))

(defun resolve-ron (game claim rons)
  (let* ((deal (game-deal game))
         (maximum (ecase (opt game :multi-ron) (:head-bump 1) (:double 2) (:triple 3)))
         (chankan (member (claim-kind claim) '(:kan :ankan))))
    (if (and (> (length rons) maximum) (= (length rons) 3) (opt game :sanchahou))
        (abort-deal game :sanchahou)
        (let ((winners (subseq rons 0 (min maximum (length rons)))))
          (conclude-win game deal winners
                        (loop for winner in winners
                              for headed = t then nil
                              collect (list* winner
                                             (best-win game deal winner (claim-tile claim)
                                                       :chankan chankan
                                                       :honba (if headed (game-honba game) 0)
                                                       :sticks (if headed (deal-sticks deal) 0))
                                             (claim-from claim))))))))

;;; Calls

(defun break-ippatsu (deal)
  (let ((players (map 'simple-vector
                      (lambda (player)
                        (if (player-ippatsu player)
                            (let ((copy (copy-player player)))
                              (setf (player-ippatsu copy) nil)
                              copy)
                            player))
                      (deal-players deal)))
        (new (copy-deal deal)))
    (setf (deal-players new) players)
    new))

(defun remove-tiles (tiles removed)
  (remove-if (lambda (tile) (member tile removed)) tiles))

(defun add-meld (deal seat meld removed)
  "DEAL with MELD added to SEAT, REMOVED taken from its hand, and the deal marked as interrupted."
  (let ((deal (updating-player (player deal seat)
                (setf (player-tiles player) (remove-tiles (player-tiles player) removed)
                      (player-melds player) (append (player-melds player) (list meld))))))
    (setf deal (break-ippatsu deal))
    (setf (deal-interrupted deal) t
          (deal-kans deal) (+ (deal-kans deal) (if (eq (meld-type meld) :quad) 1 0)))
    deal))

(defun sorted-kind-ids (game tiles)
  (sort (tile-kind-ids game tiles) #'<))

(defun suji-kind-id (game tile kinds)
  "The kind just past the far end of the sequence TILE completes, or NIL for a middle tile."
  (let* ((tileset (game-tileset game))
         (kind (tile-kind tileset tile))
         (ranks (mapcar (lambda (id) (kind-rank (tileset-kind tileset id))) kinds))
         (low (reduce #'min ranks))
         (high (reduce #'max ranks))
         (rank (kind-rank kind))
         (other (cond ((= rank low) (find-kind tileset (kind-suit kind) (1+ high)))
                      ((= rank high) (find-kind tileset (kind-suit kind) (1- low))))))
    (and other (kind-id other))))

(defun kuikae-kinds (game type tile kinds)
  (let ((mode (opt game :kuikae)))
    (when mode
      (remove nil (list (tile-kind-id (game-tileset game) tile)
                        (and (eq mode :forbid) (eq type :sequence)
                             (suji-kind-id game tile kinds)))))))

(defun mark-called-away (deal from)
  (updating-player (player deal from)
    (incf (player-called-away player))))

(defun execute-call (game seat action)
  (let* ((claim (deal-claim (game-deal game)))
         (tile (claim-tile claim))
         (from (claim-from claim))
         (own (rest action))
         (type (ecase (first action) (:pon :triplet) (:chi :sequence) (:daiminkan :quad)))
         (tiles (cons tile own))
         (kinds (sorted-kind-ids game tiles))
         (meld (make-meld type kinds :open t :from from :tiles tiles))
         (game (updating-deal (deal game)
                 (let ((deal (mark-called-away (add-meld deal seat meld own) from)))
                   (setf (deal-turn deal) seat
                         (deal-phase deal) :turn
                         (deal-drawn deal) nil
                         (deal-rinshan deal) nil
                         (deal-claim deal) nil
                         (deal-forbidden deal)
                         (and (not (eq type :quad)) (kuikae-kinds game type tile kinds)))
                   deal))))
    (emit :call :type (first action) :seat seat :from from :tiles tiles)
    (if (eq type :quad)
        (after-kan game seat t)
        game)))

(defun after-kan (game seat open)
  (let ((game (cond ((or (not open) (eq (opt game :kan-dora) :immediate))
                     (reveal-doras game 1))
                    (t (updating-deal (deal game)
                         (let ((deal (copy-deal deal)))
                           (incf (deal-pending-dora deal))
                           deal))))))
    (replacement-draw game seat)))

(defun declare-ankan (game seat id)
  (let* ((game (reveal-pending-dora game))
         (tiles (tiles-of-kind-in game (player-tiles (deal-player (game-deal game) seat)) id))
         (options (and (opt game :ankan-chankan)
                       (loop for other below (game-seats game)
                             unless (= other seat)
                               append (let ((options (seat-claim-options game other (first tiles) seat
                                                                         :kan t :ankan t)))
                                        (and options (list (cons other options))))))))
    (if (null options)
        (finish-ankan game seat (first tiles))
        (updating-deal (deal game)
          (let ((deal (copy-deal deal)))
            (setf (deal-phase deal) :chankan
                  (deal-claim deal) (make-claim :kind :ankan :tile (first tiles) :from seat
                                                :options options))
            deal)))))

(defun finish-ankan (game seat tile)
  (let* ((id (tile-kind-id (game-tileset game) tile))
         (tiles (tiles-of-kind-in game (player-tiles (deal-player (game-deal game) seat)) id))
         (meld (make-meld :quad (make-list 4 :initial-element id) :tiles tiles))
         (game (updating-deal (deal game)
                 (let ((deal (add-meld deal seat meld tiles)))
                   (setf (deal-claim deal) nil
                         (deal-phase deal) :turn
                         (deal-turn deal) seat)
                   deal))))
    (emit :call :type :ankan :seat seat :tiles tiles)
    (after-kan game seat nil)))

(defun declare-shouminkan (game seat tile)
  (let* ((game (reveal-pending-dora game))
         (game (updating-deal (deal game)
                 (updating-player (player deal seat)
                   (setf (player-tiles player) (remove tile (player-tiles player))))))
         (options (loop for other below (game-seats game)
                        unless (= other seat)
                          append (let ((options (seat-claim-options game other tile seat :kan t)))
                                   (and options (list (cons other options)))))))
    (if (null options)
        (finish-shouminkan game seat tile)
        (updating-deal (deal game)
          (let ((deal (copy-deal deal)))
            (setf (deal-phase deal) :chankan
                  (deal-claim deal) (make-claim :kind :kan :tile tile :from seat
                                                :options options))
            deal)))))

(defun finish-shouminkan (game seat tile)
  (let* ((id (tile-kind-id (game-tileset game) tile))
         (player (deal-player (game-deal game) seat))
         (pon (find-if (lambda (meld) (and (eq (meld-type meld) :triplet) (meld-open-p meld)
                                           (eql (first (meld-kinds meld)) id)))
                       (player-melds player)))
         (quad (make-meld :quad (make-list 4 :initial-element id) :open t
                          :from (meld-from pon) :tiles (append (meld-tiles pon) (list tile))))
         (game (updating-deal (deal game)
                 (let ((deal (updating-player (player deal seat)
                               (setf (player-melds player) (substitute quad pon (player-melds player))))))
                   (setf deal (break-ippatsu deal))
                   (setf (deal-interrupted deal) t
                         (deal-kans deal) (1+ (deal-kans deal))
                         (deal-claim deal) nil
                         (deal-phase deal) :turn
                         (deal-turn deal) seat)
                   deal))))
    (emit :call :type :shouminkan :seat seat :tiles (meld-tiles quad))
    (after-kan game seat t)))

;;; Nukidora

(defun declare-kita (game seat)
  (let* ((game (reveal-pending-dora game))
         (tile (kita-tile game (game-deal game) seat))
         (game (updating-deal (deal game)
                 (updating-player (player deal seat)
                   (setf (player-tiles player) (remove tile (player-tiles player))))))
         (options (and (opt game :kita-ron)
                       (loop for other below (game-seats game)
                             unless (= other seat)
                               append (let ((options (seat-claim-options game other tile seat
                                                                         :kita t)))
                                        (and options (list (cons other options))))))))
    (if (null options)
        (finish-kita game seat tile)
        (updating-deal (deal game)
          (let ((deal (copy-deal deal)))
            (setf (deal-phase deal) :chankan
                  (deal-claim deal) (make-claim :kind :kita :tile tile :from seat
                                                :options options))
            deal)))))

(defun finish-kita (game seat tile)
  (let ((game (updating-deal (deal game)
                (let ((deal (updating-player (player deal seat)
                              (setf (player-nuki player) (append (player-nuki player)
                                                                 (list tile))))))
                  (unless (opt game :kita-ippatsu)
                    (setf deal (break-ippatsu deal)))
                  (when (opt game :kita-interrupts)
                    (setf (deal-interrupted deal) t))
                  (setf (deal-claim deal) nil
                        (deal-phase deal) :turn
                        (deal-turn deal) seat)
                  deal))))
    (emit :kita :seat seat :tile tile)
    (nuki-draw game seat)))
