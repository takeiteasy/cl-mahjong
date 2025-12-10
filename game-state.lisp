;; game-state.lisp
;; Game state management for Japanese Riichi Mahjong
;;
;; Copyright (C) 2025 George Watson
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:cl-mahjong)

(defun create-game-state (game-id)
  "Create a new game state instance"
  (make-instance 'game-state :game-id game-id))

(defun add-player-to-game (game player-id name seat)
  "Add a player to the game at the specified seat.
   Returns the player instance if successful, NIL if seat is occupied."
  (let ((players (game-players game)))
    (when (gethash seat players)
      (return-from add-player-to-game nil))

    (let ((player (create-player player-id name seat)))
      (setf (gethash seat players) player)
      (format t "~&[GAME ~A] Player ~A (~A) joined at ~A~%"
              (game-id game) name player-id seat)
      player)))

(defun remove-player-from-game (game seat)
  "Remove a player from the game at the specified seat.
   Returns T if player was removed, NIL if no player at that seat."
  (let ((players (game-players game)))
    (when (gethash seat players)
      (remhash seat players)
      (format t "~&[GAME ~A] Player at ~A left the game~%"
              (game-id game) seat)
      t)))

(defun get-player (game seat)
  "Get the player at the specified seat"
  (gethash seat (game-players game)))

(defun get-player-by-id (game player-id)
  "Find a player by their ID. Returns (values player seat) or (values NIL NIL)"
  (maphash (lambda (seat player)
             (when (string= (player-id player) player-id)
               (return-from get-player-by-id (values player seat))))
           (game-players game))
  (values nil nil))

(defun game-player-count (game)
  "Return the number of players in the game"
  (hash-table-count (game-players game)))

(defun game-is-full (game)
  "Check if the game has 4 players"
  (= (game-player-count game) 4))

(defun all-players-ready (game)
  "Check if all 4 players are ready"
  (and (game-is-full game)
       (every #'player-is-ready
              (loop for seat in '(:east :south :west :north)
                    collect (get-player game seat)))))

;;; ============================================================================
;;; Game Flow
;;; ============================================================================

(defun start-game (game)
  "Start the game: create wall, deal initial hands, set phase.
   Returns T if successful, NIL if game cannot start."
  (unless (game-is-full game)
    (format t "~&[GAME ~A] Cannot start: need 4 players~%" (game-id game))
    (return-from start-game nil))

  (unless (all-players-ready game)
    (format t "~&[GAME ~A] Cannot start: not all players ready~%" (game-id game))
    (return-from start-game nil))

  (setf (game-wall game) (create-wall))
  (deal-initial-hands game)
  (setf (game-phase game) :turn-draw)
  (setf (turn-current-player (game-turn-state game))
        (game-dealer-seat game))

  (format t "~&[GAME ~A] Game started! Dealer: ~A~%"
          (game-id game) (game-dealer-seat game))

  t)

(defun deal-initial-hands (game)
  "Deal initial hands to all players.
   Each player gets 13 tiles, dealer gets 14 tiles."
  (let ((wall (game-wall game))
        (dealer-seat (game-dealer-seat game)))
    (dolist (seat '(:east :south :west :north))
      (let ((player (get-player game seat)))
        (dotimes (i 13)
          (player-add-tile player (wall-draw-tile wall)))
        (player-sort-hand player)))
    (let ((dealer (get-player game dealer-seat)))
      (player-add-tile dealer (wall-draw-tile wall))
      (player-sort-hand dealer))

    (format t "~&[GAME ~A] Dealt initial hands~%" (game-id game))))

(defun advance-turn (game)
  "Advance to the next player's turn"
  (let* ((turn-state (game-turn-state game))
         (current (turn-current-player turn-state))
         (next (next-seat current)))
    (setf (turn-current-player turn-state) next)
    (incf (turn-turn-number turn-state))
    (format t "~&[GAME ~A] Turn ~D: ~A's turn~%"
            (game-id game)
            (turn-turn-number turn-state)
            next)))

(defun current-player (game)
  "Get the current player whose turn it is"
  (get-player game (turn-current-player (game-turn-state game))))

(defun process-action (game action)
  "Process a player action. Returns (values success error-message)"
  (setf (game-last-action-at game) (get-universal-time))

  (let ((action-type (action-type action))
        (player-id (action-player-id action))
        (data (action-data action)))

    (format t "~&[GAME ~A] Processing action ~A from ~A~%"
            (game-id game) action-type player-id)

    (case action-type
      (:join-game
       (process-join-game game player-id data))
      (:leave-game
       (process-leave-game game player-id))
      (:ready
       (process-ready game player-id t))
      (:unready
       (process-ready game player-id nil))
      (:discard
       (process-discard game player-id data))
      (:call-pon
       (process-call-pon game player-id data))
      (:call-chi
       (process-call-chi game player-id data))
      (:call-kan
       (process-call-kan game player-id data))
      (:declare-riichi
       (process-declare-riichi game player-id))
      (:pass
       (process-pass game player-id))
      (t
       (values nil (format nil "Unknown action type: ~A" action-type))))))

(defun process-join-game (game player-id data)
  "Handle a player joining the game"
  (let ((name (getf data :name "Player"))
        (seat (getf data :seat)))

    (unless seat
      (return-from process-join-game
        (values nil "No seat specified")))

    (let ((player (add-player-to-game game player-id name seat)))
      (if player
          (progn
            (when (game-is-full game)
              (setf (game-phase game) :ready-check)
              (format t "~&[GAME ~A] All players joined, waiting for ready~%"
                      (game-id game)))
            (values t nil))
          (values nil "Seat already occupied")))))

(defun process-leave-game (game player-id)
  "Handle a player leaving the game"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (if player
        (progn
          (remove-player-from-game game seat)
          ;; Reset game phase if not enough players
          (when (< (game-player-count game) 4)
            (setf (game-phase game) :waiting-for-players))
          (values t nil))
        (values nil "Player not in game"))))

(defun process-ready (game player-id ready)
  "Handle a player ready/unready action"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (declare (ignore seat))
    (if player
        (progn
          (player-set-ready player ready)
          (format t "~&[GAME ~A] Player ~A is ~A~%"
                  (game-id game)
                  (player-name player)
                  (if ready "ready" "not ready"))
          (when (and ready (all-players-ready game))
            (start-game game))
          (values t nil))
        (values nil "Player not in game"))))

(defun process-discard (game player-id data)
  "Handle a player discarding a tile"
  (let ((tile (getf data :tile)))
    (multiple-value-bind (player seat) (get-player-by-id game player-id)
      (cond
        ((not player)
         (values nil "Player not in game"))
        ((not (eq seat (turn-current-player (game-turn-state game))))
         (values nil "Not your turn"))
        ((not (eq (game-phase game) :turn-discard))
         (values nil "Cannot discard now"))
        ((not tile)
         (values nil "No tile specified"))
        (t
         (let ((discarded-tile (intern (string-upcase tile) :keyword)))
           (if (player-discard-tile player discarded-tile)
               (progn
                 (let ((turn-state (game-turn-state game)))
                   (setf (turn-last-discard turn-state) discarded-tile
                         (turn-last-discard-player turn-state) seat))
                 ;; Check for possible calls from other players
                 (check-possible-calls game discarded-tile seat)
                 (setf (game-phase game) :call-window)
                 (values t nil))
               (values nil "Tile not in hand"))))))))

(defun check-possible-calls (game discarded-tile discarder-seat)
  "Check which players can make calls on the discarded tile.
   Updates the turn-state with possible call information."
  (let ((turn-state (game-turn-state game))
        (can-ron nil)
        (can-pon nil)
        (can-chi nil))
    ;; Check each player (except discarder)
    (dolist (seat '(:east :south :west :north))
      (unless (eq seat discarder-seat)
        (let ((player (get-player game seat)))
          (when player
            ;; Check for ron (winning hand)
            (let ((hand-with-tile (cons discarded-tile (player-hand player))))
              (when (and (validate-hand hand-with-tile (player-melds player))
                        (not (player-check-furiten player discarded-tile)))
                (push seat can-ron)))
            ;; Check for pon (needs 2 more of the tile)
            (when (player-can-call-pon player discarded-tile)
              (push seat can-pon))
            ;; Check for chi (only next player)
            (when (eq seat (next-seat discarder-seat))
              (when (player-can-call-chi player discarded-tile seat discarder-seat)
                (setf can-chi seat)))))))
    ;; Update turn state
    (setf (turn-can-call-ron turn-state) can-ron
          (turn-can-call-pon turn-state) can-pon
          (turn-can-call-chi turn-state) can-chi)))

(defun process-call-pon (game player-id data)
  "Handle a player calling pon"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (let ((turn-state (game-turn-state game)))
      (cond
        ((not player)
         (values nil "Player not in game"))
        ((not (eq (game-phase game) :call-window))
         (values nil "Not in call window"))
        ((not (member seat (turn-can-call-pon turn-state)))
         (values nil "Cannot call pon"))
        (t
         (let* ((tile (turn-last-discard turn-state))
                (discarder-seat (turn-last-discard-player turn-state))
                (relation (seat-relation seat discarder-seat)))
           ;; Remove 2 tiles from hand
           (player-remove-tile player tile)
           (player-remove-tile player tile)
           ;; Create pon meld
           (let ((meld (make-meld :type :pon
                                  :tiles (list tile tile tile)
                                  :from relation
                                  :called-tile tile)))
             (player-add-meld player meld))
           ;; Clear call window and give turn to caller
           (setf (game-phase game) :turn-discard
                 (turn-current-player turn-state) seat)
           ;; Cancel ippatsu for all players
           (dolist (s '(:east :south :west :north))
             (let ((p (get-player game s)))
               (when p (player-cancel-ippatsu p))))
           (format t "~&[GAME ~A] Player ~A called pon on ~A~%"
                   (game-id game) (player-name player) tile)
           (values t nil)))))))

(defun process-call-chi (game player-id data)
  "Handle a player calling chi"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (let ((turn-state (game-turn-state game)))
      (cond
        ((not player)
         (values nil "Player not in game"))
        ((not (eq (game-phase game) :call-window))
         (values nil "Not in call window"))
        ((not (eq seat (turn-can-call-chi turn-state)))
         (values nil "Cannot call chi"))
        (t
         (let* ((tile (turn-last-discard turn-state))
                (discarder-seat (turn-last-discard-player turn-state))
                (relation (seat-relation seat discarder-seat))
                (sequences (player-can-call-chi player tile seat discarder-seat))
                (chosen-sequence (or (getf data :sequence) (first sequences))))
           (unless chosen-sequence
             (return-from process-call-chi (values nil "No valid chi sequence")))
           ;; Remove tiles from hand (excluding the called tile)
           (dolist (t1 chosen-sequence)
             (unless (eq t1 tile)
               (player-remove-tile player t1)))
           ;; Create chi meld
           (let ((meld (make-meld :type :chi
                                  :tiles chosen-sequence
                                  :from relation
                                  :called-tile tile)))
             (player-add-meld player meld))
           ;; Clear call window and give turn to caller
           (setf (game-phase game) :turn-discard
                 (turn-current-player turn-state) seat)
           ;; Cancel ippatsu for all players
           (dolist (s '(:east :south :west :north))
             (let ((p (get-player game s)))
               (when p (player-cancel-ippatsu p))))
           (format t "~&[GAME ~A] Player ~A called chi on ~A~%"
                   (game-id game) (player-name player) tile)
           (values t nil)))))))

(defun process-call-kan (game player-id data)
  "Handle a player calling kan (open/closed/added)"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (let* ((turn-state (game-turn-state game))
           (kan-type (getf data :kan-type :open)) ; :open, :closed, or :added
           (tile (if (eq kan-type :open)
                     (turn-last-discard turn-state)
                     (getf data :tile))))
      (cond
        ((not player)
         (values nil "Player not in game"))

        ;; Open kan (calling from discard)
        ((eq kan-type :open)
         (cond
           ((not (eq (game-phase game) :call-window))
            (values nil "Not in call window"))
           ((not (member seat (turn-can-call-pon turn-state)))
            (values nil "Cannot call kan"))
           ((not (player-can-call-kan player tile :open))
            (values nil "Need 3 tiles for open kan"))
           (t
            (let* ((discarder-seat (turn-last-discard-player turn-state))
                   (relation (seat-relation seat discarder-seat)))
              ;; Remove 3 tiles from hand
              (dotimes (i 3)
                (player-remove-tile player tile))
              ;; Create open kan meld
              (let ((meld (make-meld :type :kan-open
                                     :tiles (list tile tile tile tile)
                                     :from relation
                                     :called-tile tile)))
                (player-add-meld player meld))
              ;; Draw replacement tile and track rinshan
              (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
                (player-add-tile player replacement-tile)
                (setf (turn-rinshan-active turn-state) t))
              ;; Cancel ippatsu for all players
              (dolist (s '(:east :south :west :north))
                (let ((p (get-player game s)))
                  (when p (player-cancel-ippatsu p))))
              ;; Give turn to caller
              (setf (game-phase game) :turn-discard
                    (turn-current-player turn-state) seat)
              (format t "~&[GAME ~A] Player ~A called open kan on ~A~%"
                      (game-id game) (player-name player) tile)
              (values t nil)))))

        ;; Closed kan (from own hand during turn)
        ((eq kan-type :closed)
         (cond
           ((not (eq seat (turn-current-player turn-state)))
            (values nil "Not your turn"))
           ((not (eq (game-phase game) :turn-draw))
            (values nil "Can only declare closed kan after drawing"))
           ((not tile)
            (values nil "No tile specified"))
           ((not (player-can-call-kan player tile :closed))
            (values nil "Need 4 tiles for closed kan"))
           (t
            ;; Remove all 4 tiles from hand
            (dotimes (i 4)
              (player-remove-tile player tile))
            ;; Create closed kan meld
            (let ((meld (make-meld :type :kan-closed
                                   :tiles (list tile tile tile tile)
                                   :from nil
                                   :called-tile nil)))
              (player-add-meld player meld))
            ;; Draw replacement tile and track rinshan
            (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
              (player-add-tile player replacement-tile)
              (setf (turn-rinshan-active turn-state) t))
            ;; Stay in draw phase (can tsumo or discard)
            (setf (game-phase game) :turn-draw)
            (format t "~&[GAME ~A] Player ~A declared closed kan on ~A~%"
                    (game-id game) (player-name player) tile)
            (values t nil))))

        ;; Added kan (adding to existing pon)
        ((eq kan-type :added)
         (cond
           ((not (eq seat (turn-current-player turn-state)))
            (values nil "Not your turn"))
           ((not (eq (game-phase game) :turn-draw))
            (values nil "Can only add kan after drawing"))
           ((not tile)
            (values nil "No tile specified"))
           ((not (player-can-call-kan player tile :added))
            (values nil "Need existing pon and 1 more tile"))
           (t
            ;; Remove 1 tile from hand
            (player-remove-tile player tile)
            ;; Find and upgrade existing pon to added kan
            (let ((existing-pon (find-if (lambda (meld)
                                          (and (eq (meld-type meld) :pon)
                                               (member tile (meld-tiles meld) :test #'eq)))
                                        (player-melds player))))
              (setf (meld-type existing-pon) :kan-added
                    (meld-tiles existing-pon) (list tile tile tile tile)))
            ;; Track chankan opportunity for other players
            (setf (turn-chankan-active turn-state) t)
            ;; Check if anyone can rob this kan
            (let ((can-rob nil))
              (dolist (s '(:east :south :west :north))
                (unless (eq s seat)
                  (let ((p (get-player game s)))
                    (when p
                      (let ((hand-with-tile (cons tile (player-hand p))))
                        (when (and (validate-hand hand-with-tile (player-melds p))
                                  (not (player-check-furiten p tile)))
                          (push s can-rob)))))))
              (if can-rob
                  ;; Enter chankan window
                  (progn
                    (setf (turn-can-call-ron turn-state) can-rob
                          (turn-last-discard turn-state) tile
                          (game-phase game) :call-window)
                    (format t "~&[GAME ~A] Player ~A added kan on ~A (chankan possible)~%"
                            (game-id game) (player-name player) tile)
                    (values t nil))
                  ;; No chankan, proceed with rinshan draw
                  (progn
                    (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
                      (player-add-tile player replacement-tile)
                      (setf (turn-rinshan-active turn-state) t))
                    (setf (game-phase game) :turn-draw)
                    (format t "~&[GAME ~A] Player ~A added kan on ~A~%"
                            (game-id game) (player-name player) tile)
                    (values t nil)))))))

        (t
         (values nil "Invalid kan type"))))))


(defun process-declare-riichi (game player-id)
  "Handle a player declaring riichi"
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (declare (ignore seat))
    (cond
      ((not player)
       (values nil "Player not in game"))

      ((not (player-can-declare-riichi player))
       (values nil "Cannot declare riichi (hand open or insufficient points)"))

      ((not (player-is-tenpai player))
       (values nil "Hand is not in tenpai"))

      (t
       ;; Declare riichi
       (player-declare-riichi player (turn-turn-number (game-turn-state game)))
       (incf (game-riichi-sticks game))
       (format t "~&[GAME ~A] Player ~A declared riichi!~%"
               (game-id game) (player-name player))
       (values t nil)))))

(defun process-pass (game player-id)
  "Handle a player passing on a call opportunity.
   When all players pass, the turn advances to the next player."
  (multiple-value-bind (player seat) (get-player-by-id game player-id)
    (let ((turn-state (game-turn-state game)))
      (cond
        ((not player)
         (values nil "Player not in game"))
        ((not (eq (game-phase game) :call-window))
         (values nil "Not in call window"))
        (t
         ;; Remove this player from possible callers
         (setf (turn-can-call-ron turn-state)
               (remove seat (turn-can-call-ron turn-state))
               (turn-can-call-pon turn-state)
               (remove seat (turn-can-call-pon turn-state)))
         (when (eq seat (turn-can-call-chi turn-state))
           (setf (turn-can-call-chi turn-state) nil))

         ;; Check if all players have passed
         (if (and (null (turn-can-call-ron turn-state))
                  (null (turn-can-call-pon turn-state))
                  (null (turn-can-call-chi turn-state)))
             ;; All passed - advance turn
             (progn
               (setf (turn-current-player turn-state)
                     (next-seat (turn-last-discard-player turn-state))
                     (game-phase game) :turn-draw)
               (format t "~&[GAME ~A] All players passed, advancing turn~%"
                       (game-id game))
               (values t nil))
             ;; Still waiting for other players
             (progn
               (format t "~&[GAME ~A] Player ~A passed~%"
                       (game-id game) (player-name player))
               (values t nil))))))))


(defun game-in-progress (game)
  "Check if the game is currently in progress"
  (member (game-phase game)
          '(:dealing :turn-draw :turn-discard :call-window)))

(defun game-waiting-for-players (game)
  "Check if the game is waiting for players"
  (eq (game-phase game) :waiting-for-players))

(defun game-in-ready-check (game)
  "Check if the game is in ready check phase"
  (eq (game-phase game) :ready-check))
