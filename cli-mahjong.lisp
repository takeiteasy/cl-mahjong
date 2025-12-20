;; cli-mahjong.lisp
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

(in-package #:cli-mahjong)

(defstruct ui-state
  (selected-tile-index 0)
  (last-key nil)
  (human-seat :south)  ; Which seat the human player occupies
  (hand-position nil)  ; (row col) of where human hand is drawn
  (active-modal nil)   ; Current modal dialog (modal-state or nil)
  (riichi-pending nil) ; T if player confirmed riichi and is choosing tile to discard
  (valid-riichi-discards nil))  ; List of tiles that maintain tempai for riichi

(defparameter *debug-ui* t)

(defparameter *game* nil
  "The current game state.")

(defun tile-to-string (tile)
  "Convert a tile to a displayable string."
  (let ((suit (tile-suit tile))
        (value (tile-value tile)))
    (cond
      ((eq suit :man) (format nil "~Am" value))
      ((eq suit :pin) (format nil "~Ap" value))
      ((eq suit :sou) (format nil "~As" value))
      ;; Honor tiles - check the tile itself, not the suit
      ((eq tile :east) "E ")
      ((eq tile :south) "S ")
      ((eq tile :west) "W ")
      ((eq tile :north) "N ")
      ((eq tile :white) "Wh")
      ((eq tile :green) "Gr")
      ((eq tile :red) "Re")
      (t "??"))))

(defun get-tile-color-pair (tile)
  "Return the color pair (fg bg) for a tile based on its suit."
  (let ((suit (tile-suit tile)))
    (cond
      ((eq suit :man) '(:red :black))      ; red for characters
      ((eq suit :pin) '(:blue :black))     ; blue for dots
      ((eq suit :sou) '(:green :black))    ; green for bamboo
      ;; Dragons - check the tile itself
      ((eq tile :red) '(:red :black))      ; red dragon
      ((eq tile :green) '(:green :black))  ; green dragon
      ((eq tile :white) '(:yellow :black)) ; white dragon (yellow)
      (t nil))))                           ; default for winds

(defun draw-tile-block (scr tile row col &optional (hidden nil) (first-in-row nil) (last-in-row nil) (highlighted nil) (is-valid nil))
  "Draw a single tile as a block using ACS box-drawing characters with shared borders.
   Format (horizontal):
   ┌──┬──┐  (first ── middle ── last)
   │XX│XX│
   └──┴──┘
   HIGHLIGHTED makes the tile reverse video (selected).
   IS-VALID highlights the tile border in green (valid for riichi discard)."
  (let ((tile-str (if hidden
                      "XX"
                      (tile-to-string tile)))
        (start-col (if first-in-row col (1+ col)))
        (border-attrs (if is-valid '(:altcharset :bold) '(:altcharset))))
    ;; Top border
    (croatoan:move scr row start-col)
    (when first-in-row
      (croatoan:add-char scr #\l :attributes border-attrs))  ; upper-left corner ┌
    (croatoan:add-char scr #\q :attributes border-attrs)  ; horizontal ─
    (croatoan:add-char scr #\q :attributes border-attrs)  ; horizontal ─
    (if last-in-row
        (croatoan:add-char scr #\k :attributes border-attrs)  ; upper-right corner ┐
        (croatoan:add-char scr #\w :attributes border-attrs))  ; top tee ┬

    ;; Middle with tile content
    (croatoan:move scr (+ row 1) start-col)
    (when first-in-row
      (croatoan:add-char scr #\x :attributes border-attrs))  ; vertical │
    (let ((attrs (cond
                   ((and highlighted is-valid) '(:reverse :bold))
                   (highlighted '(:reverse))
                   (is-valid '(:bold))
                   (t nil))))
      (if hidden
          (croatoan:add-string scr tile-str :attributes attrs)
          (croatoan:add-string scr tile-str :color-pair (if is-valid '(:green :black) (get-tile-color-pair tile)) :attributes attrs)))
    (croatoan:add-char scr #\x :attributes border-attrs)  ; vertical │

    ;; Bottom border
    (croatoan:move scr (+ row 2) start-col)
    (when first-in-row
      (croatoan:add-char scr #\m :attributes border-attrs))  ; lower-left corner └
    (croatoan:add-char scr #\q :attributes border-attrs)  ; horizontal ─
    (croatoan:add-char scr #\q :attributes border-attrs)  ; horizontal ─
    (if last-in-row
        (croatoan:add-char scr #\j :attributes border-attrs)  ; lower-right corner ┘
        (croatoan:add-char scr #\v :attributes border-attrs))))  ; bottom tee ┴

(defun draw-tile-block-vertical (scr tile row col &optional (hidden nil) (first-in-column nil) (last-in-column nil))
  "Draw a single tile as a block using ACS box-drawing characters with shared borders (vertical stacking).
   Format (vertical):
   ┌──┐  <- first
   │XX│
   ├──┤  <- junction
   │XX│  <- middle
   ├──┤  <- junction
   │XX│
   └──┘  <- last"
  (let ((tile-str (if hidden
                      "XX"
                      (tile-to-string tile))))
    ;; Top border (only for first tile, others share from previous tile's bottom)
    (when first-in-column
      (croatoan:move scr row col)
      (croatoan:add-char scr #\l :attributes '(:altcharset))  ; upper-left corner ┌
      (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
      (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
      (croatoan:add-char scr #\k :attributes '(:altcharset))) ; upper-right corner ┐

    ;; Middle with tile content
    (croatoan:move scr (+ row 1) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; vertical │
    (if hidden
        (croatoan:add-string scr tile-str)
        (croatoan:add-string scr tile-str :color-pair (get-tile-color-pair tile)))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; vertical │

    ;; Bottom border
    (croatoan:move scr (+ row 2) col)
    (if last-in-column
        (progn
          ;; Last tile: use corners └──┘
          (croatoan:add-char scr #\m :attributes '(:altcharset))  ; lower-left corner └
          (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
          (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
          (croatoan:add-char scr #\j :attributes '(:altcharset))) ; lower-right corner ┘
        (progn
          ;; Not last: use left/right tees ├──┤
          (croatoan:add-char scr #\t :attributes '(:altcharset))  ; left tee ├
          (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
          (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
          (croatoan:add-char scr #\u :attributes '(:altcharset))))))  ; right tee ┤

(defun draw-hand-horizontal (scr player row col &optional (hidden nil) (selected-index nil) (valid-tiles nil))
  "Draw a player's hand horizontally (for top/bottom positions).
   If hand has 14 tiles, draw first 13 together and 14th tile separate.
   VALID-TILES is a list of tiles to highlight as valid for riichi discard."
  (let* ((hand (player-hand player))
         (num-tiles (length hand))
         (has-14th (= num-tiles 14))
         ;; Split into main hand (13 tiles) and drawn tile (14th) if applicable
         ;; Hand is already sorted with drawn tile at position 14
         (main-hand (if has-14th (subseq hand 0 13) hand))
         (drawn-tile (when has-14th (nth 13 hand))))
    ;; Draw player info: name, seat, score
    (croatoan:move scr row col)
    (croatoan:add-string scr (format nil "~A [~A] ~D pts"
                                     (player-name player)
                                     (player-seat player)
                                     (player-score player)))

    ;; Draw main hand (13 tiles or fewer)
    (loop for tile in main-hand
          for i from 0
          for is-valid = (and valid-tiles (member tile valid-tiles :test #'eq))
          for is-selected = (and selected-index (= i selected-index))
          do (draw-tile-block scr tile (+ row 1) (+ col (* i 3))
                              hidden
                              (= i 0)
                              (= i (1- (length main-hand)))
                              is-selected
                              is-valid))  ; Pass validity for highlighting

    ;; Draw 14th tile separately with gap if present
    (when drawn-tile
      (let ((gap-offset 2)  ; 2 character gap
            (is-valid (and valid-tiles (member drawn-tile valid-tiles :test #'eq))))
        (draw-tile-block scr drawn-tile (+ row 1)
                        (+ col (* (length main-hand) 3) gap-offset)
                        hidden
                        t    ; first-in-row (standalone)
                        t    ; last-in-row (standalone)
                        (and selected-index (= selected-index 13))
                        is-valid)))))

(defun draw-hand-vertical (scr player row col &optional (hidden nil) (align-right nil))
  "Draw a player's hand vertically (for left/right positions).
   If align-right is true, name is right-aligned to fit on screen.
   If hand has 14 tiles, draw first 13 together and 14th tile separate."
  (let* ((hand (player-hand player))
         (num-tiles (length hand))
         (has-14th (= num-tiles 14))
         ;; Split into main hand (13 tiles) and drawn tile (14th) if applicable
         ;; Hand is already sorted with drawn tile at position 14
         (main-hand (if has-14th (subseq hand 0 13) hand))
         (drawn-tile (when has-14th (nth 13 hand)))
         (player-info (format nil "~A [~A] ~D"
                              (player-name player)
                              (player-seat player)
                              (player-score player)))
         (name-col (if align-right
                       (max 0 (- (+ col 4) (length player-info)))  ; Right-align to end at tile right edge
                       col)))
    ;; Draw player info: name, seat, score
    (croatoan:move scr row name-col)
    (croatoan:add-string scr player-info)

    ;; Draw main hand (13 tiles or fewer) - first tile is 3 rows, subsequent tiles are 2 rows (shared border)
    (loop for tile in main-hand
          for i from 0
          do (draw-tile-block-vertical scr tile
                                       (+ row 2 (* i 2))  ; spacing: 2 rows per tile (1 row shared)
                                       col
                                       hidden
                                       (= i 0)
                                       (= i (1- (length main-hand)))))

    ;; Draw 14th tile separately with gap if present
    (when drawn-tile
      (let ((gap-rows 1))  ; 1 row gap
        (draw-tile-block-vertical scr drawn-tile
                                  (+ row 2 (* (length main-hand) 2) gap-rows)
                                  col
                                  hidden
                                  t    ; first-in-column (standalone)
                                  t)))))

(defun init-new-game (&key (sanma nil) (human-seat nil))
  "Initialize a new game.
   SANMA specifies 3-player variant (default: nil for 4-player yonma).
   HUMAN-SEAT specifies which seat the human occupies (randomly assigned if nil)."
  (let* ((variant (if sanma :sanma :yonma))
         (game-id (format nil "game-~A" (get-universal-time)))
         (game (create-game-state game-id variant))
         (seats (if sanma
                    '(:east :south :west)
                    '(:east :south :west :north)))
         ;; Randomly assign human seat if not specified
         (actual-human-seat (or human-seat (nth (random (length seats)) seats))))
    ;; Add players to game - human gets name "You", others get AI names
    (loop for seat in seats
          for i from 0
          for name = (if (eq seat actual-human-seat)
                         "You"
                         (format nil "AI ~A" (1+ i)))
          for id = (format nil "player-~A" seat)
          do (add-player-to-game game id name seat)
             (player-set-ready (get-player game seat) t))
    (start-game game)
    (values game actual-human-seat)))

(defun get-clicked-tile-index (mouse-y mouse-x hand-row hand-col hand-size)
  "Determine which tile was clicked based on mouse coordinates.
   Returns tile index or NIL if click was not on a tile.
   Each tile is 3 chars wide (including shared border), 3 rows tall."
  (when (and (>= mouse-y (+ hand-row 1))
             (<= mouse-y (+ hand-row 2)))  ; Middle row of tiles
    (let* ((main-hand-size (if (= hand-size 14) 13 hand-size))
           (main-hand-width (* main-hand-size 3))
           (relative-x (- mouse-x hand-col)))
      (cond
        ;; Click on main hand (first 13 tiles)
        ((and (>= relative-x 0)
              (< relative-x main-hand-width))
         (let ((tile-index (floor relative-x 3)))
           (when (< tile-index main-hand-size)
             tile-index)))
        ;; Click on 14th tile (if present)
        ((= hand-size 14)
         (let* ((gap-offset 2)
                (tile-14-start (+ main-hand-width gap-offset))
                (tile-14-end (+ tile-14-start 4)))  ; 4 chars wide (standalone)
           (when (and (>= relative-x tile-14-start)
                      (< relative-x tile-14-end))
             13)))))))

(defun handle-input (game ui-state event)
  "Handle user input and return action result"
  (handler-case
      (let* ((phase (game-phase game))
             (human-seat (ui-state-human-seat ui-state))
             (human-player (get-player game human-seat))
             (current-seat (player-seat (current-player game)))
             (is-human-turn (eq current-seat human-seat))
             ;; Extract key info from event - handle all cases safely
             (key-name (cond
                         ((typep event 'croatoan:event)
                          (let ((key (croatoan:event-key event)))
                            (cond
                              ((null key) nil)
                              ((typep key 'croatoan:key) (croatoan:key-name key))
                              (t key))))  ; key is already a character or keyword
                         ((typep event 'croatoan:key)
                          (croatoan:key-name event))
                         (t event))))
        ;; Store last key for debugging
        (setf (ui-state-last-key ui-state) key-name)
        
        ;; Check if modal is active - route input there first
        (when (ui-state-active-modal ui-state)
          (let ((result (handle-modal-input (ui-state-active-modal ui-state) key-name)))
            (unless (eq result :continue)
              ;; Modal returned a selection - process it
              (handle-modal-result game ui-state result))
            (return-from handle-input :continue)))
        
        (cond
          ;; Always available commands
          ((or (eql key-name #\q) (eql key-name #\Q)) :quit)
          ((or (eql key-name #\r) (eql key-name #\R)) :refresh)
          ((or (eql key-name #\n) (eql key-name #\N)) :new-game)

          ;; Mouse events - only during human's turn in discard phase
          ((and is-human-turn (eq phase :turn-discard) (eq key-name :button-1-click))
           (handle-mouse-click game ui-state event :single)
           :continue)

          ((and is-human-turn (eq phase :turn-discard) (eq key-name :button-1-double-click))
           (handle-mouse-click game ui-state event :double)
           :continue)

          ;; Arrow keys for tile selection (only in discard phase AND human's turn)
          ((and is-human-turn (eq phase :turn-discard) (eq key-name :left))
           (let ((hand-size (length (player-hand human-player))))
             (when (> hand-size 0)
               (setf (ui-state-selected-tile-index ui-state)
                     (mod (1- (ui-state-selected-tile-index ui-state)) hand-size))))
           :continue)

          ((and is-human-turn (eq phase :turn-discard) (eq key-name :right))
           (let ((hand-size (length (player-hand human-player))))
             (when (> hand-size 0)
               (setf (ui-state-selected-tile-index ui-state)
                     (mod (1+ (ui-state-selected-tile-index ui-state)) hand-size))))
           :continue)

          ;; Enter to discard (only in discard phase AND human's turn)
          ((and is-human-turn (eq phase :turn-discard) (or (eql key-name #\Newline) (eql key-name #\Return)))
           (handle-discard game ui-state)
           :continue)

          ;; Escape key - cancel riichi-pending and reopen modal
          ((and is-human-turn (eq phase :turn-discard)
                (ui-state-riichi-pending ui-state)
                (eq key-name :escape))
           ;; Clear riichi-pending and reopen the modal
           (setf (ui-state-riichi-pending ui-state) nil
                 (ui-state-valid-riichi-discards ui-state) nil
                 (ui-state-active-modal ui-state) (make-riichi-modal))
           :continue)

          ;; Default - ignore unrecognized input
          (t :continue)))
    ;; Catch any errors during input handling and continue
    (error (e)
      (when *debug-ui*
        (setf (ui-state-last-key ui-state) (format nil "ERROR: ~A" e)))
      :continue)))

(defun process-turn-phase (game human-seat &optional ui-state)
  "Process automatic turn actions based on current phase.
   HUMAN-SEAT is the seat occupied by the human player.
   UI-STATE is optional but required for showing modals."
  ;; Don't process if modal is active
  (when (and ui-state (ui-state-active-modal ui-state))
    (return-from process-turn-phase nil))
  
  (let ((phase (game-phase game))
        (current (current-player game))
        (current-seat (player-seat (current-player game)))
        (human-player (get-player game human-seat))
        (wall (game-wall game)))
    (cond
      ;; Auto-draw for human player
      ((and (eq phase :turn-draw)
            (eq current-seat human-seat))
       (let ((drawn-tile (wall-draw-tile wall)))
         (when drawn-tile
           (player-add-tile human-player drawn-tile)
           (auto-extract-north-if-needed game human-player)
           (player-sort-hand-keep-drawn human-player)
           
           ;; Check for tsumo opportunity (player has complete hand)
           (cond
             ;; Winning hand - show tsumo modal
             ((and ui-state (check-tsumo-opportunity human-player))
              (setf (ui-state-active-modal ui-state) (make-tsumo-modal)))
             
             ;; Player is in riichi - must auto-discard (can't choose)
             ((player-riichi-declared human-player)
              ;; Auto-discard the drawn tile (last in sorted hand)
              (let ((discard-tile (find-safe-riichi-discard human-player)))
                (when discard-tile
                  (player-discard-tile human-player discard-tile)
                  (setf (game-phase game) :call-window)
                  (advance-turn game)
                  (setf (game-phase game) :turn-draw))))
             
             ;; Check if can declare riichi (closed hand and has valid discards that maintain tempai)
             ((and ui-state 
                   (player-can-declare-riichi human-player)
                   (calculate-valid-riichi-discards human-player))  ; Non-nil means at least one valid discard
              (setf (ui-state-active-modal ui-state) (make-riichi-modal))
              (setf (game-phase game) :turn-discard))
             
             ;; Normal discard phase
             (t
              (setf (game-phase game) :turn-discard))))))

      ;; AI auto-draw (just draw, don't discard yet)
      ((and (eq phase :turn-draw)
            (not (eq current-seat human-seat)))
       (let ((drawn-tile (wall-draw-tile wall)))
         (when drawn-tile
           (player-add-tile current drawn-tile)
           (auto-extract-north-if-needed game current)
           (player-sort-hand current)
           
           ;; Check if human can ron on AI's discard (after AI discards)
           ;; Transition to discard phase, but don't discard yet
           (setf (game-phase game) :turn-discard))))

      ;; AI auto-discard (separate phase to allow rendering of 14 tiles)
      ((and (eq phase :turn-discard)
            (not (eq current-seat human-seat)))
       ;; Discard first tile (simple AI)
       (let ((first-tile (first (player-hand current))))
         (when first-tile
           (player-discard-tile current first-tile)
           
           ;; Check if human can ron on this discard
           (when (and ui-state 
                      (check-ron-opportunity human-player first-tile)
                      (not (player-furiten human-player)))
             (setf (ui-state-active-modal ui-state) (make-ron-modal)))
           
           ;; Skip call window and advance turn
           (setf (game-phase game) :call-window)
           (advance-turn game)
           (setf (game-phase game) :turn-draw)))))))

(defun handle-mouse-click (game ui-state event click-type)
  "Handle mouse click on tiles. CLICK-TYPE is :single or :double."
  (handler-case
      (let* ((human-seat (ui-state-human-seat ui-state))
             (human-player (get-player game human-seat))
             (hand-size (length (player-hand human-player)))
             (mouse-y (croatoan:position-y event))
             (mouse-x (croatoan:position-x event)))
        ;; Only proceed if we have valid mouse coordinates and hand position
        (when (and mouse-y mouse-x (ui-state-hand-position ui-state))
          (destructuring-bind (hand-row hand-col) (ui-state-hand-position ui-state)
            (let ((tile-index (get-clicked-tile-index mouse-y mouse-x hand-row hand-col hand-size)))
              (when tile-index
                (case click-type
                  (:single
                   ;; Single click - select tile
                   (setf (ui-state-selected-tile-index ui-state) tile-index))
                  (:double
                   ;; Double click - select and discard
                   (setf (ui-state-selected-tile-index ui-state) tile-index)
                   (handle-discard game ui-state))))))))
    ;; Silently ignore errors in mouse handling
    (error () nil)))

(defun handle-discard (game ui-state)
  "Handle tile discard from UI"
  (let* ((human-seat (ui-state-human-seat ui-state))
         (human-player (get-player game human-seat))
         (hand (player-hand human-player))  ; Use hand as-is, already sorted with drawn tile at end
         (selected-index (ui-state-selected-tile-index ui-state))
         (selected-tile (nth selected-index hand))
         (riichi-pending (ui-state-riichi-pending ui-state))
         (valid-discards (ui-state-valid-riichi-discards ui-state)))
    (when (and selected-tile
               (< selected-index (length hand)))
      
      ;; If riichi is pending, check if selected tile is valid
      (when riichi-pending
        (unless (member selected-tile valid-discards :test #'eq)
          ;; Invalid tile for riichi - don't discard (for now, could add chombo later)
          (return-from handle-discard nil)))
      
      ;; Discard the tile
      (player-discard-tile human-player selected-tile)
      
      ;; If riichi was pending, now officially declare it
      (when riichi-pending
        (let ((turn-state (game-turn-state game)))
          (player-declare-riichi human-player (turn-turn-number turn-state))
          (incf (game-riichi-sticks game)))
        ;; Clear riichi-pending state
        (setf (ui-state-riichi-pending ui-state) nil
              (ui-state-valid-riichi-discards ui-state) nil))
      
      ;; Sort the remaining hand (now 13 tiles)
      (player-sort-hand human-player)

      ;; Skip call window - advance directly to next turn
      (setf (game-phase game) :call-window)
      (advance-turn game)
      (setf (game-phase game) :turn-draw)

      ;; Reset selection
      (setf (ui-state-selected-tile-index ui-state) 0)

      t)))

;;; ============================================================================
;;; Modal Result Handling
;;; ============================================================================

(defun handle-modal-result (game ui-state result)
  "Process the result of a modal dialog selection."
  (let* ((modal (ui-state-active-modal ui-state))
         (title (when modal (modal-state-title modal))))
    ;; Clear the modal
    (setf (ui-state-active-modal ui-state) nil)
    
    (cond
      ;; Riichi decision
      ((string= title "RIICHI?")
       (if (string= result "RIICHI")
           ;; Enter riichi-pending mode - player must choose tile to discard
           (let* ((human-seat (ui-state-human-seat ui-state))
                  (human-player (get-player game human-seat)))
             (setf (ui-state-riichi-pending ui-state) t)
             (setf (ui-state-valid-riichi-discards ui-state)
                   (calculate-valid-riichi-discards human-player)))
           ;; Skip was chosen - clear riichi pending state
           (setf (ui-state-riichi-pending ui-state) nil
                 (ui-state-valid-riichi-discards ui-state) nil)))
      
      ;; Tsumo decision
      ((string= title "TSUMO!")
       (when (string= result "TSUMO")
         ;; Player wins by tsumo - show win dialog
         (let* ((human-seat (ui-state-human-seat ui-state))
                (human-player (get-player game human-seat)))
           (setf (ui-state-active-modal ui-state)
                 (make-win-modal (player-name human-player)))
           (setf (game-phase game) :hand-ended))))
      
      ;; Ron decision
      ((string= title "RON!")
       (when (string= result "RON")
         ;; Player wins by ron - show win dialog
         (let* ((human-seat (ui-state-human-seat ui-state))
                (human-player (get-player game human-seat)))
           (setf (ui-state-active-modal ui-state)
                 (make-win-modal (player-name human-player)))
           (setf (game-phase game) :hand-ended))))
      
      ;; Win acknowledgment
      ((string= title "WIN!")
       ;; For now, just start a new round (placeholder)
       ;; TODO: implement proper round transition
       nil))))

;;; ============================================================================
;;; Win Detection
;;; ============================================================================

(defun check-tsumo-opportunity (player)
  "Check if player's current hand (with 14 tiles) is a winning hand.
   Returns T if the hand is complete."
  (and (= (length (player-hand player)) 14)
       (player-has-winning-hand player)))

(defun check-ron-opportunity (player tile)
  "Check if adding TILE to player's hand would create a winning hand.
   Also checks that player is not in furiten."
  (let ((test-hand (cons tile (player-hand player))))
    (and (validate-hand test-hand (player-melds player))
         (not (player-check-furiten player tile)))))

(defun find-safe-riichi-discard (player)
  "Find the drawn tile for auto-discard during riichi.
   The drawn tile is always at position 14 (index 13) since the hand
   is sorted with the drawn tile kept separate at the end."
  (let ((hand (player-hand player)))
    (when (= (length hand) 14)
      (nth 13 hand))))  ; Drawn tile is at the end

(defun calculate-valid-riichi-discards (player)
  "Calculate which tiles can be discarded to declare riichi while maintaining tempai.
   Returns a list of tiles that keep the hand in tempai after discarding."
  (let ((hand (player-hand player))
        (melds (player-melds player))
        (valid-tiles nil))
    (dolist (tile hand)
      ;; Try removing this tile and check if still in tempai
      (let ((test-hand (remove tile hand :test #'eq :count 1)))
        (when (zerop (calculate-shanten test-hand melds))
          (pushnew tile valid-tiles :test #'eq))))
    valid-tiles))

(defun draw-discards-horizontal (scr discards row col &optional (tiles-per-row 10) (reverse-horizontal nil) (stack-upward nil))
  "Draw discards in horizontal rows (for top/bottom players).
   Discards are arranged in rows of TILES-PER-ROW tiles.
   If REVERSE-HORIZONTAL is T, arranges right-to-left (for opposite player).
   If STACK-UPWARD is T, newer rows appear above older rows."
  (when discards
    (loop for tile in discards
          for i from 0
          for row-num = (floor i tiles-per-row)
          for col-num-base = (mod i tiles-per-row)
          for col-num = (if reverse-horizontal
                            (- (1- tiles-per-row) col-num-base)
                            col-num-base)
          for is-first = (if reverse-horizontal
                             (or (= col-num-base (1- tiles-per-row))  ; last logical = first visual
                                 (= i (1- (length discards))))
                             (= col-num-base 0))  ; first logical = first visual
          for is-last = (if reverse-horizontal
                            (= col-num-base 0)  ; first logical = last visual
                            (or (= col-num-base (1- tiles-per-row))  ; last logical = last visual
                                (= i (1- (length discards)))))
          for actual-row = (if stack-upward
                               (- row (* row-num 2)) ;; was 3
                               (+ row (* row-num 2))) ;; was 3 (gapped)
          do (draw-tile-block scr tile
                              actual-row
                              (+ col (* col-num 3))
                              nil  ; not hidden
                              is-first
                              is-last))))  ; or last tile overall

(defun draw-discards-vertical (scr discards row col &optional (tiles-per-column 6) (reverse-vertical nil) (stack-leftward nil))
  "Draw discards in vertical columns (for left/right players).
   Discards are arranged in columns of TILES-PER-COLUMN tiles.
   If REVERSE-VERTICAL is T, arranges bottom-to-top (for right player).
   If STACK-LEFTWARD is T, newer columns appear to the left of older columns."
  (when discards
    (loop for tile in discards
          for i from 0
          for col-num = (floor i tiles-per-column)
          for row-num-base = (mod i tiles-per-column)
          for row-num = (if reverse-vertical
                            (- (1- tiles-per-column) row-num-base)
                            row-num-base)
          for is-first = (if reverse-vertical
                             (or (= row-num-base (1- tiles-per-column))  ; last logical = first visual
                                 (= i (1- (length discards))))
                             (= row-num-base 0))  ; first logical = first visual
          for is-last = (if reverse-vertical
                            (= row-num-base 0)  ; first logical = last visual
                            (or (= row-num-base (1- tiles-per-column))  ; last logical = last visual
                                (= i (1- (length discards)))))
          for actual-col = (if stack-leftward
                               (- col (* col-num 3)) ;; was 4
                               (+ col (* col-num 3))) ;; was 4 (gapped)
          do (draw-tile-block-vertical scr tile
                                       (+ row (* row-num 2))
                                       actual-col
                                       nil  ; not hidden
                                       is-first
                                       is-last))))  ; or last tile overall

(defun draw-center-info (scr game row col)
  "Draw a centered info box with game information."
  (let* ((round-wind (game-round-wind game))
         (dealer-seat (game-dealer-seat game))
         (wall (game-wall game))
         (tiles-remaining (wall-tiles-remaining wall))
         (honba (game-honba game))
         (riichi-sticks (game-riichi-sticks game)))
    ;; Draw box with info
    ;; Top border
    (croatoan:move scr row col)
    (croatoan:add-char scr #\l :attributes '(:altcharset))  ; ┌
    (loop repeat 28 do (croatoan:add-char scr #\q :attributes '(:altcharset)))  ; ─
    (croatoan:add-char scr #\k :attributes '(:altcharset))  ; ┐

    ;; Round wind
    (croatoan:move scr (+ row 1) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
    (croatoan:add-string scr (format nil " Round: ~12A        " round-wind))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │

    ;; Dealer
    (croatoan:move scr (+ row 2) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
    (croatoan:add-string scr (format nil " Dealer: ~11A        " dealer-seat))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │

    ;; Tiles remaining
    (croatoan:move scr (+ row 3) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
    (croatoan:add-string scr (format nil " Tiles left: ~7D        " tiles-remaining))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │

    ;; Honba counter
    (croatoan:move scr (+ row 4) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
    (croatoan:add-string scr (format nil " Honba: ~12D        " honba))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │

    ;; Riichi sticks
    (croatoan:move scr (+ row 5) col)
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
    (croatoan:add-string scr (format nil " Riichi: ~11D        " riichi-sticks))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │

    ;; Bottom border
    (croatoan:move scr (+ row 6) col)
    (croatoan:add-char scr #\m :attributes '(:altcharset))  ; └
    (loop repeat 28 do (croatoan:add-char scr #\q :attributes '(:altcharset)))  ; ─
    (croatoan:add-char scr #\j :attributes '(:altcharset))))  ; ┘

(defun draw-game (scr game &optional ui-state)
  "Draw the entire game screen with mahjong table layout.
   Human player is always drawn at bottom, others arranged around them."
  (croatoan:clear scr)

  (when game
    (let* ((config (game-config game))
           (seats (game-config-seats config))
           (num-players (length seats))
           (screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           (selected-index (when ui-state (ui-state-selected-tile-index ui-state)))
           (human-seat (when ui-state (ui-state-human-seat ui-state)))
           (current-seat (player-seat (current-player game)))
           (phase (game-phase game))
           ;; Calculate relative positions
           (bottom-seat human-seat)
           (right-seat (next-seat human-seat config))
           (top-seat (next-seat right-seat config))
           (left-seat (when (= num-players 4) (next-seat top-seat config))))

      ;; Draw current turn indicator at top
      (when *debug-ui*
        (croatoan:move scr 0 0)
        (croatoan:add-string scr (format nil "Turn: ~A | Phase: ~A | Selected: ~A | Last key: ~S"
                                         current-seat
                                         phase
                                         (when ui-state (ui-state-selected-tile-index ui-state))
                                         (when ui-state (ui-state-last-key ui-state)))))

      (let* ((bottom-player (get-player game bottom-seat))
                (right-player (get-player game right-seat))
                (top-player (get-player game top-seat))
                (left-player (get-player game left-seat))
                ;; Calculate positions dynamically
                ;; Horizontal hand: 3 rows tall, width = 3*tiles + 1 (approx)
                (top-hand-len (when top-player (length (player-hand top-player))))
                (bottom-hand-len (when bottom-player (length (player-hand bottom-player))))
                (top-width (when top-hand-len (+ 1 (* 3 top-hand-len))))
                (bottom-width (when bottom-hand-len (+ 1 (* 3 bottom-hand-len))))
                (top-col (when top-width (max 0 (- (floor screen-width 2) (floor top-width 2)))))
                (bottom-col (when bottom-width (max 0 (- (floor screen-width 2) (floor bottom-width 2)))))
                ;; Vertical hand: 4 chars wide, height = 2*tiles + 1
                (right-hand-len (when right-player (length (player-hand right-player))))
                (left-hand-len (when left-player (length (player-hand left-player))))
                (right-height (when right-hand-len (+ 1 (* 2 right-hand-len))))
                (left-height (when left-hand-len (+ 1 (* 2 left-hand-len))))
                (right-row (when right-height (max 0 (- (floor screen-height 2) (floor right-height 2)))))
                (left-row (when left-height (max 0 (- (floor screen-height 2) (floor left-height 2)))))
                (right-col (max 0 (- screen-width 5)))  ; Right edge - 5 chars for tile width
                (left-col 0))  ; Left edge

           ;; Top player - hidden
           (when top-player
             (draw-hand-horizontal scr top-player 1 top-col t))

           ;; Left player - hidden
           (when left-player
             (draw-hand-vertical scr left-player left-row left-col t nil))  ; Left-aligned

           ;; Right player - hidden
           (when right-player
             (draw-hand-vertical scr right-player right-row right-col t t))  ; Right-aligned

           ;; Bottom player (human) - visible
           (when bottom-player
             (let ((hand-row (max 0 (- screen-height 4)))
                   (valid-tiles (when ui-state (ui-state-valid-riichi-discards ui-state))))
               (draw-hand-horizontal scr bottom-player hand-row bottom-col nil selected-index valid-tiles)
               ;; Store hand position for mouse click handling
               (when ui-state
                 (setf (ui-state-hand-position ui-state) (list hand-row bottom-col))))))

      ;; Draw center info box and discards around it
      (let* ((center-row (max 0 (- (floor screen-height 2) 3)))
             (center-col (max 0 (- (floor screen-width 2) 15)))
             ;; Center box dimensions: 7 rows tall, 30 chars wide
             (center-box-height 7)
             (center-box-width 30)
             ;; Get players for discard rendering
             (bottom-player (get-player game bottom-seat))
             (top-player (get-player game top-seat))
             (right-player (get-player game right-seat))
             (left-player (when (= num-players 4) (get-player game left-seat))))

        ;; Draw center info box
        (draw-center-info scr game center-row center-col)

        ;; Draw discards around center box
        ;; Bottom player discards: below center box (closer to their hand)
        (when bottom-player
          (let ((discards (reverse (player-discards bottom-player)))  ; Reverse list for correct order
                (discard-row (+ center-row center-box-height))  ; 1 row below center box
                (discard-col center-col ))  ; Closer to center
            (draw-discards-horizontal scr discards discard-row discard-col 10 nil)))

        ;; Top player discards: above center box (closer to their hand)
        ;; From their perspective: oldest on left (our right), newest on right (our left)
        ;; Stack upward (away from center)
        (when top-player
          (let ((discards (reverse (player-discards top-player)))  ; Reverse list AND use horizontal reverse
                (discard-row (max 0 (- center-row 3)))  ; 4 rows above center box
                (discard-col (- center-col 1)))  ; More to the right
            (draw-discards-horizontal scr discards discard-row discard-col 10 t t)))  ; reversed horizontal, stack upward

        ;; Right player discards: to the right of center box
        (when right-player
          (let ((discards (reverse (player-discards right-player)))  ; Reverse list for correct order
                (discard-row (- center-row 6))  ; Closer to center
                (discard-col (+ center-col center-box-width)))  ; Closer to center
            (draw-discards-vertical scr discards discard-row discard-col 6 t)))  ; reversed vertical

        ;; Left player discards (4-player only): to the left of center box
        ;; Stack leftward (away from center)
        (when left-player
          (let ((discards (reverse (player-discards left-player)))  ; Reverse list for correct order
                (discard-row (- center-row 0))  ; Closer to center
                (discard-col (max 0 (- center-col 5))))  ; More to the right
            (draw-discards-vertical scr discards discard-row discard-col 6 nil t))))
    
    ;; Draw modal overlay if active
    (when (and ui-state (ui-state-active-modal ui-state))
      (draw-modal scr (ui-state-active-modal ui-state))))

  (croatoan:refresh scr)))

(defun main-loop (scr &key (sanma nil))
  "Main event loop with optional game configuration."
  (multiple-value-bind (game human-seat)
      (init-new-game :sanma sanma)
    (setf *game* game)
    (let ((ui-state (make-ui-state :human-seat human-seat))
          (last-ai-turn-time 0))
      (loop
         ;; Auto-process turn phases (draw tiles, etc.)
         ;; Add delay for AI turns to make them visible
         (let ((current-seat (player-seat (current-player *game*)))
               (current-time (get-internal-real-time)))
           (when (not (eq current-seat human-seat))
             ;; AI player turn - add 1000ms delay between turns
             (when (> (- current-time last-ai-turn-time)
                      (* 1.0 internal-time-units-per-second))
               (process-turn-phase *game* human-seat ui-state)
               (setf last-ai-turn-time current-time)))
           (when (eq current-seat human-seat)
             ;; Human player turn - process immediately
             (process-turn-phase *game* human-seat ui-state)))

       ;; Render game state
       (draw-game scr *game* ui-state)
       (croatoan:refresh scr)

       ;; Handle input (non-blocking, 100ms timeout from screen init)
       (let* ((event (croatoan:get-event scr))
              (ch (when event (slot-value event 'croatoan::key))))
         (cond
           ;; Handle resize events
           ((eq ch :resize)
            nil)

           ;; Handle all other input
           ((and ch (not (eq ch :resize)))
            (let ((result (handle-input *game* ui-state event)))
              (case result
                (:quit (return))
                (:new-game
                 (multiple-value-bind (new-game new-human-seat)
                     (init-new-game :sanma sanma)
                   (setf *game* new-game
                         ui-state (make-ui-state :human-seat new-human-seat))))
                (:refresh nil)
                (t nil))))))))))

(defun main (&key (sanma nil))
  "Entry point for the TUI client.

   Optional arguments:
   - SANMA: If T, start 3-player sanma game. Default: NIL (4-player yonma)

   Examples:
   (main)            ; Start 4-player yonma game
   (main :sanma t)   ; Start 3-player sanma game"
  (handler-case
      (croatoan:with-screen (scr :input-echoing nil
                                 :input-blocking 100
                                 :enable-colors t
                                 :cursor-visible nil)
        ;; Enable mouse events
        (croatoan:set-mouse-event '(:button-1-click :button-1-double-click))
        (main-loop scr :sanma sanma))
    (sb-sys:interactive-interrupt ()
      ;; Handle Ctrl-C gracefully
      (format t "~%Interrupted.~%"))
    (error (e)
      (format t "Error: ~A~%" e)
      nil)))
