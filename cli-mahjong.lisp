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
  (last-key nil))

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

(defun draw-tile-block (scr tile row col &optional (hidden nil) (first-in-row nil) (last-in-row nil) (highlighted nil))
  "Draw a single tile as a block using ACS box-drawing characters with shared borders.
   Format (horizontal):
   ┌──┬──┐  (first ── middle ── last)
   │XX│XX│
   └──┴──┘"
  (let ((tile-str (if hidden
                      "XX"
                      (tile-to-string tile)))
        (start-col (if first-in-row col (1+ col))))
    ;; Top border
    (croatoan:move scr row start-col)
    (when first-in-row
      (croatoan:add-char scr #\l :attributes '(:altcharset)))  ; upper-left corner ┌
    (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
    (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
    (if last-in-row
        (croatoan:add-char scr #\k :attributes '(:altcharset))  ; upper-right corner ┐
        (croatoan:add-char scr #\w :attributes '(:altcharset)))  ; top tee ┬

    ;; Middle with tile content
    (croatoan:move scr (+ row 1) start-col)
    (when first-in-row
      (croatoan:add-char scr #\x :attributes '(:altcharset)))  ; vertical │
    (let ((attrs (if highlighted '(:reverse) nil)))
      (if hidden
          (croatoan:add-string scr tile-str :attributes attrs)
          (croatoan:add-string scr tile-str :color-pair (get-tile-color-pair tile) :attributes attrs)))
    (croatoan:add-char scr #\x :attributes '(:altcharset))  ; vertical │

    ;; Bottom border
    (croatoan:move scr (+ row 2) start-col)
    (when first-in-row
      (croatoan:add-char scr #\m :attributes '(:altcharset)))  ; lower-left corner └
    (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
    (croatoan:add-char scr #\q :attributes '(:altcharset))  ; horizontal ─
    (if last-in-row
        (croatoan:add-char scr #\j :attributes '(:altcharset))  ; lower-right corner ┘
        (croatoan:add-char scr #\v :attributes '(:altcharset)))))  ; bottom tee ┴

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

(defun draw-hand-horizontal (scr player row col &optional (hidden nil) (selected-index nil))
  "Draw a player's hand horizontally (for top/bottom positions).
   If hand has 14 tiles, draw first 13 together and 14th tile separate."
  (let* ((hand (player-hand player))
         (sorted-hand (sort-tiles hand))
         (num-tiles (length sorted-hand))
         (has-14th (= num-tiles 14))
         ;; Split into main hand (13 tiles) and drawn tile (14th) if applicable
         (main-hand (if has-14th (subseq sorted-hand 0 13) sorted-hand))
         (drawn-tile (when has-14th (nth 13 sorted-hand))))
    ;; Draw player name/seat
    (croatoan:move scr row col)
    (croatoan:add-string scr (player-name player))

    ;; Draw main hand (13 tiles or fewer)
    (loop for tile in main-hand
          for i from 0
          do (draw-tile-block scr tile (+ row 1) (+ col (* i 3))
                              hidden
                              (= i 0)
                              (= i (1- (length main-hand)))
                              (and selected-index (= i selected-index))))

    ;; Draw 14th tile separately with gap if present
    (when drawn-tile
      (let ((gap-offset 2))  ; 2 character gap
        (draw-tile-block scr drawn-tile (+ row 1)
                        (+ col (* (length main-hand) 3) gap-offset)
                        hidden
                        t    ; first-in-row (standalone)
                        t    ; last-in-row (standalone)
                        (and selected-index (= selected-index 13)))))))

(defun draw-hand-vertical (scr player row col &optional (hidden nil) (align-right nil))
  "Draw a player's hand vertically (for left/right positions).
   If align-right is true, name is right-aligned to fit on screen.
   If hand has 14 tiles, draw first 13 together and 14th tile separate."
  (let* ((hand (player-hand player))
         (sorted-hand (sort-tiles hand))
         (num-tiles (length sorted-hand))
         (has-14th (= num-tiles 14))
         ;; Split into main hand (13 tiles) and drawn tile (14th) if applicable
         (main-hand (if has-14th (subseq sorted-hand 0 13) sorted-hand))
         (drawn-tile (when has-14th (nth 13 sorted-hand)))
         (player-name (player-name player))
         (name-col (if align-right
                       (max 0 (- (+ col 4) (length player-name)))  ; Right-align name to end at tile right edge
                       col)))
    ;; Draw player name/seat
    (croatoan:move scr row name-col)
    (croatoan:add-string scr player-name)

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

(defun init-new-game (&key (num-players 4) (player-names nil))
  "Initialize a new game with NUM-PLAYERS players (3 or 4).
   PLAYER-NAMES is an optional list of player names."
  (unless (member num-players '(3 4))
    (error "Number of players must be 3 or 4"))

  (let* ((variant (if (= num-players 3) :sanma :yonma))
         (game-id (format nil "game-~A" (get-universal-time)))
         (game (create-game-state game-id variant))
         (seats (if (= num-players 3)
                    '(:east :south :west)
                    '(:east :south :west :north)))
         (default-names (loop for i from 1 to num-players
                             collect (format nil "Player ~A" i)))
         (names (if player-names
                    (append player-names
                            (subseq default-names (length player-names)))
                    default-names)))
    (loop for seat in seats
          for name in names
          for id = (format nil "player-~A" name)
          do (add-player-to-game game id name seat)
             (player-set-ready (get-player game seat) t))
    (start-game game)
    game))

(defun handle-input (game ui-state ch)
  "Handle user input and return action result"
  (let ((phase (game-phase game))
        (south (get-player game :south))
        ;; Extract key name if ch is a key object
        (key-name (if (typep ch 'croatoan:key)
                      (croatoan:key-name ch)
                      ch)))
    ;; Store last key for debugging
    (setf (ui-state-last-key ui-state) ch)
    (cond
      ;; Always available commands
      ((or (eql key-name #\q) (eql key-name #\Q)) :quit)
      ((or (eql key-name #\r) (eql key-name #\R)) :refresh)
      ((or (eql key-name #\n) (eql key-name #\N)) :new-game)

      ;; Arrow keys for tile selection (only in discard phase)
      ((and (eq phase :turn-discard) (eq key-name :left))
       (let ((hand-size (length (player-hand south))))
         (when (> hand-size 0)
           (setf (ui-state-selected-tile-index ui-state)
                 (mod (1- (ui-state-selected-tile-index ui-state)) hand-size))))
       :continue)

      ((and (eq phase :turn-discard) (eq key-name :right))
       (let ((hand-size (length (player-hand south))))
         (when (> hand-size 0)
           (setf (ui-state-selected-tile-index ui-state)
                 (mod (1+ (ui-state-selected-tile-index ui-state)) hand-size))))
       :continue)

      ;; Enter to discard (only in discard phase)
      ((and (eq phase :turn-discard) (or (eql key-name #\Newline) (eql key-name #\Return)))
       (handle-discard game ui-state)
       :continue)

      ;; Default
      (t :continue))))

(defun process-turn-phase (game)
  "Process automatic turn actions based on current phase"
  (let ((phase (game-phase game))
        (current (current-player game))
        (current-seat (player-seat (current-player game)))
        (south (get-player game :south))
        (wall (game-wall game)))
    (cond
      ;; Auto-draw for south player
      ((and (eq phase :turn-draw)
            (eq current-seat :south))
       (let ((drawn-tile (wall-draw-tile wall)))
         (when drawn-tile
           (player-add-tile south drawn-tile)
           (auto-extract-north-if-needed game south)
           (player-sort-hand south)
           (setf (game-phase game) :turn-discard))))

      ;; AI auto-draw (just draw, don't discard yet)
      ((and (eq phase :turn-draw)
            (not (eq current-seat :south)))
       (let ((drawn-tile (wall-draw-tile wall)))
         (when drawn-tile
           (player-add-tile current drawn-tile)
           (auto-extract-north-if-needed game current)
           (player-sort-hand current)
           ;; Transition to discard phase, but don't discard yet
           (setf (game-phase game) :turn-discard))))

      ;; AI auto-discard (separate phase to allow rendering of 14 tiles)
      ((and (eq phase :turn-discard)
            (not (eq current-seat :south)))
       ;; Discard first tile (simple AI)
       (let ((first-tile (first (player-hand current))))
         (when first-tile
           (player-discard-tile current first-tile)
           ;; Skip call window and advance turn
           (setf (game-phase game) :call-window)
           (advance-turn game)
           (setf (game-phase game) :turn-draw)))))))

(defun handle-discard (game ui-state)
  "Handle tile discard from UI"
  (let* ((south (get-player game :south))
         (sorted-hand (sort-tiles (player-hand south)))
         (selected-index (ui-state-selected-tile-index ui-state))
         (selected-tile (nth selected-index sorted-hand)))
    (when (and selected-tile
               (< selected-index (length sorted-hand)))
      ;; Discard the tile
      (player-discard-tile south selected-tile)

      ;; Skip call window - advance directly to next turn
      (setf (game-phase game) :call-window)
      (advance-turn game)
      (setf (game-phase game) :turn-draw)

      ;; Reset selection
      (setf (ui-state-selected-tile-index ui-state) 0)

      t)))

(defun draw-game (scr game &optional ui-state)
  "Draw the entire game screen with mahjong table layout."
  (croatoan:clear scr)

  (when game
    (let* ((config (game-config game))
           (seats (game-config-seats config))
           (num-players (length seats))
           (screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           (selected-index (when ui-state (ui-state-selected-tile-index ui-state)))
           (current-seat (player-seat (current-player game)))
           (phase (game-phase game)))

      ;; Draw current turn indicator at top
      (croatoan:move scr 0 0)
      (croatoan:add-string scr (format nil "Turn: ~A | Phase: ~A | Selected: ~A | Last key: ~S"
                                       current-seat
                                       phase
                                       (when ui-state (ui-state-selected-tile-index ui-state))
                                       (when ui-state (ui-state-last-key ui-state))))

      ;; Draw help for South player's turn
      (when (and (eq current-seat :south) (eq phase :turn-discard))
        (croatoan:move scr 1 0)
        (croatoan:add-string scr "Use ← → to select tile, Enter to discard"))

      (cond
        ;; 4-player layout
        ((= num-players 4)
         (let* ((east (get-player game :east))
                (south (get-player game :south))
                (west (get-player game :west))
                (north (get-player game :north))
                ;; Calculate positions dynamically
                ;; Horizontal hand: 3 rows tall, width = 3*tiles + 1 (approx)
                (north-hand-len (when north (length (player-hand north))))
                (south-hand-len (when south (length (player-hand south))))
                (north-width (when north-hand-len (+ 1 (* 3 north-hand-len))))
                (south-width (when south-hand-len (+ 1 (* 3 south-hand-len))))
                (north-col (when north-width (max 0 (- (floor screen-width 2) (floor north-width 2)))))
                (south-col (when south-width (max 0 (- (floor screen-width 2) (floor south-width 2)))))
                ;; Vertical hand: 4 chars wide, height = 2*tiles + 1
                (east-hand-len (when east (length (player-hand east))))
                (west-hand-len (when west (length (player-hand west))))
                (east-height (when east-hand-len (+ 1 (* 2 east-hand-len))))
                (west-height (when west-hand-len (+ 1 (* 2 west-hand-len))))
                (east-row (when east-height (max 0 (- (floor screen-height 2) (floor east-height 2)))))
                (west-row (when west-height (max 0 (- (floor screen-height 2) (floor west-height 2)))))
                (east-col (max 0 (- screen-width 5)))  ; Right edge - 5 chars for tile width
                (west-col 0))  ; Left edge

           ;; North player (top) - hidden
           (when north
             (draw-hand-horizontal scr north 3 north-col t))

           ;; West player (left) - hidden
           (when west
             (draw-hand-vertical scr west west-row west-col t nil))  ; Left-aligned

           ;; East player (right) - hidden
           (when east
             (draw-hand-vertical scr east east-row east-col t t))  ; Right-aligned

           ;; South player (bottom) - visible (current player)
           (when south
             (draw-hand-horizontal scr south (max 0 (- screen-height 4)) south-col nil selected-index))))

        ;; 3-player layout (sanma)
        ((= num-players 3)
         (let* ((east (get-player game :east))
                (south (get-player game :south))
                (west (get-player game :west))
                ;; Calculate positions dynamically
                (east-hand-len (when east (length (player-hand east))))
                (south-hand-len (when south (length (player-hand south))))
                (west-hand-len (when west (length (player-hand west))))
                (east-width (when east-hand-len (+ 1 (* 3 east-hand-len))))
                (south-width (when south-hand-len (+ 1 (* 3 south-hand-len))))
                (east-col (when east-width (max 0 (- (floor screen-width 2) (floor east-width 2)))))
                (south-col (when south-width (max 0 (- (floor screen-width 2) (floor south-width 2)))))
                (west-height (when west-hand-len (+ 1 (* 2 west-hand-len))))
                (west-row (when west-height (max 0 (- (floor screen-height 2) (floor west-height 2)))))
                (west-col 0))

           ;; East player (top) - hidden
           (when east
             (draw-hand-horizontal scr east 3 east-col t))

           ;; West player (left) - hidden
           (when west
             (draw-hand-vertical scr west west-row west-col t nil))  ; Left-aligned

           ;; South player (bottom) - visible (current player)
           (when south
             (draw-hand-horizontal scr south (max 0 (- screen-height 4)) south-col nil selected-index))))))

  (croatoan:refresh scr)))

(defun main-loop (scr &key (num-players 4) (player-names nil))
  "Main event loop with optional game configuration."
  (setf *game* (init-new-game :num-players num-players
                              :player-names player-names))
  (let ((ui-state (make-ui-state))
        (last-ai-turn-time 0))
    (loop
       ;; Auto-process turn phases (draw tiles, etc.)
       ;; Add delay for AI turns to make them visible
       (let ((current-seat (player-seat (current-player *game*)))
             (current-time (get-internal-real-time)))
         (when (not (eq current-seat :south))
           ;; AI player turn - add 1000ms delay between turns
           (when (> (- current-time last-ai-turn-time)
                    (* 1.0 internal-time-units-per-second))
             (process-turn-phase *game*)
             (setf last-ai-turn-time current-time)))
         (when (eq current-seat :south)
           ;; South player turn - process immediately
           (process-turn-phase *game*)))

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
            (let ((result (handle-input *game* ui-state ch)))
              (case result
                (:quit (return))
                (:new-game (setf *game* (init-new-game :num-players num-players
                                                       :player-names player-names)
                                 ui-state (make-ui-state)))
                (:refresh nil)
                (t nil)))))))))

(defun main (&key (num-players 4) (player-names nil))
  "Entry point for the TUI client.

   Optional arguments:
   - NUM-PLAYERS: Number of players (3 for sanma, 4 for regular). Default: 4
   - PLAYER-NAMES: List of player names. Default: (\"Player 1\" \"Player 2\" ...)

   Examples:
   (main)  ; Start with default 4 players
   (main :num-players 3)  ; Start sanma (3 player) game
   (main :player-names '(\"Alice\" \"Bob\" \"Charlie\" \"David\"))  ; Custom names
   (main :num-players 3 :player-names '(\"Alice\" \"Bob\" \"Charlie\"))  ; Both"
  (handler-case
      (croatoan:with-screen (scr :input-echoing nil
                                 :input-blocking 100
                                 :enable-colors t
                                 :cursor-visible nil)
        (main-loop scr :num-players num-players
                      :player-names player-names))
    (sb-sys:interactive-interrupt ()
      ;; Handle Ctrl-C gracefully
      (format t "~%Interrupted.~%"))
    (error (e)
      (format t "Error: ~A~%" e)
      nil)))
