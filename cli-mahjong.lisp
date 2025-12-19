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
  (human-seat :south))  ; Which seat the human player occupies

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
    ;; Draw player info: name, seat, score
    (croatoan:move scr row col)
    (croatoan:add-string scr (format nil "~A [~A] ~D pts"
                                     (player-name player)
                                     (player-seat player)
                                     (player-score player)))

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

(defun init-new-game (&key (num-players 4) (player-names nil) (human-seat nil))
  "Initialize a new game with NUM-PLAYERS players (3 or 4).
   PLAYER-NAMES is an optional list of player names.
   HUMAN-SEAT specifies which seat the human occupies (randomly assigned if nil)."
  (unless (member num-players '(3 4))
    (error "Number of players must be 3 or 4"))

  (let* ((variant (if (= num-players 3) :sanma :yonma))
         (game-id (format nil "game-~A" (get-universal-time)))
         (game (create-game-state game-id variant))
         (seats (if (= num-players 3)
                    '(:east :south :west)
                    '(:east :south :west :north)))
         ;; Randomly assign human seat if not specified
         (actual-human-seat (or human-seat (nth (random num-players) seats)))
         (default-names (loop for i from 1 to num-players
                             collect (format nil "Player ~A" i)))
         (names (if player-names
                    (append player-names
                            (subseq default-names (length player-names)))
                    default-names)))
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

(defun handle-input (game ui-state ch)
  "Handle user input and return action result"
  (let* ((phase (game-phase game))
         (human-seat (ui-state-human-seat ui-state))
         (human-player (get-player game human-seat))
         (current-seat (player-seat (current-player game)))
         (is-human-turn (eq current-seat human-seat))
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

      ;; Default - ignore input if not human's turn
      (t :continue))))

(defun process-turn-phase (game human-seat)
  "Process automatic turn actions based on current phase.
   HUMAN-SEAT is the seat occupied by the human player."
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
           (player-sort-hand human-player)
           (setf (game-phase game) :turn-discard))))

      ;; AI auto-draw (just draw, don't discard yet)
      ((and (eq phase :turn-draw)
            (not (eq current-seat human-seat)))
       (let ((drawn-tile (wall-draw-tile wall)))
         (when drawn-tile
           (player-add-tile current drawn-tile)
           (auto-extract-north-if-needed game current)
           (player-sort-hand current)
           ;; Transition to discard phase, but don't discard yet
           (setf (game-phase game) :turn-discard))))

      ;; AI auto-discard (separate phase to allow rendering of 14 tiles)
      ((and (eq phase :turn-discard)
            (not (eq current-seat human-seat)))
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
  (let* ((human-seat (ui-state-human-seat ui-state))
         (human-player (get-player game human-seat))
         (sorted-hand (sort-tiles (player-hand human-player)))
         (selected-index (ui-state-selected-tile-index ui-state))
         (selected-tile (nth selected-index sorted-hand)))
    (when (and selected-tile
               (< selected-index (length sorted-hand)))
      ;; Discard the tile
      (player-discard-tile human-player selected-tile)

      ;; Skip call window - advance directly to next turn
      (setf (game-phase game) :call-window)
      (advance-turn game)
      (setf (game-phase game) :turn-draw)

      ;; Reset selection
      (setf (ui-state-selected-tile-index ui-state) 0)

      t)))

(defun draw-discards-horizontal (scr discards row col &optional (tiles-per-row 6) (reverse-horizontal nil))
  "Draw discards in horizontal rows (for top/bottom players).
   Discards are arranged in rows of TILES-PER-ROW tiles.
   If REVERSE-HORIZONTAL is T, arranges right-to-left (for opposite player)."
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
          do (draw-tile-block scr tile
                              (+ row (* row-num 3))
                              (+ col (* col-num 3))
                              nil  ; not hidden
                              is-first
                              is-last))))  ; or last tile overall

(defun draw-discards-vertical (scr discards row col &optional (tiles-per-column 6) (reverse-vertical nil))
  "Draw discards in vertical columns (for left/right players).
   Discards are arranged in columns of TILES-PER-COLUMN tiles.
   If REVERSE-VERTICAL is T, arranges bottom-to-top (for right player)."
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
          do (draw-tile-block-vertical scr tile
                                       (+ row (* row-num 2))
                                       (+ col (* col-num 4))
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
      (croatoan:move scr 0 0)
      (croatoan:add-string scr (format nil "Turn: ~A | Phase: ~A | Selected: ~A | Last key: ~S"
                                       current-seat
                                       phase
                                       (when ui-state (ui-state-selected-tile-index ui-state))
                                       (when ui-state (ui-state-last-key ui-state))))

      ;; Draw help for human player's turn
      (when (and (eq current-seat human-seat) (eq phase :turn-discard))
        (croatoan:move scr 1 0)
        (croatoan:add-string scr "Use ← → to select tile, Enter to discard"))

      (cond
        ;; 4-player layout
        ((= num-players 4)
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
             (draw-hand-horizontal scr top-player 3 top-col t))

           ;; Left player - hidden
           (when left-player
             (draw-hand-vertical scr left-player left-row left-col t nil))  ; Left-aligned

           ;; Right player - hidden
           (when right-player
             (draw-hand-vertical scr right-player right-row right-col t t))  ; Right-aligned

           ;; Bottom player (human) - visible
           (when bottom-player
             (draw-hand-horizontal scr bottom-player (max 0 (- screen-height 4)) bottom-col nil selected-index))))

        ;; 3-player layout (sanma)
        ((= num-players 3)
         (let* ((bottom-player (get-player game bottom-seat))
                (right-player (get-player game right-seat))
                (top-player (get-player game top-seat))
                ;; Calculate positions dynamically
                (top-hand-len (when top-player (length (player-hand top-player))))
                (bottom-hand-len (when bottom-player (length (player-hand bottom-player))))
                (right-hand-len (when right-player (length (player-hand right-player))))
                (top-width (when top-hand-len (+ 1 (* 3 top-hand-len))))
                (bottom-width (when bottom-hand-len (+ 1 (* 3 bottom-hand-len))))
                (top-col (when top-width (max 0 (- (floor screen-width 2) (floor top-width 2)))))
                (bottom-col (when bottom-width (max 0 (- (floor screen-width 2) (floor bottom-width 2)))))
                (right-height (when right-hand-len (+ 1 (* 2 right-hand-len))))
                (right-row (when right-height (max 0 (- (floor screen-height 2) (floor right-height 2)))))
                (right-col 0))

           ;; Top player - hidden
           (when top-player
             (draw-hand-horizontal scr top-player 3 top-col t))

           ;; Right player (left side for 3-player) - hidden
           (when right-player
             (draw-hand-vertical scr right-player right-row right-col t nil))  ; Left-aligned

           ;; Bottom player (human) - visible
           (when bottom-player
             (draw-hand-horizontal scr bottom-player (max 0 (- screen-height 4)) bottom-col nil selected-index)))))

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
                (discard-row (+ center-row center-box-height 1))  ; 1 row below center box
                (discard-col (- center-col 1)))  ; Closer to center
            (draw-discards-horizontal scr discards discard-row discard-col 6 nil)))

        ;; Top player discards: above center box (closer to their hand)
        ;; From their perspective: oldest on left (our right), newest on right (our left)
        (when top-player
          (let ((discards (reverse (player-discards top-player)))  ; Reverse list AND use horizontal reverse
                (discard-row (max 0 (- center-row 4)))  ; 4 rows above center box
                (discard-col (+ center-col 2)))  ; More to the right
            (draw-discards-horizontal scr discards discard-row discard-col 6 t)))  ; reversed horizontal

        ;; Right player discards: to the right of center box
        (when right-player
          (let ((discards (reverse (player-discards right-player)))  ; Reverse list for correct order
                (discard-row (- center-row 1))  ; Closer to center
                (discard-col (+ center-col center-box-width 1)))  ; Closer to center
            (draw-discards-vertical scr discards discard-row discard-col 6 t)))  ; reversed vertical

        ;; Left player discards (4-player only): to the left of center box
        (when left-player
          (let ((discards (reverse (player-discards left-player)))  ; Reverse list for correct order
                (discard-row (- center-row 1))  ; Closer to center
                (discard-col (max 0 (- center-col 12))))  ; More to the right
            (draw-discards-vertical scr discards discard-row discard-col 6 nil)))))

  (croatoan:refresh scr)))

(defun main-loop (scr &key (num-players 4) (player-names nil))
  "Main event loop with optional game configuration."
  (multiple-value-bind (game human-seat)
      (init-new-game :num-players num-players :player-names player-names)
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
               (process-turn-phase *game* human-seat)
               (setf last-ai-turn-time current-time)))
           (when (eq current-seat human-seat)
             ;; Human player turn - process immediately
             (process-turn-phase *game* human-seat)))

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
                (:new-game
                 (multiple-value-bind (new-game new-human-seat)
                     (init-new-game :num-players num-players :player-names player-names)
                   (setf *game* new-game
                         ui-state (make-ui-state :human-seat new-human-seat))))
                (:refresh nil)
                (t nil))))))))))

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
