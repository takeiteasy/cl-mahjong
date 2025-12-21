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
  (valid-riichi-discards nil)  ; List of tiles that maintain tempai for riichi
  (pending-win nil))  ; Plist with win info: (:type :tsumo/:ron :winning-tile TILE :loser-seat SEAT)

(defparameter *debug-ui* nil)

(defparameter *game* nil
  "The current game state.")

;;; ============================================================================
;;; Modal State Structure
;;; ============================================================================

(defstruct modal-state
  "State for a modal dialog.
   message-lines can contain strings or (:text TEXT :color :COLOR) plists for colored text."
  (title "" :type string)
  (message-lines nil :type list)         ; List of message lines (strings or colored plists)
  (buttons nil :type list)               ; List of button label strings
  (selected-index 0 :type integer)       ; Currently selected button
  (result nil))                          ; Set when user confirms selection

;;; ============================================================================
;;; Modal Creation Helpers
;;; ============================================================================

(defun make-riichi-modal ()
  "Create a modal for riichi declaration prompt."
  (make-modal-state
   :title "RIICHI?"
   :message-lines '("You are in tempai. Declare riichi?")
   :buttons '("RIICHI" "SKIP")
   :selected-index 0))

(defun make-tsumo-modal ()
  "Create a modal for tsumo (self-draw win) prompt."
  (make-modal-state
   :title "TSUMO!"
   :message-lines '("You have completed your hand!")
   :buttons '("TSUMO" "SKIP")
   :selected-index 0))

(defun make-ron-modal ()
  "Create a modal for ron (win on discard) prompt."
  (make-modal-state
   :title "RON!"
   :message-lines '("You can win on this discard!")
   :buttons '("RON" "SKIP")
   :selected-index 0))

(defun format-yaku-name (yaku-keyword)
  "Convert a yaku keyword to a readable name."
  (let ((name (symbol-name yaku-keyword)))
    (string-capitalize (substitute #\Space #\- name))))

(defun limit-hand-name (limit-hand)
  "Get the display name for a limit hand."
  (case limit-hand
    (:mangan "MANGAN")
    (:haneman "HANEMAN")
    (:baiman "BAIMAN")
    (:sanbaiman "SANBAIMAN")
    (:yakuman "YAKUMAN")
    (t nil)))

(defun make-score-result-modal (score-result game winner-seat is-tsumo)
  "Create a modal for displaying detailed win result.
   Shows yaku list, fu/han, points, and score changes for all players."
  (let* ((yaku-list (score-result-yaku-list score-result))
         (total-han (score-result-total-han score-result))
         (fu (score-result-fu score-result))
         (limit-hand (score-result-limit-hand score-result))
         (payment (score-result-payment score-result))
         (winner-receives (payment-split-winner-receives payment))
         (config (game-config game))
         (lines nil))
    
    ;; Title line depends on limit hand
    (if limit-hand
        (push (format nil "~A!" (limit-hand-name limit-hand)) lines)
        (push (format nil "~A Han ~A Fu" total-han fu) lines))
    
    ;; Yaku list
    (push "" lines)  ; separator
    (dolist (yaku yaku-list)
      (let ((name (format-yaku-name (first yaku)))
            (han (second yaku)))
        (push (format nil "~A: ~A Han" name han) lines)))
    
    ;; Dora count if any
    (when (> (score-result-dora-count score-result) 0)
      (push (format nil "Dora: ~A Han" (score-result-dora-count score-result)) lines))
    
    ;; Points separator
    (push "" lines)
    (push (format nil "Total: ~A points" winner-receives) lines)
    
    ;; Player score changes
    (push "" lines)
    (dolist (seat (game-config-seats config))
      (let* ((player (get-player game seat))
             (name (player-name player))
             (score-change (cond
                             ((eq seat winner-seat) winner-receives)
                             (is-tsumo
                              ;; Tsumo: all others pay
                              (if (eq seat (game-dealer-seat game))
                                  (- (or (payment-split-dealer-pays payment) 
                                         (payment-split-non-dealer-pays payment)))
                                  (- (payment-split-non-dealer-pays payment))))
                             (t
                              ;; Ron: only loser pays
                              (if (eq seat (payment-split-loser-seat payment))
                                  (- (payment-split-direct-payment payment))
                                  0)))))
        (when (and score-change (/= score-change 0))
          (push (list :text (format nil "~A: ~@D" name score-change)
                      :color (if (> score-change 0) :green :red))
                lines))))
    
    (make-modal-state
     :title (if is-tsumo "TSUMO" "RON")
     :message-lines (nreverse lines)
     :buttons '("OK")
     :selected-index 0)))

(defun make-win-modal (winner-name &optional score-result game winner-seat is-tsumo)
  "Create a modal for showing win result.
   If score-result is provided, shows detailed scoring info."
  (if (and score-result game)
      (make-score-result-modal score-result game winner-seat is-tsumo)
      (make-modal-state
       :title (if is-tsumo "TSUMO" "RON")
       :message-lines (list (format nil "~A wins the hand!" winner-name))
       :buttons '("OK")
       :selected-index 0)))

;;; ============================================================================
;;; Modal Drawing
;;; ============================================================================

(defun get-line-text (line)
  "Extract text from a line (which may be a string or plist with :text key)."
  (if (stringp line)
      line
      (getf line :text "")))

(defun get-line-color (line)
  "Extract color from a line plist, or nil for plain text."
  (unless (stringp line)
    (getf line :color)))

(defun calculate-modal-dimensions (modal)
  "Calculate the width and height needed for the modal.
   Returns (values width height)."
  (let* ((title-len (length (modal-state-title modal)))
         (message-lines (modal-state-message-lines modal))
         (max-message-len (if message-lines
                              (reduce #'max message-lines 
                                      :key (lambda (l) (length (get-line-text l)))
                                      :initial-value 0)
                              0))
         (buttons (modal-state-buttons modal))
         ;; Each button: [ LABEL ] with 2 chars padding, 3 chars for brackets+spaces
         ;; Plus 2 chars spacing between buttons
         (button-width (+ (reduce #'+ buttons :key #'length)
                          (* (length buttons) 5)  ; brackets and padding per button
                          (* (1- (length buttons)) 2)))  ; spacing between
         ;; Width is max of title, messages, buttons + border padding
         (content-width (max title-len max-message-len button-width))
         (total-width (+ content-width 4))  ; 2 chars padding on each side
         ;; Height: top border, title, separator, message-lines, buttons, bottom border
         (num-lines (length message-lines))
         (total-height (+ 4 num-lines)))  ; top, title, separator, lines, buttons, bottom
    (values total-width total-height)))

(defun draw-modal (scr modal)
  "Draw a modal dialog centered on screen using ACS box-drawing characters."
  (multiple-value-bind (width height) (calculate-modal-dimensions modal)
    (let* ((screen-height (croatoan:height scr))
           (screen-width (croatoan:width scr))
           (start-row (max 0 (- (floor screen-height 2) (floor height 2))))
           (start-col (max 0 (- (floor screen-width 2) (floor width 2))))
           (inner-width (- width 2))  ; Width inside borders
           (title (modal-state-title modal))
           (message-lines (modal-state-message-lines modal))
           (buttons (modal-state-buttons modal))
           (selected (modal-state-selected-index modal)))
      
      ;; Top border: ┌────────────┐
      (croatoan:move scr start-row start-col)
      (croatoan:add-char scr #\l :attributes '(:altcharset))  ; ┌
      (loop repeat inner-width do
        (croatoan:add-char scr #\q :attributes '(:altcharset)))  ; ─
      (croatoan:add-char scr #\k :attributes '(:altcharset))  ; ┐
      
      ;; Title row: │   TITLE    │
      (croatoan:move scr (+ start-row 1) start-col)
      (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
      (let* ((title-padding (floor (- inner-width (length title)) 2))
             (title-str (format nil "~A~A~A"
                               (make-string title-padding :initial-element #\Space)
                               title
                               (make-string (- inner-width title-padding (length title))
                                           :initial-element #\Space))))
        (croatoan:add-string scr title-str :attributes '(:bold)))
      (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
      
      ;; Separator: ├────────────┤
      (croatoan:move scr (+ start-row 2) start-col)
      (croatoan:add-char scr #\t :attributes '(:altcharset))  ; ├
      (loop repeat inner-width do
        (croatoan:add-char scr #\q :attributes '(:altcharset)))  ; ─
      (croatoan:add-char scr #\u :attributes '(:altcharset))  ; ┤
      
      ;; Message lines
      (loop for line in message-lines
            for row from (+ start-row 3)
            do (croatoan:move scr row start-col)
               (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
               (let* ((text (get-line-text line))
                      (color (get-line-color line))
                      (padding (floor (- inner-width (length text)) 2))
                      (attrs (case color
                               (:green '(:bold))
                               (:red '(:bold))
                               (t nil))))
                 ;; Left padding
                 (croatoan:add-string scr (make-string padding :initial-element #\Space))
                 ;; Text with optional color
                 (case color
                   (:green 
                    (croatoan:add-string scr text :fgcolor :green :attributes attrs))
                   (:red 
                    (croatoan:add-string scr text :fgcolor :red :attributes attrs))
                   (t
                    (croatoan:add-string scr text :attributes attrs)))
                 ;; Right padding
                 (let ((remaining (- inner-width padding (length text))))
                   (when (> remaining 0)
                     (croatoan:add-string scr (make-string remaining :initial-element #\Space)))))
               (croatoan:add-char scr #\x :attributes '(:altcharset)))  ; │
      
      ;; Button row
      (let ((button-row (+ start-row 3 (length message-lines))))
        (croatoan:move scr button-row start-col)
        (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
        
        ;; Build button string with highlighting
        (let* ((button-strs (loop for btn in buttons
                                  for i from 0
                                  collect (if (= i selected)
                                              (format nil "[ ~A ]" btn)
                                              (format nil "  ~A  " btn))))
               (buttons-combined (format nil "~{~A~^  ~}" button-strs))
               (btn-padding (floor (- inner-width (length buttons-combined)) 2)))
          ;; Left padding
          (croatoan:add-string scr (make-string btn-padding :initial-element #\Space))
          ;; Draw each button with appropriate highlighting
          (loop for btn in buttons
                for i from 0
                do (if (= i selected)
                       (progn
                         (croatoan:add-string scr "[ ")
                         (croatoan:add-string scr btn :attributes '(:reverse :bold))
                         (croatoan:add-string scr " ]"))
                       (progn
                         (croatoan:add-string scr "  ")
                         (croatoan:add-string scr btn)
                         (croatoan:add-string scr "  ")))
                   (when (< i (1- (length buttons)))
                     (croatoan:add-string scr "  ")))  ; Spacing between buttons
          ;; Right padding
          (let ((remaining (- inner-width btn-padding (length buttons-combined))))
            (when (> remaining 0)
              (croatoan:add-string scr (make-string remaining :initial-element #\Space)))))
        
        (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
        
        ;; Bottom border: └────────────┘
        (croatoan:move scr (+ button-row 1) start-col)
        (croatoan:add-char scr #\m :attributes '(:altcharset))  ; └
        (loop repeat inner-width do
          (croatoan:add-char scr #\q :attributes '(:altcharset)))  ; ─
        (croatoan:add-char scr #\j :attributes '(:altcharset))))))  ; ┘

;;; ============================================================================
;;; Modal Input Handling
;;; ============================================================================

(defun handle-modal-input (modal key-name)
  "Handle input for a modal dialog.
   Returns :continue if modal is still active, or the selected button label if confirmed."
  (let ((num-buttons (length (modal-state-buttons modal))))
    (cond
      ;; Left arrow - select previous button
      ((eq key-name :left)
       (setf (modal-state-selected-index modal)
             (mod (1- (modal-state-selected-index modal)) num-buttons))
       :continue)
      
      ;; Right arrow - select next button
      ((eq key-name :right)
       (setf (modal-state-selected-index modal)
             (mod (1+ (modal-state-selected-index modal)) num-buttons))
       :continue)
      
      ;; Enter - confirm selection
      ((or (eql key-name #\Newline) (eql key-name #\Return) (eq key-name :enter))
       (let ((selected-button (nth (modal-state-selected-index modal)
                                   (modal-state-buttons modal))))
         (setf (modal-state-result modal) selected-button)
         selected-button))
      
      ;; Any other key - ignore
      (t :continue))))

(defun modal-active-p (ui-state)
  "Check if there is an active modal in the UI state."
  (and (slot-boundp ui-state 'active-modal)
       (slot-value ui-state 'active-modal)))

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
      ((eq tile :white) '(:blue :black)) ; white dragon (blue)
      (t nil))))                           ; default for winds

(defun draw-tile-block (scr tile row col &optional (hidden nil) (first-in-row nil) (last-in-row nil) (highlighted nil) (is-valid nil))
  "Draw a single tile as a block using ACS box-drawing characters with shared borders.
   Format (horizontal):
   ┌──┬──┐  (first ── middle ── last)
   │  │  │
   └──┴──┘
   HIGHLIGHTED makes the tile reverse video (selected).
   IS-VALID highlights the tile border in green (valid for riichi discard)."
  (let ((tile-str (if hidden
                      "  "
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
   │  │
   ├──┤  <- junction
   │  │  <- middle
   ├──┤  <- junction
   │  │
   └──┘  <- last"
  (let ((tile-str (if hidden
                      "  "
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

;;; ============================================================================
;;; Grid-Based Tile Drawing (Proper Box-Character Junctions)
;;; ============================================================================

(defun get-junction-char-horizontal (grid-row grid-col num-rows num-cols)
  "Determine the correct ACS box-drawing character for a junction point
   in a horizontal grid layout.
   GRID-ROW and GRID-COL are 0-indexed row/column in the grid.
   NUM-ROWS and NUM-COLS are the total grid dimensions.
   Returns the character for the junction at the top-left of tile (grid-row, grid-col),
   or bottom-right corner/edge for last row/col."
  (let ((is-top (= grid-row 0))
        (is-bottom (= grid-row num-rows))
        (is-left (= grid-col 0))
        (is-right (= grid-col num-cols)))
    (cond
      ;; Corners
      ((and is-top is-left) #\l)       ; ┌ top-left
      ((and is-top is-right) #\k)      ; ┐ top-right
      ((and is-bottom is-left) #\m)    ; └ bottom-left
      ((and is-bottom is-right) #\j)   ; ┘ bottom-right
      ;; Edges
      (is-top #\w)                     ; ┬ top tee
      (is-bottom #\v)                  ; ┴ bottom tee
      (is-left #\t)                    ; ├ left tee
      (is-right #\u)                   ; ┤ right tee
      ;; Interior
      (t #\n))))                       ; ┼ cross

(defun draw-tile-grid-horizontal (scr rows row col)
  "Draw a 2D grid of tiles with proper box-drawing junctions.
   ROWS is a list of rows (each row is a list of tiles).
   ROW/COL is the screen position of the top-left corner."
  (when (null rows)
    (return-from draw-tile-grid-horizontal))
  (let ((border-attrs '(:altcharset))
        (num-rows (length rows)))
    ;; For each row, draw: top border, content, then bottom border after last row
    (loop for row-idx from 0 below num-rows
          for tile-row = (nth row-idx rows)
          for num-tiles = (length tile-row)
          for screen-y = (+ row (* row-idx 2))
          for is-first-row = (= row-idx 0)
          for is-last-row = (= row-idx (1- num-rows))
          for prev-row = (when (> row-idx 0) (nth (1- row-idx) rows))
          for prev-num-tiles = (if prev-row (length prev-row) 0)
          do
             ;; Draw top border line
             (croatoan:move scr screen-y col)
             (loop for tile-idx from 0 below num-tiles
                   for is-first-tile = (= tile-idx 0)
                   for is-last-tile = (= tile-idx (1- num-tiles))
                   ;; has-above: true if there's a tile above OR at prev-row's right edge
                   ;; (tile-idx <= prev-num-tiles catches the right edge of shorter prev-row)
                   for has-above = (and prev-row (<= tile-idx prev-num-tiles))
                   ;; at-prev-edge: true if we're exactly at the right edge of prev-row
                   for at-prev-edge = (and prev-row (= tile-idx prev-num-tiles))
                   do
                      ;; Left junction of tile
                      (croatoan:add-char scr
                                         (cond
                                           ((and is-first-tile is-first-row) #\l)       ; ┌
                                           ((and is-first-tile has-above) #\t)          ; ├
                                           ((and is-first-tile (not has-above)) #\l)    ; ┌
                                           ;; At the right edge of prev-row, need special handling
                                           (at-prev-edge #\n)                           ; ┼ (connects to prev-row's right edge)
                                           ((and has-above (not is-first-tile)) #\n)    ; ┼
                                           (t #\w))                                     ; ┬
                                         :attributes border-attrs)
                      ;; Horizontal line
                      (croatoan:add-char scr #\q :attributes border-attrs)
                      (croatoan:add-char scr #\q :attributes border-attrs))
             ;; Right edge of row
             (let* ((has-above-at-end (and prev-row (>= prev-num-tiles num-tiles)))
                    (prev-row-longer (and prev-row (> prev-num-tiles num-tiles))))
               (croatoan:add-char scr
                                  (cond
                                    (is-first-row #\k)                              ; ┐
                                    (prev-row-longer #\v)                           ; ┴ (no content below, connects to trailing border)
                                    (has-above-at-end #\u)                          ; ┤ (prev-row ends exactly here or extends)
                                    (t #\k))                                        ; ┐ (prev-row is shorter)
                                  :attributes border-attrs))
             ;; Draw trailing borders for previous row (if it was longer)
             ;; Each phantom tile position needs: ── (horizontal) then ┴ or ┘ (junction)
             (when (and prev-row (> prev-num-tiles num-tiles))
               (loop for extra-idx from num-tiles below prev-num-tiles
                     for is-last-extra = (= extra-idx (1- prev-num-tiles))
                     do (croatoan:add-char scr #\q :attributes border-attrs)   ; ─
                        (croatoan:add-char scr #\q :attributes border-attrs)   ; ─
                        (croatoan:add-char scr (if is-last-extra #\j #\v)      ; ┘ or ┴
                                           :attributes border-attrs)))
             
             ;; Draw content line
             (croatoan:move scr (1+ screen-y) col)
             (croatoan:add-char scr #\x :attributes border-attrs)  ; │
             (loop for tile in tile-row
                   do (croatoan:add-string scr (tile-to-string tile) 
                                           :color-pair (get-tile-color-pair tile))
                      (croatoan:add-char scr #\x :attributes border-attrs)))  ; │
    
    ;; Draw bottom border after last row
    (let* ((last-row (nth (1- num-rows) rows))
           (num-tiles (length last-row))
           (screen-y (+ row (* num-rows 2))))
      (croatoan:move scr screen-y col)
      (croatoan:add-char scr #\m :attributes border-attrs)  ; └
      (loop for tile-idx from 0 below num-tiles
            do (croatoan:add-char scr #\q :attributes border-attrs)
               (croatoan:add-char scr #\q :attributes border-attrs)
               (when (< tile-idx (1- num-tiles))
                 (croatoan:add-char scr #\v :attributes border-attrs)))  ; ┴
      (croatoan:add-char scr #\j :attributes border-attrs))))

(defun get-junction-char-vertical (grid-row grid-col num-rows num-cols)
  "Determine the correct ACS box-drawing character for a junction point
   in a vertical grid layout (columns share vertical borders).
   GRID-ROW is which row within a column (0-indexed).
   GRID-COL is which column (0-indexed).
   NUM-ROWS is tiles per column, NUM-COLS is number of columns."
  (let ((is-top (= grid-row 0))
        (is-bottom (= grid-row num-rows))
        (is-left (= grid-col 0))
        (is-right (= grid-col num-cols)))
    (cond
      ;; Corners
      ((and is-top is-left) #\l)       ; ┌
      ((and is-top is-right) #\k)      ; ┐
      ((and is-bottom is-left) #\m)    ; └
      ((and is-bottom is-right) #\j)   ; ┘
      ;; Edges
      (is-top #\w)                     ; ┬
      (is-bottom #\v)                  ; ┴
      (is-left #\t)                    ; ├
      (is-right #\u)                   ; ┤
      ;; Interior
      (t #\n))))                       ; ┼

(defun draw-tile-grid-vertical (scr columns row col)
  "Draw a 2D grid of tiles arranged in columns with proper box-drawing junctions.
   COLUMNS is a list of columns (each column is a list of tiles, top to bottom).
   ROW/COL is the screen position of the top-left corner."
  (when (null columns)
    (return-from draw-tile-grid-vertical))
  (let ((border-attrs '(:altcharset))
        (num-cols (length columns))
        (max-col-len (loop for c in columns maximize (length c))))
    ;; For each tile row from 0 to max-col-len (inclusive for bottom border)
    (loop for tile-row from 0 to max-col-len
          for screen-y = (+ row (* tile-row 2))
          for is-first-tile-row = (= tile-row 0)
          for is-bottom-border = (= tile-row max-col-len)
          do
             ;; Build column states: :active, :ending, or :ended
             (let ((col-states (loop for col-idx from 0 below num-cols
                                     for this-col = (nth col-idx columns)
                                     for col-len = (length this-col)
                                     collect (cond
                                               ((< tile-row col-len) :active)
                                               ((= tile-row col-len) :ending)
                                               (t :ended)))))
               ;; Draw border row if any column is active or ending
               (when (some (lambda (s) (member s '(:active :ending))) col-states)
                 (croatoan:move scr screen-y col)
                 ;; Draw left junction (or space if first column ended)
                 (let ((first-state (first col-states)))
                   (if (member first-state '(:active :ending))
                       (croatoan:add-char scr
                                          (cond
                                            ((and is-first-tile-row (eq first-state :active)) #\l)
                                            ((eq first-state :ending) #\m)
                                            ((eq first-state :active) #\t)
                                            (t #\Space))
                                          :attributes border-attrs)
                       (croatoan:add-char scr #\Space)))
                 ;; Draw each column: 2 horizontal chars + 1 junction
                 (loop for col-idx from 0 below num-cols
                       for state = (nth col-idx col-states)
                       for is-last-col = (= col-idx (1- num-cols))
                       for next-state = (if is-last-col nil (nth (1+ col-idx) col-states))
                       do
                          ;; Horizontal line (or spaces if ended)
                          (if (member state '(:active :ending))
                              (progn
                                (croatoan:add-char scr #\q :attributes border-attrs)
                                (croatoan:add-char scr #\q :attributes border-attrs))
                              (progn
                                (croatoan:add-char scr #\Space)
                                (croatoan:add-char scr #\Space)))
                          ;; Right junction/separator
                          (croatoan:add-char scr
                                             (cond
                                               ;; Both current and next are ended - space
                                               ((and (eq state :ended) (or (null next-state) (eq next-state :ended)))
                                                #\Space)
                                               ;; Current ended, next active/ending - need left junction for next
                                               ((and (eq state :ended) (member next-state '(:active :ending)))
                                                (cond
                                                  (is-first-tile-row #\l)
                                                  ((eq next-state :ending) #\m)
                                                  (t #\t)))
                                               ;; Current active/ending, next is ended - this is right edge
                                               ((and (member state '(:active :ending)) (eq next-state :ended))
                                                (cond
                                                  ((and is-first-tile-row (eq state :active)) #\k)
                                                  ((eq state :ending) #\j)
                                                  (t #\u)))
                                               ;; Last column
                                               (is-last-col
                                                (cond
                                                  ((and is-first-tile-row (eq state :active)) #\k)
                                                  ((eq state :ending) #\j)
                                                  ((eq state :active) #\u)
                                                  (t #\Space)))
                                               ;; Both active/ending - normal junction
                                               ((and (member state '(:active :ending)) (member next-state '(:active :ending)))
                                                (cond
                                                  ((and (eq state :ending) (eq next-state :ending)) #\v)
                                                  ((eq state :ending) #\v)
                                                  ((eq next-state :ending) #\n)
                                                  (is-first-tile-row #\w)
                                                  (t #\n)))
                                               (t #\Space))
                                             :attributes border-attrs)))
               ;; Draw content row (not for bottom border)
               (unless is-bottom-border
                 ;; Check if any column has a tile at this row
                 (when (some (lambda (c) (< tile-row (length c))) columns)
                   (croatoan:move scr (1+ screen-y) col)
                   ;; Draw left border (│ if first column has tile, space if not)
                   (if (< tile-row (length (first columns)))
                       (croatoan:add-char scr #\x :attributes border-attrs)
                       (croatoan:add-char scr #\Space))
                   ;; Draw each column: 2 chars content + separator
                   (loop for col-idx from 0 below num-cols
                         for this-col = (nth col-idx columns)
                         for has-tile = (< tile-row (length this-col))
                         for is-last-col = (= col-idx (1- num-cols))
                         for next-col = (unless is-last-col (nth (1+ col-idx) columns))
                         for next-has-tile = (and next-col (< tile-row (length next-col)))
                         do
                            (if has-tile
                                (let ((tile (nth tile-row this-col)))
                                  (croatoan:add-string scr (tile-to-string tile)
                                                       :color-pair (get-tile-color-pair tile)))
                                (progn
                                  (croatoan:add-char scr #\Space)
                                  (croatoan:add-char scr #\Space)))
                            ;; Draw separator only if current or next has content
                            (if (or has-tile next-has-tile)
                                (croatoan:add-char scr #\x :attributes border-attrs)
                                (croatoan:add-char scr #\Space)))))))))

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
         ;; Player wins by tsumo - calculate score and show detailed win dialog
         (let* ((human-seat (ui-state-human-seat ui-state))
                (human-player (get-player game human-seat))
                (winning-tile (nth 13 (player-hand human-player)))  ; Drawn tile is last
                (hand (player-hand human-player))
                (melds (player-melds human-player))
                (context (build-winning-context game human-seat winning-tile t nil))
                (score-result (calculate-score hand melds winning-tile context)))
           ;; Store win info for scoring
           (setf (ui-state-pending-win ui-state)
                 (list :type :tsumo
                       :winning-tile winning-tile
                       :loser-seat nil
                       :score-result score-result))
           (setf (ui-state-active-modal ui-state)
                 (make-win-modal (player-name human-player) score-result game human-seat t))
           (setf (game-phase game) :hand-ended))))
      
      ;; Ron decision
      ((string= title "RON!")
       (when (string= result "RON")
         ;; Player wins by ron - calculate score and show detailed win dialog
         (let* ((human-seat (ui-state-human-seat ui-state))
                (human-player (get-player game human-seat))
                (turn-state (game-turn-state game))
                (winning-tile (turn-last-discard turn-state))
                (loser-seat (turn-last-discard-player turn-state))
                (hand (cons winning-tile (player-hand human-player)))  ; Add winning tile
                (melds (player-melds human-player))
                (context (build-winning-context game human-seat winning-tile nil loser-seat))
                (score-result (calculate-score hand melds winning-tile context)))
           ;; Store win info for scoring
           (setf (ui-state-pending-win ui-state)
                 (list :type :ron
                       :winning-tile winning-tile
                       :loser-seat loser-seat
                       :score-result score-result))
           (setf (ui-state-active-modal ui-state)
                 (make-win-modal (player-name human-player) score-result game human-seat nil))
           (setf (game-phase game) :hand-ended))))
      
      ;; Win acknowledgment - calculate score and start new round
      ((or (string= title "RON") (string= title "TSUMO"))
       (process-win-and-start-new-round game ui-state)))))

;;; ============================================================================
;;; Round Transition
;;; ============================================================================

(defun process-win-and-start-new-round (game ui-state)
  "Process the winning hand, distribute points, and start a new round.
   If the winner was dealer, dealer stays. Otherwise rotate dealer."
  (let* ((pending-win (ui-state-pending-win ui-state))
         (win-type (getf pending-win :type))
         (loser-seat (getf pending-win :loser-seat))
         (score (getf pending-win :score-result))
         (winner-seat (ui-state-human-seat ui-state))
         (winner (get-player game winner-seat))
         (config (game-config game))
         (dealer-seat (game-dealer-seat game)))
    
    ;; Apply payments from pre-calculated score
    (when score
      (let* ((payment (score-result-payment score))
             (context (make-winning-context
                       :tsumo-p (eq win-type :tsumo)
                       :winner-seat winner-seat
                       :loser-seat loser-seat)))
        (apply-payment-to-players game winner-seat payment context)))
    
    ;; Award riichi sticks to winner
    (when (> (game-riichi-sticks game) 0)
      (incf (player-score winner) (* 1000 (game-riichi-sticks game)))
      (setf (game-riichi-sticks game) 0))
    
    ;; Handle dealer rotation
    (let ((dealer-won (eq winner-seat dealer-seat)))
      (if dealer-won
          ;; Dealer stays, increment honba
          (incf (game-honba game))
          ;; Dealer rotates to next seat
          (progn
            (setf (game-dealer-seat game) (next-seat dealer-seat config))
            (setf (game-honba game) 0))))
    
    ;; Clear pending win
    (setf (ui-state-pending-win ui-state) nil)
    
    ;; Reset all players for new hand
    (dolist (seat (game-config-seats config))
      (let ((player (get-player game seat)))
        (when player
          (player-reset-for-new-hand player))))
    
    ;; Create new wall and deal new hands
    (setf (game-wall game) (create-wall config))
    (deal-initial-hands game)
    
    ;; Reset turn state
    (let ((turn-state (game-turn-state game)))
      (setf (turn-current-player turn-state) (game-dealer-seat game)
            (turn-turn-number turn-state) 1
            (turn-last-discard turn-state) nil
            (turn-last-discard-player turn-state) nil))
    
    ;; Reset UI state
    (setf (ui-state-selected-tile-index ui-state) 0
          (ui-state-riichi-pending ui-state) nil
          (ui-state-valid-riichi-discards ui-state) nil)
    
    ;; Start new round in discard phase (dealer has 14 tiles)
    (setf (game-phase game) :turn-discard)))

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
  "Draw discards in horizontal rows (for top/bottom players) using grid-based rendering.
   Discards are arranged in rows of TILES-PER-ROW tiles with proper box-character junctions.
   If REVERSE-HORIZONTAL is T, mirrors tiles within each row (right-to-left).
   If STACK-UPWARD is T, newer rows appear above older rows (and grid is positioned accordingly)."
  (when discards
    (let* ((num-tiles (length discards))
           (num-rows (ceiling num-tiles tiles-per-row)))
      ;; Build rows as a 2D list
      (let ((rows (loop for row-idx from 0 below num-rows
                        for row-start = (* row-idx tiles-per-row)
                        for row-end = (min (+ row-start tiles-per-row) num-tiles)
                        collect (subseq discards row-start row-end))))
        ;; Apply reverse-horizontal: mirror tiles within each row
        (when reverse-horizontal
          (setf rows (mapcar #'reverse rows)))
        ;; Apply stack-upward: reverse row order
        (when stack-upward
          (setf rows (reverse rows)))
        ;; Calculate starting row for upward stacking (grid top is at start-row)
        (let ((start-row (if stack-upward
                             (- row (* (1- num-rows) 2))
                             row)))
          ;; Pass 2D row structure directly to grid function
          (draw-tile-grid-horizontal scr rows start-row col))))))

(defun draw-discards-vertical (scr discards row col &optional (tiles-per-column 6) (reverse-vertical nil) (stack-rightward nil))
  "Draw discards in vertical columns (for left/right players) using grid-based rendering.
   Discards are arranged in columns of TILES-PER-COLUMN tiles with proper box-character junctions.
   If REVERSE-VERTICAL is T, mirrors tiles within each column (bottom-to-top).
   If STACK-RIGHTWARD is T, newer columns appear to the right of older columns."
  (when discards
    (let* ((num-tiles (length discards))
           (num-cols (ceiling num-tiles tiles-per-column)))
      ;; Build columns as a 2D list
      (let ((columns (loop for col-idx from 0 below num-cols
                           for col-start = (* col-idx tiles-per-column)
                           for col-end = (min (+ col-start tiles-per-column) num-tiles)
                           collect (subseq discards col-start col-end))))
        ;; Apply reverse-vertical: mirror tiles within each column
        (when reverse-vertical
          (setf columns (mapcar #'reverse columns)))
        ;; Apply stack-rightward: reverse column order so older is on left, newer on right
        (when stack-rightward
          (setf columns (reverse columns)))
        ;; Calculate starting column for leftward stacking (grid left is at start-col)
        (let ((start-col (if (not stack-rightward)
                             ;; Stack leftward: newer on left, so rightmost col is first
                             (- col (* (1- num-cols) 3))
                             col)))
          ;; Pass 2D column structure directly to grid function
          (draw-tile-grid-vertical scr columns row start-col))))))

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
        ;; 
        ;; From each player's perspective, discards appear the same way:
        ;; oldest on their left, newest on their right, rows/columns expand toward their hand.
        ;;
        ;; Bottom player discards: oldest at left, newest at right. Rows stack down.
        (when bottom-player
          (let* ((discards (reverse (player-discards bottom-player)))  ; oldest to newest
                 (discard-row (+ center-row center-box-height))
                 (discard-col center-col))
            (draw-discards-horizontal scr discards discard-row discard-col 10 nil nil)))

        ;; Top player discards: rotated 180° from bottom.
        ;; Oldest at our right, newest at our left. Rows stack up (toward their hand).
        (when top-player
          (let* ((discards (reverse (player-discards top-player)))  ; oldest to newest
                 (num-discards (length discards))
                 (tiles-per-row 10)
                 ;; Grid bottom row should be at center-row - 2 (leave 1 line gap above center box)
                 ;; draw-discards-horizontal will calculate actual start from this
                 (discard-row (- center-row 3))
                 ;; Align right edge with center-box
                 (grid-width (+ 1 (* (min num-discards tiles-per-row) 3)))
                 (discard-col (max 0 (- (+ center-col center-box-width) grid-width))))
            (draw-discards-horizontal scr discards discard-row discard-col 10 t t)))

        ;; Right player discards: rotated 90° CW from bottom.
        ;; Oldest at our bottom (their left), newest at our top (their right).
        ;; Columns expand rightward toward their hand.
        (when right-player
          (let* ((discards (reverse (player-discards right-player)))  ; oldest to newest
                 (tiles-per-column 6)
                 (num-cols (ceiling (length discards) tiles-per-column))
                 ;; Position grid just to the right of center box
                 ;; Since stack-rightward=nil causes leftward offset, add compensation
                 (discard-row center-row)
                 (discard-col (+ center-col center-box-width 1 (* (1- num-cols) 3))))
            (draw-discards-vertical scr discards discard-row discard-col 6 t nil)))  ; reverse-vert, stack-right (newer on RIGHT)

        ;; Left player discards: rotated 90° CCW from bottom.
        ;; Oldest at our top (their left), newest at our bottom (their right).
        ;; Columns expand leftward toward their hand.
        (when left-player
          (let* ((discards (reverse (player-discards left-player)))  ; oldest to newest
                 (tiles-per-column 6)
                 (num-cols (ceiling (length discards) tiles-per-column))
                 ;; Grid right edge should be at center-col - 1
                 ;; Calculate left edge position: right-edge - width + 1
                 (grid-width (1+ (* num-cols 3)))
                 (discard-row center-row)
                 (discard-col (max 0 (- center-col grid-width))))
            (draw-discards-vertical scr discards discard-row discard-col 6 nil t))))  ; stack-right (newer on LEFT)
    
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
