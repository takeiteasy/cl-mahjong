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

(defpackage #:cli-mahjong
  (:use #:cl #:cl-mahjong #:croatoan)
  (:export #:main
           ;; Debug/SLIME exports
           #:*game*
           #:*game-paused*
           #:*step-requested*
           #:*debug-ui*
           #:pause-game
           #:resume-game
           #:step-game
           #:refresh-screen))

(in-package #:cli-mahjong)

(defstruct ui-state
  (selected-tile-index 0)
  (last-key nil)
  (human-seat :south) ; Which seat the human player occupies
  (hand-position nil) ; (row col) of where human hand is drawn
  (active-modal nil) ; Current modal dialog (modal-state or nil)
  (riichi-pending nil) ; T if player confirmed riichi and is choosing tile to discard
  (valid-riichi-discards nil) ; List of tiles that maintain tempai for riichi
  (pending-win nil)) ; Plist with win info: (:type :tsumo/:ron :winning-tile TILE :loser-seat SEAT)

(defparameter *debug-ui* t)

(defparameter *game* nil
              "The current game state.")

(defparameter *game-paused* nil
              "When T, the game loop pauses AI turns. Set via SLIME to pause.")

(defparameter *step-requested* nil
              "When T, advance one AI turn then pause again. Set via (step-game).")

(defun pause-game ()
  "Pause the game loop. AI turns will not advance until resumed or stepped."
  (setf *game-paused* t)
  (format t "Game paused. Use (resume-game) or (step-game) to continue.~%")
  :paused)

(defun resume-game ()
  "Resume the game loop after pausing."
  (setf *game-paused* nil
        *step-requested* nil)
  (format t "Game resumed.~%")
  :resumed)

(defun step-game ()
  "Advance exactly one AI turn, then pause again."
  (setf *step-requested* t)
  :stepping)

(defparameter *refresh-requested* nil
              "When T, the main loop will clear and redraw the screen.")

(defun refresh-screen ()
  "Request a full screen refresh/redraw."
  (setf *refresh-requested* t)
  :refreshing)

;;; ============================================================================
;;; Modal State Structure
;;; ============================================================================

(defstruct modal-state
  "State for a modal dialog.
   message-lines can contain strings or (:text TEXT :color :COLOR) plists for colored text."
  (title "" :type string)
  (message-lines nil :type list) ; List of message lines (strings or colored plists)
  (buttons nil :type list) ; List of button label strings
  (selected-index 0 :type integer) ; Currently selected button
  (escape nil) ; Button label to auto-select on Escape key (e.g., "SKIP")
  (call-options nil) ; List of call-option structs when this is a call modal
  (result nil)) ; Set when user confirms selection

;;; ============================================================================
;;; Modal Creation Helpers
;;; ============================================================================

(defun make-riichi-modal ()
  "Create a modal for riichi declaration prompt."
  (make-modal-state
   :title "RIICHI?"
   :message-lines '("You are in tempai. Declare riichi?")
   :buttons '("RIICHI" "SKIP")
   :escape "SKIP"))

(defun make-tsumo-modal (tile)
  "Create a modal for tsumo (self-draw win) prompt."
  (make-modal-state
   :title "TSUMO!"
   :message-lines (list (format nil "You can call tsumo on ~A?" (tile-to-string tile)))
   :buttons '("TSUMO" "SKIP")
   :escape "SKIP"))

(defun make-closed-kan-modal (tile)
  "Create a modal for closed kan (ankan) prompt - when player has 4 of same tile."
  (make-modal-state
   :title "CLOSED KAN?"
   :message-lines (list (format nil "Declare closed kan with ~A?" (tile-to-string tile)))
   :buttons '("KAN" "SKIP")
   :escape "SKIP"))

(defun make-added-kan-modal (tile)
  "Create a modal for added kan (kakan) prompt - upgrading a pon to kan."
  (make-modal-state
   :title "ADDED KAN?"
   :message-lines (list (format nil "Upgrade pon to kan with ~A?" (tile-to-string tile)))
   :buttons '("KAN" "SKIP")
   :escape "SKIP"))

(defun make-call-options-modal (call-options tile)
  "Create a modal for selecting from multiple call options.
   CALL-OPTIONS is a list of call-option structs.
   Returns a modal with buttons for each option plus SKIP."
  (let ((buttons (append (mapcar #'call-option-label call-options)
                   '("SKIP"))))
    (make-modal-state
     :title "CALL?"
     :message-lines (list (format nil "Call on ~A?" (tile-to-string tile)))
     :buttons buttons
     :escape "SKIP"
     :call-options call-options)))

(defun find-closed-kan-tiles (player)
  "Find all tiles for which player can declare closed kan (has 4 of same tile).
   Returns list of tiles or NIL."
  (let ((hand (player-hand player))
        (kan-tiles nil))
    (dolist (tile (remove-duplicates hand :test #'eq))
      (when (>= (count tile hand :test #'eq) 4)
            (push tile kan-tiles)))
    kan-tiles))

(defun find-added-kan-tiles (player)
  "Find all tiles for which player can declare added kan (has tile matching existing pon).
   Returns list of tiles or NIL."
  (let ((hand (player-hand player))
        (melds (player-melds player))
        (kan-tiles nil))
    (dolist (meld melds)
      (when (eq (meld-type meld) :pon)
            (let ((pon-tile (first (meld-tiles meld))))
              (when (member pon-tile hand :test #'eq)
                    (push pon-tile kan-tiles)))))
    kan-tiles))

(defun has-yaku-for-any-wait (hand melds waits game player-seat)
  "Check if winning on any waiting tile would give at least 1 yaku.
   Returns T if at least one wait would have yaku, NIL if no yaku possible."
  (when (and waits game)
    (dolist (wait waits)
      (let* ((test-hand (cons wait hand))
             ;; Build a simple ron context 
             (context (build-winning-context game player-seat wait nil nil))
             (score-result (calculate-score test-hand melds wait context)))
        (when score-result
          (return-from has-yaku-for-any-wait t))))
    nil))

(defun make-exhaustive-draw-modal (game)
  "Create a modal for exhaustive draw (ryuukyoku) showing tenpai status.
   Players in tenpai receive points from those not in tenpai.
   Yonma: Total pool is 3000 (1000 per noten or split among tenpai)
   Sanma: Total pool is 4000 (2000 per noten or split among tenpai)
   If all tenpai or all noten: no payment."
  (let* ((config (game-config game))
         (seats (game-config-seats config))
         (num-players (game-config-num-players config))
         ;; Determine pool based on variant (yonma: 3000, sanma: 4000)
         (total-pool (if (= num-players 3) 4000 3000))
         (tenpai-data nil)  ; List of (seat . wait-tiles)
         (noten-seats nil)
         (lines nil))
    ;; Check tenpai status for each player and collect waits
    (dolist (seat seats)
      (let* ((player (get-player game seat))
             (hand (player-hand player))
             (melds (player-melds player))
             (shanten (calculate-shanten hand melds)))
        (if (zerop shanten)
            (let ((waits (calculate-waiting-tiles hand melds)))
              (push (cons seat waits) tenpai-data))
            (push seat noten-seats))))
    (setf tenpai-data (nreverse tenpai-data))
    (setf noten-seats (nreverse noten-seats))
    (let ((tenpai-seats (mapcar #'car tenpai-data)))

      (push "Wall exhausted - no winner" lines)
      (push "" lines)

      ;; Calculate and display payments
      (cond
       ;; All players tenpai or all noten - no payment
       ((or (null tenpai-seats) (null noten-seats))
         (push "No tenpai payment" lines)
         (push "" lines)
         (dolist (seat seats)
           (let* ((player (get-player game seat))
                  (is-tenpai (member seat tenpai-seats))
                  (tenpai-entry (assoc seat tenpai-data))
                  (waits (cdr tenpai-entry)))
             (if is-tenpai
                 (push (list :text (format nil "~A: Tenpai (~{~A~^,~})"
                                           (player-name player)
                                           (mapcar #'tile-to-string waits))
                             :color :green)
                       lines)
                 (push (list :text (format nil "~A: Noten" (player-name player))
                             :color :white)
                       lines)))))
       ;; Some tenpai, some noten - calculate payments
       (t
         (let* ((num-tenpai (length tenpai-seats))
                (num-noten (length noten-seats))
                ;; Split pool among tenpai players
                (receive-each (floor total-pool num-tenpai))
                ;; Each noten pays their share
                (pay-each (floor total-pool num-noten)))
           (push (format nil "Tenpai: ~A  Noten: ~A" num-tenpai num-noten) lines)
           (push "" lines)
           (dolist (seat seats)
             (let* ((player (get-player game seat))
                    (is-tenpai (member seat tenpai-seats))
                    (tenpai-entry (assoc seat tenpai-data))
                    (waits (cdr tenpai-entry))
                    (change (if is-tenpai receive-each (- pay-each))))
               (if is-tenpai
                   (push (list :text (format nil "~A: ~@D (~{~A~^,~})"
                                             (player-name player) change
                                             (mapcar #'tile-to-string waits))
                               :color :green)
                         lines)
                   (push (list :text (format nil "~A: ~@D" (player-name player) change)
                               :color :red)
                         lines))))))))

      (make-modal-state
       :title "DRAW"
       :message-lines (nreverse lines)
       :buttons '("OK")
       :selected-index 0)))

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
    (push "" lines) ; separator
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
       :message-lines nil
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
                          (* (length buttons) 5) ; brackets and padding per button
                          (* (1- (length buttons)) 2))) ; spacing between
         ;; Width is max of title, messages, buttons + border padding
         (content-width (max title-len max-message-len button-width))
         (total-width (+ content-width 4)) ; 2 chars padding on each side
         ;; Height: top border, title, separator, message-lines, buttons, bottom border
         (num-lines (length message-lines))
         (total-height (+ 4 num-lines))) ; top, title, separator, lines, buttons, bottom
    (values total-width total-height)))

(defun draw-modal (scr modal)
  "Draw a modal dialog centered on screen using ACS box-drawing characters."
  (multiple-value-bind (width height) (calculate-modal-dimensions modal)
    (let* ((screen-height (height scr))
           (screen-width (width scr))
           (start-row (max 0 (- (floor screen-height 2) (floor height 2))))
           (start-col (max 0 (- (floor screen-width 2) (floor width 2))))
           (inner-width (- width 2)) ; Width inside borders
           (title (modal-state-title modal))
           (message-lines (modal-state-message-lines modal))
           (buttons (modal-state-buttons modal))
           (selected (modal-state-selected-index modal)))

      ;; Top border: ┌────────────┐
      (move scr start-row start-col)
      (add-char scr #\l :attributes '(:altcharset)) ; ┌
      (loop repeat inner-width do
              (add-char scr #\q :attributes '(:altcharset))) ; ─
      (add-char scr #\k :attributes '(:altcharset)) ; ┐

      ;; Title row: │   TITLE    │
      (move scr (+ start-row 1) start-col)
      (add-char scr #\x :attributes '(:altcharset)) ; │
      (let* ((title-padding (floor (- inner-width (length title)) 2))
             (title-str (format nil "~A~A~A"
                          (make-string title-padding :initial-element #\Space)
                          title
                          (make-string (- inner-width title-padding (length title))
                                       :initial-element #\Space))))
        (add-string scr title-str :attributes '(:bold)))
      (add-char scr #\x :attributes '(:altcharset)) ; │

      ;; Separator: ├────────────┤
      (move scr (+ start-row 2) start-col)
      (add-char scr #\t :attributes '(:altcharset)) ; ├
      (loop repeat inner-width do
              (add-char scr #\q :attributes '(:altcharset))) ; ─
      (add-char scr #\u :attributes '(:altcharset)) ; ┤

      ;; Message lines
      (loop for line in message-lines
            for row from (+ start-row 3)
            do (move scr row start-col)
              (add-char scr #\x :attributes '(:altcharset)) ; │
              (let* ((text (get-line-text line))
                     (color (get-line-color line))
                     (padding (floor (- inner-width (length text)) 2))
                     (attrs (case color
                              (:green '(:bold))
                              (:red '(:bold))
                              (t nil))))
                ;; Left padding
                (add-string scr (make-string padding :initial-element #\Space))
                ;; Text with optional color
                (case color
                  (:green
                   (add-string scr text :fgcolor :green :attributes attrs))
                  (:red
                   (add-string scr text :fgcolor :red :attributes attrs))
                  (t
                   (add-string scr text :attributes attrs)))
                ;; Right padding
                (let ((remaining (- inner-width padding (length text))))
                  (when (> remaining 0)
                        (add-string scr (make-string remaining :initial-element #\Space)))))
              (add-char scr #\x :attributes '(:altcharset))) ; │

      ;; Button row
      (let ((button-row (+ start-row 3 (length message-lines))))
        (move scr button-row start-col)
        (add-char scr #\x :attributes '(:altcharset)) ; │

        ;; Build button string with highlighting
        (let* ((button-strs (loop for btn in buttons
                                  for i from 0
                                  collect (if (= i selected)
                                              (format nil "[ ~A ]" btn)
                                              (format nil "  ~A  " btn))))
               (buttons-combined (format nil "~{~A~^  ~}" button-strs))
               (btn-padding (floor (- inner-width (length buttons-combined)) 2)))
          ;; Left padding
          (add-string scr (make-string btn-padding :initial-element #\Space))
          ;; Draw each button with appropriate highlighting
          (loop for btn in buttons
                for i from 0
                do (if (= i selected)
                       (progn
                        (add-string scr "[ ")
                        (add-string scr btn :attributes '(:reverse :bold))
                        (add-string scr " ]"))
                       (progn
                        (add-string scr "  ")
                        (add-string scr btn)
                        (add-string scr "  ")))
                  (when (< i (1- (length buttons)))
                        (add-string scr "  "))) ; Spacing between buttons
          ;; Right padding
          (let ((remaining (- inner-width btn-padding (length buttons-combined))))
            (when (> remaining 0)
                  (add-string scr (make-string remaining :initial-element #\Space)))))

        (add-char scr #\x :attributes '(:altcharset)) ; │

        ;; Bottom border: └────────────┘
        (move scr (+ button-row 1) start-col)
        (add-char scr #\m :attributes '(:altcharset)) ; └
        (loop repeat inner-width do
                (add-char scr #\q :attributes '(:altcharset))) ; ─
        (add-char scr #\j :attributes '(:altcharset)))))) ; ┘

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

     ;; Escape - auto-select escape button if defined
     ((or (eq key-name :escape) (eql key-name #\Esc))
       (let ((escape-button (modal-state-escape modal)))
         (if escape-button
             (progn
              (setf (modal-state-result modal) escape-button)
              escape-button)
             :continue)))

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
     ((eq suit :man) '(:red :black)) ; red for characters
     ((eq suit :pin) '(:blue :black)) ; blue for dots
     ((eq suit :sou) '(:green :black)) ; green for bamboo
     ;; Dragons - check the tile itself
     ((eq tile :red) '(:red :black)) ; red dragon
     ((eq tile :green) '(:green :black)) ; green dragon
     ((eq tile :white) '(:blue :black)) ; white dragon (blue)
     (t nil)))) ; default for winds

(defun draw-tile-block (scr tile row col &optional (hidden nil) (first-in-row nil) (last-in-row nil) (highlighted nil) (is-valid nil))
  "Draw a single tile as a block using ACS box-drawing characters with shared borders.
   Format (horizontal):
   ┌──┬──┬──┐  (first ── middle ── last)
   │  │  │  │
   └──┴──┴──┘
   HIGHLIGHTED makes the tile reverse video (selected).
   IS-VALID highlights the tile border in green (valid for riichi discard)."
  (let ((tile-str (if hidden
                      "  "
                      (tile-to-string tile)))
        (start-col (if first-in-row col (1+ col)))
        (border-attrs (if is-valid '(:altcharset :bold) '(:altcharset))))
    ;; Top border
    (move scr row start-col)
    (when first-in-row
          (add-char scr #\l :attributes border-attrs)) ; upper-left corner ┌
    (add-char scr #\q :attributes border-attrs) ; horizontal ─
    (add-char scr #\q :attributes border-attrs) ; horizontal ─
    (if last-in-row
        (add-char scr #\k :attributes border-attrs) ; upper-right corner ┐
        (add-char scr #\w :attributes border-attrs)) ; top tee ┬

    ;; Middle with tile content
    (move scr (+ row 1) start-col)
    (when first-in-row
          (add-char scr #\x :attributes border-attrs)) ; vertical │
    (let ((attrs (cond
                  ((and highlighted is-valid) '(:reverse :bold))
                  (highlighted '(:reverse))
                  (is-valid '(:bold))
                  (t nil))))
      (if hidden
          (add-string scr tile-str :attributes attrs)
          (add-string scr tile-str :color-pair (if is-valid '(:green :black) (get-tile-color-pair tile)) :attributes attrs)))
    (add-char scr #\x :attributes border-attrs) ; vertical │

    ;; Bottom border
    (move scr (+ row 2) start-col)
    (when first-in-row
          (add-char scr #\m :attributes border-attrs)) ; lower-left corner └
    (add-char scr #\q :attributes border-attrs) ; horizontal ─
    (add-char scr #\q :attributes border-attrs) ; horizontal ─
    (if last-in-row
        (add-char scr #\j :attributes border-attrs) ; lower-right corner ┘
        (add-char scr #\v :attributes border-attrs)))) ; bottom tee ┴

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
          (move scr row col)
          (add-char scr #\l :attributes '(:altcharset)) ; upper-left corner ┌
          (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
          (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
          (add-char scr #\k :attributes '(:altcharset))) ; upper-right corner ┐

    ;; Middle with tile content
    (move scr (+ row 1) col)
    (add-char scr #\x :attributes '(:altcharset)) ; vertical │
    (if hidden
        (add-string scr tile-str)
        (add-string scr tile-str :color-pair (get-tile-color-pair tile)))
    (add-char scr #\x :attributes '(:altcharset)) ; vertical │

    ;; Bottom border
    (move scr (+ row 2) col)
    (if last-in-column
        (progn
         ;; Last tile: use corners └──┘
         (add-char scr #\m :attributes '(:altcharset)) ; lower-left corner └
         (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
         (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
         (add-char scr #\j :attributes '(:altcharset))) ; lower-right corner ┘
        (progn
         ;; Not last: use left/right tees ├──┤
         (add-char scr #\t :attributes '(:altcharset)) ; left tee ├
         (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
         (add-char scr #\q :attributes '(:altcharset)) ; horizontal ─
         (add-char scr #\u :attributes '(:altcharset)))))) ; right tee ┤

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
     ((and is-top is-left) #\l) ; ┌ top-left
     ((and is-top is-right) #\k) ; ┐ top-right
     ((and is-bottom is-left) #\m) ; └ bottom-left
     ((and is-bottom is-right) #\j) ; ┘ bottom-right
     ;; Edges
     (is-top #\w) ; ┬ top tee
     (is-bottom #\v) ; ┴ bottom tee
     (is-left #\t) ; ├ left tee
     (is-right #\u) ; ┤ right tee
     ;; Interior
     (t #\n)))) ; ┼ cross

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
            (move scr screen-y col)
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
                    (add-char scr
                              (cond
                               ((and is-first-tile is-first-row) #\l) ; ┌
                               ((and is-first-tile has-above) #\t) ; ├
                               ((and is-first-tile (not has-above)) #\l) ; ┌
                               ;; At the right edge of prev-row, need special handling
                               (at-prev-edge #\n) ; ┼ (connects to prev-row's right edge)
                               ((and has-above (not is-first-tile)) #\n) ; ┼
                               (t #\w)) ; ┬
                              :attributes border-attrs)
                    ;; Horizontal line
                    (add-char scr #\q :attributes border-attrs)
                    (add-char scr #\q :attributes border-attrs))
            ;; Right edge of row
            (let* ((has-above-at-end (and prev-row (>= prev-num-tiles num-tiles)))
                   (prev-row-longer (and prev-row (> prev-num-tiles num-tiles))))
              (add-char scr
                        (cond
                         (is-first-row #\k) ; ┐
                         (prev-row-longer #\v) ; ┴ (no content below, connects to trailing border)
                         (has-above-at-end #\u) ; ┤ (prev-row ends exactly here or extends)
                         (t #\k)) ; ┐ (prev-row is shorter)
                        :attributes border-attrs))
            ;; Draw trailing borders for previous row (if it was longer)
            ;; Each phantom tile position needs: ── (horizontal) then ┴ or ┘ (junction)
            (when (and prev-row (> prev-num-tiles num-tiles))
                  (loop for extra-idx from num-tiles below prev-num-tiles
                        for is-last-extra = (= extra-idx (1- prev-num-tiles))
                        do (add-char scr #\q :attributes border-attrs) ; ─
                          (add-char scr #\q :attributes border-attrs) ; ─
                          (add-char scr (if is-last-extra #\j #\v) ; ┘ or ┴
                                    :attributes border-attrs)))

            ;; Draw content line
            (move scr (1+ screen-y) col)
            (add-char scr #\x :attributes border-attrs) ; │
            (loop for tile in tile-row
                  do (add-string scr (tile-to-string tile)
                                 :color-pair (get-tile-color-pair tile))
                    (add-char scr #\x :attributes border-attrs))) ; │

    ;; Draw bottom border after last row
    (let* ((last-row (nth (1- num-rows) rows))
           (num-tiles (length last-row))
           (screen-y (+ row (* num-rows 2))))
      (move scr screen-y col)
      (add-char scr #\m :attributes border-attrs) ; └
      (loop for tile-idx from 0 below num-tiles
            do (add-char scr #\q :attributes border-attrs)
              (add-char scr #\q :attributes border-attrs)
              (when (< tile-idx (1- num-tiles))
                    (add-char scr #\v :attributes border-attrs))) ; ┴
      (add-char scr #\j :attributes border-attrs))))

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
     ((and is-top is-left) #\l) ; ┌
     ((and is-top is-right) #\k) ; ┐
     ((and is-bottom is-left) #\m) ; └
     ((and is-bottom is-right) #\j) ; ┘
     ;; Edges
     (is-top #\w) ; ┬
     (is-bottom #\v) ; ┴
     (is-left #\t) ; ├
     (is-right #\u) ; ┤
     ;; Interior
     (t #\n)))) ; ┼

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
                    (move scr screen-y col)
                    ;; Draw left junction (or space if first column ended)
                    (let ((first-state (first col-states)))
                      (if (member first-state '(:active :ending))
                          (add-char scr
                                    (cond
                                     ((and is-first-tile-row (eq first-state :active)) #\l)
                                     ((eq first-state :ending) #\m)
                                     ((eq first-state :active) #\t)
                                     (t #\Space))
                                    :attributes border-attrs)
                          (add-char scr #\Space)))
                    ;; Draw each column: 2 horizontal chars + 1 junction
                    (loop for col-idx from 0 below num-cols
                          for state = (nth col-idx col-states)
                          for is-last-col = (= col-idx (1- num-cols))
                          for next-state = (if is-last-col nil (nth (1+ col-idx) col-states))
                          do
                            ;; Horizontal line (or spaces if ended)
                            (if (member state '(:active :ending))
                                (progn
                                 (add-char scr #\q :attributes border-attrs)
                                 (add-char scr #\q :attributes border-attrs))
                                (progn
                                 (add-char scr #\Space)
                                 (add-char scr #\Space)))
                            ;; Right junction/separator
                            (add-char scr
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
                      (move scr (1+ screen-y) col)
                      ;; Draw left border (│ if first column has tile, space if not)
                      (if (< tile-row (length (first columns)))
                          (add-char scr #\x :attributes border-attrs)
                          (add-char scr #\Space))
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
                                    (add-string scr (tile-to-string tile)
                                                :color-pair (get-tile-color-pair tile)))
                                  (progn
                                   (add-char scr #\Space)
                                   (add-char scr #\Space)))
                              ;; Draw separator only if current or next has content
                              (if (or has-tile next-has-tile)
                                  (add-char scr #\x :attributes border-attrs)
                                  (add-char scr #\Space)))))))))


(defun draw-hand-horizontal (scr player row col &optional (hidden nil) (selected-index nil) (valid-tiles nil) (is-dealer nil) (game nil))
  "Draw a player's hand horizontally (for top/bottom positions).
   If player has a drawn tile (one more than expected), draw it separately.
   VALID-TILES is a list of tiles to highlight as valid for riichi discard.
   IS-DEALER: if true, adds (DEALER) label.
   GAME: if provided, enables no-yaku warning display."
  (let* ((hand (player-hand player))
         (num-tiles (length hand))
         (num-melds (length (player-melds player)))
         ;; Expected hand size: 13 tiles minus 3 per meld (pon/chi take 2 from hand + 1 called)
         ;; Kan takes 3 or 4, but let's use 3 as base
         (expected-tiles (- 13 (* 3 num-melds)))
         ;; Has drawn tile when we have one more than expected
         (has-drawn-tile (> num-tiles expected-tiles))
         ;; Split into main hand and drawn tile if applicable
         (main-hand (if has-drawn-tile (butlast hand) hand))
         (drawn-tile (when has-drawn-tile (car (last hand)))))
    ;; Draw player info: name, seat, score
    (move scr row col)
    (add-string scr (format nil "~A [~A] ~D pts"
                      (player-name player)
                      (player-seat player)
                      (player-score player)))
    (when is-dealer
          (add-string scr " (DEALER)" :color-pair '(:yellow :black)))
    (when (player-riichi-declared player)
          (add-string scr " *RIICHI*" :color-pair '(:red :black)))
    ;; Display furiten or tenpai status
    ;; If a tile is selected, show hypothetical waits after discarding it
    (let* ((full-hand (player-hand player))
           (melds (player-melds player))
           (num-melds (length melds))
           ;; Expected hand size = 13 - 3*melds (e.g., 7 tiles with 2 melds)
           (expected-tiles (- 13 (* 3 num-melds)))
           ;; Has drawn tile when we have more than expected
           (has-drawn (> (length full-hand) expected-tiles))
           ;; If tile selected, calculate waits for hand AFTER discarding selected
           (selected-tile (when (and selected-index (< selected-index (length full-hand)))
                           (nth selected-index full-hand)))
           ;; Hypothetical hand without the selected tile
           (hypo-hand (if selected-tile
                          (remove selected-tile full-hand :test #'eq :count 1)
                          full-hand))
           (hypo-shanten (calculate-shanten hypo-hand melds)))
      ;; Check for furiten first (on current full hand waits)
      (let ((furiten-tiles (find-furiten-tiles player)))
        (cond
          ;; Furiten takes precedence
          (furiten-tiles
           (add-string scr (format nil " FURITEN(~{~A~^,~})"
                                   (mapcar #'tile-to-string furiten-tiles))
                       :color-pair '(:red :black)))
          ;; If tile selected and would be tenpai after discard, show hypothetical waits
          ((and selected-tile (zerop hypo-shanten))
           (let ((waits (calculate-waiting-tiles hypo-hand melds)))
             (when waits
               (add-string scr (format nil " TENPAI(~{~A~^,~})"
                                       (mapcar #'tile-to-string waits))
                           :color-pair '(:cyan :black))
               ;; Check for no yaku if game context available
               (when (and game
                          (not (has-yaku-for-any-wait hypo-hand melds waits game (player-seat player))))
                 (add-string scr " (No Yaku!)" :color-pair '(:magenta :black))))))
          ;; Otherwise if already in tenpai (has drawn tile), show current waits
          ((and has-drawn
                (zerop (calculate-shanten (butlast full-hand) melds)))
           (let* ((tenpai-hand (butlast full-hand))
                  (waits (calculate-waiting-tiles tenpai-hand melds)))
             (when waits
               (add-string scr (format nil " TENPAI(~{~A~^,~})"
                                       (mapcar #'tile-to-string waits))
                           :color-pair '(:green :black))
               ;; Check for no yaku if game context available
               (when (and game
                          (not (has-yaku-for-any-wait tenpai-hand melds waits game (player-seat player))))
                 (add-string scr " (No Yaku!)" :color-pair '(:magenta :black)))))))))

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
                              is-valid)) ; Pass validity for highlighting

    ;; Draw 14th tile separately with gap if present
    (when drawn-tile
          (let ((gap-offset 2) ; 2 character gap
                              (is-valid (and valid-tiles (member drawn-tile valid-tiles :test #'eq)))
                              (main-hand-len (length main-hand)))
            (draw-tile-block scr drawn-tile (+ row 1)
                             (+ col (* main-hand-len 3) gap-offset)
                             hidden
                             t ; first-in-row (standalone)
                             t ; last-in-row (standalone)
                             (and selected-index (= selected-index main-hand-len))
                             is-valid)))))

(defun draw-hand-vertical (scr player row col &optional (hidden nil) (align-right nil) (is-dealer nil))
  "Draw a player's hand vertically (for left/right positions).
   If align-right is true, name is right-aligned to fit on screen.
   If player has a drawn tile (one more than expected), draw it separately.
   IS-DEALER: if true, adds (DEALER) label."
  (let* ((hand (player-hand player))
         (num-tiles (length hand))
         (num-melds (length (player-melds player)))
         ;; Expected hand size: 13 tiles minus 3 per meld
         (expected-tiles (- 13 (* 3 num-melds)))
         ;; Has drawn tile when we have one more than expected
         (has-drawn-tile (> num-tiles expected-tiles))
         ;; Split into main hand and drawn tile if applicable
         (main-hand (if has-drawn-tile (butlast hand) hand))
         (drawn-tile (when has-drawn-tile (car (last hand))))
         (base-info (format nil "~A [~A] ~D"
                      (player-name player)
                      (player-seat player)
                      (player-score player)))
         (dealer-str (if is-dealer " (DEALER)" ""))
         (riichi-str (if (player-riichi-declared player) " *RIICHI*" ""))
         (total-len (+ (length base-info) (length dealer-str) (length riichi-str)))
         (name-col (if align-right
                       (max 0 (- (+ col 4) total-len)) ; Right-align to end at tile right edge
                       col)))
    ;; Draw player info: name, seat, score
    (move scr row name-col)
    (add-string scr base-info)
    (when is-dealer
          (add-string scr dealer-str :color-pair '(:yellow :black)))
    (when (player-riichi-declared player)
          (add-string scr riichi-str :color-pair '(:red :black)))

    ;; Draw main hand (13 tiles or fewer) - first tile is 3 rows, subsequent tiles are 2 rows (shared border)
    (loop for tile in main-hand
          for i from 0
          do (draw-tile-block-vertical scr tile
                                       (+ row 2 (* i 2)) ; spacing: 2 rows per tile (1 row shared)
                                       col
                                       hidden
                                       (= i 0)
                                       (= i (1- (length main-hand)))))

    ;; Draw 14th tile separately with gap if present
    (when drawn-tile
          (let ((gap-rows 1)) ; 1 row gap
            (draw-tile-block-vertical scr drawn-tile
                                      (+ row 2 (* (length main-hand) 2) gap-rows)
                                      col
                                      hidden
                                      t ; first-in-column (standalone)
                                      t)))))

;;; ============================================================================
;;; Called Meld Display
;;; ============================================================================

(defun meld-to-string (meld)
  "Convert a meld to a display string like '5m pon' or '1p-2p-3p chi'.
   TODO: Upgrade to box-drawing character version for visual consistency."
  (let* ((type (meld-type meld))
         (tiles (meld-tiles meld))
         (type-str (case type
                     (:pon "pon")
                     (:chi "chi")
                     (:kan-open "kan")
                     (:kan-closed "[kan]")
                     (:kan-added "kan+")
                     (t "?"))))
    (case type
      (:chi
       ;; Show sequence: 1p-2p-3p chi
       (format nil "~{~A~^-~} ~A"
         (mapcar #'tile-to-string tiles)
         type-str))
      (t
       ;; Show first tile + type: 5m pon
       (format nil "~A ~A" (tile-to-string (first tiles)) type-str)))))

(defun draw-called-melds (scr player row col &optional (direction :horizontal))
  "Draw a player's called melds as text (e.g., '5m pon', '1p-2p-3p chi').
   DIRECTION can be :horizontal (melds side by side) or :vertical (stacked).
   ROW/COL is the starting position."
  (let ((melds (player-melds player)))
    (when melds
          (if (eq direction :horizontal)
              ;; Draw horizontally (for top/bottom players)
              (let ((x col))
                (dolist (meld melds)
                  (let ((meld-str (meld-to-string meld)))
                    (move scr row x)
                    (add-string scr meld-str)
                    (incf x (+ (length meld-str) 2))))) ; 2 space gap between melds
              ;; Draw vertically (for left/right players)  
              (let ((y row))
                (dolist (meld melds)
                  (let ((meld-str (meld-to-string meld)))
                    (move scr y col)
                    (add-string scr meld-str)
                    (incf y))))))))

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

(defun get-clicked-tile-index (mouse-y mouse-x hand-row hand-col hand-size &optional (num-melds 0))
  "Determine which tile was clicked based on mouse coordinates.
   Returns tile index or NIL if click was not on a tile.
   Each tile is 3 chars wide (including shared border), 3 rows tall.
   NUM-MELDS is the number of open melds the player has."
  (when (and (>= mouse-y (+ hand-row 1))
             (<= mouse-y (+ hand-row 2))) ; Middle row of tiles
        ;; Expected hand size without drawn tile = 13 - 3*melds
        (let* ((expected-hand-size (- 13 (* 3 num-melds)))
               ;; Has drawn tile when hand is larger than expected
               (has-drawn-tile (> hand-size expected-hand-size))
               (main-hand-size (if has-drawn-tile (1- hand-size) hand-size))
               (main-hand-width (* main-hand-size 3))
               (relative-x (- mouse-x hand-col)))
          (cond
           ;; Click on main hand tiles
           ((and (>= relative-x 0)
                 (< relative-x main-hand-width))
             (let ((tile-index (floor relative-x 3)))
               (when (< tile-index main-hand-size)
                     tile-index)))
           ;; Click on drawn tile (if present)
           (has-drawn-tile
             (let* ((gap-offset 2)
                    (drawn-tile-start (+ main-hand-width gap-offset))
                    (drawn-tile-end (+ drawn-tile-start 4))) ; 4 chars wide (standalone)
               (when (and (>= relative-x drawn-tile-start)
                          (< relative-x drawn-tile-end))
                     main-hand-size)))))))

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
                             (t key)))) ; key is already a character or keyword
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
         (if drawn-tile
             ;; Got a tile - process normally
             (progn
              (player-add-tile human-player drawn-tile)
              (auto-extract-north-if-needed game human-player)
              (player-sort-hand-keep-drawn human-player)

              ;; Check for tsumo opportunity (player has complete hand)
              (cond
               ;; Winning hand - show tsumo modal
               ((and ui-state (check-tsumo-opportunity human-player))
                 (setf (ui-state-active-modal ui-state) (make-tsumo-modal drawn-tile)))

               ;; Check for closed kan opportunity (has 4 of same tile)
               ;; Skip for riichi players for now (could add wait-preserving kan later)
               ((and ui-state
                     (not (player-riichi-declared human-player))
                     (find-closed-kan-tiles human-player))
                 (let ((kan-tile (first (find-closed-kan-tiles human-player))))
                   (setf (ui-state-active-modal ui-state) (make-closed-kan-modal kan-tile))
                   (setf (game-phase game) :turn-discard)))

               ;; Check for added kan opportunity (has tile matching existing pon)
               ;; Skip for riichi players for now
               ((and ui-state
                     (not (player-riichi-declared human-player))
                     (find-added-kan-tiles human-player))
                 (let ((kan-tile (first (find-added-kan-tiles human-player))))
                   (setf (ui-state-active-modal ui-state) (make-added-kan-modal kan-tile))
                   (setf (game-phase game) :turn-discard)))

               ;; Player is in riichi - check for wait-preserving closed kan, then auto-discard
               ((player-riichi-declared human-player)
                 ;; Check if drawn tile can complete a closed kan WITHOUT changing the wait
                 ;; In riichi, only closed kan is allowed, and only if wait is preserved
                 (let* ((drawn (car (last (player-hand human-player))))
                        (count-in-hand (count drawn (player-hand human-player) :test #'eq))
                        (can-kan (and (= count-in-hand 4)
                                      (kan-preserves-wait-p human-player drawn))))
                   (if (and ui-state can-kan)
                       ;; Can declare closed kan on drawn tile (wait is preserved)
                       (progn
                        (setf (ui-state-active-modal ui-state) (make-closed-kan-modal drawn))
                        (setf (game-phase game) :turn-discard))
                       ;; No valid kan opportunity - auto-discard the drawn tile
                       (let ((discard-tile (find-safe-riichi-discard human-player)))
                         (when discard-tile
                               (player-discard-tile human-player discard-tile)
                               (setf (game-phase game) :call-window)
                               (advance-turn game)
                               (setf (game-phase game) :turn-draw))))))

               ;; Check if can declare riichi (closed hand and has valid discards that maintain tempai)
               ((and ui-state
                     (player-can-declare-riichi human-player)
                     (calculate-valid-riichi-discards human-player)) ; Non-nil means at least one valid discard
                                                                    (setf (ui-state-active-modal ui-state) (make-riichi-modal))
                                                                    (setf (game-phase game) :turn-discard))

               ;; Normal discard phase
               (t
                 (setf (game-phase game) :turn-discard))))
             ;; Wall is empty - exhaustive draw
             (when ui-state
                   (setf (ui-state-active-modal ui-state) (make-exhaustive-draw-modal game))
                   (setf (game-phase game) :hand-ended)))))

     ;; AI auto-draw (just draw, don't discard yet)
     ((and (eq phase :turn-draw)
           (not (eq current-seat human-seat)))
       (let ((drawn-tile (wall-draw-tile wall)))
         (if drawn-tile
             ;; Got a tile - process normally
             (progn
              (player-add-tile current drawn-tile)
              (auto-extract-north-if-needed game current)
              (player-sort-hand-keep-drawn current) ; Keep drawn tile at end for debug visibility

              ;; Transition to discard phase, but don't discard yet
              (setf (game-phase game) :turn-discard))
             ;; Wall is empty - exhaustive draw
             (when ui-state
                   (setf (ui-state-active-modal ui-state) (make-exhaustive-draw-modal game))
                   (setf (game-phase game) :hand-ended)))))

     ;; AI auto-discard (separate phase to allow rendering of 14 tiles)
     ((and (eq phase :turn-discard)
           (not (eq current-seat human-seat)))
       ;; Discard the drawn tile (14th tile) for easier debugging
       (let ((discard-tile (nth 13 (player-hand current))))
         (when discard-tile
               (player-discard-tile current discard-tile)

               ;; Store the discard info in turn state before checking calls
               (let ((turn-state (game-turn-state game)))
                 (setf (turn-last-discard turn-state) discard-tile
                   (turn-last-discard-player turn-state) current-seat))

               ;; Check for call opportunities for human player
               (let* ((config (game-config game))
                      (can-ron (and ui-state
                                    (check-ron-opportunity human-player discard-tile
                                                           game human-seat current-seat)
                                    (not (player-furiten human-player))))
                      (can-kan (and (not (player-riichi-declared human-player))
                                    (player-can-call-kan human-player discard-tile :open)))
                      (can-pon (and (not (player-riichi-declared human-player))
                                    (player-can-call-pon human-player discard-tile)))
                      (chi-sequences (and (not (player-riichi-declared human-player))
                                          (game-config-chi-enabled-p config)
                                          (eq current-seat (prev-seat human-seat config))
                                          (player-can-call-chi human-player discard-tile human-seat current-seat)))
                      ;; Collect all available call options
                      (call-options (collect-call-options human-player discard-tile human-seat current-seat config
                                                          :can-ron-p can-ron
                                                          :can-kan-p can-kan
                                                          :can-pon-p can-pon
                                                          :chi-sequences chi-sequences)))
                 (if call-options
                     ;; Show multi-option modal with all available calls
                     (progn
                      (setf (ui-state-active-modal ui-state)
                        (make-call-options-modal call-options discard-tile))
                      (setf (game-phase game) :call-window))
                     ;; No call opportunity - advance turn normally
                     (progn
                      (setf (game-phase game) :call-window)
                      (advance-turn game)
                      (setf (game-phase game) :turn-draw))))))))))

(defun handle-mouse-click (game ui-state event click-type)
  "Handle mouse click on tiles. CLICK-TYPE is :single or :double."
  (handler-case
      (let* ((human-seat (ui-state-human-seat ui-state))
             (human-player (get-player game human-seat))
             (hand-size (length (player-hand human-player)))
             (num-melds (length (player-melds human-player)))
             (mouse-y (position-y event))
             (mouse-x (position-x event)))
        ;; Only proceed if we have valid mouse coordinates and hand position
        (when (and mouse-y mouse-x (ui-state-hand-position ui-state))
              (destructuring-bind (hand-row hand-col) (ui-state-hand-position ui-state)
                (let ((tile-index (get-clicked-tile-index mouse-y mouse-x hand-row hand-col hand-size num-melds)))
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
         (hand (player-hand human-player)) ; Use hand as-is, already sorted with drawn tile at end
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
                    (winning-tile (nth 13 (player-hand human-player))) ; Drawn tile is last
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

     ;; Closed Kan decision (ankan - 4 tiles from hand)
     ((string= title "CLOSED KAN?")
       (if (string= result "KAN")
           ;; Player declares closed kan
           (let* ((human-seat (ui-state-human-seat ui-state))
                  (human-player (get-player game human-seat))
                  (kan-tiles (find-closed-kan-tiles human-player))
                  (tile (first kan-tiles)))
             (when tile
                   ;; Remove 4 tiles from hand
                   (player-remove-tile human-player tile)
                   (player-remove-tile human-player tile)
                   (player-remove-tile human-player tile)
                   (player-remove-tile human-player tile)
                   ;; Create closed kan meld (from :self means closed)
                   (let ((meld (make-meld :type :kan-closed
                                          :tiles (list tile tile tile tile)
                                        :from nil
                                          :called-tile nil)))
                     (player-add-meld human-player meld))
                   ;; Draw replacement tile from dead wall
                   (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
                     (when replacement-tile
                           (player-add-tile human-player replacement-tile)
                           (player-sort-hand-keep-drawn human-player)))
                   ;; Reveal next dora indicator
                   (wall-reveal-next-dora (game-wall game))
                   (setf (game-phase game) :turn-discard)))
           ;; Player chose to skip - continue to normal discard
           (setf (game-phase game) :turn-discard)))

     ;; Added Kan decision (kakan - upgrade pon to kan)
     ((string= title "ADDED KAN?")
       (if (string= result "KAN")
           ;; Player upgrades pon to kan
           (let* ((human-seat (ui-state-human-seat ui-state))
                  (human-player (get-player game human-seat))
                  (kan-tiles (find-added-kan-tiles human-player))
                  (tile (first kan-tiles)))
             (when tile
                   ;; Remove 1 tile from hand
                   (player-remove-tile human-player tile)
                   ;; Find and upgrade the pon meld to kan
                   (let ((pon-meld (find-if (lambda (meld)
                                              (and (eq (meld-type meld) :pon)
                                                   (member tile (meld-tiles meld) :test #'eq)))
                                       (player-melds human-player))))
                     (when pon-meld
                           ;; Upgrade meld type to kan and add 4th tile
                           (setf (meld-type pon-meld) :kan-added)
                           (push tile (meld-tiles pon-meld))))
                   ;; Draw replacement tile from dead wall
                   (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
                     (when replacement-tile
                           (player-add-tile human-player replacement-tile)
                           (player-sort-hand-keep-drawn human-player)))
                   ;; Reveal next dora indicator
                   (wall-reveal-next-dora (game-wall game))
                   (setf (game-phase game) :turn-discard)))
           ;; Player chose to skip - continue to normal discard
           (setf (game-phase game) :turn-discard)))

     ;; Multi-option call modal
     ((string= title "CALL?")
       (if (string= result "SKIP")
           ;; Player chose to skip - advance turn normally
           (progn
            (advance-turn game)
            (setf (game-phase game) :turn-draw))
           ;; Find the selected call option and process it
           (let* ((call-options (modal-state-call-options modal))
                  (selected-option (find result call-options
                                     :key #'call-option-label
                                     :test #'string=)))
             (when selected-option
                   (let* ((call-type (call-option-type selected-option))
                          (human-seat (ui-state-human-seat ui-state))
                          (human-player (get-player game human-seat))
                          (turn-state (game-turn-state game))
                          (tile (turn-last-discard turn-state))
                          (discarder-seat (turn-last-discard-player turn-state))
                          (config (game-config game))
                          (relation (seat-relation human-seat discarder-seat config)))
                     (case call-type
                       ;; Ron
                       (:ron
                        (let* ((winning-tile tile)
                               (hand (cons winning-tile (player-hand human-player)))
                               (melds (player-melds human-player))
                               (context (build-winning-context game human-seat winning-tile nil discarder-seat))
                               (score-result (calculate-score hand melds winning-tile context)))
                          (setf (ui-state-pending-win ui-state)
                            (list :type :ron
                                  :winning-tile winning-tile
                                  :loser-seat discarder-seat
                                  :score-result score-result))
                          (setf (ui-state-active-modal ui-state)
                            (make-win-modal (player-name human-player) score-result game human-seat nil))
                          (setf (game-phase game) :hand-ended)))

                       ;; Kan
                       (:kan
                        (player-remove-tile human-player tile)
                        (player-remove-tile human-player tile)
                        (player-remove-tile human-player tile)
                        (let ((meld (make-meld :type :kan-open
                                               :tiles (list tile tile tile tile)
                                             :from relation
                                               :called-tile tile)))
                          (player-add-meld human-player meld))
                        (player-sort-hand human-player)
                        (let ((replacement-tile (wall-draw-kan-replacement (game-wall game))))
                          (when replacement-tile
                                (player-add-tile human-player replacement-tile)
                                (player-sort-hand-keep-drawn human-player)))
                        (setf (turn-current-player turn-state) human-seat)
                        (wall-reveal-next-dora (game-wall game))
                        (setf (game-phase game) :turn-discard))

                       ;; Pon
                       (:pon
                        (player-remove-tile human-player tile)
                        (player-remove-tile human-player tile)
                        (let ((meld (make-meld :type :pon
                                               :tiles (list tile tile tile)
                                             :from relation
                                               :called-tile tile)))
                          (player-add-meld human-player meld))
                        (player-sort-hand human-player)
                        (setf (turn-current-player turn-state) human-seat)
                        (setf (game-phase game) :turn-discard))

                       ;; Chi (with specific sequence from call-option-tiles)
                       (:chi
                        (let ((chosen-sequence (call-option-tiles selected-option)))
                          (dolist (t1 chosen-sequence)
                            (unless (eq t1 tile)
                              (player-remove-tile human-player t1)))
                          (let ((meld (make-meld :type :chi
                                                 :tiles chosen-sequence
                                               :from relation
                                                 :called-tile tile)))
                            (player-add-meld human-player meld))
                          (player-sort-hand human-player)
                          (setf (turn-current-player turn-state) human-seat)
                          (setf (game-phase game) :turn-discard)))))))))

     ;; Win acknowledgment - calculate score and start new round
     ((or (string= title "RON") (string= title "TSUMO"))
       (process-win-and-start-new-round game ui-state))

     ;; Exhaustive draw acknowledgment - apply tenpai payments and start new round
     ((string= title "DRAW")
       (process-exhaustive-draw game ui-state)))))

;;; ============================================================================
;;; Round Transition
;;; ============================================================================

(defun process-win-and-start-new-round (game ui-state)
  "Process the winning hand, distribute points, and start a new round.
   If the winner was dealer, dealer stays. Otherwise rotate dealer."
  (let* ((pending-win (ui-state-pending-win ui-state))
         (win-type (getf pending-win :type))
         (winning-tile (getf pending-win :winning-tile))
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
                           :winning-tile winning-tile
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

(defun process-exhaustive-draw (game ui-state)
  "Process exhaustive draw (ryuukyoku): apply tenpai payments and start new round.
   Tenpai players receive points from noten players.
   Dealer rotates if dealer is noten (paid points)."
  (let* ((config (game-config game))
         (seats (game-config-seats config))
         (dealer-seat (game-dealer-seat game))
         (tenpai-seats nil)
         (noten-seats nil))

    ;; Determine tenpai status for each player
    (dolist (seat seats)
      (let* ((player (get-player game seat))
             (hand (player-hand player))
             (melds (player-melds player))
             (shanten (calculate-shanten hand melds)))
        (if (zerop shanten)
            (push seat tenpai-seats)
            (push seat noten-seats))))

    ;; Apply tenpai payments if some tenpai and some noten
    (when (and tenpai-seats noten-seats)
          (let* ((num-tenpai (length tenpai-seats))
                 (num-noten (length noten-seats))
                 (total-pool (* 2000 num-noten))
                 (receive-each (floor total-pool num-tenpai))
                 (pay-each (floor total-pool num-noten)))
            ;; Tenpai players receive
            (dolist (seat tenpai-seats)
              (let ((player (get-player game seat)))
                (incf (player-score player) receive-each)))
            ;; Noten players pay
            (dolist (seat noten-seats)
              (let ((player (get-player game seat)))
                (decf (player-score player) pay-each)))))

    ;; Handle dealer rotation
    ;; Dealer stays if tenpai, rotates if noten (or if nobody was noten - no payment case)
    (let ((dealer-is-tenpai (member dealer-seat tenpai-seats)))
      (if dealer-is-tenpai
          ;; Dealer is tenpai - stays, increment honba
          (incf (game-honba game))
          ;; Dealer is noten - rotates, reset honba
          (progn
           (setf (game-dealer-seat game) (next-seat dealer-seat config))
           (setf (game-honba game) 0))))

    ;; Clear pending data
    (setf (ui-state-pending-win ui-state) nil)

    ;; Reset all players for new hand
    (dolist (seat seats)
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

(defun check-ron-opportunity (player tile &optional game player-seat loser-seat)
  "Check if adding TILE to player's hand would create a winning hand with yaku.
   Also checks that player is not in furiten.
   When GAME is provided, also verifies the hand has at least 1 valid yaku."
  (let* ((test-hand (cons tile (player-hand player)))
         (melds (player-melds player)))
    ;; First check structural validity and furiten
    (when (and (validate-hand test-hand melds)
               (not (player-check-furiten player tile)))
      ;; If game context provided, verify yaku existence
      (if game
          (let* ((context (build-winning-context game player-seat tile nil loser-seat))
                 (score-result (calculate-score test-hand melds tile context)))
            (not (null score-result)))  ; Has yaku if score-result is non-nil
          ;; No game context - just structural check (backwards compat)
          t))))

(defun find-safe-riichi-discard (player)
  "Find the drawn tile for auto-discard during riichi.
   The drawn tile is always at position 14 (index 13) since the hand
   is sorted with the drawn tile kept separate at the end."
  (let ((hand (player-hand player)))
    (when (= (length hand) 14)
          (nth 13 hand)))) ; Drawn tile is at the end

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

;;; ============================================================================
;;; 8-Segment Display for Tile Counter (ASCII only)
;;; ============================================================================

(defun draw-8-segment-digit (scr digit row col)
  "Draw a single digit (0-9) as an 8-segment ASCII display.
   Each digit is 5 chars wide and 5 rows tall.
   Each row is one of: :full :outside :left :right
   
   ;; 0: *****    1:     *    2: *****    3: *****    4: *   *
   ;;    *   *           *           *           *       *   *
   ;;    *   *           *       *****       *****       *****
   ;;    *   *           *       *               *           *
   ;;    *****           *       *****       *****           *
   ;;
   ;; 5: *****    6: *****    7: *****    8: *****    9: *****
   ;;    *           *               *       *   *       *   *
   ;;    *****       *****           *       *****       *****
   ;;        *       *   *           *       *   *           *
   ;;    *****       *****           *       *****       *****"
  (let ((rows (case digit
                (0 '(:full :outside :outside :outside :full))
                (1 '(:right :right :right :right :right))
                (2 '(:full :right :full :left :full))
                (3 '(:full :right :full :right :full))
                (4 '(:outside :outside :full :right :right))
                (5 '(:full :left :full :right :full))
                (6 '(:full :left :full :outside :full))
                (7 '(:full :right :right :right :right))
                (8 '(:full :outside :full :outside :full))
                (9 '(:full :outside :full :right :full))
                (t '(:full :full :full :full :full))))) ; fallback shows 8
    (loop for r from 0 to 4
          for row-type in rows
          do (move scr (+ row r) col)
            (add-string scr (case row-type
                              (:full "*****")
                              (:outside "*   *")
                              (:left "*    ")
                              (:right "    *"))))))

(defun draw-8-segment-number (scr number row col)
  "Draw a 2-digit number using 8-segment displays.
   NUMBER should be 0-99. Numbers >= 100 show as 99."
  (let* ((clamped (min 99 (max 0 number)))
         (tens (floor clamped 10))
         (ones (mod clamped 10)))
    ;; Draw tens digit
    (draw-8-segment-digit scr tens row col)
    ;; Draw ones digit with 1 char gap
    (draw-8-segment-digit scr ones row (+ col 6))))

;;; ============================================================================
;;; Center Info Box
;;; ============================================================================

(defun draw-center-info (scr game row col &optional human-seat)
  "Draw a centered info box with game information.
   Features: 8-segment tile counter, wind+round display.
   HUMAN-SEAT is used to determine relative positions for riichi border coloring."
  (let* ((round-wind (game-round-wind game))
         (round-number (game-round-number game))
         (config (game-config game))
         (wall (game-wall game))
         (tiles-remaining (wall-tiles-remaining wall))
         (honba (game-honba game))
         (seats (game-config-seats config))
         (num-players (length seats))
         ;; Box dimensions: 32 wide x 13 tall
         (box-width 32)
         (box-height 13)
         ;; Calculate relative seats from human's perspective
         (bottom-seat human-seat)
         (right-seat (when human-seat (next-seat human-seat config)))
         (top-seat (when right-seat (next-seat right-seat config)))
         (left-seat (when (and (= num-players 4) top-seat) (next-seat top-seat config)))
         ;; Check riichi status for each relative position
         (bottom-riichi (and bottom-seat
                             (let ((p (get-player game bottom-seat)))
                               (and p (player-riichi-declared p)))))
         (right-riichi (and right-seat
                            (let ((p (get-player game right-seat)))
                              (and p (player-riichi-declared p)))))
         (top-riichi (and top-seat
                          (let ((p (get-player game top-seat)))
                            (and p (player-riichi-declared p)))))
         (left-riichi (and left-seat
                           (let ((p (get-player game left-seat)))
                             (and p (player-riichi-declared p))))))

    ;; Draw box border with riichi coloring
    ;; Top border (red if top player is in riichi)
    (move scr row col)
    (let ((top-attrs (if top-riichi '(:altcharset :bold) '(:altcharset)))
          (top-color (when top-riichi '(:red :black))))
      (add-char scr #\l :attributes top-attrs :color-pair top-color) ; ┌
      (loop repeat (- box-width 2) do (add-char scr #\q :attributes top-attrs :color-pair top-color)) ; ─
      (add-char scr #\k :attributes top-attrs :color-pair top-color)) ; ┐

    ;; Side borders (rows 1-11)
    (loop for r from 1 to (- box-height 2) do
            (move scr (+ row r) col)
            ;; Left border (red if left player is in riichi)
            (let ((left-attrs (if left-riichi '(:altcharset :bold) '(:altcharset)))
                  (left-color (when left-riichi '(:red :black))))
              (add-char scr #\x :attributes left-attrs :color-pair left-color)) ; │
            (loop repeat (- box-width 2) do (add-char scr #\Space))
            ;; Right border (red if right player is in riichi)
            (let ((right-attrs (if right-riichi '(:altcharset :bold) '(:altcharset)))
                  (right-color (when right-riichi '(:red :black))))
              (add-char scr #\x :attributes right-attrs :color-pair right-color))) ; │

    ;; Bottom border (red if bottom player is in riichi)
    (move scr (+ row (1- box-height)) col)
    (let ((bottom-attrs (if bottom-riichi '(:altcharset :bold) '(:altcharset)))
          (bottom-color (when bottom-riichi '(:red :black))))
      (add-char scr #\m :attributes bottom-attrs :color-pair bottom-color) ; └
      (loop repeat (- box-width 2) do (add-char scr #\q :attributes bottom-attrs :color-pair bottom-color)) ; ─
      (add-char scr #\j :attributes bottom-attrs :color-pair bottom-color)) ; ┘

    ;; Draw 8-segment display for tiles remaining (centered, starting row+3 - moved down 1)
    (draw-8-segment-number scr tiles-remaining (+ row 3) (+ col 10))

    ;; Draw wind + round (centered below 8-segment - moved down 1)
    (let* ((wind-str (case round-wind
                       (:east "East")
                       (:south "South")
                       (:west "West")
                       (:north "North")
                       (t "?")))
           (round-str (format nil "~A ~D" wind-str round-number))
           (str-col (+ col (floor (- box-width (length round-str)) 2))))
      (move scr (+ row 9) str-col)
      (add-string scr round-str))

    ;; Draw honba counter (centered - moved down 1)
    (let* ((honba-str (format nil "Honba: ~D" honba))
           (str-col (+ col (floor (- box-width (length honba-str)) 2))))
      (move scr (+ row 10) str-col)
      (add-string scr honba-str))))

(defun draw-game (scr game &optional ui-state)
  "Draw the entire game screen with mahjong table layout.
   Human player is always drawn at bottom, others arranged around them."
  (clear scr)

  (when game
        (let* ((config (game-config game))
               (seats (game-config-seats config))
               (num-players (length seats))
               (screen-height (height scr))
               (screen-width (width scr))
               (selected-index (when ui-state (ui-state-selected-tile-index ui-state)))
               (human-seat (when ui-state (ui-state-human-seat ui-state)))
               (current-seat (player-seat (current-player game)))
               (dealer-seat (game-dealer-seat game))
               (phase (game-phase game))
               ;; Calculate relative positions
               (bottom-seat human-seat)
               (right-seat (next-seat human-seat config))
               (top-seat (next-seat right-seat config))
               (left-seat (when (= num-players 4) (next-seat top-seat config))))

          ;; Draw current turn indicator at top

          (when *debug-ui*
                (move scr 0 0)
                (add-string scr (format nil "Turn: ~A | Phase: ~A | Selected: ~A | Last key: ~S"
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
                 (right-col (max 0 (- screen-width 5))) ; Right edge - 5 chars for tile width
                 (left-col 0)) ; Left edge

            (when top-player
                  (draw-hand-horizontal scr top-player 1 top-col (not *debug-ui*) nil nil (eq (player-seat top-player) dealer-seat))
                  ;; Draw top player's melds in top-left corner
                  (draw-called-melds scr top-player 4 0 :horizontal))
            (when left-player
                  (draw-hand-vertical scr left-player left-row left-col (not *debug-ui*) nil (eq (player-seat left-player) dealer-seat)) ; Left-aligned
                  ;; Draw left player's melds in bottom-left corner (below their hand)
                  (let ((meld-row (min (- screen-height 5) (+ left-row left-height 1))))
                    (draw-called-melds scr left-player meld-row 0 :vertical)))
            (when right-player
                  (draw-hand-vertical scr right-player right-row right-col (not *debug-ui*) t (eq (player-seat right-player) dealer-seat)) ; Right-aligned
                  ;; Draw right player's melds in top-right corner (above their hand)
                  (draw-called-melds scr right-player (max 4 (- right-row 2)) (- screen-width 15) :vertical))
            (when bottom-player
                  (let ((hand-row (max 0 (- screen-height 4)))
                        (valid-tiles (when ui-state (ui-state-valid-riichi-discards ui-state))))
                    (draw-hand-horizontal scr bottom-player hand-row bottom-col nil selected-index valid-tiles (eq (player-seat bottom-player) dealer-seat) game)
                    ;; Store hand position for mouse click handling
                    (when ui-state
                          (setf (ui-state-hand-position ui-state) (list hand-row bottom-col)))
                    ;; Draw bottom player's melds stacked vertically on right side
                    (draw-called-melds scr bottom-player hand-row (- screen-width 20) :vertical))))

          ;; Draw center info box and discards around it
          (let* ((center-row (max 0 (- (floor screen-height 2) 6))) ; Adjust for taller box
                                                                   (center-col (max 0 (- (floor screen-width 2) 16))) ; Adjust for wider box
                                                                   ;; Center box dimensions: 13 rows tall, 32 chars wide
                                                                   (center-box-height 13)
                                                                   (center-box-width 32)
                                                                   ;; Get players for discard rendering
                                                                   (bottom-player (get-player game bottom-seat))
                                                                   (top-player (get-player game top-seat))
                                                                   (right-player (get-player game right-seat))
                                                                   (left-player (when (= num-players 4) (get-player game left-seat))))

            ;; Draw center info box
            (draw-center-info scr game center-row center-col human-seat)

            ;; Draw discards around center box
            ;; 
            ;; From each player's perspective, discards appear the same way:
            ;; oldest on their left, newest on their right, rows/columns expand toward their hand.
            ;;
            ;; Bottom player discards: oldest at left, newest at right. Rows stack down.
            (when bottom-player
                  (let* ((discards (reverse (player-discards bottom-player))) ; oldest to newest
                                                                             (discard-row (+ center-row center-box-height))
                                                                             (discard-col center-col))
                    (draw-discards-horizontal scr discards discard-row discard-col 10 nil nil)))

            ;; Top player discards: rotated 180° from bottom.
            ;; Oldest at our right, newest at our left. Rows stack up (toward their hand).
            (when top-player
                  (let* ((discards (reverse (player-discards top-player))) ; oldest to newest
                                                                          (num-discards (length discards))
                                                                          (tiles-per-row 10)
                                                                          ;; Grid bottom row should be at center-row - 2 (leave 1 line gap above center box)
                                                                          ;; draw-discards-horizontal will calculate actual start from this
                                                                          (discard-row (- center-row 3))
                                                                          ;; Align right edge with center-box
                                                                          (grid-width (+ 1 (* (min num-discards tiles-per-row) 3)))
                                                                          (discard-col (max 0 (- (+ center-col center-box-width) grid-width))))
                    (draw-discards-horizontal scr discards discard-row discard-col tiles-per-row t t)))

            ;; Right player discards: rotated 90° CW from bottom.
            ;; Oldest at our bottom (their left), newest at our top (their right).
            ;; Columns expand rightward toward their hand.
            (when right-player
                  (let* ((discards (reverse (player-discards right-player))) ; oldest to newest
                                                                            (tiles-per-column 6)
                                                                            (num-cols (ceiling (length discards) tiles-per-column))
                                                                            ;; Position grid just to the right of center box
                                                                            ;; Since stack-rightward=nil causes leftward offset, add compensation
                                                                            (discard-row center-row)
                                                                            (discard-col (+ center-col center-box-width 1 (* (1- num-cols) 3))))
                    (draw-discards-vertical scr discards discard-row discard-col tiles-per-column t nil))) ; reverse-vert, stack-right (newer on RIGHT)

            ;; Left player discards: rotated 90° CCW from bottom.
            ;; Oldest at our top (their left), newest at our bottom (their right).
            ;; Columns expand leftward toward their hand.
            (when left-player
                  (let* ((discards (reverse (player-discards left-player))) ; oldest to newest
                                                                           (tiles-per-column 6)
                                                                           (num-cols (ceiling (length discards) tiles-per-column))
                                                                           ;; Grid right edge should be at center-col - 1
                                                                           ;; Calculate left edge position: right-edge - width + 1
                                                                           (grid-width (1+ (* num-cols 3)))
                                                                           (discard-row center-row)
                                                                           (discard-col (max 0 (- center-col grid-width))))
                    (draw-discards-vertical scr discards discard-row discard-col tiles-per-column nil t)))) ; stack-right (newer on LEFT)

          ;; Draw modal overlay if active
          (when (and ui-state (ui-state-active-modal ui-state))
                (draw-modal scr (ui-state-active-modal ui-state))))

        (refresh scr)))

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
               ;; Check pause state: skip if paused unless step requested
               (when (and (or (not *game-paused*) *step-requested*)
                          (> (- current-time last-ai-turn-time)
                             (* 1.0 internal-time-units-per-second)))
                     (process-turn-phase *game* human-seat ui-state)
                     (setf last-ai-turn-time current-time)
                     ;; After stepping, clear the step request and stay paused
                     (when *step-requested*
                       (setf *step-requested* nil))))
         (when (eq current-seat human-seat)
               ;; Human player turn - process immediately
               (process-turn-phase *game* human-seat ui-state)))

       ;; Check for refresh request (from SLIME)
       ;; Use endwin/refresh cycle to force full terminal redraw
       (when *refresh-requested*
         (croatoan:end-screen)
         (croatoan:refresh scr)
         (clear scr)
         (setf *refresh-requested* nil))

       ;; Render game state
       (draw-game scr *game* ui-state)
       (refresh scr)

       ;; Handle input (non-blocking, 100ms timeout from screen init)
       ;; Wrap in handler-case to catch edge case terminal events
       (let* ((event (handler-case
                         (croatoan:get-event scr)
                       (error () nil)))
              (ch (when event
                        (handler-case
                            (croatoan:event-key event)
                          (error () nil)))))
         (cond
          ;; Handle resize events
          ((eq ch :resize)
            nil)
          ;; Skip nil keys
          ((null ch) nil)

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


(defun start-swank-server (&key (port 4005) (interface "127.0.0.1"))
  "Start a Swank server for SLIME integration (silently).
   Returns T if server started successfully, NIL otherwise.
   
   Connect from Emacs with: M-x slime-connect RET <interface> RET <port> RET
   
   Useful runtime commands:
   - (setf *game* ...) to modify game state
   - (player-hand (get-player *game* :south)) to inspect hands
   - (setf (player-hand ...) ...) to modify hands
   - Access *debug-ui* to toggle debug mode"
  (handler-case
      (progn
        ;; Load swank silently (suppress warnings that corrupt TUI)
        (unless (find-package :swank)
          (with-open-file (null-stream "/dev/null" :direction :output :if-exists :supersede)
            (let ((*standard-output* null-stream)
                  (*error-output* null-stream)
                  (*trace-output* null-stream))
              (asdf:load-system :swank))))
        ;; Create the server silently
        (let ((swank-package (find-package :swank)))
          (with-open-file (null-stream "/dev/null" :direction :output :if-exists :supersede)
            (let ((*standard-output* null-stream)
                  (*error-output* null-stream)
                  (*trace-output* null-stream))
              (funcall (intern "CREATE-SERVER" swank-package)
                       :port port
                       :interface interface
                       :dont-close t)))
          t))
    (error (e)
      (declare (ignore e))
      nil)))

(defun main (&key (sanma nil) (swank nil) (swank-port 4005) (swank-interface "127.0.0.1"))
  "Entry point for the TUI client.

   Optional arguments:
   - SANMA: If T, start 3-player sanma game. Default: NIL (4-player yonma)
   - SWANK: If T, start a Swank server for SLIME integration. Default: NIL
   - SWANK-PORT: Port for Swank server. Default: 4005
   - SWANK-INTERFACE: Interface to bind Swank server. Default: 127.0.0.1

   Examples:
   (main)                        ; Start 4-player yonma game
   (main :sanma t)               ; Start 3-player sanma game
   (main :swank t)               ; Start with Swank on default port 4005
   (main :swank t :swank-port 4006) ; Start with Swank on port 4006

   With Swank enabled, connect from Emacs:
   M-x slime-connect RET 127.0.0.1 RET 4005 RET

   Runtime debugging:
   - *game* holds the current game state
   - (player-hand (get-player *game* :south)) to view hands
   - (setf (player-hand ...) '(...)) to modify hands
   - *debug-ui* toggles debug overlay

   Pause/Step controls (via SLIME):
   - (pause-game) - Pause AI turns
   - (resume-game) - Resume normal play
   - (step-game) - Advance one AI turn then pause"
  ;; Start Swank server if requested
  (when swank
    (start-swank-server :port swank-port :interface swank-interface))
  (handler-case
      (with-screen (scr :input-echoing nil
                        :input-blocking 100
                        :enable-colors t
                        :cursor-visible nil)
                   ;; Enable mouse events
                   (set-mouse-event '(:button-1-click :button-1-double-click))
                   (main-loop scr :sanma sanma))
    (sb-sys:interactive-interrupt ()
                                  ;; Handle Ctrl-C gracefully
                                  (format t "~%Interrupted.~%"))
    (error (e)
      (format t "Error: ~A~%" e)
      nil)))
