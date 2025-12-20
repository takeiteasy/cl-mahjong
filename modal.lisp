;; modal.lisp
;; Generic modal dialog system for CLI Mahjong
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

;;; ============================================================================
;;; Modal State Structure
;;; ============================================================================

(defstruct modal-state
  "State for a modal dialog"
  (title "" :type string)
  (message nil :type (or null string))  ; Optional message below title
  (buttons nil :type list)              ; List of button label strings
  (selected-index 0 :type integer)      ; Currently selected button
  (result nil))                         ; Set when user confirms selection

;;; ============================================================================
;;; Modal Creation Helpers
;;; ============================================================================

(defun make-riichi-modal ()
  "Create a modal for riichi declaration prompt."
  (make-modal-state
   :title "RIICHI?"
   :message "You are in tempai. Declare riichi?"
   :buttons '("RIICHI" "SKIP")
   :selected-index 0))

(defun make-tsumo-modal ()
  "Create a modal for tsumo (self-draw win) prompt."
  (make-modal-state
   :title "TSUMO!"
   :message "You have completed your hand!"
   :buttons '("TSUMO" "SKIP")
   :selected-index 0))

(defun make-ron-modal ()
  "Create a modal for ron (win on discard) prompt."
  (make-modal-state
   :title "RON!"
   :message "You can win on this discard!"
   :buttons '("RON" "SKIP")
   :selected-index 0))

(defun make-win-modal (winner-name)
  "Create a modal for showing win result."
  (make-modal-state
   :title "WIN!"
   :message (format nil "~A wins the hand!" winner-name)
   :buttons '("OK")
   :selected-index 0))

;;; ============================================================================
;;; Modal Drawing
;;; ============================================================================

(defun calculate-modal-dimensions (modal)
  "Calculate the width and height needed for the modal.
   Returns (values width height)."
  (let* ((title-len (length (modal-state-title modal)))
         (message-len (if (modal-state-message modal)
                          (length (modal-state-message modal))
                          0))
         (buttons (modal-state-buttons modal))
         ;; Each button: [ LABEL ] with 2 chars padding, 3 chars for brackets+spaces
         ;; Plus 2 chars spacing between buttons
         (button-width (+ (reduce #'+ buttons :key #'length)
                          (* (length buttons) 5)  ; brackets and padding per button
                          (* (1- (length buttons)) 2)))  ; spacing between
         ;; Width is max of title, message, buttons + border padding
         (content-width (max title-len message-len button-width))
         (total-width (+ content-width 4))  ; 2 chars padding on each side
         ;; Height: top border, title, separator, message (optional), buttons, bottom border
         (total-height (if (modal-state-message modal) 6 5)))
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
           (message (modal-state-message modal))
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
      
      ;; Message row (if present): │  message   │
      (let ((button-row (if message (+ start-row 4) (+ start-row 3))))
        (when message
          (croatoan:move scr (+ start-row 3) start-col)
          (croatoan:add-char scr #\x :attributes '(:altcharset))  ; │
          (let* ((msg-padding (floor (- inner-width (length message)) 2))
                 (msg-str (format nil "~A~A~A"
                                 (make-string msg-padding :initial-element #\Space)
                                 message
                                 (make-string (- inner-width msg-padding (length message))
                                             :initial-element #\Space))))
            (croatoan:add-string scr msg-str))
          (croatoan:add-char scr #\x :attributes '(:altcharset)))  ; │
        
        ;; Button row: │ [BTN1]  [BTN2] │
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
