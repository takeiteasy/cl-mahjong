;; types.lisp
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

(deftype tile ()
  "Tiles represented as keyword symbols for efficiency"
  '(member
    ;; Man (characters) 1-9
    :1m :2m :3m :4m :5m :6m :7m :8m :9m
    ;; Pin (circles) 1-9
    :1p :2p :3p :4p :5p :6p :7p :8p :9p
    ;; Sou (bamboo) 1-9
    :1s :2s :3s :4s :5s :6s :7s :8s :9s
    ;; Red fives (aka-dora)
    :5m-r :5p-r :5s-r
    ;; Winds (honors)
    :east :south :west :north
    ;; Dragons (honors)
    :white :green :red))

(defconstant +all-tiles+
  '(:1m :2m :3m :4m :5m :6m :7m :8m :9m
    :1p :2p :3p :4p :5p :6p :7p :8p :9p
    :1s :2s :3s :4s :5s :6s :7s :8s :9s
    :east :south :west :north
    :white :green :red)
  "All 34 unique tiles in Japanese Riichi Mahjong")

(defstruct meld
  "Represents a called meld (pon, chi, kan)"
  (type nil :type (member nil :pon :chi :kan-open :kan-closed :kan-added))
  (tiles nil :type list)  ; List of tile keywords
  (from nil :type (or null (member :left :across :right)))  ; Who we called from
  (called-tile nil :type (or null tile)))  ; Which tile was called

(defclass player ()
  ((id
    :initarg :id
    :accessor player-id
    :type string
    :documentation "Unique player identifier (websocket connection ID)")
   (name
    :initarg :name
    :accessor player-name
    :initform "Player"
    :type string
    :documentation "Player display name")
   (seat
    :initarg :seat
    :accessor player-seat
    :type (member :east :south :west :north)
    :documentation "Seat position")
   (hand
    :accessor player-hand
    :initform nil
    :type list  ; List of tile keywords
    :documentation "Concealed tiles in hand")
   (discards
    :accessor player-discards
    :initform nil
    :type list  ; List of tile keywords
    :documentation "Tiles discarded by player (in order)")
   (melds
    :accessor player-melds
    :initform nil
    :type list  ; List of meld structs
    :documentation "Called melds (pon, chi, kan)")
   (score
    :accessor player-score
    :initform 25000
    :type integer
    :documentation "Current point total")
   (riichi-declared
    :accessor player-riichi-declared
    :initform nil
    :type boolean
    :documentation "Whether player has declared riichi")
   (riichi-turn
    :accessor player-riichi-turn
    :initform nil
    :type (or null integer)
    :documentation "Turn number when riichi was declared")
   (ippatsu-active
    :accessor player-ippatsu-active
    :initform nil
    :type boolean
    :documentation "Whether ippatsu is still possible")
   (furiten
    :accessor player-furiten
    :initform nil
    :type boolean
    :documentation "Whether player is in furiten")
   (ready
    :accessor player-ready
    :initform nil
    :type boolean
    :documentation "Whether player is ready to start"))
  (:documentation "Represents a player in the game"))


(defclass wall ()
  ((live-wall
    :accessor wall-live-wall
    :initform nil
    :type list  ; List of tile keywords
    :documentation "Tiles available for normal draws")
   (dead-wall
    :accessor wall-dead-wall
    :initform nil
    :type list  ; List of 14 tile keywords
    :documentation "Dead wall (last 14 tiles)")
   (dora-indicators
    :accessor wall-dora-indicators
    :initform nil
    :type list  ; List of tile keywords (visible)
    :documentation "Dora indicator tiles (revealed)")
   (ura-dora-indicators
    :accessor wall-ura-dora-indicators
    :initform nil
    :type list  ; List of tile keywords (hidden until riichi win)
    :documentation "Ura-dora indicator tiles (hidden)")
   (kan-count
    :accessor wall-kan-count
    :initform 0
    :type integer
    :documentation "Number of kans declared (affects dora count)")
   (remaining-draws
    :accessor wall-remaining-draws
    :type integer
    :documentation "Number of tiles left to draw"))

  (:documentation "Represents the tile wall with dead wall and dora indicators"))

(defclass turn-state ()
  ((current-player
    :accessor turn-current-player
    :initform :east
    :type (member :east :south :west :north)
    :documentation "Whose turn it is")
   (turn-number
    :accessor turn-turn-number
    :initform 1
    :type integer
    :documentation "Turn counter for the current hand")
   (last-discard
    :accessor turn-last-discard
    :initform nil
    :type (or null tile)
    :documentation "Most recently discarded tile")
   (last-discard-player
    :accessor turn-last-discard-player
    :initform nil
    :type (or null (member :east :south :west :north))
    :documentation "Who discarded the last tile")
   (can-call-ron
    :accessor turn-can-call-ron
    :initform nil
    :type list  ; List of seats that can call ron
    :documentation "Seats that can call ron on last discard")
   (can-call-pon
    :accessor turn-can-call-pon
    :initform nil
    :type list  ; List of seats that can call pon
    :documentation "Seats that can call pon")
   (can-call-chi
    :accessor turn-can-call-chi
    :initform nil
    :type (or null (member :east :south :west :north))
    :documentation "Seat that can call chi (only next player)")
   (call-deadline
    :accessor turn-call-deadline
    :initform nil
    :type (or null integer)
    :documentation "Timestamp when call window closes")
   (rinshan-active
    :accessor turn-rinshan-active
    :initform nil
    :type boolean
    :documentation "True if current draw is from kan replacement")
   (chankan-active
    :accessor turn-chankan-active
    :initform nil
    :type boolean
    :documentation "True if current win is robbing a kan")
   (last-action-type
    :accessor turn-last-action-type
    :initform nil
    :type (or null keyword)
    :documentation "Type of last action (:draw :discard :kan etc.)"))

  (:documentation "Tracks the current turn state"))


(deftype game-phase ()
  "The current phase of the game"
  '(member :waiting-for-players   ; Waiting for 4 players to join
           :ready-check           ; All players joined, waiting for ready
           :dealing               ; Dealing initial hands
           :turn-draw             ; Current player draws tile
           :turn-discard          ; Current player must discard
           :call-window           ; Window for other players to call
           :hand-ended            ; Hand completed (ron/tsumo/draw)
           :game-ended))          ; All rounds complete

(defclass game-state ()
  ((game-id
    :initarg :game-id
    :accessor game-id
    :type string
    :documentation "Unique game identifier")
   (players
    :accessor game-players
    :initform (make-hash-table :test 'equal)  ; seat -> player object
    :documentation "Hash table mapping seat to player object")
   (wall
    :accessor game-wall
    :initform nil
    :type (or null wall)
    :documentation "The tile wall")
   (phase
    :accessor game-phase
    :initform :waiting-for-players
    :type game-phase
    :documentation "Current game phase")
   (turn-state
    :accessor game-turn-state
    :initform (make-instance 'turn-state)
    :type turn-state
    :documentation "Current turn information")
   (round-wind
    :accessor game-round-wind
    :initform :east
    :type (member :east :south :west :north)
    :documentation "Current round wind")
   (dealer-seat
    :accessor game-dealer-seat
    :initform :east
    :type (member :east :south :west :north)
    :documentation "Current dealer seat")
   (honba
    :accessor game-honba
    :initform 0
    :type integer
    :documentation "Honba counter (repeat hands)")
   (riichi-sticks
    :accessor game-riichi-sticks
    :initform 0
    :type integer
    :documentation "Number of riichi sticks on the table")
   (created-at
    :accessor game-created-at
    :initform (get-universal-time)
    :type integer
    :documentation "Timestamp when game was created")
   (last-action-at
    :accessor game-last-action-at
    :initform (get-universal-time)
    :type integer
    :documentation "Timestamp of last action"))

  (:documentation "Represents the complete game state"))


(deftype action-type ()
  "Types of actions players can perform"
  '(member
    ;; Lobby actions
    :join-game
    :leave-game
    :ready
    :unready
    ;; Gameplay actions
    :discard
    :call-ron
    :call-tsumo
    :call-pon
    :call-chi
    :call-kan
    :declare-riichi
    :pass))

(defstruct action
  "Represents a player action"
  (type nil :type action-type)
  (player-id nil :type string)
  (seat nil :type (or null (member :east :south :west :north)))
  (data nil :type list))  ; Plist with action-specific data

(defstruct group
  "Represents a single meld or sequence in a decomposed hand"
  (type nil :type (member :pon :chi :kan))
  (tiles nil :type list)          ; Exactly 3 or 4 tiles
  (concealed-p t :type boolean))  ; T if from hand, NIL if called meld

(defstruct hand-decomposition
  "Represents the exact decomposition of a winning hand"
  (pattern-type nil :type (member :standard :chiitoitsu :kokushi))
  (groups nil :type list)        ; List of group structs
  (pair nil :type (or null tile)) ; The pair tile
  (wait-type nil :type (member :ryanmen :kanchan :penchan :tanki :shanpon :kokushi-13)))

(defstruct winning-context
  "Complete context for scoring a winning hand"
  ;; Basic win info
  (tsumo-p nil :type boolean)       ; T if self-draw, NIL if ron
  (winning-tile nil :type tile)     ; The tile that completed the hand
  ;; Player info
  (winner-seat nil :type (or null (member :east :south :west :north)))
  (loser-seat nil :type (or null (member :east :south :west :north)))
  (dealer-p nil :type boolean)      ; Is winner the dealer?
  (loser-is-dealer-p nil :type boolean)
  ;; Game context
  (round-wind nil :type (or null (member :east :south :west :north)))
  (seat-wind nil :type (or null (member :east :south :west :north)))
  (honba 0 :type integer)
  (riichi-sticks 0 :type integer)
  ;; Riichi status
  (riichi-declared-p nil :type boolean)
  (double-riichi-p nil :type boolean)   ; Riichi on first turn
  (ippatsu-active-p nil :type boolean)  ; Won within one turn cycle
  (riichi-win-p nil :type boolean)      ; For ura-dora eligibility
  ;; Situational events
  (rinshan-p nil :type boolean)     ; Won on kan replacement
  (chankan-p nil :type boolean)     ; Robbed a kan
  (haitei-p nil :type boolean)      ; Last tile from wall (tsumo)
  (houtei-p nil :type boolean)      ; Last discard (ron)
  (tenhou-p nil :type boolean)      ; Dealer win on deal
  (chiihou-p nil :type boolean)     ; Non-dealer win on first draw
  ;; Dora
  (dora-tiles nil :type list)           ; Actual dora tiles (not indicators)
  (ura-dora-tiles nil :type list)       ; Ura-dora tiles
  (aka-dora-enabled-p nil :type boolean)
  ;; Turn info
  (turn-number 0 :type integer)
  (first-turn-p nil :type boolean))

(defstruct fu-breakdown
  "Detailed breakdown of fu calculation"
  (base-fu 0 :type integer)        ; 20/25/30
  (wait-fu 0 :type integer)        ; 0 or 2
  (pair-fu 0 :type integer)        ; 0 or 2 (dragon or seat/round wind)
  (group-fu-list nil :type list)   ; List of (group . fu-value)
  (tsumo-fu 0 :type integer)       ; 0 or 2
  (total-fu 0 :type integer))      ; Rounded to nearest 10

(defstruct payment-split
  "Payment distribution for winner(s)"
  (winner-seat nil :type (or null (member :east :south :west :north)))
  (loser-seat nil :type (or null (member :east :south :west :north)))
  (winner-receives 0 :type integer)
  (dealer-pays 0 :type integer)     ; For tsumo
  (non-dealer-pays 0 :type integer) ; For tsumo
  (direct-payment 0 :type integer)) ; For ron

(defstruct score-result
  "Complete scoring information for a winning hand"
  (yaku-list nil :type list)        ; ((:yaku-name han) ...)
  (han-from-yaku 0 :type integer)   ; Han from yaku only
  (dora-count 0 :type integer)      ; Dora han
  (total-han 0 :type integer)       ; Total han
  (fu 0 :type integer)              ; Fu value
  (fu-breakdown nil)                ; fu-breakdown struct
  (base-points 0 :type integer)     ; Base points before payment split
  (limit-hand nil :type (or null keyword)) ; :mangan :haneman :baiman etc.
  (payment nil))                    ; payment-split struct
