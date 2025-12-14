# cl-mahjong

> **WARNING** - work in progress!

Riichi mahjong game logic library library for Common Lisp.

Common Lisp 用のコアリーチ麻雀ゲームロジックライブラiリ

I am not an expert in common lisp or mahjong. I am happy to accept patches.

Common Lisp や麻雀の専門家ではありません。パッチは喜んで受け付けます。私はあまり日本語を話せませんwwww 🙇

## Features

- **Hand Validation** - Standard, Chiitoitsu (7 pairs), Kokushi (13 orphans)
- **Shanten Calculation** - Distance to tenpai
- **Ukeire Calculation** - Acceptance count for improving hands
- **Yaku Detection** - All standard yaku from 1-han through yakuman
- **Fu Calculation** - Complete minipoints system with all components
- **Scoring** - Han/fu to points conversion with limit hands
- **Payment Distribution** - Ron and tsumo payment splits
- **Furiten Checking** - Dead hand detection
- **Event Tracking** - Rinshan, chankan, haitei, houtei, ippatsu
- **Game State Management** - Complete game flow
- **Sanma Support** - Full 3-player mahjong with North extraction (kita)

### Notes

- **Yonma (4-player)** and **Sanma (3-player)** game variants
- Wall management with dead wall and dora indicators
- Player hands, melds, discards, and scores
- Turn state tracking
- Event-based yaku (rinshan, chankan, ippatsu, etc.)
- Fu calculation with base, wait, group, pair, and tsumo components
- Limit hands: Mangan, Haneman, Baiman, Sanbaiman, Yakuman
- Payment distribution for dealer/non-dealer, ron/tsumo
- Dora counting (regular dora, ura-dora, aka-dora support)

### Sanma (3-Player) Features

- **Modified tile set**: Excludes 2-8 man (character tiles), 108 tiles total
- **Three players**: East, South, West seats only
- **North extraction (Kita/抜き北)**: North tiles can be extracted as dora
- **Chi disabled**: No sequence calls allowed
- **Modified scoring**: Different starting points (35,000) and tsumo payments
- **Adjusted wall**: 91 live tiles, 17 dead wall tiles

### Rules Implementation

Follows **WRC/EMA** (World Riichi Championship / European Mahjong Association) rules:

- Japanese Riichi Mahjong variant
- Aka-dora support (red fives)
- Standard yaku set
- Furiten enforcement
- Atamahane (head bump) for multiple ron

I would like to add custom rulesets as maybe other mahjong varients too. However for now that's a **TODO**.

## Usage

### Basic Hand Validation

```lisp
;; Check if hand is complete
(let ((hand '(:2m :3m :4m :5p :6p :7p :8s :8s :8s :white :white :white :9m :9m)))
  (validate-hand hand nil))
;; => T, :STANDARD
```

### Calculate Shanten

```lisp
;; Calculate distance to tenpai
(let ((hand '(:1m :2m :4m :5m :7m :8m :1p :3p :5p :7p :9p :east :south)))
  (calculate-shanten hand nil))
;; => 2 (2-shanten, need 2 exchanges to reach tenpai)
```

### Detect Yaku and Score

```lisp
;; Complete scoring example
(let* ((hand '(:2m :3m :4m :5p :6p :7p :2s :3s :4s :6s :7s :8s :white :white))
       (melds nil)
       (winning-tile :white)
       (context (make-winning-context
                 :tsumo-p t
                 :winning-tile winning-tile
                 :winner-seat :south
                 :dealer-p nil
                 :round-wind :east
                 :seat-wind :south
                 :riichi-declared-p t
                 :riichi-win-p t
                 :dora-tiles '(:1m))))

  ;; Calculate complete score
  (let ((score (calculate-score hand melds winning-tile context)))
    (print-score-result score)))

;; Output:
;; Yaku: RIICHI (1 han), MENZEN-TSUMO (1 han)
;; Han: 2 yaku + 1 dora = 3 total
;; Fu: 40
;; Base points: 1280
;; Payment: 5200 points (dealer pays 1300, others 700)
```

### Game State Management

```lisp
;; Create a 4-player (yonma) game - default
(let ((game (create-game-state "game-1")))

  ;; Add players
  (add-player-to-game game "p1" "Alice" :east)
  (add-player-to-game game "p2" "Bob" :south)
  (add-player-to-game game "p3" "Charlie" :west)
  (add-player-to-game game "p4" "Diana" :north)

  ;; Start game (creates wall, deals hands)
  (start-game game)

  ;; Process ron declaration
  (process-call-ron game "p2"))
```

### Sanma (3-Player) Game

```lisp
;; Create a 3-player (sanma) game
(let ((game (create-game-state "sanma-1" :sanma)))

  ;; Add three players
  (add-player-to-game game "p1" "Alice" :east)
  (add-player-to-game game "p2" "Bob" :south)
  (add-player-to-game game "p3" "Charlie" :west)

  ;; Start game
  (start-game game)

  ;; Extract North tiles (kita)
  (let ((player (get-player game :east)))
    ;; When player draws a North, they can extract it
    (when (player-can-extract-north player +sanma-config+)
      (player-extract-north player)
      ;; Extracted Norths count as dora
      (player-extracted-norths player))) ; => number of extracted Norths

  ;; Chi is automatically disabled in sanma
  ;; Tsumo payments are calculated differently (2x base for each player)
  )
```

### API Reference

#### Hand Analysis
- `validate-hand` - Check if hand is complete
- `decompose-hand` - Get exact grouping for fu calculation
- `calculate-shanten` - Distance to tenpai
- `calculate-ukeire` - Tiles that improve hand

#### Scoring
- `detect-yaku` - Detect all applicable yaku
- `calculate-fu` - Minipoints calculation
- `calculate-score` - Complete scoring (yaku + fu + dora → points)
- `count-dora` - Count dora tiles in hand (includes extracted Norths)

#### Game Management
- `create-game-state` - Initialize new game (accepts `:sanma` or `:yonma`)
- `add-player-to-game` - Add player at seat
- `start-game` - Deal initial hands
- `process-call-ron` - Handle ron declaration
- `process-call-tsumo` - Handle tsumo declaration
- `build-winning-context` - Extract win context from game state
- `check-furiten-for-ron` - Validate ron eligibility
- `apply-payment-to-players` - Distribute payments

#### Sanma-Specific
- `player-can-extract-north` - Check if North extraction is available
- `player-extract-north` - Extract a North tile as dora
- `player-extracted-norths` - Get count of extracted Norths
- `+sanma-config+` - Global sanma configuration
- `+yonma-config+` - Global yonma configuration

## LICENSE

```
Copyright (C) 2025 George Watson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```
