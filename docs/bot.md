# Bots

A bot picks an action for a seat from its [legal actions](game.md). `play-bots` runs a whole game with one bot per seat.

```lisp
(multiple-value-bind (game events) (start-game 'riichi :seed 42)
  (play-bots game (list (make-bot :hard) (make-bot :easy) (make-bot :easy) (make-bot :dead))
             :events events))                   ; => the finished game
```

## Built-in bots

| Level | Turn | Claims |
|---|---|---|
| `:dead` | Discards the drawn tile, or the first legal discard after a call. Never wins, declares riichi or kans | Passes |
| `:easy` | Tsumo, kita, then riichi on the discard with the most live waits, then the discard with the lowest shanten and most ukeire | Ron, otherwise pass |
| `:hard` | Like easy, but never riichis into furiten, counts only unseen tiles, folds to the safest tile when another seat is in riichi at `:fold-shanten` (default 2) or worse, and closes a kan that keeps its shanten | Ron, and pon of a yakuhai tile[^calls] |

`(make-bot :hard :fold-shanten 3)` passes initargs to the bot. The built-in bots are deterministic and hold no state, so one object can play any seat.

## Writing a bot

| Piece | Meaning |
|---|---|
| `(choose-action bot game seat actions)` | Return one of `actions`, the `legal-actions` of `seat` |
| `(observe bot seat events)` | Called with every batch of [events](game.md#events); does nothing by default |
| A function | `(lambda (game seat actions) ...)` is a bot |

```lisp
(defclass greedy (bot) ())
(defmethod choose-action ((bot greedy) game seat actions)
  (or (find :tsumo actions :key #'first)
      (find :ron actions :key #'first)
      (first actions)))
```

Choose from `actions`, not the hand: kuikae and riichi restrictions exist only there. A bot receives the whole game, so every hand is readable; playing fair is up to the bot.[^view]

## Running games

`(play-bots game bots &key events on-events deals)` asks each deciding seat in turn and starts the next deal until the game ends.

| Key | Meaning |
|---|---|
| `bots` | A list or vector, one per seat |
| `events` | The events of `start-game`, announced first |
| `on-events` | Function called with every batch of events |
| `deals` | Stop after this many deals |

An illegal action signals `illegal-action`.

## Helpers

The built-in bots use these; so can a UI.

| Function | Result |
|---|---|
| `(seat-visible game seat)` | Kind-count vector of the tiles `seat` sees outside its own hand and melds, for the `:visible` key of [`shanten` and `ukeire`](analysis.md#live-shanten) |
| `(discard-options game seat &key live)` | Each distinct legal discard as `(tile shanten total ukeire)`, best first; readers `discard-option-tile`, `-shanten`, `-total`, `-ukeire` |
| `(tenpai-waits game seat &key discard)` | `(kind . remaining)` for each winning kind, or `NIL` when not tenpai |
| `(seat-furiten-p game seat &key discard)` | Whether `seat` is furiten, or would be after the discard |
| `(tile-safety game seat tile)` | 0 (dangerous) to 4 against the seats in riichi[^safety] |

`discard` is a tile of a hand that still holds its drawn tile. With `:live`, `discard-options` takes the copies `seat` sees off each kind's count.

```lisp
(tenpai-waits game 1)                      ; => ((#<KIND SOU-1> . 4) (#<KIND SOU-4> . 4))
(first (discard-options game 0))           ; => (17 0 8 ((#<KIND SOU-1> . 4) ...))
```

### What `seat-visible` counts

| Counted | Not counted |
|---|---|
| Every seat's discards, a called tile once in its meld | The seat's own hand and melds |
| Other seats' melds, closed kans included | Hands of other seats |
| Every seat's nuki, dora indicators | Bonus tiles |
| The hand of a seat in open riichi | |

### Tile safety

| Score | Tile |
|---|---|
| 4 | Discarded by every seat in riichi, or no seat is in riichi |
| 3 | An honor whose other copies are all out of reach |
| 2 | Suji of the riichi seat's discards: both neighbours three ranks away for a middle tile |
| 1 | Terminal or honor |
| 0 | Anything else |

The score is the lowest across the seats in riichi.

[^calls]: A yakuhai tile is a dragon, the seat wind or the round wind. The hard bot makes no other calls ([#42](https://github.com/takeiteasy/cl-mahjong/issues/42)).
[^view]: See [PLAN.md](PLAN.md#limitations): there is no per-seat view ([#28](https://github.com/takeiteasy/cl-mahjong/issues/28)).
[^safety]: Tiles passed since the declaration, kabe and open hands are not read ([#41](https://github.com/takeiteasy/cl-mahjong/issues/41)).
