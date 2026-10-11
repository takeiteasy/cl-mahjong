# Seat view

`(game-view game seat)` returns what `seat` may see of a [game](game.md): its own hand, and for every seat the public tiles. It holds no deal, wall, claim or player, so nothing hidden can be read from it. [Bots](bot.md) are given only this view.

```lisp
(let ((view (game-view game 1)))
  (view-hand view)                      ; => (tile ...) seat 1's concealed tiles
  (view-dora view)                      ; => the dora indicators
  (seen-discards (svref (view-seats view) 0)))   ; => seat 0's discards
```

## Table

| Reader | Meaning |
|---|---|
| `view-seat` | The viewing seat |
| `view-variant` | The [variant](variant.md); rules are public. `view-tileset` and `(view-opt view key)` read its tileset and options |
| `view-phase`, `view-deal-phase` | `game-phase`; `deal-phase` (`:ended` between deals and after the game) |
| `view-dealer`, `view-turn` | Dealer and seat to act |
| `view-kyoku`, `view-honba`, `view-sticks` | Round, honba and riichi sticks |
| `view-round-wind`, `view-seat-wind` | Wind kinds |
| `view-points` | Every seat's points, as `game-points` |
| `view-dora` | Dora indicators showing |
| `view-wall-left` | Live wall tiles left |
| `view-claim` | During a claim, `(:kind kind :tile tile :from seat)`; no options or answers |
| `view-result`, `view-final` | `game-result` and `game-final` |

## Own seat

| Reader | Meaning |
|---|---|
| `view-hand` | The concealed tiles, drawn tile included |
| `view-drawn`, `view-forbidden` | The drawn tile and the kinds kuikae forbids; only on the seat's own turn |
| `view-furiten` | Whether the seat is furiten; while it holds a tile to discard, only a passed ron counts[^furiten] |
| `view-actions` | `legal-actions` of the seat; `NIL` when it is not the seat's decision |

## Every seat

`(view-seats view)` is a vector with one `seen-seat` per seat, the viewer included. `(view-own view)` is the viewer's own.

| Reader | Meaning |
|---|---|
| `seen-discards`, `seen-melds` | In order, called tiles included |
| `seen-nuki`, `seen-bonus` | Tiles set aside |
| `seen-hand-size` | Concealed tile count |
| `seen-riichi`, `seen-open-riichi` | `:pending`, `:active` or `NIL`; whether it is open |
| `seen-dead` | The reason a [dead hand](game.md#dead-hands) died, or `NIL` |
| `seen-shown` | Concealed tiles the viewer sees: the hand of another seat in open riichi |

## Events

`(seat-events seat events)` hides the tile of every `:draw` by another seat. `play-bots` gives each bot's `observe` its own `seat-events`; `on-events` gets all events.

## Variants that show more

`(shown-tiles game viewer seat)` decides which concealed tiles of `seat` the viewer sees. `seen-shown` and `seat-visible` both use it.

[^furiten]: Use `seat-furiten-p` with `:discard` to ask about a discard.
