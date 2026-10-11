# Record and replay

A game records its start and every action it takes. Replaying that log rebuilds the game and its events, so a game can be saved, restored and replayed.

```lisp
(let* ((game (start-game 'riichi :seed 42))
       (game (apply-action game 0 '(:discard 17))))
  (game-log game)
  ;; => (:seed 42 :walls nil :points (25000 25000 25000 25000) :kyoku 0 :honba 0 :sticks 0
  ;;     :actions ((0 (:discard 17))))
  (replay-game 'riichi (game-log game)))   ; => (values game events)
```

## Log

| Key | Holds |
|---|---|
| `:seed` | The seed that shuffles every deal without a stacked wall |
| `:walls` | Each stacked wall given to `start-game`, as its tile ids in wall order |
| `:points :kyoku :honba :sticks` | How the game started |
| `:actions` | `(seat action)` per `apply-action`, and `(:next-deal)` per `next-deal`, in order |

Only accepted actions are logged. A wrong declaration that [`:chombo`](game.md#chombo) accepts is logged; one that signals is not. Events are not logged: replay produces them again.

## Functions

| Function | Result |
|---|---|
| `game-log game` | The log plist |
| `replay-game variant log` | `(values game events)`; `variant` is a name or a variant |
| `game-record game` | `(:variant name . log)`, readable by `read` |
| `restore-game record` | `(values game events)` |
| `write-game game stream` | Prints the record |
| `read-game stream` | Reads a record and restores it, with `*read-eval*` off |

`replay-game` plays every action through `apply-action`, so it checks legality as the game did.

## Limits

| Limit | Behavior |
|---|---|
| Stacked walls | `start-game` rejects a wall that has been drawn from |
| Variant | A record holds the variant's name; `game-record` signals unless the game's variant is the registered one[^variant] |
| Cost | Restoring replays every action; a full hanchan replays in tens of milliseconds |

[^variant]: A variant redefined after a record is written may reject the record's actions when it is restored.
