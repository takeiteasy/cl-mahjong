# Game

A game plays a [variant](variant.md) from deal to final standings. State is immutable: `apply-action` returns a new game and the events that happened.

```lisp
(multiple-value-bind (game events) (start-game 'riichi :seed 42)
  (legal-actions game 0)                   ; => ((:discard 17) (:riichi 17) (:tsumo) ...)
  (apply-action game 0 '(:discard 17)))    ; => (values game events)
```

Seats are numbered from 0, the first dealer. `deciding-seats` lists who owes an action. After a deal ends, `next-deal` starts the next one until `game-over-p`.

## Phases

| `game-phase` | Meaning |
|---|---|
| `:playing` | A deal is in progress |
| `:deal-over` | The deal ended; `game-result` says how; call `next-deal` |
| `:game-over` | `game-final` holds the standings |

During a deal, `deal-phase` of `game-deal` is `:turn` (one seat acts), `:claim` (seats answer a discard) or `:chankan` (seats answer a kan or a kita).

## Actions

| Action | When |
|---|---|
| `(:discard tile)` | On your turn |
| `(:riichi tile)` | Closed, tenpai after the discard, enough points and wall |
| `(:tsumo)` | The drawn tile completes a hand with a yaku |
| `(:ankan kind)` | Four of a kind in hand; in riichi only if the waits do not change |
| `(:shouminkan tile)` | Add a tile to an open triplet |
| `(:kyuushu)` | First turn, nine terminal or honor kinds |
| `(:kita)` | Set a north aside as dora and draw a replacement; needs `:nukidora`; in riichi only the drawn north |
| `(:ron)` | The discard (or added kan tile) completes a hand with a yaku and you are not furiten |
| `(:pon tile tile)`, `(:chi tile tile)`, `(:daiminkan tile tile tile)` | Tiles from your hand used with the discard |
| `(:pass)` | Decline a claim |

A claim resolves once every seat with an option has answered. Ron beats pon and kan, which beat chi; chi is only for the next seat. After a call the caller discards and does not draw.

## Events

Each event is `(type . plist)`.

| Event | Fields |
|---|---|
| `:deal-start` | `:kyoku :dealer :honba :dora` |
| `:draw` | `:seat :tile`, `:rinshan` for a replacement draw, `:kita` after a kita |
| `:discard` | `:seat :tile :riichi` |
| `:call` | `:type :seat :tiles`, `:from` for open calls |
| `:kita` | `:seat :tile` |
| `:riichi` | `:seat`, once the discard passes |
| `:dora` | `:tile`, a new indicator |
| `:win` | `:seat :from :han :fu :limit :points :yaku :pao :score`; `:pao` lists `(seat . amount)` for liable seats |
| `:deal-end` | `:type :deltas :points` |
| `:game-end` | `:final` |

## Deal rules

| Rule | Behavior |
|---|---|
| Deal | 13 tiles each in rounds of four from the dealer, who draws first |
| Furiten | Own discards hold a wait, a passed ron until your next discard, or in riichi a passed ron for the deal |
| Riichi | The stick is paid when the discard passes; the hand is then locked to the drawn tile |
| Kuikae | A call is offered only while a legal discard remains |
| Calls | None on the last discard, none in riichi |
| Kan | Needs a replacement tile and a live wall tile; a closed kan can only be robbed for kokushi |
| Kita | Needs a live wall tile and a [`:nuki` slot](wall.md#layout); the north stays out of the hand and melds and counts as dora in scoring ([scoring.md](scoring.md#dora)) |
| Multiple ron | Honba and sticks go to the nearest winner |
| Deal end | `game-result` holds `:type` (`:tsumo :ron :exhaustive :abortive`), `:deltas`, and details such as `:winners` or `:reason` |

Exhaustive draws pay noten payments (a hand is tenpai unless the concealed hand and own nukidora hold every copy of each winning tile; copies in a meld do not count), or nagashi mangan like a tsumo of that limit. The dealer repeats on a win or, with `:renchan :tenpai`, when tenpai; honba reset after a non-dealer win. After a deal, game fields describe the next deal.

## Game end

| Condition | Option |
|---|---|
| A player below 0 | `:tobi` |
| East (`:tonpuu`) or east and south (`:hanchan`) played | `:length` |
| Nobody at the target after the last round, and a further wind remains | `:west-extension` |
| The leading dealer wins (or is tenpai) in the last round or the extension | `:agari-yame`, `:tenpai-yame` |

`game-final` holds `:order` (seats, first place first), `:points` (leftover sticks go to first place) and `:scores`: points over the target, plus `:uma` by place, plus the oka for first place.

## Options

Variants override these in `:options`; the defaults are Mahjong Soul ranked rules.

| Option | Default | Meaning |
|---|---|---|
| `:start-points`, `:target-points` | 25000, 30000 | |
| `:length` | `:hanchan` | `:tonpuu` or `:hanchan` |
| `:west-extension` | on | Play one more wind below the target |
| `:tobi`, `:agari-yame`, `:tenpai-yame` | on, on, off | Early game end |
| `:kuikae` | `:forbid` | Discard forbidden after a call: `:forbid` the kind and the sequence's far end, `:same-kind`, or `nil` |
| `:kan-dora` | `:after-discard` | Open kan dora timing: `:after-discard` flips once the discard passes, `:on-discard` flips as the discard is made so a ron on it counts it, `:immediate` flips at once. A closed kan always flips at once |
| `:chi` | on | |
| `:ankan-chankan` | `:kokushi` | Who may rob a closed kan; `nil` for nobody |
| `:multi-ron` | `:double` | Winners allowed: `:head-bump`, `:double`, `:triple` |
| `:sanchahou` | on | Three rons abort the deal when more than allowed |
| `:kyuushu`, `:suufon-renda`, `:suucha-riichi`, `:suukaikan` | on | Abortive draws |
| `:nagashi-mangan` | `:mangan` | Limit paid for nagashi; `nil` for off |
| `:noten-payment` | 3000 | Total paid from noten to tenpai |
| `:riichi-cost`, `:riichi-min-tiles` | 1000, 4 | |
| `:nukidora` | off | Offer `(:kita)` for a north |
| `:kita-ippatsu` | off | A kita keeps ippatsu; off breaks every seat's ippatsu |
| `:kita-interrupts` | on | A kita counts as an interruption: no later double riichi, first-turn win or kyuushu |
| `:kita-rinshan` | on | A tsumo on the kita replacement draw scores rinshan |
| `:kita-ron` | on | Other seats may ron on the north as it is set aside; passing makes them furiten |
| `:renchan` | `:tenpai` | Dealer repeats when tenpai; `:win-only` |
| `:initial-dora` | 1 | Dora indicators showing at the start of a deal |
| `:uma`, `:oka` | `(15 5 -5 -15)`, on | Thousands by place; oka is players x (target - start) to first |
| `:score-rounding` | off | `:half-away` rounds each other seat's points to thousands, a half going away from zero, and gives first place the negated sum |
| `:winds` | east south west north | Seat and round wind kinds |

## Starting mid-game

`start-game` takes `:seed`, `:walls` (stacked walls for the first deals, see [wall.md](wall.md)), and `:points :kyoku :honba :sticks` to begin part way through. `kyoku` counts rounds from 0; the dealer is `kyoku mod players`.

## Limitations

See [PLAN.md](PLAN.md#limitations): legal actions are recomputed on every move ([#8](https://github.com/takeiteasy/cl-mahjong/issues/8)).
