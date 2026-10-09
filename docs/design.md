# Design

A variant is data compiled into an immutable `ruleset`. The engine never branches on variant names.

## Definitions

| Definition | Macro | Contents |
|---|---|---|
| Tile set | `deftileset` | suits, ranks, honors, bonus tiles, copies, attributes (red fives), wall layout; see [tileset.md](tileset.md) |
| Hand shape | `defshape` | sets + pair, seven pairs, kokushi, lambda; see [shape.md](shape.md) |
| Yaku | `defyaku` | condition, han (open/closed), excludes/implies, yakuman; see [yaku.md](yaku.md) |
| Scoring | `defscoring` | fu, han to base points, limits, dora, payments; see [scoring.md](scoring.md) |
| Variant | `defvariant` | players, tileset, shapes, yaku, scoring, options; see [variant.md](variant.md) |

Each takes `:extends` to inherit from a definition of the same kind, then adds, removes or overrides parts. Definitions are stored by name and compiled on first lookup; redefining one recompiles the definitions that extend it.

```lisp
(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8)))

(defvariant sanma :extends riichi
  (:players 3) (:tileset sanma-108)
  (:set :chi nil :winds (:east :south :west)))
```

An event ruleset is a variant that extends another and changes a few rules.

## Rules

- Rule conditions are patterns (`every-tile`, `some-group`, `wait`, ...) with a `(:lambda ...)` escape hatch; see [pattern.md](pattern.md). Patterns compile to closures when the yaku is looked up.
- Every reading of a hand (decomposition and winning-tile placement) is scored; the highest points win, then han, then fu.
- Shanten and ukeire work over the shapes the variant declares; see [analysis.md](analysis.md).
- A hand is closed when no meld is open; a closed kan keeps it closed.
- Tiles are compact ids from the compiled tileset: a kind id for shapes and patterns, a tile id for one physical tile. A red five has the kind of a five and a `:red` attribute.
- Rule definitions are trusted Lisp.[^trusted]

## Layout

| Path | Content |
|---|---|
| `src/` | engine: registry, random, tileset, [wall](wall.md), hand, [shapes](shape.md), [analysis](analysis.md), win, [patterns](pattern.md), [yaku](yaku.md), [scoring](scoring.md), [variant](variant.md), [game](game.md) (state and `play`: actions) |
| `variants/` | `riichi`, `sanma`, event rulesets (`events`); see [dsl.md](dsl.md) |
| `tests/` | fiveam suites |

[^trusted]: Sandboxed loading of untrusted rule files is listed under Limitations in [PLAN.md](PLAN.md).
