# Design

A variant is data compiled into an immutable `ruleset`. The engine never branches on variant names.

## Definitions

| Definition | Macro | Contents |
|---|---|---|
| Tile set | `deftileset` | suits, ranks, honors, bonus tiles, copies, attributes (red fives) |
| Hand shape | `defshape` | sets + pair, seven pairs, kokushi |
| Yaku | `defyaku` | condition, han (open/closed), excludes/implies, yakuman |
| Scoring | `defscoring` | fu, han to base points, limits, payments |
| Variant | `defvariant` | players, tileset, shapes, yaku, scoring, options |

Each takes `:extends` to inherit from a definition of the same kind, then adds, removes or overrides parts.

```lisp
(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8)))

(defvariant sanma :extends riichi
  (:players 3) (:tileset sanma-108)
  (:set :chi nil :nukidora :north))
```

An event ruleset is a variant that extends another and changes a few rules.

## Rules

- Rule conditions are patterns (`every-tile`, `some-group`, `wait`, ...) with a `(:lambda ...)` escape hatch. Patterns compile to closures when the variant resolves.
- Every decomposition of a hand is scored; the highest wins.
- Shanten and ukeire work over the shapes the variant declares.
- A hand is closed when no meld is open; a closed kan keeps it closed.
- Tiles are compact ids from the compiled tileset. A red five matches a five in shapes and is distinct for dora.
- Rule definitions are trusted Lisp.[^trusted]

## Layout

| Path | Content |
|---|---|
| `src/` | engine: tiles, wall, shapes, analysis, patterns, yaku, scoring, variant, game |
| `variants/` | `riichi`, `sanma` |
| `tests/` | fiveam suites |

[^trusted]: Sandboxed loading of untrusted rule files is listed under Limitations in [PLAN.md](PLAN.md).
