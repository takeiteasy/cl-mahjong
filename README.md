# cl-mahjong

Mahjong rules engine for Common Lisp. A whole variant (tiles, wall, hand shapes, yaku, scoring, game flow) is defined in a DSL, and every definition can inherit from another.

> **Work in progress.** The API will change before v1. Riichi plays end to end; other variants and the validation corpus are not done. See [docs/PLAN.md](docs/PLAN.md).

```lisp
(let ((game (start-game 'riichi :seed 42)))
  (legal-actions game 0)                  ; => ((:discard 17) (:riichi 17) ...)
  (apply-action game 0 '(:discard 17)))   ; => (values new-game events)
```

## Docs

| Topic | Page |
|---|---|
| Design overview | [docs/design.md](docs/design.md) |
| Tilesets and inheritance | [docs/tileset.md](docs/tileset.md) |
| Wall and dead wall | [docs/wall.md](docs/wall.md) |
| Hand shapes | [docs/shape.md](docs/shape.md) |
| Decomposition, shanten, ukeire | [docs/analysis.md](docs/analysis.md) |
| Patterns, waits, win context | [docs/pattern.md](docs/pattern.md) |
| Yaku | [docs/yaku.md](docs/yaku.md) |
| Fu, limits, dora, payments | [docs/scoring.md](docs/scoring.md) |
| Variants and deltas | [docs/variant.md](docs/variant.md) |
| DSL examples (sanma, events) | [docs/dsl.md](docs/dsl.md) |
| Game flow | [docs/game.md](docs/game.md) |
| Roadmap and limitations | [docs/PLAN.md](docs/PLAN.md) |

## Tests

```
sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
