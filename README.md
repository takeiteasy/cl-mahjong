# cl-mahjong

Mahjong rules engine for Common Lisp. A whole variant (tiles, wall, hand shapes, yaku, scoring, game flow) is defined in a DSL, and every definition can inherit from another.

> **Work in progress.** The API is unstable and will change before v1. Riichi is playable end to end; other variants and the validation corpus are not done yet.

Status: restarting around riichi. See [docs/PLAN.md](docs/PLAN.md) and [docs/design.md](docs/design.md).

| Topic | Page |
|---|---|
| Tilesets and inheritance | [docs/tileset.md](docs/tileset.md) |
| Wall and dead wall | [docs/wall.md](docs/wall.md) |
| Hand shapes | [docs/shape.md](docs/shape.md) |
| Decomposition, shanten, ukeire | [docs/analysis.md](docs/analysis.md) |
| Patterns, waits, win context | [docs/pattern.md](docs/pattern.md) |
| Yaku and the riichi yaku set | [docs/yaku.md](docs/yaku.md) |
| Fu, limits, dora and payments | [docs/scoring.md](docs/scoring.md) |
| Variants | [docs/variant.md](docs/variant.md) |
| Game flow, calls, riichi, game end | [docs/game.md](docs/game.md) |

## Tests

```
sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

## License

```
cl-mahjong
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
