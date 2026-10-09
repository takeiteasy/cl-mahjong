# cl-mahjong

Mahjong rules engine for Common Lisp. A whole variant (tiles, wall, hand shapes, yaku, scoring, game flow) is defined in a DSL, and every definition can inherit from another.

Status: work in progress, restarting around riichi. See [docs/PLAN.md](docs/PLAN.md) and [docs/design.md](docs/design.md).

| Topic | Page |
|---|---|
| Tilesets and inheritance | [docs/tileset.md](docs/tileset.md) |
| Wall and dead wall | [docs/wall.md](docs/wall.md) |

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
