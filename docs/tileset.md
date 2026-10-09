# Tilesets

`deftileset` defines the tiles of a game and the wall layout. A tileset can extend another.

```lisp
(deftileset riichi-136
  (:suits man pin sou)
  (:honors (wind east south west north)
           (dragon white green red))
  (:copies 4)
  (:red (man 5 1) (pin 5 1) (sou 5 1))
  (:wall (:dead-wall 14) (:replacement 0 1 2 3)
         (:dora 4 6 8 10 12) (:ura 5 7 9 11 13)))

(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8)))

(find-tileset 'sanma-108)   ; compiled on first use, then cached
```

## Clauses

| Clause | Form | Meaning |
|---|---|---|
| `:suits` | `suit...` | Numbered suits |
| `:ranks` | `low high` | Rank range of every suit (default `1 9`) |
| `:honors` | `(group name...)...` | Honor tiles |
| `:bonus` | `(name copies)...` | Bonus tiles such as flowers |
| `:copies` | `n` | Copies of each suit and honor tile |
| `:red` | `(suit rank n)...` | `n` copies of the tile carry the `:red` attribute |
| `:remove` | see below | Delete tile kinds |
| `:add` | `(name copies)...` | Append bonus tile kinds |
| `:wall` | see [wall.md](wall.md) | Dead wall layout |

`:remove` forms:

| Form | Removes |
|---|---|
| `east` or `(east)` | One honor or bonus kind |
| `(dragon)` | A whole honor group |
| `(man)` | A whole suit |
| `(man 5)` | One rank |
| `(man 2 8)` | A rank range |

## Inheritance

| Clause | With `:extends` |
|---|---|
| `:remove`, `:add` | Accumulate onto the parent's |
| Any other | Replaces the parent's clause |

Redefining a tileset recompiles every tileset that extends it. Errors in a definition surface on first lookup.

## Tile ids

| Id | Range | Use |
|---|---|---|
| Kind | `0..n-1` | A distinct tile such as `man 5`; shapes and patterns work on kinds |
| Tile | `0..total-1` | One physical tile; the wall holds tiles |

A red five has the same kind as a normal five and a `:red` attribute.

```lisp
(let* ((ts (find-tileset 'riichi-136))
       (five (find-kind ts 'pin 5)))
  (tiles-of-kind ts five)              ; => (52 53 54 55)
  (tile-red-p ts 52)                   ; => T
  (tile-kind ts 53))                   ; => the pin 5 kind
```

## API

| Function | Result |
|---|---|
| `find-tileset name` | Compiled tileset |
| `tileset-kind-count`, `tileset-tile-count` | Counts |
| `tileset-kind ts id` | Kind by id |
| `find-kind ts suit rank` / `find-kind ts name` | Kind or `NIL` |
| `kind-id`, `kind-name`, `kind-suit`, `kind-rank`, `kind-group`, `kind-category`, `kind-copies` | Kind readers |
| `kind-honor-p kind`, `terminal-p ts kind` | Classes |
| `tile-kind ts tile`, `tile-kind-id ts tile` | Kind of a tile |
| `tile-attrs ts tile`, `tile-red-p ts tile` | Attributes |
| `tiles-of-kind ts kind` | Tile ids of a kind |
