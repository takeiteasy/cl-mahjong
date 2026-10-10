# Wall

A wall is a seeded shuffle of a tileset's tiles, split into a live wall and a dead wall. Walls are immutable: every operation returns a new wall.

```lisp
(let ((wall (make-wall (find-tileset 'riichi-136) :seed 42)))
  (multiple-value-bind (tile wall) (wall-draw wall)
    (list tile (wall-live-count wall))))   ; => (tile 121)
```

The same tileset and seed give the same wall on every Lisp implementation.[^rng]

## Layout

The `:wall` clause of `deftileset` places the dead wall. Positions index into the dead wall.

| Subclause | Meaning |
|---|---|
| `(:dead-wall n)` | Tiles reserved at the end of the shuffle; `0` when omitted |
| `(:replacement p...)` | Slots drawn by `wall-draw-replacement`, in order |
| `(:dora p...)` | Dora indicator slots, in reveal order |
| `(:ura p...)` | Ura indicator slots, revealed alongside the dora |
| `(:nuki n)` | Replacement draws for tiles set aside as dora (sanma kita); `0` when omitted; needs `:replacement` slots |

Positions must be unique and inside the dead wall. A kan draws from the `:replacement` slots and a kita from the same slots after the ones already drawn, cycling; each kind of draw has its own budget (the number of `:replacement` slots for kans, `n` for kita). A drawn slot is refilled from the end of the live wall, so a later draw from it takes that tile.

## Operations

| Function | Result |
|---|---|
| `make-wall tileset &key seed tiles` | New wall; random seed when omitted. `tiles` stacks it: every tile id once, in wall order |
| `wall-draw wall` | `(values tile new-wall)`; `(values nil wall)` when empty |
| `wall-draw-replacement wall` | Tile from the next replacement slot; the last live tile refills the slot |
| `wall-draw-nuki wall` | Tile for a set-aside tile, up to `:nuki`; `(values nil wall)` when none is left |
| `wall-reveal-dora wall` | Wall with one more indicator revealed, or `NIL` when all are |
| `wall-dora-indicators`, `wall-ura-indicators` | Revealed indicator tiles; one is revealed initially |
| `wall-dead-tiles wall` | Dead wall tiles by slot |
| `wall-live-count`, `wall-exhausted-p` | Live wall size |

A replacement or nuki draw shortens the live wall by one; the dead wall stays the same size.

[^rng]: The shuffle uses splitmix64 and Fisher-Yates in `src/random.lisp`, not `cl:random`.
