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

Positions must be unique and inside the dead wall.

## Operations

| Function | Result |
|---|---|
| `make-wall tileset &key seed tiles` | New wall; random seed when omitted. `tiles` stacks it: every tile id once, in wall order |
| `wall-draw wall` | `(values tile new-wall)`; `(values nil wall)` when empty |
| `wall-draw-replacement wall` | Tile from the next replacement slot; the last live tile refills the slot |
| `wall-reveal-dora wall` | Wall with one more indicator revealed, or `NIL` when all are |
| `wall-dora-indicators`, `wall-ura-indicators` | Revealed indicator tiles; one is revealed initially |
| `wall-dead-tiles wall` | Dead wall tiles by slot |
| `wall-live-count`, `wall-exhausted-p` | Live wall size |

A replacement draw shortens the live wall by one; the dead wall stays the same size.

[^rng]: The shuffle uses splitmix64 and Fisher-Yates in `src/random.lisp`, not `cl:random`.
