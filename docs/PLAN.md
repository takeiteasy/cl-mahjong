# Plan

cl-mahjong is a Common Lisp rules engine where a whole mahjong variant (tiles, wall, hand shapes, yaku, scoring, game-flow options) is defined in a DSL. Every definition can inherit from another. v1 supports riichi fully. See [design.md](design.md).

## Phases

| # | Deliverable | Ticket | Status |
|---|---|---|---|
| 0 | Skeleton, docs, tracker | n/a | done |
| 1 | Registry, inheritance, tileset and wall DSL | [#1](https://todo.sr.ht/~takeiteasy/cl-mahjong/1) | done |
| 2 | Shape DSL, decomposition, shanten | [#2](https://todo.sr.ht/~takeiteasy/cl-mahjong/2) | done |
| 3 | Pattern language, `defyaku`, riichi yaku | [#3](https://todo.sr.ht/~takeiteasy/cl-mahjong/3) | open |
| 4 | Scoring DSL | [#4](https://todo.sr.ht/~takeiteasy/cl-mahjong/4) | open |
| 5 | Game-flow state machine | [#5](https://todo.sr.ht/~takeiteasy/cl-mahjong/5) | open |
| 6 | Variant deltas, sanma, event examples | [#6](https://todo.sr.ht/~takeiteasy/cl-mahjong/6) | open |
| 7 | Riichi validation corpus | [#7](https://todo.sr.ht/~takeiteasy/cl-mahjong/7) | open |

## Limitations

| Limitation | Ticket |
|---|---|
| `master` is still the GitHub default branch | [#8](https://todo.sr.ht/~takeiteasy/cl-mahjong/8) |
| No Tenhou log replay validation | [#9](https://todo.sr.ht/~takeiteasy/cl-mahjong/9) |
| No HK variant | [#10](https://todo.sr.ht/~takeiteasy/cl-mahjong/10) |
| No MCR variant | [#11](https://todo.sr.ht/~takeiteasy/cl-mahjong/11) |
| No CLI; the library has no UI | [#12](https://todo.sr.ht/~takeiteasy/cl-mahjong/12) |
| No bot interface | [#13](https://todo.sr.ht/~takeiteasy/cl-mahjong/13) |
| Rule files must be trusted Lisp; no sandboxed loading | [#14](https://todo.sr.ht/~takeiteasy/cl-mahjong/14) |
| Shanten uses generic shape search, no fast path | [#15](https://todo.sr.ht/~takeiteasy/cl-mahjong/15) |
