# Plan

cl-mahjong is a Common Lisp rules engine where a whole mahjong variant (tiles, wall, hand shapes, yaku, scoring, game-flow options) is defined in a DSL. Every definition can inherit from another. v1 supports riichi fully. See [design.md](design.md).

## Phases

| # | Deliverable | Ticket | Status |
|---|---|---|---|
| 0 | Skeleton, docs, tracker | n/a | done |
| 1 | Registry, inheritance, tileset and wall DSL | sr.ht #1 | done |
| 2 | Shape DSL, decomposition, shanten | sr.ht #2 | done |
| 3 | Pattern language, `defyaku`, riichi yaku | sr.ht #3 | done |
| 4 | Scoring DSL | sr.ht #4 | done |
| 5 | Game-flow state machine, multiple ron | sr.ht #5, sr.ht #18 | done |
| 6 | Variant deltas, sanma, event examples | sr.ht #6 | done |
| 7 | Riichi validation corpus | sr.ht #7 | done |
| 8 | Tenhou variant and log replay | n/a | done |

## Limitations

| Limitation | Ticket |
|---|---|
| Open riichi cannot be declared in game flow; sashikomi is any ron in open riichi | [#20](https://github.com/takeiteasy/cl-mahjong/issues/20) |
| No WRC rules variant | [#9](https://github.com/takeiteasy/cl-mahjong/issues/9) |
| Sanma logs before 2022-01-23 are not replayed | [#18](https://github.com/takeiteasy/cl-mahjong/issues/18) |
| No HK variant | [#1](https://github.com/takeiteasy/cl-mahjong/issues/1) |
| No MCR variant | [#2](https://github.com/takeiteasy/cl-mahjong/issues/2) |
| No CLI; the library has no UI | [#3](https://github.com/takeiteasy/cl-mahjong/issues/3) |
| No bot interface | [#4](https://github.com/takeiteasy/cl-mahjong/issues/4) |
| Rule files must be trusted Lisp; no sandboxed loading | [#5](https://github.com/takeiteasy/cl-mahjong/issues/5) |
| Shanten uses generic shape search, no fast path | [#6](https://github.com/takeiteasy/cl-mahjong/issues/6) |
| No pao for suukantsu or daiminkan rinshan | [#16](https://github.com/takeiteasy/cl-mahjong/issues/16) |
| Game flow recomputes legal actions and scans every kind for waits | [#8](https://github.com/takeiteasy/cl-mahjong/issues/8) |
| Pattern selector names are not checked against the tileset | [#7](https://github.com/takeiteasy/cl-mahjong/issues/7) |
