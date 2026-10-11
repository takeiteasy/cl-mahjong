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
| 9 | Hong Kong variant: bonus tiles, fan table, game switches | #1, #21, #22, #23, #24 | done |
| 10 | WRC variant | #9 | done |
| 11 | Bot interface, dead/easy/hard bots | #4 | done |

## Limitations

| Limitation | Ticket |
|---|---|
| Sanma logs before 2022-01-23 are not replayed | [#18](https://github.com/takeiteasy/cl-mahjong/issues/18) |
| Bonus wins are not checked while dealing | [#26](https://github.com/takeiteasy/cl-mahjong/issues/26) |
| Wrong declarations cannot happen: illegal actions are rejected, no chombo | [#27](https://github.com/takeiteasy/cl-mahjong/issues/27) |
| No per-seat view; every hand is visible in the game state | [#28](https://github.com/takeiteasy/cl-mahjong/issues/28) |
| No action log; a game cannot be replayed from its actions | [#29](https://github.com/takeiteasy/cl-mahjong/issues/29) |
| A game cannot be saved and restored | [#30](https://github.com/takeiteasy/cl-mahjong/issues/30) |
| No player identities or turn timeouts | [#31](https://github.com/takeiteasy/cl-mahjong/issues/31) |
| No Washizu (visible tiles) variant | [#32](https://github.com/takeiteasy/cl-mahjong/issues/32) |
| No MCR variant | [#2](https://github.com/takeiteasy/cl-mahjong/issues/2) |
| No CLI; the library has no UI | [#3](https://github.com/takeiteasy/cl-mahjong/issues/3) |
| Rule files must be trusted Lisp; no sandboxed loading | [#5](https://github.com/takeiteasy/cl-mahjong/issues/5) |
| Shanten uses generic shape search, no fast path; waits are found by testing every kind | [#6](https://github.com/takeiteasy/cl-mahjong/issues/6) |
| Pattern selector names are not checked against the tileset | [#7](https://github.com/takeiteasy/cl-mahjong/issues/7) |
