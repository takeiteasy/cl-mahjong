# Plan

cl-mahjong is a Common Lisp rules engine where a whole mahjong variant (tiles, wall, hand shapes, yaku, scoring, game-flow options) is defined in a DSL. Every definition can inherit from another. v1 supports riichi fully. See [design.md](design.md).

## Phases

| # | Deliverable | Ticket | Status |
|---|---|---|---|
| 0 | Skeleton, docs, tracker | n/a | done |
| 1 | Registry, inheritance, tileset and wall DSL | [#1](https://todo.sr.ht/~takeiteasy/cl-mahjong/1) | done |
| 2 | Shape DSL, decomposition, shanten | [#2](https://todo.sr.ht/~takeiteasy/cl-mahjong/2) | done |
| 3 | Pattern language, `defyaku`, riichi yaku | [#3](https://todo.sr.ht/~takeiteasy/cl-mahjong/3) | done |
| 4 | Scoring DSL | [#4](https://todo.sr.ht/~takeiteasy/cl-mahjong/4) | done |
| 5 | Game-flow state machine, multiple ron | [#5](https://todo.sr.ht/~takeiteasy/cl-mahjong/5), [#18](https://todo.sr.ht/~takeiteasy/cl-mahjong/18) | done |
| 6 | Variant deltas, sanma, event examples | [#6](https://todo.sr.ht/~takeiteasy/cl-mahjong/6) | done |
| 7 | Riichi validation corpus | [#7](https://todo.sr.ht/~takeiteasy/cl-mahjong/7) | done |

## Limitations

| Limitation | Ticket |
|---|---|
| Impossible win contexts are not validated | [#25](https://todo.sr.ht/~takeiteasy/cl-mahjong/25) |
| Uncapped kazoe, aotenjou and the yakuman cap are not supported | [#26](https://todo.sr.ht/~takeiteasy/cl-mahjong/26) |
| Shanten does not discount tiles held in full | [#27](https://todo.sr.ht/~takeiteasy/cl-mahjong/27) |
| No house-rule yaku (open riichi, paarenchan, daisharin family) | [#28](https://todo.sr.ht/~takeiteasy/cl-mahjong/28) |
| Pinfu fu rules are fixed | [#29](https://todo.sr.ht/~takeiteasy/cl-mahjong/29) |
| Renhou is a mangan floor, not a 5 han yaku | [#30](https://todo.sr.ht/~takeiteasy/cl-mahjong/30) |
| Upstream yaku class unit tests are not ported | [#31](https://todo.sr.ht/~takeiteasy/cl-mahjong/31) |
| No Tenhou log replay validation | [#9](https://todo.sr.ht/~takeiteasy/cl-mahjong/9) |
| No WRC or Tenhou rules variants | [#20](https://todo.sr.ht/~takeiteasy/cl-mahjong/20), [#21](https://todo.sr.ht/~takeiteasy/cl-mahjong/21) |
| No sanma nukidora (kita) | [#22](https://todo.sr.ht/~takeiteasy/cl-mahjong/22) |
| Sanma tsumo does not split the missing north share | [#23](https://todo.sr.ht/~takeiteasy/cl-mahjong/23) |
| No HK variant | [#10](https://todo.sr.ht/~takeiteasy/cl-mahjong/10) |
| No MCR variant | [#11](https://todo.sr.ht/~takeiteasy/cl-mahjong/11) |
| No CLI; the library has no UI | [#12](https://todo.sr.ht/~takeiteasy/cl-mahjong/12) |
| No bot interface | [#13](https://todo.sr.ht/~takeiteasy/cl-mahjong/13) |
| Rule files must be trusted Lisp; no sandboxed loading | [#14](https://todo.sr.ht/~takeiteasy/cl-mahjong/14) |
| Shanten uses generic shape search, no fast path | [#15](https://todo.sr.ht/~takeiteasy/cl-mahjong/15) |
| Game flow recomputes legal actions and scans every kind for waits | [#19](https://todo.sr.ht/~takeiteasy/cl-mahjong/19) |
| Pattern selector names are not checked against the tileset | [#16](https://todo.sr.ht/~takeiteasy/cl-mahjong/16) |
