# Validation corpus

The riichi rules are checked against 509 cases recorded from the test suite of the MIT-licensed Python [`mahjong`](https://github.com/MahjongRepository/mahjong) library. 482 run and the rest are skipped with a reason. The corpus passes when every run case matches.

```
sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

## Cases

| Kind | Calls | Checks |
|---|---|---|
| `hand` | `evaluate-score` + `best-score` | fu, han, yaku, points, payments, limit; or no score |
| `scores` | han/fu to payments; nagashi mangan as 5 han 30 fu | points, payments, limit |
| `shanten` | `shanten` with `:live t` | the number, or an error |
| `agari` | `decompose` | any split exists |
| `divide` | `decompose` | the same set of splits |

One test per upstream file runs in `tests/corpus.lisp` and lists each failing case with the expected and actual value. Skips appear in the fiveam summary.

## Differences from upstream

Each case names its variant and sets every option, so none inherits `*riichi-options*`. A case sets the kazoe mode and yakuman cap by extending the variant's scoring.[^mapping]

| Upstream | Here |
|---|---|
| Open tanyao off by default | `kuitan` is set per case |
| Winds default to none | `seat-wind` and `round-wind` may be `NIL` |
| Red fives count with `has_aka_dora` | `tiles` is passed only then |
| Kazoe and sextuple limits are config | `yakuman` limit `:han`, `:per` and `:max` |
| `Aotenjou` calculator | `aotenjou` variant |
| House yaku are config flags | [`house-rules` variant](dsl.md#house-rules) and its options |
| Short hands imply open sets | Shanten cases add open triplets of absent kinds |
| Shanten discounts tiles held in full | `:live t`; see [analysis.md](analysis.md#live-shanten) |

## Skipped

| Reason | Cases |
|---|---|
| Impossible contexts are not validated | 23 |
| Meld tiles absent from the hand | 4 |
| Aotenjou kokushi with riichi or menzen-tsumo[^kokushi] | 2 |

Whole-game checks against recorded games are in [replay.md](replay.md).

Not ported: the upstream unit tests of single yaku classes, melds and tile conversion.

## Regenerate

```
python3 tools/riichi-corpus/generate.py
```

The script clones the library at the pinned commit, runs its tests with `recorder.py` capturing every call, and rewrites `tests/corpus/riichi.sexp`. It needs git and Python 3 and gives identical output on every run.[^pin]

## Limitations

See [PLAN.md](PLAN.md#limitations):

| Skipped | Ticket |
|---|---|
| Impossible win contexts are not validated | [#10](https://github.com/takeiteasy/cl-mahjong/issues/10) |
| Upstream yaku class unit tests are not ported | [#15](https://github.com/takeiteasy/cl-mahjong/issues/15) |

[^mapping]: Hands are written as notation with `0` for a red five. A case lists the concealed tiles with the winning tile, its melds, dora and ura indicators, honba and sticks. The mapping lives in `tools/riichi-corpus/generate.py`.
[^kokushi]: Upstream's aotenjou kokushi ignores riichi and menzen-tsumo yet counts ura dora. Here every yaku stacks, so those two cases differ by one han.
[^pin]: The pinned commit and the upstream MIT notice are in the header of `tests/corpus/riichi.sexp`.
