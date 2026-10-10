# Validation corpus

The riichi rules are checked against 509 cases recorded from the test suite of the MIT-licensed Python [`mahjong`](https://github.com/MahjongRepository/mahjong) library. 378 run and the rest are skipped with a reason. The corpus passes when every run case matches.

```
sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

## Cases

| Kind | Calls | Checks |
|---|---|---|
| `hand` | `evaluate-score` + `best-score` | fu, han, yaku, points, payments, limit; or no score |
| `scores` | han/fu to payments | points, payments, limit |
| `shanten` | `shanten` | the number, or an error |
| `agari` | `decompose` | any split exists |
| `divide` | `decompose` | the same set of splits |

One test per upstream file runs in `tests/corpus.lisp` and lists each failing case with the expected and actual value. Skips appear in the fiveam summary.

## Differences from upstream

Each case sets every option, so none inherits `*riichi-options*`.[^mapping]

| Upstream | Here |
|---|---|
| Open tanyao off by default | `kuitan` is set per case |
| Winds default to none | `seat-wind` and `round-wind` may be `NIL` |
| Red fives count with `has_aka_dora` | `tiles` is passed only then |
| Renhou is a 5 han yaku | Renhou is a mangan floor; han is not compared |
| Short hands imply open sets | Shanten cases add open triplets of absent kinds |
| Shanten discounts tiles held in full | Not compared; see [analysis.md](analysis.md) |

## Skipped

| Reason | Cases |
|---|---|
| Han of 13 or more scored as multiples (uncapped kazoe) | 25 |
| Shanten of hands holding four of a kind | 25 |
| Nuki dora | 22 |
| Impossible contexts are not validated | 19 |
| Aotenjou scoring | 18 |
| Open riichi, paarenchan, nagashi mangan | 13 |
| Daisharin, daichikurin, daisuurin, daichisei | 4 |
| Meld tiles absent from the hand | 4 |
| Pinfu fu rules as options | 1 |

Open riichi, the house yakuman and the fu rules are not part of riichi here; [dsl.md](dsl.md) shows how a variant adds yaku.

Not ported: the upstream unit tests of single yaku classes, melds and tile conversion.

## Regenerate

```
python3 tools/riichi-corpus/generate.py
```

The script clones the library at the pinned commit, runs its tests with `recorder.py` capturing every call, and rewrites `tests/corpus/riichi.sexp`. It needs git and Python 3 and gives identical output on every run.[^pin]

## Limitations

See [PLAN.md](PLAN.md#limitations): impossible win contexts are not validated ([#25](https://todo.sr.ht/~takeiteasy/cl-mahjong/25)), and sanma has no nukidora ([#22](https://todo.sr.ht/~takeiteasy/cl-mahjong/22)).

[^mapping]: Hands are written as notation with `0` for a red five. A case lists the concealed tiles with the winning tile, its melds, dora and ura indicators, honba and sticks. The mapping lives in `tools/riichi-corpus/generate.py`.
[^pin]: The pinned commit and the upstream MIT notice are in the header of `tests/corpus/riichi.sexp`.
