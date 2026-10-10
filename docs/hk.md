# Hong Kong

`hk` plays Hong Kong mahjong: 144 tiles with flowers and seasons, fan scoring without fu, and three fan to win. `hk-full-shooter` changes who pays a ron.

```lisp
(start-game 'hk :seed 42)
```

## Flowers

Flowers and seasons are [bonus tiles](game.md#bonus-tiles): a seat sets one aside as soon as it holds one and draws a replacement.

| Yaku | Fan | When |
|---|---|---|
| `own-flower`, `own-season` | 1 each | A flower / season numbered like the seat; east is 1 |
| `no-flowers` | 1 | No bonus tile |
| `all-flowers`, `all-seasons` | 2 | All four of a group |

## Fixed-fan hands

| Hand | Result |
|---|---|
| Eight flowers | The seat that sets aside all eight bonus tiles wins the limit at once, paid as a tsumo |
| Seven robs one | A seat holding seven wins the limit when another seat sets aside the eighth; that seat pays as for a ron |

## Yaku

Haku, hatsu, chun, jikaze, bakaze, haitei, houtei, rinshan and chankan score 1 fan as in [riichi](yaku.md#riichi). A win on a flower replacement scores `rinshan`.

| Fan | Yaku |
|---|---|
| 1 | `common-hand` (all sequences, no honors), `concealed-hand`, `self-drawn` |
| 3 | `all-pungs`, `mixed-one-suit` |
| 4 | `seven-pairs` (closed) |
| 5 | `small-three-dragons` |
| 6 | `small-four-winds` |
| 7 | `pure-one-suit`, `mixed-terminals` |
| 8 | `great-three-dragons` |
| Limit | `kokushi`, `chuuren`, `suuankou`, `tsuuiisou`, `chinroutou`, `suukantsu`, `great-four-winds`, `tenhou`, `chiihou` |

A higher yaku drops the lower ones it contains: small three dragons replaces the dragon pungs, pure one suit replaces mixed one suit.

## Scoring

The `hk` [scoring](scoring.md#fan-table) has no fu and no dora.

| Fan | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Points | 1 | 2 | 4 | 8 | 16 | 24 | 32 | 48 | 64 | 96 | 128 |

A hand needs 3 fan, and a limit hand scores 128.

| Win | Payments |
|---|---|
| Tsumo | Each other seat pays the table value |
| Ron, `hk` | The discarder pays the table value and each other seat half of it |
| Ron, `hk-full-shooter` | The discarder pays twice the table value |

The dealer pays and receives the same as any seat. No sticks or honba are paid.

## Game flow

| Rule | Option |
|---|---|
| East, south, west and north rounds | `:length :full` |
| The dealer repeats on a win or any exhaustive draw | `:renchan :draw` |
| No riichi, furiten, kuikae, noten payment or abortive draws | `:riichi nil`, `:furiten nil`, `:kuikae nil`, `:noten-payment 0` |
| Eight flowers and seven robs one | `:bonus-win (8 :yakuman)`, `:bonus-rob t` |
| Every seat starts at 0 and the game ends after the last round | `:start-points 0`, `:west-extension nil` |

The yaku and table are definitions in `variants/hk.lisp`, so another parlour's values are a variant that extends `hk`.[^extend]

[^extend]: Yaku values are replaced with `(defyaku name :extends yaku (:han n))` and added to the variant with `(:add :yaku name)`; a replaced table is a `defscoring` that extends `hk`.
