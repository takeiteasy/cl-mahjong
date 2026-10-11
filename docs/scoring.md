# Scoring

`defscoring` turns a reading into points: fu, han plus dora, limits, and who pays what. `evaluate-score` scores every reading of one winner's hand and `best-score` picks the highest.

```lisp
(let* ((ts (find-tileset 'riichi-136))
       (scores (evaluate-score 'riichi ts '(standard seven-pairs kokushi) *riichi-yaku*
                               counts melds
                               (make-win-context :win-tile tile :seat-wind :east :round-wind :east
                                                 :riichi t :dora-indicators indicators)
                               :options *riichi-options* :honba 1 :riichi-sticks 2))
       (best (best-score scores)))
  (list (score-fu best) (score-han best) (score-limit best) (score-points best)))
```

## Clauses

| Clause | Meaning |
|---|---|
| `(:fu entry...)` | The fu table; see [Fu](#fu). Optional with `:points` |
| `(:points (fan base)...)` | A fan table; see [Fan table](#fan-table) |
| `(:minimum n [:excluding yaku...])` | Fewest han a hand scores with, not counting the named yaku; see [Fan table](#fan-table) |
| `(:limits (name base [:han n])...)` | Base points of each limit; `:han` is the han that reaches it. A `yakuman` limit is required and is the base of one yakuman; see [Kazoe](#kazoe-and-the-yakuman-cap) |
| `(:floor yaku limit)` | A hand holding `yaku` scores at least `limit`; the yaku's han are not added |
| `(:kiriage base)` | The base points that round up to the lowest limit when the `kiriage` option is on |
| `(:aotenjou n)` | Scores without limits, each yakuman counting `n` han; see [Aotenjou](#aotenjou) |
| `(:dora (:indicator next))` | Indicator to dora: the next kind of the same suit or honor group, wrapping round; or a function `(tileset kind) -> kind id` |
| `(:payments entry...)` | `(:ron :dealer m :other m [:others m])`, `(:tsumo :dealer m :other (from-dealer from-other))`, `(:pao [:ron r] [:honba h])`, `(:round n)`, `(:honba n)`, `(:riichi-stick n)`, `(:absent none\|split)` |

`:extends` inherits a scoring. A child entry replaces the parent's entry with the same key, and a new entry is added.

```lisp
(defscoring loose :extends riichi
  (:limits (mangan 2000 :han 4))
  (:fu (:pair dragon 4)))
```

## Fu

| Entry | Meaning |
|---|---|
| `(:base n)`, `(:round n)` | Starting fu; fu round up to a multiple of `n` |
| `(:closed-ron n)`, `(:tsumo n)` | Added for a closed ron / a tsumo |
| `(:open-minimum n)` | Floor for an open hand |
| `(:group types [selector] n)` | Fu of the first matching group; `triplet` also matches a quad |
| `(:concealed n)`, `(:quad n)` | Multipliers for a concealed group / a quad |
| `(:pair selector n)` | Fu of the pair; every matching entry adds |
| `(:wait names n)` | Fu for those [waits](pattern.md#waits) |
| `(:flat yaku n [:tsumo m])` | Fixed fu, rounding skipped, when the reading holds `yaku`; `m` on a tsumo |
| `(:no-tsumo-fu yaku...)` | No tsumo fu when the reading holds one of these |
| `(:lambda (hand reading) ...)` | Replaces the whole table |

A concealed group is one [`group-concealed-p`](pattern.md#win-hand) accepts, so a triplet completed by ron counts as open. Only the sets family earns group fu; other shapes score `:base`.

| Hand (riichi) | Fu |
|---|---|
| Pinfu tsumo / ron | 20 / 30 |
| Chiitoitsu | 25 |
| Open hand, no other fu | 30 (`open-pinfu-fu`) |
| Concealed terminal quad | 32 |

## Han and limits

Han are the reading's yaku plus dora, ura dora and red fives. A reading with no yaku does not score: dora never make a yaku.

| Han | Result |
|---|---|
| 1-4 | `fu x 2^(han + 2)` base points, up to the lowest limit |
| 5 / 6-7 / 8-10 / 11-12 | mangan / haneman / baiman / sanbaiman |
| 13+ | kazoe yakuman, or sanbaiman when the `yakuman` limit has no `:han` |
| Yakuman reading | `yakuman x` the `yakuman` base |

## Fan table

`(:points (fan base)...)` replaces the fu formula: the base points are those of the highest row at or below the hand's han, so the last row caps the table. Fu is `0`. Yakuman readings still use the `yakuman` limit.

`(:minimum n)` rejects a hand with fewer han, dora included, unless it is a yakuman. `:excluding` names yaku whose han do not count towards `n`; they still score.

```lisp
(:minimum 8 :excluding own-flower own-season)
```

```lisp
(defscoring fan
  (:points (0 1) (1 2) (2 4) (3 8) (4 16))
  (:minimum 2)
  (:limits (yakuman 64 :max 1))
  (:payments (:ron :dealer 1 :other 1) (:tsumo :dealer 1 :other (1 1))))
```

| Han | Base |
|---|---|
| 1 | Rejected by `:minimum 2` |
| 2 | 4 |
| 7 | 16, the last row |

## Kazoe and the yakuman cap

The `yakuman` limit takes `:han`, `:per` and `:max`.

| Option | Meaning |
|---|---|
| `:han n` | Han that score a kazoe yakuman; without it 13 han stay a sanbaiman |
| `:per n` | Every `n` han from `:han` add a yakuman: 26 han score two |
| `:max n` | Most yakuman a hand scores, yakuman readings included |

| Limit | 13 han | 26 han | 7 yakuman |
|---|---|---|---|
| `(yakuman 8000 :han 13 :max 6)` (riichi) | 1 | 1 | 6 |
| `(yakuman 8000 :han 13 :per 13)` | 1 | 2 | 7 |
| `(yakuman 8000)` | sanbaiman | sanbaiman | 7 |

```lisp
(defscoring uncapped :extends riichi
  (:limits (yakuman 8000 :han 13 :per 13)))
```

## Aotenjou

`(:aotenjou n)` drops every limit: base points are `fu x 2^(han + 2)`, with each yakuman worth `n` han on top of the other yaku and dora. Payments follow the table as usual. The `aotenjou` scoring and [variant](dsl.md#house-rules) pay no honba or sticks and score kokushi 40 fu on a ron, 30 on a tsumo.

| Hand | Result |
|---|---|
| Kokushi ron, non-dealer | 13 han 40 fu, 5242900 |
| Same, dealer | 7864400 |

## Dora

The context carries the dora inputs.

| Field | Content |
|---|---|
| `dora-indicators` | Tile ids of the revealed indicators; a kan dora is one more |
| `ura-indicators` | Tile ids; counted only with riichi or double riichi |
| `tiles` | Tile ids of the whole hand, melds and winning tile included; red fives are counted from these |
| `nuki` | Tile ids set aside as dora; each is one han, and counts again when the dora kind is its own |
| `dealer` | The winner is the dealer |

Dora count over the concealed tiles and every meld, so a quad of a dora kind counts four. Sanma's man 1 and 9 follow each other because the next kind is taken from the tileset.

## Payments

Each payer's share rounds up on its own. `:honba n` is paid per honba by the discarder on a ron, and split between the other players on a tsumo. The winner also receives `:riichi-stick` per stick.

| `payments` | Meaning |
|---|---|
| `(:ron n)` | Paid by the discarder |
| `(:ron n :others m)` | The discarder pays `n` and every other seat but the winner pays `m`; see [below](#ron-paid-by-every-seat) |
| `(:dealer n :other m)` | Tsumo; `:dealer` is `NIL` when the winner deals |
| `(:ron n :from seat)` | Paid whole by `seat`, the [rinshan pao](#rinshan-pao) giver |
| `:pao ((seat . n)...)` | Added when a seat is [liable](#pao); the other amounts are what remains |

## Ron paid by every seat

`:others m` on `:ron` makes each seat besides the winner and the discarder pay `m` times the base on a ron. Multipliers may be fractions; a tsumo ignores `:others`.[^others]

```lisp
(defscoring half-shooter
  (:payments (:ron :dealer 1 :other 1 :others 1/2)))
```

| Base 4 | Discarder | Each other seat | Winner gets |
|---|---|---|---|
| `:others 1/2` | 4 | 2 | 8 |

## Pao

A seat [liable](yaku.md#pao) for a yakuman pays that yakuman as a ron. The rest of the hand is paid as usual.

| Win | Liable seat pays | Others pay |
|---|---|---|
| Tsumo | The pao yakuman and every honba | The rest, as a tsumo |
| Ron, `:ron split` | Half the pao yakuman | The discarder pays the rest of the ron |
| Ron, `:ron liable` | The whole pao yakuman | The discarder pays the rest of the ron |

`:honba` says who pays the honba of a ron: `discarder`, `liable` or `split`. The defaults are `split` and `discarder`. A liable discarder pays both parts.

```lisp
(defscoring strict :extends riichi
  (:payments (:pao :ron liable :honba liable)))
```

### Rinshan pao

`:rinshan` makes the seat whose discard made a daiminkan pay the whole hand when the kan's replacement draw wins.[^rinshan]

| `:rinshan` | Win is scored as | The giver pays |
|---|---|---|
| `none` (default) | A tsumo | Nothing extra |
| `tsumo` | A tsumo | Every share of the tsumo and the honba |
| `ron` | A ron: ron fu, no menzen-tsumo, rinshan still scores | The ron and the honba |

```lisp
(defscoring giver-pays :extends riichi
  (:payments (:pao :rinshan tsumo)))
```

## Results

| Function | Result |
|---|---|
| `(evaluate-score scoring ts shapes yaku counts melds context &key options honba riichi-sticks players)` | A list of scores, one per reading with a yaku; `players` defaults to 4 |
| `(reading-score scoring reading &key honba riichi-sticks players)` | The score of one reading, or `NIL` |
| `(best-score scores)` | Most points, then han, then fu; `NIL` when empty |

| Reader | Value |
|---|---|
| `score-reading` | The scored [reading](yaku.md#evaluation) |
| `score-fu`, `score-han` | Fu; han including dora |
| `score-dora` | `(:dora n :ura n :aka n :nuki n)` |
| `score-limit`, `score-base` | Limit name or `NIL`; base points |
| `score-points`, `score-payments` | What the winner receives, honba and sticks included; who pays what |

## Riichi

`variants/riichi.lisp` defines `riichi`. These options come from `*riichi-options*`.

| Option | Default | Effect |
|---|---|---|
| `kiriage` | off | 30 fu 4 han and 60 fu 3 han score mangan |
| `double-wind-fu` | 4 | Most fu a pair earns; 2 pays a double wind pair 2 |
| `rinshan-tsumo-fu` | on | A win on a replacement draw earns tsumo fu |
| `renhou` | off | `:yakuman` scores renhou as a yakuman, `:mangan` as a mangan floor, `:han` as a 5 han yaku that stacks |
| `open-pinfu-fu` | on | An open hand with no other fu scores 30 |
| `pinfu-tsumo-fu` | off | Pinfu tsumo earns tsumo fu (30 instead of 20) |

## Sanma

A sanma tsumo is paid by the two other seats. `(:absent split)` splits the missing north's share between the payers: the share is the rounded four-player amount, added to each payer's and rounded up again.

| Hand | `sanma` (none) | `sanma-split` |
|---|---|---|
| Non-dealer tsumo, base 240 | 500 / 300 | 700 / 500 |
| Dealer tsumo, base 240 | 500 all | 800 all |
| Mangan | 4000 / 2000 | 5000 / 3000 |

```lisp
(defscoring sanma-split :extends sanma
  (:payments (:absent split)))
```

The `sanma-split` variant uses it.

[^rinshan]: It replaces any yakuman pao on the hand, which is not also charged.

[^others]: A hand with a pao liability pays as usual and ignores `:others`.
