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
| `(:fu entry...)` | The fu table; see [Fu](#fu) |
| `(:limits (name base [:han n])...)` | Base points of each limit; `:han` is the han that reaches it. A `yakuman` limit is required and is the base of one yakuman |
| `(:floor yaku limit)` | A hand holding `yaku` scores at least `limit`; the yaku's han are not added |
| `(:kiriage base)` | The base points that round up to the lowest limit when the `kiriage` option is on |
| `(:dora (:indicator next))` | Indicator to dora: the next kind of the same suit or honor group, wrapping round; or a function `(tileset kind) -> kind id` |
| `(:payments entry...)` | `(:ron :dealer m :other m)`, `(:tsumo :dealer m :other (from-dealer from-other))`, `(:round n)`, `(:honba n)`, `(:riichi-stick n)` |

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
| `(:flat yaku n)` | Fixed fu, rounding skipped, when the reading holds `yaku` |
| `(:no-tsumo-fu yaku...)` | No tsumo fu when the reading holds one of these |
| `(:lambda (hand reading) ...)` | Replaces the whole table |

A concealed group is one [`group-concealed-p`](pattern.md#win-hand) accepts, so a triplet completed by ron counts as open. Only the sets family earns group fu; other shapes score `:base`.

| Hand (riichi) | Fu |
|---|---|
| Pinfu tsumo / ron | 20 / 30 |
| Chiitoitsu | 25 |
| Open hand, no other fu | 30 |
| Concealed terminal quad | 32 |

## Han and limits

Han are the reading's yaku plus dora, ura dora and red fives. A reading with no yaku does not score: dora never make a yaku.

| Han | Result |
|---|---|
| 1-4 | `fu x 2^(han + 2)` base points, up to the lowest limit |
| 5 / 6-7 / 8-10 / 11-12 | mangan / haneman / baiman / sanbaiman |
| 13+ | kazoe yakuman |
| Yakuman reading | `yakuman x` the `yakuman` base |

## Dora

The context carries the dora inputs.

| Field | Content |
|---|---|
| `dora-indicators` | Tile ids of the revealed indicators; a kan dora is one more |
| `ura-indicators` | Tile ids; counted only with riichi or double riichi |
| `tiles` | Tile ids of the whole hand, melds and winning tile included; red fives are counted from these |
| `dealer` | The winner is the dealer |

Dora count over the concealed tiles and every meld, so a quad of a dora kind counts four. Sanma's man 1 and 9 follow each other because the next kind is taken from the tileset.

## Payments

Each payer's share rounds up on its own. `:honba n` is paid per honba by the discarder on a ron, and split between the other players on a tsumo. The winner also receives `:riichi-stick` per stick.

| `payments` | Meaning |
|---|---|
| `(:ron n)` | Paid by the discarder |
| `(:dealer n :other m)` | Tsumo; `:dealer` is `NIL` when the winner deals |

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
| `score-dora` | `(:dora n :ura n :aka n)` |
| `score-limit`, `score-base` | Limit name or `NIL`; base points |
| `score-points`, `score-payments` | What the winner receives, honba and sticks included; who pays what |

## Riichi

`variants/riichi.lisp` defines `riichi`. These options come from `*riichi-options*`.

| Option | Default | Effect |
|---|---|---|
| `kiriage` | off | 30 fu 4 han and 60 fu 3 han score mangan |
| `kazoe-yakuman` | on | 13 han score yakuman; off caps at sanbaiman |
| `double-wind-fu` | 4 | Most fu a pair earns; 2 pays a double wind pair 2 |
| `rinshan-tsumo-fu` | on | A win on a replacement draw earns tsumo fu |
| `renhou` | off | `:yakuman` scores renhou as a yakuman, `:mangan` as a mangan floor |

## Limitations

See [PLAN.md](PLAN.md#limitations): pao is not applied ([#17](https://todo.sr.ht/~takeiteasy/cl-mahjong/17)), sanma tsumo is paid by the two other seats and the missing north is not split ([#23](https://todo.sr.ht/~takeiteasy/cl-mahjong/23)), and the sanma honba value is unchecked ([#24](https://todo.sr.ht/~takeiteasy/cl-mahjong/24)).
