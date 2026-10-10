# Yaku

`defyaku` declares a scoring pattern. `evaluate-yaku` finds every way a winning hand reads and the yaku each reading earns.

```lisp
(defyaku sanshoku (:han 2 :open 1)
  (:when (across-suits sequence)))

(let ((ts (find-tileset 'riichi-136)))
  (evaluate-yaku ts '(standard seven-pairs kokushi) *riichi-yaku*
                 counts melds
                 (make-win-context :win-tile tile :seat-wind :east :round-wind :east
                                   :tsumo t)
                 :options *riichi-options*))
```

## Clauses

| Clause | Meaning |
|---|---|
| `(:han n [:open m] [:closed-only t] [:needs-yaku pattern])` | `n` han closed, `m` open (default `n`); `:closed-only` never scores with an open meld; `:needs-yaku` drops the yaku when `pattern` holds and it is the reading's only yaku |
| `(:yakuman n [:closed-only t] [:double-when pattern] [:needs-yaku pattern] [:pao selector])` | `n` yakuman, or `(:lambda (hand) ...)` returning the multiple; doubles when `pattern` holds and the `double-yakuman` option is on; see [Pao](#pao) |
| `(:when pattern...)` | All must hold; see [pattern.md](pattern.md) |
| `(:implies yaku...)` | When this scores, those are dropped |
| `(:excludes yaku...)` | Never together; the lower value is dropped, on a tie the later in the list |

A yaku has `:han` or `:yakuman`, and needs `:when`. `:extends` inherits a yaku; a child clause replaces the parent's clause with the same key.

```lisp
(defyaku sanshoku-3 :extends sanshoku (:han 3))
```

Yakuman replace every other yaku in a reading, unless `:stack-yakuman` is given to `evaluate-yaku` (a scoring with `:aotenjou` does).

## Pao

`:pao selector` makes the seat that fed the last meld completing the yakuman liable for it. Riichi sets `dragon` on daisangen and `wind` on daisuushii.

| Hand | Liable seat |
|---|---|
| Every triplet or quad matching `selector` is a meld and the last one is open | The `from` of that meld |
| One matching set is concealed, or the last matching meld is a closed kan | Nobody |

With option `pao-held` a concealed matching set does not block pao when its caller already held it as a triplet at the last call; a set built or completed later, or a ron on a shanpon, still does.

Melds record the giver with `make-meld ... :from` and the triplets the caller held with `:held`, a list of kind ids.[^held] A shouminkan keeps the giver and `:held` of its pon. How a liable seat pays is in [Scoring](scoring.md#pao).

`:pao kan` makes the seat that fed the latest daiminkan liable, the context's [`kan-from`](pattern.md#win-context). A shouminkan or ankan after it clears the seat. `suukantsu` has no pao; a yaku extending it adds one:

```lisp
(defyaku suukantsu-pao :extends suukantsu
  (:yakuman 1 :pao kan))
```

## Evaluation

| Function | Result |
|---|---|
| `(evaluate-yaku ts shapes yaku counts melds context &key options stack-yakuman)` | A list of readings |

`yaku` is a list of yaku or their names, like `shapes`. `counts` include the winning tile named by the context ([pattern.md](pattern.md#win-context)). A reading is one decomposition with the winning tile in one group, so a hand with several splits or waits has several readings. Readings that earn no yaku are included. An impossible [win context](pattern.md#win-context) signals `invalid-hand`.

| Reader | Value |
|---|---|
| `reading-decomposition`, `reading-win-group`, `reading-wait` | Where the winning tile sits |
| `reading-hand` | The [win hand](pattern.md#win-hand) |
| `reading-yaku` | `(name . value)` pairs; `value` is han, or the yakuman multiple in a yakuman reading |
| `reading-han` | Han of the non-yakuman yaku; `0` in a yakuman reading unless `stack-yakuman` |
| `reading-yakuman` | Total yakuman multiple |
| `reading-pao` | `(seat . yakuman)` for each liable seat; `NIL` without pao |

Dora do not count towards han; [scoring](scoring.md) adds them and picks the best reading.[^dora] Yakuman that imply their lower yaku (daisangen drops haku, hatsu, chun and shousangen) drop them even when yakuman stack.

House-rule yaku are in [dsl.md](dsl.md#house-rules).

## Riichi

`*riichi-yaku*` lists the yaku of `variants/riichi.lisp`; pass `*riichi-options*` as `:options` for the defaults below. Without `:options` every option is off.

| Option | Default | Effect |
|---|---|---|
| `kuitan` | on | Tanyao scores with open melds |
| `double-yakuman` | on | Yakuman with `:double-when` can double |
| `renhou` | off | `:yakuman` scores renhou as a yakuman, `:mangan` as `renhou-mangan`, a 5 han yaku that [scoring](scoring.md) treats as a mangan floor, `:han` as `renhou-han`, a 5 han yaku that stacks with the others |
| `pao-held` | off | A concealed set held at the last call keeps [pao](#pao) |

| Han (closed / open) | Yaku |
|---|---|
| 1 / – | riichi, ippatsu, menzen-tsumo, pinfu, iipeikou |
| 1 / 1 | tanyao, haku, hatsu, chun, jikaze, bakaze, haitei, houtei, rinshan, chankan |
| 2 / – | double-riichi, chiitoitsu |
| 2 / 1 | chanta, ittsu, sanshoku |
| 2 / 2 | sanshoku-doukou, sanankou, sankantsu, toitoi, shousangen, honroutou |
| 3 / 2 | junchan, honitsu |
| 3 / – | ryanpeikou |
| 6 / 5 | chinitsu |

Double riichi replaces riichi, ryanpeikou iipeikou and junchan chanta. A double wind scores jikaze and bakaze.

| Yakuman | Doubles on |
|---|---|
| kokushi | thirteen-sided wait |
| suuankou | tanki wait |
| chuuren | nine-sided wait |
| daisuushii | always |
| daisangen, shousuushii, tsuuiisou, chinroutou, ryuuiisou, suukantsu | – |
| tenhou, chiihou, renhou | – |

## Limitations

See [PLAN.md](PLAN.md#limitations): selector names are not checked against the tileset.

[^dora]: Dora, red fives, kazoe yakuman and limits belong to scoring.

[^held]: The game fills `:held` on each open call with the kinds of which the caller still has three or more concealed tiles.
