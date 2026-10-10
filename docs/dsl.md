# DSL examples

Every definition takes `:extends`, so a ruleset is usually a few lines on top of another. Each kind has its own page; this one shows them together.

## Sanma

A [tileset](tileset.md) delta, then a [variant](variant.md) delta on riichi.

```lisp
(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8))
  (:wall (:dead-wall 14) (:replacement 0 1 2 3)
         (:dora 4 6 8 10 12) (:ura 5 7 9 11 13) (:nuki 4)))

(defscoring sanma :extends riichi
  (:payments (:honba 200)))

(defvariant sanma :extends riichi
  (:players 3) (:tileset sanma-108) (:scoring sanma)
  (:set :chi nil :winds (:east :south :west)
        :start-points 35000 :target-points 40000 :uma (15 0 -15)
        :noten-payment 2000 :suufon-renda nil :suucha-riichi nil
        :riichi-min-tiles 3 :nukidora t))
```

A tsumo is paid by the two other seats at the normal multipliers.[^sanma] Each honba is worth 200: the discarder pays it on a ron, each payer pays 100 on a tsumo. A north can be set aside with `(:kita)`; see [game.md](game.md#actions).

## Tenhou

A [variant](variant.md) delta on riichi, used by the [log replay](replay.md).

```lisp
(defvariant tenhou :extends riichi
  (:set :double-yakuman nil :uma (20 10 -10 -20)))

(defvariant tenhou-sanma :extends sanma
  (:set :double-yakuman nil :uma (20 0 -20)))
```

## Event rulesets

| Variant | Changes |
|---|---|
| `akadora-frenzy` | Two red fives per suit |
| `yaku-festival` | Adds the daisharin yakuman, renhou is a yakuman |
| `house-rules` | The house-rule yaku below, on |
| `aotenjou` | `house-rules` scored without limits |
| `sanma-tonpuu` | Sanma played east only; three levels: riichi, sanma, event |
| `dora-bomb` | Three starting dora indicators, immediate kan dora |

```lisp
(deftileset riichi-136-akadora :extends riichi-136
  (:red (man 5 2) (pin 5 2) (sou 5 2)))

(defyaku daisharin (:yakuman 1 :closed-only t)
  (:when (option daisharin)
         (shape seven-pairs)
         (every-tile (pin-2 pin-3 pin-4 pin-5 pin-6 pin-7 pin-8))))

(defvariant yaku-festival :extends riichi
  (:add :yaku daisharin)
  (:remove :yaku renhou-mangan renhou-han)
  (:set :renhou :yakuman :daisharin t))

(defvariant sanma-tonpuu :extends sanma
  (:set :length :tonpuu :start-points 40000 :target-points 45000))

(defvariant dora-bomb :extends riichi
  (:set :initial-dora 3 :kan-dora :immediate))
```

## House rules

`house-rules` extends riichi with yaku that each need an option or a [context](pattern.md#win-context) field, so they stay off elsewhere. It also turns on [pao](yaku.md#pao) for the fourth quad (`suukantsu-pao` replaces `suukantsu`), for a concealed set held at the last call, and for a daiminkan rinshan win paid whole as a tsumo.

| Yaku | Value | Needs |
|---|---|---|
| `open-riichi`, `double-open-riichi` | 2 / 3 han, replacing riichi / double riichi | `open-riichi` context |
| `sashikomi` | Yakuman | Option `sashikomi`; a ron in open riichi |
| `paarenchan` | One yakuman per dealer win | `paarenchan` context; another yaku unless option `paarenchan-without-yaku` |
| `daisharin` | Yakuman | Option `daisharin`; seven pairs 2-8 of pin |
| `daisuurin`, `daichikurin` | Yakuman | Option `daisharin` set to `:all`; man / sou |
| `daichisei` | Yakuman | Option `daichisei`; seven pairs of honors |

The variant sets `sashikomi`, `daisharin :all`, `daichisei` and `pao-held`; its scoring adds `(:pao :rinshan tsumo)`. The [`aotenjou` scoring](scoring.md#aotenjou) extends that scoring and the `aotenjou` variant extends `house-rules` with it.

## Hong Kong

A tileset with [bonus tiles](tileset.md), a [fan table](scoring.md#fan-table) and game options with no riichi; see [hk.md](hk.md).

```lisp
(defscoring hk
  (:points (0 1) (1 2) (2 4) (3 8) (4 16))
  (:minimum 3)
  (:limits (yakuman 128 :max 1))
  (:payments (:ron :dealer 1 :other 1 :others 1/2) (:tsumo :dealer 1 :other (1 1))))

(defvariant hk-full-shooter :extends hk
  (:scoring hk-full-shooter))
```

## Pages

| Kind | Page |
|---|---|
| Tileset | [tileset.md](tileset.md) |
| Shape | [shape.md](shape.md) |
| Yaku | [yaku.md](yaku.md) |
| Scoring | [scoring.md](scoring.md) |
| Variant, game options | [variant.md](variant.md), [game.md](game.md) |
| Hong Kong | [hk.md](hk.md) |

[^sanma]: The missing north's share is not paid. Limitations: [PLAN.md](PLAN.md#limitations).
