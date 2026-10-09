# DSL examples

Every definition takes `:extends`, so a ruleset is usually a few lines on top of another. Each kind has its own page; this one shows them together.

## Sanma

A [tileset](tileset.md) delta, then a [variant](variant.md) delta on riichi.

```lisp
(deftileset sanma-108 :extends riichi-136
  (:remove (man 2 8)))

(defvariant sanma :extends riichi
  (:players 3) (:tileset sanma-108)
  (:set :chi nil :winds (:east :south :west)
        :start-points 35000 :target-points 40000 :uma (15 0 -15)
        :noten-payment 2000 :suufon-renda nil :suucha-riichi nil))
```

A tsumo is paid by the two other seats at the normal multipliers.[^sanma]

## Event rulesets

| Variant | Changes |
|---|---|
| `akadora-frenzy` | Two red fives per suit |
| `yaku-festival` | Adds the daisharin yakuman, renhou is a yakuman |
| `sanma-tonpuu` | Sanma played east only; three levels: riichi, sanma, event |
| `dora-bomb` | Three starting dora indicators, immediate kan dora |

```lisp
(deftileset riichi-136-akadora :extends riichi-136
  (:red (man 5 2) (pin 5 2) (sou 5 2)))

(defyaku daisharin (:yakuman 1 :closed-only t)
  (:when (shape seven-pairs)
         (every-tile (pin-2 pin-3 pin-4 pin-5 pin-6 pin-7 pin-8))))

(defvariant yaku-festival :extends riichi
  (:add :yaku daisharin)
  (:remove :yaku renhou-mangan)
  (:set :renhou :yakuman))

(defvariant sanma-tonpuu :extends sanma
  (:set :length :tonpuu :start-points 40000 :target-points 45000))

(defvariant dora-bomb :extends riichi
  (:set :initial-dora 3 :kan-dora :immediate))
```

## Pages

| Kind | Page |
|---|---|
| Tileset | [tileset.md](tileset.md) |
| Shape | [shape.md](shape.md) |
| Yaku | [yaku.md](yaku.md) |
| Scoring | [scoring.md](scoring.md) |
| Variant, game options | [variant.md](variant.md), [game.md](game.md) |

[^sanma]: The missing north's share is not paid. Limitations: [PLAN.md](PLAN.md#limitations).
