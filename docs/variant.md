# Variant

A variant bundles the definitions a game plays with. `defvariant` registers it by name; `find-variant` returns the compiled value.

```lisp
(defvariant riichi
  (:players 4)
  (:tileset riichi-136)
  (:shapes standard seven-pairs kokushi)
  (:yaku riichi double-riichi ippatsu ...)
  (:scoring riichi)
  (:options :kuitan t :kiriage nil))
```

## Clauses

| Clause | Content |
|---|---|
| `(:players n)` | Seats, at least 2 |
| `(:tileset name)` | A [tileset](tileset.md) |
| `(:shapes name...)` | [Shapes](shape.md) a hand may complete |
| `(:yaku name...)` | [Yaku](yaku.md) that score |
| `(:scoring name)` | A [scoring](scoring.md) |
| `(:options key value...)` | Options read by yaku patterns and by the [game](game.md) |

`:extends` inherits a variant. A child clause replaces the parent's clause of the same name, and `:options` is replaced whole. Deltas edit the inherited lists instead.

```lisp
(defvariant festival :extends riichi
  (:add :yaku daisharin)
  (:remove :yaku renhou-mangan)
  (:set :renhou :yakuman :kuitan nil))
```

## Option validation

`build-variant` checks every option when the variant is built.

| Check | Rejected |
|---|---|
| Value | A value of the wrong type or outside the allowed set, such as `:kuikae :forbd` or `:multi-ron :dobule` |
| Key | A key that is not a [game option](game.md#options), a scoring option, or read by one of the variant's [yaku](yaku.md)[^lambda] |

A child variant keeps the keys its parent accepted, so removing a yaku does not reject the options it read.

```lisp
(defvariant typo :extends riichi
  (:set :kuittan nil))   ; invalid: unknown option :kuittan
```

## Deltas

| Delta | Targets | Effect |
|---|---|---|
| `(:set key value ...)` | options | Replace or add options |
| `(:add target name...)` | `:yaku`, `:shapes` | Append names |
| `(:remove target name...)` | `:yaku`, `:shapes`, `:options` | Drop names; a removed option falls back to the [game default](game.md#options) |

Deltas apply in order to the parent's resolved lists. Removing a name that is not there, or adding one that is, is an error.[^eager]

[^eager]: A delta never carries to grandchildren; each level sees the already-edited lists, so a child that replaces `(:yaku ...)` starts fresh.

Every named definition must exist when the variant is looked up.

| Reader | Value |
|---|---|
| `variant-name`, `variant-players` | Keyword, seat count |
| `variant-tileset` | The compiled tileset |
| `variant-shapes`, `variant-yaku`, `variant-scoring` | The names |
| `variant-options` | The options plist |
| `(variant-option variant key)` | An option, falling back to the [game defaults](game.md#options) |

[^lambda]: A key is known to a yaku when one of its `(option key)` patterns names it. A `(:lambda ...)` yakuman that reads options itself does not declare any.
