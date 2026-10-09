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

`:extends` inherits a variant. A child clause replaces the parent's clause of the same name, and `:options` is replaced whole.

```lisp
(defvariant three-seats :extends riichi
  (:players 3) (:tileset sanma-108))
```

Every named definition must exist when the variant is looked up.

| Reader | Value |
|---|---|
| `variant-name`, `variant-players` | Keyword, seat count |
| `variant-tileset` | The compiled tileset |
| `variant-shapes`, `variant-yaku`, `variant-scoring` | The names |
| `variant-options` | The options plist |
| `(variant-option variant key)` | An option, falling back to the [game defaults](game.md#options) |
