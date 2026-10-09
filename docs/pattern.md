# Patterns

A pattern is a condition on a winning hand. `defyaku` ([yaku.md](yaku.md)) lists patterns under `:when`; all must hold. Patterns compile to closures when the yaku is looked up, and unknown or malformed patterns signal `invalid-definition` then.

```lisp
(:when (shape standard)
       (every-group (sequence pair))
       (not (pair value))
       (wait ryanmen))
```

## Patterns

| Pattern | True when |
|---|---|
| `t` | Always |
| `(and p...)`, `(or p...)`, `(not p)` | Combinators |
| `(shape name...)` | The decomposition is one of these shapes |
| `(closed)` | No meld is open |
| `(context flag)` | A [win context](#win-context) flag is set |
| `(option key)` | The evaluation option is true |
| `(every-tile sel)`, `(some-tile sel)` | Every / some tile of the hand and melds matches |
| `(one-suit)` | The suited tiles belong to one suit |
| `(every-group types [sel] [:concealed t])` | Every group matches (the hand has groups) |
| `(some-group types [sel] [:concealed t])` | At least one group matches |
| `(count-groups n types [sel] [:concealed t])` | At least `n` groups match |
| `(pair sel)` | A pair matches `sel` |
| `(wait type...)` | The winning tile completed one of these [waits](#waits) |
| `(across-suits types)` | Groups of `types` start at the same rank in every suit |
| `(full-straight)` | One suit has sequences starting at the lowest rank and three and six above it |
| `(twin-sequences n)` | At least `n` pairs of identical concealed sequences |
| `(:lambda (hand) body...)` | The body is true; `hand` is the [win hand](#win-hand) |

A group matches when its type is in `types`, it holds a tile matching `sel` (if given), and it is concealed (with `:concealed t`).

| `types` | Meaning |
|---|---|
| `sequence`, `triplet`, `quad`, `pair`, `single` | Exactly that type, alone or in a list |
| `any` | Every type |

Patterns over groups are false for [lambda shapes](shape.md#lambda-shapes), which have none.

## Selectors

A selector picks tile kinds. A list of selectors means any of them.

| Selector | Kinds |
|---|---|
| `simple`, `terminal`, `honor`, `suit` | The tile classes of [shapes](shape.md#families), plus all suited kinds |
| `wind`, `dragon` | Honors of that group |
| `seat-wind`, `round-wind` | The wind in the [win context](#win-context) |
| `value` | Dragons, the seat wind and the round wind |
| `man`, `sou-2`, `white`... | A suit name, or a kind name |

```lisp
(every-tile (sou-2 sou-3 sou-4 sou-6 sou-8 green))
```

## Win context

The caller describes how the hand was won with `make-win-context`.

| Field | Meaning |
|---|---|
| `win-tile` | Tile id of the winning tile |
| `seat-wind`, `round-wind` | Honor kind name such as `:east` |
| `tsumo` | Won by self-draw |
| `riichi`, `double-riichi`, `ippatsu` | Riichi state |
| `last-tile` | Won on the last wall tile (haitei when `tsumo`, houtei otherwise) |
| `rinshan`, `chankan` | Won on a replacement draw / by robbing a kan |
| `tenhou`, `chiihou`, `renhou` | First-turn wins |

`(context flag)` reads the boolean fields.

## Win hand

A win hand is one decomposition with the winning tile placed in one group. A lambda pattern and `evaluate-yaku` ([yaku.md](yaku.md)) work with these.

| Reader | Value |
|---|---|
| `win-hand-tileset`, `win-hand-counts`, `win-hand-melds` | The analysed hand; counts include the winning tile |
| `win-hand-pre-win-counts` | Counts without the winning tile |
| `win-hand-decomposition`, `win-hand-win-kind` | Decomposition and kind id of the winning tile |
| `win-hand-win-group` | Group holding the winning tile; `NIL` outside the sets family |
| `win-hand-wait` | The [wait](#waits) |
| `win-hand-closed-p` | No open meld |
| `win-hand-context`, `win-hand-options` | Context and options |
| `(group-concealed-p hand group)` | See below |

A group is concealed unless it is an open meld or a triplet completed by ron. The hand stays closed after a ron.

## Waits

The winning tile sits in each concealed group that holds it, one win hand per distinct group.

| Shape family | Wait |
|---|---|
| Sets | `tanki` (pair), `shanpon` (triplet), `kanchan` (middle of a sequence), `penchan` (edge against the lowest or highest rank), otherwise `ryanmen` |
| Pairs | `tanki` |
| Each | `thirteen-sided` when the hand before the winning tile held one of every required kind, otherwise `tanki` |
| Lambda | `NIL` |

Penchan uses the tileset's rank bounds, so it follows `:ranks`.
