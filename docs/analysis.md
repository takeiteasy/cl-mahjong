# Hand analysis

Analysis tells whether a hand is complete, how it splits into groups, how far it is from complete (shanten) and which draws help (ukeire). It works over the [shapes](shape.md) you pass in, usually a variant's.

```lisp
(let* ((ts (find-tileset 'riichi-136))
       (shapes '(standard seven-pairs kokushi))
       (counts (hand-counts ts tiles)))
  (shanten ts shapes counts '())      ; => 0, :STANDARD
  (ukeire ts shapes counts '())       ; => ((#<KIND PIN-3> . 4))
  (decompose ts shapes counts '()))   ; => () while the hand is incomplete
```

## Hands

| Input | Form |
|---|---|
| `counts` | Vector with one count per kind id: `(hand-counts tileset tiles)` builds one from tile ids |
| `melds` | List of `(make-meld type kinds :open bool)`; type is `:sequence`, `:triplet` or `:quad` |

A hand has 3n+1 or 3n+2 tiles, counting each meld as 3, and holds no more copies of a kind than the tileset has, melds included; anything else signals `invalid-hand`. Bonus tiles do not belong in a hand.

`(hand-closed-p melds)` is true when no meld is open. A closed kan keeps a hand closed.

```lisp
(hand-closed-p (list (make-meld :quad '(27 27 27 27))))             ; => T
(hand-closed-p (list (make-meld :triplet '(27 27 27) :open t)))     ; => NIL
```

## Functions

| Function | Result |
|---|---|
| `(decompose ts shapes counts melds)` | List of decompositions, across all shapes |
| `(complete-p ts shapes counts melds)` | True when any shape completes |
| `(shanten ts shapes counts melds &key live visible)` | `(values n shape-name)` for the closest shape; `NIL` when no shape applies |
| `(ukeire ts shapes counts melds &key live visible)` | `(kind . remaining)` for each kind that lowers the shanten |

`shapes` holds shapes or their names.

## Shanten

| Value | Meaning |
|---|---|
| `-1` | Complete |
| `0` | One tile from complete (tenpai) |
| `n` | `n` tiles beyond tenpai |

3n+1 and 3n+2 hands both work; for 3n+2 the value is the shanten before discarding.

### Live shanten

By default shanten ignores how many copies of a tile are left. `:live t` counts only kinds with copies left in the hand's tileset, so a wait on a tile held in full does not count. `:visible`, a kind-count vector of other tiles seen, also turns it on and uses those copies up.[^live]

```lisp
(shanten ts shapes counts '())            ; 123456789p + EEEE => 0
(shanten ts shapes counts '() :live t)    ; => 1
```

In sanma, pass the nuki in `visible`. [`ukeire`](#ukeire) takes the same keys.

## Ukeire

`ukeire` takes a 3n+1 hand. `remaining` is the kind's copies minus those in the hand, its melds and `visible`, a kind-count vector of other tiles seen (discards, dora indicators, other players' melds). With `:live` or `:visible` a draw counts only when it lowers the [live shanten](#live-shanten), so a draw that completes only a dead partial is left out.

```lisp
(ukeire ts shapes counts '() :visible (hand-counts ts discards))
```

## Decompositions

A decomposition is the shape's name and its groups, melds first, then the rest in kind order.

| Reader | Value |
|---|---|
| `decomposition-shape` | Shape name keyword |
| `decomposition-groups` | List of groups |
| `group-type` | `:sequence`, `:triplet`, `:quad`, `:pair`, `:single` |
| `group-kinds` | Kind id of every tile in the group |
| `group-open-p` | True for an open meld |

`111222333m` with a pair splits as three sequences and as three triplets, so a hand can have several decompositions; a hand that fits two shapes (seven pairs and standard) has one per shape.

## Rules

- Sequences follow suit and rank, never kind ids, so sanma's man 1 and man 9 do not connect.
- Honors never form sequences.
- Each split is listed once.
- A decomposition does not place the winning tile; [pattern.md](pattern.md#waits) expands it per placement.

## Limitations

See [PLAN.md](PLAN.md#limitations): shanten for sets shapes uses a generic search with no fast path.

[^live]: Live mode counts a partial group only when it can take a copy of one of its waits, and adds one for each group with no lone tile that can take a copy to start from. Each copy goes to one group, so two partials waiting on the last copy of a kind count once. A tenpai with no live wait is 1-shanten, and a kokushi missing a kind with no copies left, or seven pairs without enough kinds to pair, has no shanten (`NIL`).
