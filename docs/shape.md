# Shapes

`defshape` declares a way a hand can be complete. A variant lists the shapes it accepts; analysis ([analysis.md](analysis.md)) works over exactly those. A shape can extend another.

```lisp
(defshape standard
  (:sets 4 :of (sequence triplet))
  (:pair 1))

(defshape seven-pairs
  (:pairs 7 :distinct t))

(defshape kokushi
  (:each terminal honor)
  (:extra 1 :of (terminal honor)))

(find-shape 'standard)   ; compiled on first use, then cached
```

## Families

A shape uses the clauses of exactly one family.

| Family | Clause | Meaning |
|---|---|---|
| Sets | `(:sets n :of (type...))` | `n` sets; `type` is `sequence` or `triplet` (default both) |
| | `(:pair n)` | `n` pairs (default `0`) |
| Pairs | `(:pairs n :distinct t)` | `n` pairs; with `:distinct` a quad is not two pairs |
| Each | `(:each class...)` | One of every kind in the classes |
| | `(:extra n :of (class...))` | `n` more tiles, each a copy of a kind in these classes (default: the `:each` classes) |
| Lambda | `(:lambda :complete f :shanten g)` | Escape hatch; see below |

Tile classes:

| Class | Kinds |
|---|---|
| `terminal` | First and last rank of every suit |
| `honor` | Every honor kind |
| `simple` | Suit kinds that are not terminals |

Classes resolve against the tileset when a hand is analysed, so `kokushi` covers the terminals and honors of whatever tileset the variant uses.

## Melds

| Family | Melds |
|---|---|
| Sets | Each meld is one of the shape's sets, so a hand with `k` melds needs `n - k` more. A meld whose type the shape does not allow rules the shape out |
| Pairs, each | Any meld rules the shape out |
| Lambda | Passed to the functions |

A quad counts as a triplet. A shape that a meld rules out has no decompositions and no shanten.

## Lambda shapes

`:complete` is required; `:shanten` is optional. Both take `(tileset counts melds)`, where `counts` is a kind-count vector ([analysis.md](analysis.md#hands)). Each is a function, the name of one, or a form evaluated without a lexical environment.

```lisp
(defshape all-man
  (:lambda :complete (lambda (tileset counts melds)
                       (declare (ignore melds))
                       (loop for count across counts
                             for id from 0
                             always (or (zerop count)
                                        (eq :man (kind-suit (tileset-kind tileset id))))))))
```

| Without `:shanten` | Result |
|---|---|
| Complete hand | `-1` |
| One tile from complete (after the best discard for 3n+2 hands) | `0` |
| Anything else | `NIL`, unknown |

## Inheritance

A child clause replaces the parent's clause with the same key. A child cannot move a shape to another family.

```lisp
(defshape sanma-standard :extends standard
  (:sets 4 :of (triplet)))
```

Redefining a shape recompiles every shape that extends it. Errors in a definition surface on first lookup as `invalid-definition`.

## API

| Function | Result |
|---|---|
| `defshape name [:extends parent] clause...` | Registers a shape |
| `find-shape name` | Compiled shape |
| `shape-name shape`, `shape-family shape` | Name keyword; `:sets`, `:pairs`, `:each` or `:lambda` |
