# WRC

`wrc` plays the World Riichi Championship rules: [riichi](yaku.md#riichi) without red fives, abortive draws, tobi or agari-yame.

```lisp
(start-game 'wrc :seed 42)
```

## Differences from riichi

| Rule | Value |
|---|---|
| Red fives | None (`wrc-136`) |
| Points | 30000 start, 30000 target, no oka |
| Uma | 15 / 5 / -5 / -15 thousand |
| Abortive draws | None; play goes on after four kans, four riichi and four winds |
| Multiple ron | Head bump: only the nearest winner is paid |
| Game end | After south; no west extension, no tobi, no agari-yame |
| Nagashi mangan | Off |
| Kazoe | 13 han without a yakuman stay a sanbaiman |
| Yakuman | Different yakuman stack without limit; no double yakuman |
| Pao | Daisangen, daisuushii and suukantsu (`suukantsu-pao`) |

Everything else is the [riichi](game.md#options) default.[^unchecked]

[^unchecked]: Kiriage, double-wind pair fu, open pinfu fu, kan dora timing, riichi with fewer than 1000 points and leftover sticks at game end follow the riichi defaults and are not checked against the rulebook.
