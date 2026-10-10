# Log replay

A replay plays a recorded Tenhou hanchan through the [game flow](game.md) on the `tenhou` [variant](variant.md) and reports the first place the engine disagrees with the log.

```
python3 tools/tenhou-replay/fetch_logs.py --mode 4p
python3 tools/tenhou-replay/convert.py tools/tenhou-replay/logs/ tools/tenhou-replay/converted/
CL_MAHJONG_REPLAY_DIR=tools/tenhou-replay/converted/ sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

No Tenhou logs are in this repository; zips, logs and converted output are git-ignored. `fetch_logs.py` scrapes the DL zips linked from [tenhou.net/ranking.html](https://tenhou.net/ranking.html) and unzips the `.mjlog` files for the converter. It fetches the newest 4-player zip into `tools/tenhou-replay/logs/`; `--mode 4p|3p|both` (default 4p), `--count N`, `--list-only` and `--max-logs N` adjust the selection. Sanma logs (`--mode 3p` or `both`) are fetched but `convert.py` reports and skips them. Older logs are also at [tenhou.net/sc/raw](https://tenhou.net/sc/raw/). Read the usage terms there.[^terms] Without `CL_MAHJONG_REPLAY_DIR` the test is skipped; `CL_MAHJONG_REPLAY_LIMIT=N` replays N evenly spaced logs, and `CL_MAHJONG_REPLAY_THREADS=N` sets the worker threads (default 4).

## What is checked

| When | Check |
|---|---|
| Each deal start | Kyoku, honba, sticks, dealer, points, dora indicator, every seat's tiles |
| Each action | The logged action is legal in the engine |
| Each win | Winners, discarder, han, fu (below mangan), limit, yaku, liable seat |
| Each draw | Abortive reason, tenpai seats, nagashi mangan |
| Each deal end | Point changes of every seat |
| Game end | Final points and final scores, exactly |

A failure names the log, deal, and seat, with the legal actions when an action is refused.

## Converting logs

`convert.py` reads `.mjlog` or `.xml` files and writes one `.sexp` per game. It accepts only 4-player hanchan with red fives and open tanyao; any other log is reported and skipped.

## How a log is replayed

| Step | Behavior |
|---|---|
| Wall | Each deal's hands, draws, rinshan draws, dora and ura tiles are stacked into a wall; Tenhou tile ids equal `riichi-136` tile ids |
| Actions | A logged action is matched to a legal action by tile kind and red flag, not by tile id[^ids] |
| Passes | Tenhou does not log them; every seat that is not the logged caller or winner passes |
| Whole game | One `start-game` plays every deal, so dealer rotation, honba, sticks and game end are exercised |

## Self-play check

The suite also records seeded self-play games in the same format and replays them. This tests the replay machinery with calls, kans, riichi and wins, but not the rules, because the engine is both the source and the subject.

## Tenhou variants

| Variant | Differs from `riichi` |
|---|---|
| `tenhou` | No double yakuman, uma 20 10 -10 -20, tenpai-yame, open kan dora flips as the discard is made[^rules] |
| `tenhou-2021` | `tenhou` with final scores rounded to thousands[^rounding]; used for logs before 2022-01-23 |

`convert.py` tags each log with its variant.

## Results

11,460 of 11,463 houou logs (2011 to 2026) replay exactly. The other three are in the limitations.

## Limitations

Three logs diverge where a player holds all four of a wait's tiles; Tenhou counts that hand noten and the engine counts it tenpai ([#27](https://todo.sr.ht/~takeiteasy/cl-mahjong/27)).

See [PLAN.md](PLAN.md#limitations): sanma replay is blocked by missing nukidora ([#22](https://todo.sr.ht/~takeiteasy/cl-mahjong/22)) and the north share ([#23](https://todo.sr.ht/~takeiteasy/cl-mahjong/23)), and legal actions are recomputed on every move ([#19](https://todo.sr.ht/~takeiteasy/cl-mahjong/19)).

[^terms]: Tenhou publishes logs for fair play on Tenhou and asks that general mahjong software contact support@c-egg.com first, so converted logs are not committed.
[^ids]: After a call the engine keeps one tile per kind and red flag, so its tile ids drift from the log's.
[^rules]: Taken from [tenhou.net/man](https://tenhou.net/man/): composite yakuman stack, kazoe yakuman at 13 han, no kiriage, no renhou, no kuikae, triple ron aborts, double ron allowed. The replay established the rest: tenpai-yame, a game that continues in the west round while a trailing dealer repeats, and dora flipping as an open kan's discard is made.
[^rounding]: Each seat's points round to thousands with a half going away from zero, then the target is subtracted and uma added; first place takes the negated sum of the others, which absorbs the oka. Logs from 2022-01-23 keep one decimal and are exact.
