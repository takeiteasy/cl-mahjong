# Log replay

A replay plays a recorded Tenhou hanchan through the [game flow](game.md) on the `tenhou` [variant](variant.md) and reports the first place the engine disagrees with the log.

```
python3 tools/tenhou-replay/convert.py logs/ converted/
CL_MAHJONG_REPLAY_DIR=converted/ sbcl --noinform --non-interactive --eval '(asdf:test-system "cl-mahjong")'
```

No Tenhou logs are in this repository. Download your own from [tenhou.net/sc/raw](https://tenhou.net/sc/raw/) and read the usage terms there.[^terms] Without `CL_MAHJONG_REPLAY_DIR` the test is skipped.

## What is checked

| When | Check |
|---|---|
| Each deal start | Kyoku, honba, sticks, dealer, points, dora indicator, every seat's tiles |
| Each action | The logged action is legal in the engine |
| Each win | Winners, discarder, han, fu (below mangan), limit, yaku, liable seat |
| Each draw | Abortive reason, tenpai seats, nagashi mangan |
| Each deal end | Point changes of every seat |
| Game end | Final points; final scores within 1000 |

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

## Tenhou variant

`tenhou` extends `riichi` with no double yakuman and Tenhou's uma (20 10 -10 -20). Every other riichi default matches the ranked rules.[^rules]

## Limitations

See [PLAN.md](PLAN.md#limitations): sanma replay is blocked by missing nukidora ([#22](https://todo.sr.ht/~takeiteasy/cl-mahjong/22)) and the north share ([#23](https://todo.sr.ht/~takeiteasy/cl-mahjong/23)), and legal actions are recomputed on every move ([#19](https://todo.sr.ht/~takeiteasy/cl-mahjong/19)).

[^terms]: Tenhou publishes logs for fair play on Tenhou and asks that general mahjong software contact support@c-egg.com first, so converted logs are not committed.
[^ids]: After a call the engine keeps one tile per kind and red flag, so its tile ids drift from the log's.
[^rules]: Taken from [tenhou.net/man](https://tenhou.net/man/): composite yakuman stack, kazoe yakuman at 13 han, no kiriage, no renhou, no kuikae, triple ron aborts, double ron allowed. Whether nagashi pays honba and the oka are not stated there; the replay checks them.
