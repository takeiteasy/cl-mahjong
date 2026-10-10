#!/usr/bin/env python3
"""Convert Tenhou mjlog XML files to the sexp replay format read by tests/replay.lisp.

    convert.py IN_DIR OUT_DIR

Logs are not included in this repository; download your own (see docs/replay.md).
Only hanchan with red fives and open tanyao are accepted, 4-player or sanma.
"""
import argparse
import gzip
import json
import re
import sys
from pathlib import Path

TAG = re.compile(r"<(\w+)((?:\s+\w+=\"[^\"]*\")*)\s*/?>")
ATTR = re.compile(r"(\w+)=\"([^\"]*)\"")
NO_AKA, NO_KUITAN, HANCHAN, SANMA = 0x02, 0x04, 0x08, 0x10

# Tenhou yaku ids to cl-mahjong names; dora ids are counted in han only.
YAKU = {
    0: "menzen-tsumo", 1: "riichi", 2: "ippatsu", 3: "chankan", 4: "rinshan", 5: "haitei",
    6: "houtei", 7: "pinfu", 8: "tanyao", 9: "iipeikou",
    10: "jikaze", 11: "jikaze", 12: "jikaze", 13: "jikaze",
    14: "bakaze", 15: "bakaze", 16: "bakaze", 17: "bakaze",
    18: "haku", 19: "hatsu", 20: "chun", 21: "double-riichi", 22: "chiitoitsu", 23: "chanta",
    24: "ittsu", 25: "sanshoku", 26: "sanshoku-doukou", 27: "sankantsu", 28: "toitoi",
    29: "sanankou", 30: "shousangen", 31: "honroutou", 32: "ryanpeikou", 33: "junchan",
    34: "honitsu", 35: "chinitsu", 36: "renhou", 37: "tenhou", 38: "chiihou", 39: "daisangen",
    40: "suuankou", 41: "suuankou", 42: "tsuuiisou", 43: "ryuuiisou", 44: "chinroutou",
    45: "chuuren", 46: "chuuren", 47: "kokushi", 48: "kokushi", 49: "daisuushii",
    50: "shousuushii", 51: "suukantsu",
}
DORA_IDS = {52, 53, 54}
LIMITS = {0: None, 1: "mangan", 2: "haneman", 3: "baiman", 4: "sanbaiman", 5: "yakuman"}
DRAW_LETTERS, DISCARD_LETTERS = "TUVW", "DEFG"
RYUUKYOKU = {"yao9": "kyuushu", "reach4": "suucha-riichi", "ron3": "sanchahou",
             "kan4": "suukaikan", "kaze4": "suufon-renda", "nm": "nagashi"}


class Sym(str):
    """Printed bare."""


def sx(value):
    if isinstance(value, Sym):
        return str(value)
    if value is True:
        return "t"
    if value is False or value is None:
        return "nil"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, dict):
        return "(" + " ".join(f":{key} {sx(item)}" for key, item in value.items()) + ")"
    if isinstance(value, (list, tuple)):
        return "(" + " ".join(sx(item) for item in value) + ")"
    raise TypeError(f"cannot print {value!r}")


def ints(text):
    return [int(part) for part in text.split(",")] if text else []


def decode_meld(m):
    """(type, tiles, called) for the N tag bitfield M; called is None for closed and added kans."""
    from_who = m & 3
    if m & 4:
        t = m >> 10
        index = t % 3
        t //= 3
        base = t // 7 * 9 + t % 7
        tiles = [(base + i) * 4 + ((m >> (3 + 2 * i)) & 3) for i in range(3)]
        return "chi", tiles, tiles[index]
    if m & 8:
        t = m >> 9
        index = t % 3
        kind = t // 3
        unused = (m >> 5) & 3
        tiles = [kind * 4 + k for k in range(4) if k != unused]
        return "pon", tiles, tiles[index]
    if m & 16:
        t = m >> 9
        kind = t // 3
        added = kind * 4 + ((m >> 5) & 3)
        return "shouminkan", [added], None
    if m & 32:
        return "kita", [m >> 8], None
    called = m >> 8
    kind = called // 4
    tiles = [kind * 4 + k for k in range(4)]
    return ("daiminkan", tiles, called) if from_who else ("ankan", tiles, None)


def attrs(text):
    return dict(ATTR.findall(text))


def check_rules(go):
    kind = int(go["type"])
    if kind & NO_AKA or kind & NO_KUITAN or not kind & HANCHAN:
        raise ValueError(f"unsupported game type {kind}; need a hanchan with aka and kuitan")
    return kind


def sanma_tile(tile):
    """The sanma-108 id of a Tenhou tile id: sanma has no man 2 to 8, which are tile ids 4 to 31."""
    return tile if tile < 4 else tile - 28


def identity(tile):
    return tile


def win(tag, seats, tile):
    yaku, han, yakuman = [], 0, False
    pairs = ints(tag.get("yaku", ""))
    for id_, count in zip(pairs[::2], pairs[1::2]):
        han += count
        if id_ not in DORA_IDS:
            yaku.append(YAKU[id_])
    if "yakuman" in tag:
        yakuman = True
        yaku = [YAKU[id_] for id_ in ints(tag["yakuman"])]
    fu, points, limit = ints(tag["ten"])
    sc = ints(tag["sc"])
    seat, source = int(tag["who"]), int(tag["fromWho"])
    return {
        "seat": seat, "from": None if seat == source else source,
        "fu": fu, "points": points, "limit": Sym(":" + LIMITS[limit]) if LIMITS[limit] else None,
        "han": None if yakuman else han, "yaku": [Sym(":" + name) for name in sorted(set(yaku))],
        "pao": int(tag["paoWho"]) if "paoWho" in tag and int(tag["paoWho"]) != seat else None,
        "deltas": [d * 100 for d in sc[1::2]][:seats],
        "ura": [tile(t) for t in ints(tag.get("doraHaiUra", ""))],
    }


def owari(tag, seats):
    parts = tag["owari"].split(",")
    return {"points": [int(p) * 100 for p in parts[0::2]][:seats],
            "scores": [round(float(s) * 1000) for s in parts[1::2]][:seats]}


ROUNDING_ENDED = "20220123"


def scoring_variant(name, scores, seats):
    """Tenhou rounded final scores to whole thousands until January 2022, then kept one decimal."""
    exact = any(score % 1000 for score in scores) or name[:8] >= ROUNDING_ENDED
    if seats == 3:
        if not exact:
            raise ValueError("sanma logs before 2022-01-23 are not supported")
        return Sym(":tenhou-sanma")
    return Sym(":tenhou" if exact else ":tenhou-2021")


def finish_deal(deal, tag, wins, kind, seats):
    if kind == "AGARI":
        deal["result"] = {"type": Sym(":tsumo" if wins[0]["from"] is None else ":ron"), "wins": wins}
        deal["ura"] = next((win["ura"] for win in wins if win["ura"]), [])
    else:
        reason = RYUUKYOKU.get(tag["type"]) if "type" in tag else None
        revealed = [seat for seat in range(seats) if f"hai{seat}" in tag]
        abortive = reason is not None and reason != "nagashi"
        deal["result"] = {"type": Sym(":abortive" if abortive else ":exhaustive"),
                          "reason": Sym(":" + reason) if reason else None,
                          "seats": revealed,
                          "tenpai": [] if abortive else revealed,
                          "deltas": [d * 100 for d in ints(tag["sc"])[1::2]][:seats]}


def convert(text, name):
    game = {"id": name, "deals": []}
    deal = None
    wins = []
    seats = 4
    tile = identity
    riichi = []
    rinshan_next = None
    tags = [(m.group(1), attrs(m.group(2))) for m in TAG.finditer(text)]
    for tag_name, tag in tags:
        if tag_name == "GO":
            game["type"] = check_rules(tag)
            if game["type"] & SANMA:
                seats, tile = 3, sanma_tile
        elif tag_name == "INIT":
            seed = ints(tag["seed"])
            deal = {"kyoku": seed[0] // 4 * seats + seed[0] % 4, "honba": seed[1], "sticks": seed[2],
                    "dealer": int(tag["oya"]), "points": [p * 100 for p in ints(tag["ten"])][:seats],
                    "dora": tile(seed[5]), "hands": [[tile(t) for t in ints(tag[f"hai{seat}"])]
                                                     for seat in range(seats)],
                    "draws": [], "rinshan": [], "dora-added": [], "ura": [], "events": [],
                    "result": None}
            game["deals"].append(deal)
            wins, riichi, rinshan_next = [], [False] * seats, None
        elif re.fullmatch(r"[TUVW]\d+", tag_name):
            seat = DRAW_LETTERS.index(tag_name[0])
            drawn = tile(int(tag_name[1:]))
            if rinshan_next == seat:
                deal["rinshan"].append(drawn)
                rinshan_next = None
            else:
                deal["draws"].append(drawn)
        elif re.fullmatch(r"[DEFG]\d+", tag_name):
            seat = DISCARD_LETTERS.index(tag_name[0])
            deal["events"].append([Sym(":discard"), seat, tile(int(tag_name[1:])), riichi[seat]])
            riichi[seat] = False
        elif tag_name == "REACH" and tag["step"] == "1":
            riichi[int(tag["who"])] = True
        elif tag_name == "N":
            seat = int(tag["who"])
            kind, tiles, called = decode_meld(int(tag["m"]))
            tiles = [tile(t) for t in tiles]
            if kind == "kita":
                deal["events"].append([Sym(":kita"), seat] + tiles)
            else:
                deal["events"].append([Sym(":call"), seat, Sym(":" + kind)] + tiles)
            if kind in ("daiminkan", "ankan", "shouminkan", "kita"):
                rinshan_next = seat
        elif tag_name == "DORA":
            deal["dora-added"].append(tile(int(tag["hai"])))
        elif tag_name == "AGARI":
            wins.append(win(tag, seats, tile))
            if "owari" in tag:
                game["owari"] = owari(tag, seats)
        elif tag_name == "RYUUKYOKU":
            finish_deal(deal, tag, [], "RYUUKYOKU", seats)
            if "owari" in tag:
                game["owari"] = owari(tag, seats)
        if tag_name == "AGARI":
            finish_deal(deal, tag, wins, "AGARI", seats)
    if "owari" not in game:
        raise ValueError("the log has no final result (owari)")
    game["variant"] = scoring_variant(name, game["owari"]["scores"], seats)
    return game


def read_log(path):
    """Log text; houou zips store gzip-compressed logs under a .mjlog name."""
    raw = path.read_bytes()
    if raw[:2] == b"\x1f\x8b":
        raw = gzip.decompress(raw)
    return raw.decode("utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("target", type=Path)
    args = parser.parse_args()
    args.target.mkdir(parents=True, exist_ok=True)
    failed = 0
    for path in sorted(args.source.glob("*")):
        if path.suffix not in (".mjlog", ".xml"):
            continue
        try:
            game = convert(read_log(path), path.stem)
        except (ValueError, KeyError) as error:
            print(f"{path.name}: {error}", file=sys.stderr)
            failed += 1
            continue
        (args.target / f"{path.stem}.sexp").write_text(sx(game) + "\n", encoding="utf-8")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
