#!/usr/bin/env python3
"""Regenerate tests/corpus/riichi.sexp from the MIT-licensed Python `mahjong` library.

Clones the library at a pinned commit, runs its own test suite with recorder.py capturing every
call, and converts the calls to s-expressions. Needs git and python3; see docs/corpus.md.
"""
import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path

UPSTREAM = "https://github.com/MahjongRepository/mahjong.git"
PINNED = "51675182ae0c040a4cb143eb1e109dc06e0a78b2"
HERE = Path(__file__).resolve().parent
DEFAULT_OUT = HERE.parent.parent / "tests" / "corpus" / "riichi.sexp"

AKA = {16, 52, 88}
WINDS = {27: "east", 28: "south", 29: "west", 30: "north"}
MELD_TYPES = {"chi": "sequence", "pon": "triplet", "kan": "quad", "shouminkan": "quad"}
KAZOE_LIMITED, KAZOE_SANBAIMAN, KAZOE_NO_LIMIT = 0, 1, 2
KAZOE = {KAZOE_LIMITED: "limited", KAZOE_SANBAIMAN: "sanbaiman", KAZOE_NO_LIMIT: "multiple"}

YAKU = {
    "Menzen Tsumo": "menzen-tsumo", "Riichi": "riichi", "Ippatsu": "ippatsu", "Chankan": "chankan",
    "Rinshan Kaihou": "rinshan", "Haitei Raoyue": "haitei", "Houtei Raoyui": "houtei",
    "Double Riichi": "double-riichi", "Renhou": "renhou-han", "Pinfu": "pinfu",
    "Tanyao": "tanyao", "Iipeiko": "iipeikou", "Yakuhai (haku)": "haku",
    "Yakuhai (hatsu)": "hatsu", "Yakuhai (chun)": "chun", "Sanshoku Doujun": "sanshoku",
    "Ittsu": "ittsu", "Chantai": "chanta", "Chiitoitsu": "chiitoitsu", "Toitoi": "toitoi",
    "San Ankou": "sanankou", "San Kantsu": "sankantsu", "Sanshoku Doukou": "sanshoku-doukou",
    "Honroutou": "honroutou", "Shou Sangen": "shousangen", "Honitsu": "honitsu",
    "Junchan": "junchan", "Ryanpeikou": "ryanpeikou", "Chinitsu": "chinitsu",
    "Kokushi Musou": "kokushi", "Kokushi Musou Juusanmen Matchi": "kokushi",
    "Chuuren Poutou": "chuuren", "Daburu Chuuren Poutou": "chuuren", "Daisangen": "daisangen",
    "Shousuushii": "shousuushii", "Dai Suushii": "daisuushii", "Tsuu Iisou": "tsuuiisou",
    "Chinroutou": "chinroutou", "Ryuuiisou": "ryuuiisou", "Suu Kantsu": "suukantsu",
    "Suu Ankou": "suuankou", "Suu Ankou Tanki": "suuankou", "Tenhou": "tenhou",
    "Chiihou": "chiihou", "Renhou (yakuman)": "renhou",
    "Open Riichi": "open-riichi", "Double Open Riichi": "double-open-riichi",
    "Sashikomi": "sashikomi", "Daisharin": "daisharin", "Daisuurin": "daisuurin",
    "Daichikurin": "daichikurin", "Daichisei": "daichisei",
}
HOUSE_YAKU = {"open-riichi", "double-open-riichi", "sashikomi", "paarenchan", "daisharin",
              "daisuurin", "daichikurin", "daichisei"}
YAKU_PATTERNS = [(re.compile(r"Yakuhai \(seat wind \w+\)"), "jikaze"),
                 (re.compile(r"Yakuhai \(round wind \w+\)"), "bakaze"),
                 (re.compile(r"Paarenchan( \d+)?"), "paarenchan")]
BONUS = {"Dora", "Aka Dora", "Ura Dora"}
NO_SCORE_ERRORS = {"no_yaku", "hand_not_winning"}
LIMITS = {"mangan": "mangan", "kiriage mangan": "mangan", "haneman": "haneman", "baiman": "baiman",
          "sanbaiman": "sanbaiman", "kazoe sanbaiman": "sanbaiman"}


class Skip(Exception):
    pass


class Sym(str):
    """Printed bare, as a keyword when it starts with a colon."""


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


def kw(name):
    return Sym(":" + name)


def tile_notation(ids, aka=False):
    """34-or-136 style ids already reduced to (kind, red) pairs -> '123m456p0s7z'."""
    out = []
    for letter, low, high in (("m", 0, 9), ("p", 9, 18), ("s", 18, 27), ("z", 27, 34)):
        digits = [("0" if red else str(kind - low + 1))
                  for kind, red in sorted(ids) if low <= kind < high
                  for red in [red and aka]]
        if digits:
            out.append("".join(digits) + letter)
    return "".join(out)


def tiles136(ids, aka):
    return tile_notation([(i // 4, i in AKA) for i in ids], aka)


def tiles34(counts):
    return tile_notation([(kind, False) for kind, n in enumerate(counts) for _ in range(n)])


def without_melds(tiles, melds):
    remaining = list(tiles)
    for meld in melds:
        for tile in meld["tiles"]:
            if tile in remaining:
                remaining.remove(tile)
                continue
            same = sorted((t for t in remaining if t // 4 == tile // 4), key=lambda t: t in AKA)
            if not same:
                raise Skip("meld tiles are not in the hand")
            remaining.remove(same[0])
    return remaining


def meld_entries(melds, aka):
    out = []
    for meld in melds or []:
        if meld["type"] not in MELD_TYPES:
            raise Skip(f"unsupported meld {meld['type']}")
        out.append([kw(MELD_TYPES[meld["type"]]), tiles136(meld["tiles"], aka), kw("open"),
                    bool(meld["opened"])])
    return out


def our_yaku(names):
    out = []
    for name in names:
        if name in BONUS:
            continue
        mapped = YAKU.get(name) or next((v for pattern, v in YAKU_PATTERNS if pattern.fullmatch(name)),
                                        None)
        if mapped is None:
            raise Skip(f"unknown yaku {name}")
        if mapped not in out:
            out.append(mapped)
    return sorted(out)


def our_options(options):
    daisharin = kw("all") if options["has_daisharin_other_suits"] else options["has_daisharin"]
    return {"kuitan": options["has_open_tanyao"], "double-yakuman": options["has_double_yakuman"],
            "renhou": kw("yakuman" if options["renhou_as_yakuman"] else "han"),
            "kiriage": options["kiriage"], "double-wind-fu": 4, "rinshan-tsumo-fu": True,
            "open-pinfu-fu": options["fu_for_open_pinfu"],
            "pinfu-tsumo-fu": options["fu_for_pinfu_tsumo"],
            "sashikomi": options["has_sashikomi_yakuman"], "daisharin": daisharin,
            "daichisei": options["has_daichisei"],
            "paarenchan-without-yaku": not options["paarenchan_needs_yaku"]}


def house_rules(config, names=()):
    options = config["options"]
    return bool(config["is_open_riichi"] or config["paarenchan"] or options["has_daisharin"]
                or options["has_daichisei"] or options["has_sashikomi_yakuman"]
                or HOUSE_YAKU.intersection(names))


def rules(config, house=False, aotenjou=False):
    """The variant a case plays and, for riichi scoring, how its kazoe and yakuman cap are set."""
    options = config["options"]
    if aotenjou:
        return {"variant": kw("aotenjou")}
    if options["is_three_player"]:
        if house:
            raise Skip("house rules in sanma are not covered")
        name = "sanma"
    else:
        name = "house-rules" if house else "riichi"
    return {"variant": kw(name), "kazoe": kw(KAZOE[options["kazoe_limit"]]),
            "cap": 6 if options["limit_to_sextuple_yakuman"] else None}


def our_limit(level):
    if level.endswith("yakuman"):
        return kw("yakuman")
    if level in LIMITS:
        return kw(LIMITS[level])
    return None


def our_payments(cost, config):
    main, extra = cost["main"] + cost["main_bonus"], cost["additional"] + cost["additional_bonus"]
    if not config["is_tsumo"]:
        return [kw("ron"), main]
    if config["is_dealer"]:
        return [kw("dealer"), None, kw("other"), main]
    return [kw("dealer"), main, kw("other"), extra]


def expectation(cost, config):
    return {"points": cost["total"], "payments": our_payments(cost, config),
            "limit": our_limit(cost["yaku_level"])}


def context_args(config):
    flags = {"tsumo": "is_tsumo", "riichi": "is_riichi", "double-riichi": "is_daburu_riichi",
             "ippatsu": "is_ippatsu", "rinshan": "is_rinshan", "chankan": "is_chankan",
             "tenhou": "is_tenhou", "chiihou": "is_chiihou", "renhou": "is_renhou"}
    out = {key: True for key, attr in flags.items() if config[attr]}
    if config["is_haitei"] or config["is_houtei"]:
        out["last-tile"] = True
    out["seat-wind"] = kw(WINDS[config["player_wind"]]) if config["player_wind"] else None
    out["round-wind"] = kw(WINDS[config["round_wind"]]) if config["round_wind"] else None
    out["dealer"] = config["is_dealer"]
    if config["is_open_riichi"]:
        out["open-riichi"] = True
    if config["paarenchan"]:
        out["paarenchan"] = config["paarenchan"]
    if config["num_nuki_dora"] < 0:
        raise Skip("impossible contexts are not validated")
    if config["num_nuki_dora"]:
        out["nuki"] = config["num_nuki_dora"]
    return out


def nagashi_entry(record):
    inp, result = record["input"], record["result"]
    config = inp["config"]
    return {"kind": kw("scores"), "han": result["han"], "fu": result["fu"], "yakuman": False,
            "context": {"tsumo": config["is_tsumo"], "dealer": config["is_dealer"]},
            "options": our_options(config["options"]), **rules(config),
            "honba": config["tsumi_number"], "sticks": config["kyoutaku_number"],
            "players": 3 if config["options"]["is_three_player"] else 4,
            "expect": expectation(result["cost"], config)}


def hand_entry(record):
    inp, result = record["input"], record["result"]
    config, melds = inp["config"], inp.get("melds") or []
    aotenjou = inp["factory"] == "Aotenjou"
    if config["is_nagashi_mangan"]:
        return nagashi_entry(record)
    aka = config["options"]["has_aka_dora"]
    if result["error"] and result["error"] not in NO_SCORE_ERRORS:
        raise Skip("impossible contexts are not validated")
    names = [] if result["error"] else our_yaku(result["yaku"])
    entry = {"kind": kw("hand"),
             "hand": tiles136(without_melds(inp["tiles"], melds), aka),
             "win": tiles136([inp["win_tile"]], aka),
             "melds": meld_entries(melds, aka),
             "context": context_args(config),
             "dora": tiles136(inp.get("dora_indicators") or [], False),
             "ura": tiles136(inp.get("ura_dora_indicators") or [], False),
             "aka": aka,
             "options": our_options(config["options"]),
             **rules(config, house_rules(config, names), aotenjou),
             "honba": config["tsumi_number"], "sticks": config["kyoutaku_number"],
             "players": 3 if config["options"]["is_three_player"] else 4}
    if result["error"]:
        entry["expect"] = {"none": result["error"]}
        return entry
    expect = {"yaku": names, "yakuman": result["yakuman"]}
    if aotenjou or not result["yakuman"]:
        expect["fu"] = result["fu"]
        expect["han"] = result["han"]
    entry["expect"] = {**expect, **expectation(result["cost"], config)}
    return entry


def scores_entry(record):
    inp, result = record["input"], record["result"]
    config = inp["config"]
    if "raised" in result:
        raise Skip("upstream raises")
    return {"kind": kw("scores"), "han": inp["han"], "fu": inp["fu"],
            "yakuman": bool(inp.get("is_yakuman")),
            "context": {"tsumo": config["is_tsumo"], "dealer": config["is_dealer"]},
            "options": our_options(config["options"]), **rules(config),
            "honba": config["tsumi_number"], "sticks": config["kyoutaku_number"],
            "players": 3 if config["options"]["is_three_player"] else 4,
            "expect": expectation(result, config)}


def shanten_entry(record):
    inp, result = record["input"], record["result"]
    counts = inp["tiles_34"]
    method = inp["method"]
    if method == "calculate_shanten":
        shapes = ["standard"]
        if sum(counts) >= 13:
            shapes += ["seven-pairs"] * bool(inp.get("use_chiitoitsu", True))
            shapes += ["kokushi"] * bool(inp.get("use_kokushi", True))
    else:
        shapes = [{"calculate_shanten_for_regular_hand": "standard",
                   "calculate_shanten_for_chiitoitsu_hand": "seven-pairs",
                   "calculate_shanten_for_kokushi_hand": "kokushi"}[method]]
    expect = kw("error") if isinstance(result, dict) else result
    return {"kind": kw("shanten"), "hand": tiles34(counts), "pad": max(0, 4 - sum(counts) // 3),
            "shapes": [Sym(s) for s in shapes], "players": 3 if inp.get("is_three_player") else 4, "expect": expect}


def agari_entry(record):
    inp = record["input"]
    sets = inp.get("open_sets_34") or []
    counts = list(inp["tiles_34"])
    melds = []
    for group in sets:
        for kind in group:
            counts[kind] -= 1
        melds.append([kw("quad" if len(group) == 4 else "sequence" if group[0] != group[1] else "triplet"),
                      tile_notation([(k, False) for k in group]), kw("open"), True])
    if min(counts) < 0:
        raise Skip("meld tiles are not in the hand")
    return {"kind": kw("agari"), "hand": tiles34(counts), "melds": melds,
            "expect": record["result"]}


def divide_entry(record):
    inp = record["input"]
    melds = inp.get("melds") or []
    counts = list(inp["tiles_34"])
    for meld in melds:
        for tile in meld["tiles"]:
            counts[tile // 4] -= 1
    if min(counts) < 0:
        raise Skip("meld tiles are not in the hand")
    return {"kind": kw("divide"), "hand": tiles34(counts), "melds": meld_entries(melds, False),
            "expect": [sorted(sorted(block) for block in division) for division in record["result"]]}


BUILDERS = {"hand": hand_entry, "scores": scores_entry, "shanten": shanten_entry,
            "agari": agari_entry, "divide": divide_entry}


def case_id(record):
    path, _, name = record["test"].partition("::")
    return f"{Path(path).stem.removeprefix('tests_')}::{name}#{record['n']}"


def convert(records):
    seen, entries, skips = {}, [], Counter()
    for record in records:
        identity = json.dumps([record["kind"], record["input"]], sort_keys=True)
        if identity in seen:
            continue
        seen[identity] = True
        header = {"id": case_id(record)}
        try:
            body = BUILDERS[record["kind"]](record)
            entries.append({**header, **body})
        except Skip as reason:
            skips[str(reason)] += 1
            entries.append({**header, "kind": kw(record["kind"]), "skip": str(reason)})
    return entries, skips


def banner(license_text, commit):
    lines = [";;;; Riichi validation corpus, generated by tools/riichi-corpus/generate.py: do not edit.",
             f";;;; Source: {UPSTREAM.removesuffix('.git')} at {commit}",
             ";;;; Each case is a call recorded while running the library's own tests.",
             ";;;;"]
    lines += [";;;; " + line if line else ";;;;" for line in license_text.strip().splitlines()]
    return "\n".join(lines) + "\n\n"


def run(command, **kwargs):
    subprocess.run(command, check=True, **kwargs)


def record_upstream(work):
    checkout, venv, raw = work / "mahjong", work / "venv", work / "raw.jsonl"
    run(["git", "clone", "-q", UPSTREAM, str(checkout)])
    run(["git", "-C", str(checkout), "checkout", "-q", PINNED])
    run([sys.executable, "-m", "venv", str(venv)])
    python = str(venv / "bin" / "python")
    run([python, "-m", "pip", "install", "-q", "pytest", "-e", str(checkout)])
    env = {**os.environ, "CORPUS_RAW": str(raw), "PYTHONPATH": str(HERE)}
    run([python, "-m", "pytest", "tests", "-p", "recorder", "-o", "addopts=", "-q",
         "-p", "no:cacheprovider"], cwd=checkout, env=env)
    return checkout, raw


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT)
    arguments = parser.parse_args()
    with tempfile.TemporaryDirectory() as directory:
        checkout, raw = record_upstream(Path(directory))
        records = [json.loads(line) for line in raw.read_text().splitlines()]
        license_text = (checkout / "LICENSE.txt").read_text()
    entries, skips = convert(records)
    arguments.out.parent.mkdir(parents=True, exist_ok=True)
    body = "\n".join(sx(entry) for entry in entries)
    arguments.out.write_text(banner(license_text, PINNED) + "(\n" + body + ")\n")
    print(f"{len(entries)} cases, {sum(skips.values())} skipped")
    for reason, count in skips.most_common():
        print(f"  {count:4} {reason}")


if __name__ == "__main__":
    main()
