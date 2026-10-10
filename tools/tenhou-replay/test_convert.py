import gzip
import tempfile
import unittest
from pathlib import Path

import convert


def encode_chi(tiles, index):
    """Chi of three consecutive suit tiles; INDEX picks the called one. The bit layout is Tenhou's."""
    kinds = [tile // 4 for tile in tiles]
    base = kinds[0]
    t = (base // 9 * 7 + base % 9) * 3 + index
    return (t << 10) | ((tiles[2] % 4) << 7) | ((tiles[1] % 4) << 5) | ((tiles[0] % 4) << 3) | 4 | 3


def encode_pon(kind, unused, index):
    return ((kind * 3 + index) << 9) | (unused << 5) | 8 | 1


def encode_shouminkan(kind, added):
    return ((kind * 3) << 9) | (added << 5) | 16 | 1


def encode_daiminkan(called):
    return (called << 8) | 1


def encode_ankan(called):
    return called << 8


class MeldTest(unittest.TestCase):
    def test_chi(self):
        tiles = [4 * 2 + 1, 4 * 3 + 2, 4 * 4 + 3]
        self.assertEqual(("chi", tiles, tiles[1]), convert.decode_meld(encode_chi(tiles, 1)))

    def test_chi_in_second_suit(self):
        tiles = [4 * 10, 4 * 11 + 1, 4 * 12 + 2]
        self.assertEqual(("chi", tiles, tiles[0]), convert.decode_meld(encode_chi(tiles, 0)))

    def test_pon(self):
        kind, kind_tiles = 27, [108, 109, 111]
        self.assertEqual(("pon", kind_tiles, 109), convert.decode_meld(encode_pon(kind, 2, 1)))

    def test_kans(self):
        self.assertEqual(("shouminkan", [75], None), convert.decode_meld(encode_shouminkan(18, 3)))
        self.assertEqual(("daiminkan", [72, 73, 74, 75], 74), convert.decode_meld(encode_daiminkan(74)))
        self.assertEqual(("ankan", [72, 73, 74, 75], None), convert.decode_meld(encode_ankan(72)))

    def test_kita(self):
        self.assertEqual(("kita", [121], None), convert.decode_meld(31008))

    def test_sanma_tile_ids_skip_man_two_to_eight(self):
        self.assertEqual([0, 3, 4, 7, 24, 107],
                         [convert.sanma_tile(tile) for tile in (0, 3, 32, 35, 52, 135)])


class GameTest(unittest.TestCase):
    def test_rejects_other_rules(self):
        for kind in (169 | 0x02, 169 | 0x04, 169 ^ 0x08):
            with self.assertRaises(ValueError):
                convert.check_rules({"type": str(kind)})
        self.assertEqual(169, convert.check_rules({"type": "169"}))
        self.assertEqual(185, convert.check_rules({"type": "185"}))

    def test_converts_a_tsumo(self):
        hands = " ".join(f'hai{seat}="{",".join(str(seat * 13 + i) for i in range(13))}"' for seat in range(4))
        text = (f'<mjloggm><GO type="169"/><INIT seed="0,0,0,1,2,100" ten="250,250,250,250" oya="0" {hands}/>'
                '<T60/><D60/><U61/><E61/>'
                '<AGARI ba="0,0" hai="1" ten="30,3900,1" yaku="0,1,8,1,52,1" who="0" fromWho="0" '
                'sc="250,39,250,-13,250,-13,250,-13" owari="289,30.0,237,5.0,237,-15.0,237,-20.0"/></mjloggm>')
        game = convert.convert(text, "x")
        deal = game["deals"][0]
        self.assertEqual([60], deal["draws"][:1])
        win = deal["result"]["wins"][0]
        self.assertEqual(3, win["han"])
        self.assertEqual([":menzen-tsumo", ":tanyao"], [str(y) for y in win["yaku"]])
        self.assertEqual([3900, -1300, -1300, -1300], win["deltas"])
        self.assertEqual([28900, 23700, 23700, 23700], game["owari"]["points"])
        self.assertEqual(30000, game["owari"]["scores"][0])

    def test_converts_a_sanma_game_with_kita(self):
        hands = " ".join(f'hai{seat}="{",".join(str(36 + seat * 13 + i) for i in range(13))}"'
                         for seat in range(3))
        text = (f'<mjloggm><GO type="185"/><INIT seed="0,0,0,1,2,100" ten="350,350,350,0" oya="0" '
                f'{hands} hai3=""/><T121/><N who="0" m="31008" /><T52/><D52/><U40/><E40/><V41/><F41/>'
                '<AGARI ba="0,0" hai="1" ten="30,3900,1" yaku="0,1,8,1,52,1" who="0" fromWho="0" '
                'sc="350,39,350,-20,350,-19,0,0" owari="389,40.0,330,0.0,330,-20.0,0,0.0"/></mjloggm>')
        game = convert.convert(text, "2023010100gm")
        deal = game["deals"][0]
        self.assertEqual(":tenhou-sanma", str(game["variant"]))
        self.assertEqual(3, len(deal["hands"]))
        self.assertEqual(3, convert.convert(text.replace('seed="0,0', 'seed="4,0'), "2023010100gm")["deals"][0]["kyoku"])
        self.assertEqual([8, 8 + 13, 8 + 26], [hand[0] for hand in deal["hands"]])
        self.assertEqual([93], deal["draws"][:1])
        self.assertEqual([24], deal["rinshan"])
        self.assertEqual([":kita", 0, 93], [str(deal["events"][0][0]), *deal["events"][0][1:]])
        self.assertEqual([3900, -2000, -1900], deal["result"]["wins"][0]["deltas"])
        self.assertEqual([38900, 33000, 33000], game["owari"]["points"])

    def test_double_ron_takes_ura_from_the_riichi_winner(self):
        deal = {}
        wins = [{"from": 1, "ura": []}, {"from": 1, "ura": [60]}]
        convert.finish_deal(deal, {}, wins, "AGARI", 4)
        self.assertEqual([60], deal["ura"])


class ScoringEraTest(unittest.TestCase):
    def test_picks_the_variant_by_final_score_precision_and_date(self):
        self.assertEqual(":tenhou-2021", convert.scoring_variant("2013010100gm", [60000, -20000], 4))
        self.assertEqual(":tenhou", convert.scoring_variant("2013010100gm", [56100, -20000], 4))
        self.assertEqual(":tenhou", convert.scoring_variant("2023010100gm", [60000, -20000], 4))
        self.assertEqual(":tenhou-sanma", convert.scoring_variant("2023010100gm", [60000, -20000], 3))
        with self.assertRaises(ValueError):
            convert.scoring_variant("2013010100gm", [60000, -20000], 3)


class ReadLogTest(unittest.TestCase):
    def test_reads_plain_and_gzip_logs(self):
        with tempfile.TemporaryDirectory() as directory:
            plain, packed = Path(directory, "a.mjlog"), Path(directory, "b.mjlog")
            plain.write_bytes("<mjloggm/>".encode())
            packed.write_bytes(gzip.compress("<mjloggm/>".encode()))
            self.assertEqual("<mjloggm/>", convert.read_log(plain))
            self.assertEqual("<mjloggm/>", convert.read_log(packed))


if __name__ == "__main__":
    unittest.main()
