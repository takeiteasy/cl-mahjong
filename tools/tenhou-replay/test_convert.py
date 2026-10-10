import unittest

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

    def test_nuki_is_rejected(self):
        with self.assertRaises(ValueError):
            convert.decode_meld(32)


class GameTest(unittest.TestCase):
    def test_rejects_other_rules(self):
        for kind in (169 | 0x02, 169 | 0x04, 169 ^ 0x08, 169 | 0x10):
            with self.assertRaises(ValueError):
                convert.check_rules({"type": str(kind)})
        self.assertEqual(169, convert.check_rules({"type": "169"}))

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


if __name__ == "__main__":
    unittest.main()
