import unittest

import fetch_logs

BASE = "https://tenhou.net/ranking.html"
PAGE = ('<a href="/0/log/mjlog_pf4-20_n30.zip">a</a>'
        '<a href="/0/log/mjlog_pf3-20_n31.zip">b</a>'
        '<a href="/0/log/mjlog_pf4-20_n29.zip">c</a>'
        '<a href="/0/log/mjlog_pf4-20_n30.zip">dup</a>'
        '<a href="/other.zip">x</a>')


class FindZipsTest(unittest.TestCase):
    def test_finds_unique_zips_in_page_order_with_modes(self):
        self.assertEqual(
            [("https://tenhou.net/0/log/mjlog_pf4-20_n30.zip", "4p"),
             ("https://tenhou.net/0/log/mjlog_pf3-20_n31.zip", "3p"),
             ("https://tenhou.net/0/log/mjlog_pf4-20_n29.zip", "4p")],
            fetch_logs.find_zips(PAGE, BASE))


class SelectZipsTest(unittest.TestCase):
    zips = fetch_logs.find_zips(PAGE, BASE)

    def test_takes_the_newest_per_mode(self):
        self.assertEqual([self.zips[0]], fetch_logs.select_zips(self.zips, "4p", 1))

    def test_both_modes_take_count_each(self):
        self.assertEqual(self.zips, fetch_logs.select_zips(self.zips, "both", 2))
        self.assertEqual(self.zips[:2], fetch_logs.select_zips(self.zips, "both", 1))

    def test_other_modes_are_skipped(self):
        self.assertEqual([self.zips[1]], fetch_logs.select_zips(self.zips, "3p", 5))


if __name__ == "__main__":
    unittest.main()
