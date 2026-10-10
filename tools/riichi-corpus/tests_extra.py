"""Whole-hand calls standing in for upstream's unit tests of single yaku classes.

generate.py copies this file into the upstream checkout so recorder.py captures each call. The
calls assert nothing: the recorded upstream result is the expected value.
"""
import pytest
from mahjong.hand_calculating.hand import HandCalculator
from mahjong.hand_calculating.hand_config import HandConfig, OptionalRules
from mahjong.tile import TilesConverter


def _estimate(tiles, win_tile, **options):
    config = HandConfig(options=OptionalRules(**options))
    return HandCalculator.estimate_hand_value(tiles, win_tile, config=config)


@pytest.mark.parametrize("suit", ["pin", "man", "sou"])
@pytest.mark.parametrize("other_suits", [False, True])
def test_daisharin_pairs_two_to_eight(suit, other_suits):
    tiles = TilesConverter.string_to_136_array(**{suit: "22334455667788"})
    win_tile = TilesConverter.string_to_136_array(**{suit: "8"})[0]
    _estimate(tiles, win_tile, has_daisharin=True, has_daisharin_other_suits=other_suits)


@pytest.mark.parametrize("other_suits", [False, True])
def test_pairs_one_to_seven_are_not_daisharin(other_suits):
    tiles = TilesConverter.string_to_136_array(pin="11223344556677")
    win_tile = TilesConverter.string_to_136_array(pin="7")[0]
    _estimate(tiles, win_tile, has_daisharin=True, has_daisharin_other_suits=other_suits)
