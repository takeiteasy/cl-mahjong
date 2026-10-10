"""Pytest plugin that records every top-level call into the mahjong library as JSON lines.

Loaded with `-p recorder`; CORPUS_RAW names the output file.
"""
import functools
import json
import os

from mahjong.agari import Agari
from mahjong.hand_calculating.divider import HandDivider
from mahjong.hand_calculating.hand import HandCalculator
from mahjong.hand_calculating.hand_config import HandConfig
from mahjong.hand_calculating.scores import ScoresCalculator
from mahjong.shanten import Shanten

_depth = 0
_calls = {}
_out = None


def _plain(value):
    if isinstance(value, (str, int, float, bool, type(None))):
        return value
    if isinstance(value, (list, tuple, set, frozenset)):
        return [_plain(item) for item in value]
    if isinstance(value, dict):
        return {str(key): _plain(item) for key, item in value.items()}
    if hasattr(value, "__dict__"):
        return {key: _plain(item) for key, item in vars(value).items()
                if not key.startswith("_") and key != "yaku"}
    return repr(value)


def _test_id():
    current = os.environ.get("PYTEST_CURRENT_TEST", "unknown")
    return current.rsplit(" ", 1)[0]


def _record(kind, inputs, result):
    test = _test_id()
    _calls[test] = _calls.get(test, 0) + 1
    _out.write(json.dumps({"kind": kind, "test": test, "n": _calls[test],
                           "input": _plain(inputs), "result": _plain(result)}) + "\n")


def _wrap(owner, name, kind, describe):
    original = getattr(owner, name)

    @functools.wraps(original)
    def wrapper(*args, **kwargs):
        global _depth
        outermost = _depth == 0
        _depth += 1
        try:
            result = original(*args, **kwargs)
        except Exception as error:
            if outermost:
                _record(kind, describe(args, kwargs), {"raised": type(error).__name__, "message": str(error)})
            raise
        finally:
            _depth -= 1
        if outermost:
            _record(kind, describe(args, kwargs), result)
        return result

    setattr(owner, name, staticmethod(wrapper))


def _hand_inputs(args, kwargs):
    names = ["tiles", "win_tile", "melds", "dora_indicators", "config", "scores_calculator_factory",
             "ura_dora_indicators"]
    given = dict(zip(names, args))
    given.update(kwargs)
    given["config"] = given.get("config") or HandConfig()
    factory = given.pop("scores_calculator_factory", ScoresCalculator)
    given["factory"] = factory.__name__
    return given


def _scores_inputs(args, kwargs):
    names = ["han", "fu", "config", "is_yakuman"]
    given = dict(zip(names, args))
    given.update(kwargs)
    return given


def _positional(*names):
    def describe(args, kwargs):
        given = dict(zip(names, args))
        given.update(kwargs)
        return given
    return describe


def _hand_result(response):
    return {"error": response.error, "han": response.han, "fu": response.fu,
            "yaku": None if response.yaku is None else [y.name for y in response.yaku],
            "yakuman": None if response.yaku is None else any(y.is_yakuman for y in response.yaku),
            "cost": response.cost}


def pytest_configure(config):
    global _out
    _out = open(os.environ["CORPUS_RAW"], "w", buffering=1)

    original = HandCalculator.estimate_hand_value

    @functools.wraps(original)
    def estimate(*args, **kwargs):
        global _depth
        outermost = _depth == 0
        _depth += 1
        try:
            response = original(*args, **kwargs)
        finally:
            _depth -= 1
        if outermost:
            _record("hand", _hand_inputs(args, kwargs), _hand_result(response))
        return response

    HandCalculator.estimate_hand_value = staticmethod(estimate)
    _wrap(ScoresCalculator, "calculate_scores", "scores", _scores_inputs)
    for method in ("calculate_shanten", "calculate_shanten_for_regular_hand",
                   "calculate_shanten_for_chiitoitsu_hand", "calculate_shanten_for_kokushi_hand"):
        _wrap(Shanten, method, "shanten",
              lambda args, kwargs, method=method: dict(
                  _positional("tiles_34", "use_chiitoitsu", "use_kokushi", "is_three_player")(args, kwargs)
                  if method == "calculate_shanten"
                  else _positional("tiles_34", "is_three_player")(args, kwargs), method=method))
    _wrap(Agari, "is_agari", "agari", _positional("tiles_34", "open_sets_34"))
    _wrap(HandDivider, "divide_hand", "divide", _positional("tiles_34", "melds"))


def pytest_unconfigure(config):
    if _out:
        _out.close()
