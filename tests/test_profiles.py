import pandas as pd

from polaris_profiler import profiles


def _toy_workload():
    return pd.DataFrame({
        "profile": [0] * 5 + [1] * 5,
        "max_mem": [1, 2, 3, 4, 5, 10, 20, 30, 40, 50],
        "gpu_wrk_util": [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],
        "cpu_usage": [5, 5, 5, 5, 5, 9, 9, 9, 9, 9],
        "duration": [10, 20, 30, 40, 50, 1, 2, 3, 4, 5],
    })


def test_decide_quantile():
    assert profiles.decide_quantile(-1.0) == 0.5
    assert profiles.decide_quantile(2.0) == 0.05


def test_estimate_resources_median():
    predicted = profiles.estimate_resources(_toy_workload(), 0, estimator="median")
    assert predicted == (3, 0, 5, 30)


def test_estimate_resources_selects_profile():
    predicted = profiles.estimate_resources(_toy_workload(), 1, estimator="median")
    assert predicted == (30, 1, 9, 3)


def test_all_estimators_return_one_value_per_feature():
    workload = _toy_workload()
    for name in profiles.ESTIMATORS:
        predicted = profiles.estimate_resources(workload, 0, estimator=name)
        assert len(predicted) == 4


def test_uniquify(tmp_path):
    from polaris_profiler.plotting import uniquify
    target = tmp_path / "out.csv"
    assert uniquify(target) == target
    target.write_text("x")
    assert uniquify(target).name == "out (1).csv"
