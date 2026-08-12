"""Feedback-loop semantics with stubbed models."""
import numpy as np
import pandas as pd

from polaris_profiler import config
from polaris_profiler.feedback import FeedbackLoop


class FakeClusterer:
    labels_ = np.array([0] * 5 + [1] * 5)


class FakeClassifier:
    def predict(self, X):
        return [1]


class FakeEncoder:
    def transform(self, X):
        return np.zeros((len(X), 3))


def _workload():
    df = pd.DataFrame({
        "job_name": [f"j{i}" for i in range(10)],
        "user": ["u"] * 10,
        "task_name": ["t"] * 10,
        "group": ["g"] * 10,
        "workload": ["w"] * 10,
        "max_mem": np.linspace(1, 10, 10),
        "gpu_wrk_util": np.linspace(0, 1, 10),
        "cpu_usage": np.linspace(5, 9, 10),
        "duration": np.linspace(10, 100, 10),
    })
    for feature in config.CLUSTERING_FEATURES:
        if feature not in df:
            df[feature] = np.linspace(0, 5, 10)
    return df


def _loop(**cfg_overrides):
    return FeedbackLoop(
        clusterer=FakeClusterer(), classifier=FakeClassifier(),
        encoder=FakeEncoder(), workload=_workload(),
        cfg=config.FeedbackConfig(estimator="median", **cfg_overrides),
    )


def _row():
    return pd.Series({"job_name": "j-new", "user": "u", "task_name": "t",
                      "group": "g", "workload": "w", "max_mem": 25.0,
                      "gpu_wrk_util": 0.9, "cpu_usage": 8.0, "duration": 3.0})


def test_observe_folds_row_under_predicted_profile():
    loop = _loop()
    record = loop.observe(_row())
    assert record["Profile"] == 1
    folded = loop.workload.iloc[-1]
    assert folded["profile"] == 1          # not NaN: contributes to estimates
    assert folded["duration"] == 3.0       # kept, not dropped


def test_folded_observations_influence_estimates():
    loop = _loop()
    baseline = loop.predict(_row()[config.METADATA_CATEGORIES])[1]
    for _ in range(20):
        loop.observe(_row())
    updated = loop.predict(_row()[config.METADATA_CATEGORIES])[1]
    assert updated != baseline             # the loop actually adapts


def test_quality_check_runs_with_string_metadata_columns():
    # regression: acquires_scores crashed on pandas >= 2 when the workload
    # contained string columns (fillna over df.mean()); the 1000th violation
    # then 500'd in service mode and re-clustering was unreachable
    loop = _loop(violations_per_check=1, rmse_violation_threshold=1e-6,
                 recluster_below_score=-1)  # never actually recluster
    record = loop.observe(_row())
    assert record["violation"]
    assert loop.n_violations == 1
    assert loop.acquires_history            # quality check really ran
