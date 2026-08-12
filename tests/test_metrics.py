import math

import numpy as np
import pandas as pd

from polaris_profiler import config, metrics


def test_rmse_zero_for_perfect_prediction():
    assert metrics.rmse([1, 2, 3], [1, 2, 3]) == 0.0


def test_rmse_matches_manual_computation():
    assert math.isclose(metrics.rmse([0, 0], [3, 4]), math.sqrt(12.5))


def test_rmse_percentage_overall():
    # rmse([10,10],[10,20]) = sqrt(50) ; sum(true) = 20
    expected = math.sqrt(50) / 20 * 100
    assert math.isclose(metrics.rmse_percentage_overall([10, 10], [10, 20]), expected)


def test_rmse_percentage_per_feature():
    result = metrics.rmse_percentage_per_feature([100, 0], [50, 5])
    assert result[0] == 50.0
    assert result[1] == 0.0  # zero true value guarded


def test_cl_size_score_bounds():
    assert metrics.cl_size_score(26, 26) == 1.0
    assert metrics.cl_size_score(26, 52) == 0.5
    assert metrics.cl_size_score(26, 0) == 0.0


def test_outlier_score():
    assert metrics.outlier_score(0, 100) == 1.0
    assert metrics.outlier_score(25, 100) == 0.75


def test_compute_acquires_score_equal_weights():
    scores = {"mean_cluster_size": 26, "n_outliers": 0, "avg_silhouette": 1.0}
    assert math.isclose(
        metrics.compute_acquires_score(scores, optimal_cluster_size=26,
                                       dataset_size=100), 1.0)


def test_rmse_percentage_overall_zero_sum_does_not_divide_by_zero():
    assert metrics.rmse_percentage_overall([0, 0], [1, 1]) == float("inf")


def test_acquires_scores_tolerates_string_columns_and_nans():
    # regression: fillna(df.mean()) over string metadata columns raised
    # TypeError on pandas >= 2, killing the quality-check path
    rng = np.random.default_rng(0)
    df = pd.DataFrame(rng.random((12, len(config.CLUSTERING_FEATURES))),
                      columns=config.CLUSTERING_FEATURES)
    df.loc[0, "cpu_usage"] = np.nan            # NaN imputation path
    df["job_name"] = [f"job{i}" for i in range(12)]   # string column
    df["profile"] = [0] * 5 + [1] * 5 + [-1] * 2
    scores = metrics.acquires_scores(df)
    assert scores["n_outliers"] == 2
    assert scores["n_clusters"] == 2
    assert scores["mean_cluster_size"] == 5.0
    assert -1.0 <= scores["avg_silhouette"] <= 1.0
