"""Prediction-error metrics and the ACQUIRES clustering-quality score."""
from typing import Dict, List, Optional, Sequence

import numpy as np
import pandas as pd

from . import config


# --- RMSE-percentage metrics ------------------------------------------------

def rmse(true_values: Sequence[float], predicted_values: Sequence[float]) -> float:
    t, p = np.asarray(true_values, dtype=float), np.asarray(predicted_values, dtype=float)
    return float(np.sqrt(np.mean((t - p) ** 2)))


def rmse_percentage_overall(true_values: Sequence[float],
                            predicted_values: Sequence[float]) -> float:
    """Single RMSE over all features, as a percentage of the summed true values."""
    total = sum(true_values)
    if total == 0:
        return float("inf")
    return rmse(true_values, predicted_values) / total * 100


def rmse_percentage_per_feature(true_values: Sequence[float],
                                predicted_values: Sequence[float]) -> List[float]:
    """Per-feature |error| as a percentage of that feature's true value."""
    return [
        abs(t - p) / t * 100 if t != 0 else 0.0
        for t, p in zip(true_values, predicted_values)
    ]


# --- ACQUIRES score ---------------------------------------------------------

def cl_size_score(optimal_cluster_size: float, actual_cluster_size: float) -> float:
    """1 when actual == optimal, decaying towards 0 as sizes diverge."""
    return 1 - abs(optimal_cluster_size - actual_cluster_size) / max(optimal_cluster_size,
                                                                     actual_cluster_size)


def outlier_score(n_outliers: int, dataset_size: int) -> float:
    """Proportion of non-outlier points."""
    return 1 - n_outliers / dataset_size


def acquires_scores(profiles_data: pd.DataFrame,
                    clustering_features: List[str] = config.CLUSTERING_FEATURES
                    ) -> Dict[str, float]:
    """Raw ingredients of the ACQUIRES score for a profiled dataset."""
    from sklearn.metrics import silhouette_samples  # heavy import, keep local

    # restrict to the numeric columns the score uses — mean() over the string
    # metadata columns raises on pandas >= 2
    data = profiles_data[clustering_features + ["profile"]]
    data = data.fillna(data.mean())
    labels = data.profile.values
    n_clusters = int(np.max(labels)) + 1
    return {
        "n_outliers": int(np.count_nonzero(labels == -1)),
        "n_clusters": n_clusters,
        "mean_cluster_size": float(np.mean(
            [np.count_nonzero(labels == i) for i in range(n_clusters)])),
        "avg_silhouette": -1.0 if labels.max() <= 0 else float(np.mean(
            silhouette_samples(data[clustering_features], labels,
                               metric="euclidean"))),
    }


def compute_acquires_score(scores: Dict[str, float],
                           optimal_cluster_size: float,
                           dataset_size: int,
                           weights: Optional[List[float]] = None) -> float:
    """Weighted combination of cluster-size, outlier, and silhouette scores."""
    components = [
        cl_size_score(optimal_cluster_size, scores["mean_cluster_size"]),
        outlier_score(scores["n_outliers"], dataset_size),
        scores["avg_silhouette"],
    ]
    if weights is None:
        weights = [1.0 / len(components)] * len(components)
    return sum(c * w for c, w in zip(components, weights)) / sum(weights)
