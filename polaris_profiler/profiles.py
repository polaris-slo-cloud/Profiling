"""Profile prediction and per-profile resource estimation.

The original scripts differed only in *which statistic* of the profile's
distribution they returned (mean, median, 5th quantile, IQR, skew-adaptive
quantile) — here that choice is a named estimator strategy.
"""
from typing import Dict, List, Sequence, Tuple

import pandas as pd
from scipy.stats import skew

from . import config


def decide_quantile(skew_value: float) -> float:
    """Map a skewness coefficient to the quantile to report.

    Symmetric / left-skewed distributions -> median; right-skewed -> a
    conservative low quantile (the distributions here are heavy right tails).
    """
    if skew_value < 0:
        return 0.5
    return 0.05


# Each estimator maps (series, skew_value) -> scalar prediction.
ESTIMATORS = {
    "mean": lambda s, sk: s.mean(),
    "median": lambda s, sk: s.median(),
    "q05": lambda s, sk: s.quantile(0.05),
    "iqr": lambda s, sk: s.quantile(0.75) - s.quantile(0.25),
    "skew-adaptive": lambda s, sk: s.quantile(decide_quantile(sk)),
}


def predict_profile(classifier, encoder, metadata: Sequence) -> int:
    """Predict the profile label for one workload's metadata row."""
    encoded = encoder.transform([metadata])
    return classifier.predict(encoded)[0]


def compute_skewness(profile_data: pd.DataFrame,
                     features: List[str] = config.RESOURCE_FEATURES) -> Dict[str, float]:
    """Skewness coefficient of each feature within one profile."""
    return {f: skew(profile_data[f], nan_policy="omit") for f in features}


def compute_skewness_by_profile(workload: pd.DataFrame,
                                features: List[str] = config.RESOURCE_FEATURES
                                ) -> Dict[int, Dict[str, float]]:
    """Per-profile skewness of each feature, keyed by profile label."""
    return {
        label: compute_skewness(group, features)
        for label, group in workload.groupby("profile")
    }


def estimate_resources(workload: pd.DataFrame,
                       profile_label: int,
                       features: List[str] = config.RESOURCE_FEATURES,
                       estimator: str = "q05") -> Tuple[float, ...]:
    """Estimate resource usage for a workload assigned to `profile_label`.

    Returns one value per feature, in the order of `features`.
    """
    estimate = ESTIMATORS[estimator]
    profile_data = workload[workload["profile"] == profile_label]
    skews = compute_skewness(profile_data, features) if estimator == "skew-adaptive" \
        else dict.fromkeys(features)
    return tuple(estimate(profile_data[f], skews[f]) for f in features)


def bayesian_update_prior(mu_0: float, sigma_0: float, data: pd.Series) -> float:
    """Posterior mean of a normal prior updated with observed data.

    Kept from the original exploration; not used by the default loop.
    """
    mu, n, sigma = data.mean(), len(data), data.std()
    return (sigma_0**2 * mu + n * sigma**2 * mu_0) / (sigma_0**2 + n * sigma**2)
