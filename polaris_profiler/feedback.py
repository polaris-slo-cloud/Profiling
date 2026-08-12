"""The PolarisProfiler online feedback loop.

For each incoming workload: predict its profile from metadata, estimate its
resource usage from the profile's history, measure the error against the
observed usage, and fold the workload back into the reference dataset.
When errors accumulate, re-check clustering quality (ACQUIRES) and, if it
degraded badly, re-run HDBSCAN and retrain the profile classifier.

Two entry points share the same code path:

- `run(test_workload)`  — batch mode, used by the CLIs (one call per dataset)
- `predict(metadata)` / `observe(row)` — incremental mode, used by the
  HTTP service (one call per scheduling decision / completed workload)
"""
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

import pandas as pd
from tqdm import tqdm

from . import config, metrics, profiles

logger = logging.getLogger(__name__)


@dataclass
class FeedbackResult:
    """Outcome of one batch feedback-loop run."""
    per_workload: pd.DataFrame          # per-feature RMSE%, Overall, Profile
    n_violations: int = 0
    n_reclusterings: int = 0
    acquires_history: List[float] = field(default_factory=list)
    best_acquires_score: float = 0.0


class FeedbackLoop:
    """Online profiling loop over a stream of workloads.

    Instance state (reference dataset, violation counters, ACQUIRES history)
    persists across calls, so the loop can run batch-style or be driven
    one observation at a time by a long-running service.
    """

    def __init__(self, clusterer, classifier, encoder,
                 workload: pd.DataFrame,
                 cfg: Optional[config.FeedbackConfig] = None):
        self.clusterer = clusterer
        self.classifier = classifier
        self.encoder = encoder
        self.workload = workload.copy()
        self.cfg = cfg or config.FeedbackConfig()
        if "profile" not in self.workload:
            self.workload["profile"] = clusterer.labels_
        self.n_violations = 0
        self.n_reclusterings = 0
        self.acquires_history: List[float] = []
        self.best_acquires_score = self.cfg.initial_acquires_score

    # --- incremental API (used by the service) ------------------------------

    def predict(self, metadata) -> Tuple[int, Dict[str, float]]:
        """Profile label + resource estimates for one workload's metadata.

        Read-only: does not change loop state. `metadata` is a sequence of
        values in cfg.metadata_categories order.
        """
        cfg = self.cfg
        label = profiles.predict_profile(self.classifier, self.encoder, metadata)
        estimates = profiles.estimate_resources(
            self.workload, label,
            features=cfg.resource_features, estimator=cfg.estimator)
        return int(label), dict(zip(cfg.resource_features, estimates))

    def observe(self, row: pd.Series) -> Dict:
        """Process one completed workload: predict, score, fold in, re-check.

        `row` carries the metadata categories plus the observed resource
        features. Returns the per-workload record (per-feature RMSE%,
        Overall, Profile, violation flag).
        """
        cfg = self.cfg
        label, estimates = self.predict(row[cfg.metadata_categories])
        true = [row[f] for f in cfg.resource_features]
        predicted = [estimates[f] for f in cfg.resource_features]

        overall = metrics.rmse_percentage_overall(true, predicted)
        per_feature = metrics.rmse_percentage_per_feature(true, predicted)
        record = {config.RESOURCE_LABELS[f]: v
                  for f, v in zip(cfg.resource_features, per_feature)}
        record.update(Overall=overall, Profile=label,
                      violation=overall > cfg.rmse_violation_threshold)

        # Fold the observed workload back into the reference dataset under its
        # predicted profile, so it contributes to future estimates. (The
        # legacy script folded rows without a profile label and dropped the
        # duration, which made folded observations inert — see MIGRATION.md.)
        folded = row.copy()
        folded["profile"] = label
        self.workload = pd.concat(
            [self.workload, pd.DataFrame([folded])], ignore_index=True)

        if record["violation"]:
            self.n_violations += 1
            logger.info("RMSE percentage exceeded threshold for row %s.", row.name)
            if self.n_violations % cfg.violations_per_check == 0:
                self._check_quality()
        return record

    # --- batch API (used by the CLIs) ---------------------------------------

    def run(self, test_workload: pd.DataFrame, progress: bool = True) -> FeedbackResult:
        iterator = test_workload.iterrows()
        if progress:
            iterator = tqdm(iterator, total=len(test_workload), desc="Processing rows")
        records = [self.observe(row) for _, row in iterator]
        return FeedbackResult(
            per_workload=pd.DataFrame(records).drop(columns="violation"),
            n_violations=self.n_violations,
            n_reclusterings=self.n_reclusterings,
            acquires_history=list(self.acquires_history),
            best_acquires_score=self.best_acquires_score,
        )

    # --- profile inspection (used by the service) ---------------------------

    def profile_summary(self) -> List[Dict]:
        """Size and current resource estimates of every known profile."""
        cfg = self.cfg
        out = []
        for label, group in self.workload.groupby("profile"):
            if pd.isna(label):
                continue
            estimates = profiles.estimate_resources(
                self.workload, label,
                features=cfg.resource_features, estimator=cfg.estimator)
            out.append({"profile": int(label), "size": int(len(group)),
                        "estimates": dict(zip(cfg.resource_features, estimates))})
        return out

    # --- internals ----------------------------------------------------------

    def _check_quality(self) -> None:
        cfg = self.cfg
        scores = metrics.acquires_scores(self.workload, cfg.clustering_features)
        acquires = metrics.compute_acquires_score(
            scores, cfg.optimal_cluster_size, len(self.workload))
        self.acquires_history.append(acquires)

        if acquires >= self.best_acquires_score:
            self.best_acquires_score = acquires
        elif acquires < cfg.recluster_below_score:
            self.n_reclusterings += 1
            logger.info("ACQUIRES score dropped below %.2f. Re-running HDBSCAN.",
                        cfg.recluster_below_score)
            self._recluster()
            post = metrics.acquires_scores(self.workload, cfg.clustering_features)
            logger.info("New ACQUIRES score: %f",
                        metrics.compute_acquires_score(
                            post, cfg.optimal_cluster_size, len(self.workload)))
            logger.info("New silhouette score: %f", post["avg_silhouette"])
            self._retrain_classifier()

    def _recluster(self) -> None:
        self.clusterer = self.clusterer.fit(self.workload[self.cfg.clustering_features])
        old_labels = self.workload["profile"].values
        self.workload["profile"] = self.clusterer.labels_
        n_changed = int((old_labels != self.workload["profile"].values).sum())
        logger.info("After HDBSCAN, %d profiles changed clusters.", n_changed)

    def _retrain_classifier(self) -> None:
        import xgboost as xgb
        from sklearn.metrics import f1_score
        from sklearn.model_selection import train_test_split

        cfg = self.cfg
        # Outliers (-1) are excluded from training; remaining labels are
        # compacted to 0..k-1 consistently for both X and y (the original
        # script remapped after splitting, which could desync labels).
        train_data = self.workload[self.workload["profile"] != -1].copy()
        mapping = {label: idx for idx, label
                   in enumerate(sorted(train_data["profile"].unique()))}
        train_data["profile"] = train_data["profile"].map(mapping)

        features = self.encoder.transform(train_data[cfg.metadata_categories])
        X_train, X_val, y_train, y_val = train_test_split(
            features, train_data["profile"], test_size=0.2,
            stratify=train_data["profile"], random_state=42)

        model = xgb.XGBClassifier(objective="multi:softprob",
                                  num_class=len(mapping))
        model.fit(X_train, y_train)
        f1 = f1_score(y_val, model.predict(X_val), average="macro")
        if f1 < cfg.f1_score_threshold:
            logger.warning("F1 after retraining dropped below threshold: %f", f1)
        self.classifier = model
