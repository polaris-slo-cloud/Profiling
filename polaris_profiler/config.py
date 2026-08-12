"""Central configuration: repository paths, feature groups, loop thresholds.

Paths resolve in three steps: an environment variable wins if set, then the
new repository layout (data/, models/, ...) if it exists, then the legacy
ml_data-profiling/ layout — so the code works before and after running
scripts/migrate_layout.sh.
"""
import os
from dataclasses import dataclass, field
from pathlib import Path

# --- Repository layout -----------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
LEGACY_DIR = REPO_ROOT / "ml_data-profiling"
LEGACY_EXPERIMENTS = LEGACY_DIR / "experiments"


def _env_or_first_existing(env_var: str, *candidates: Path) -> Path:
    if env_var in os.environ:
        return Path(os.environ[env_var])
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


DATA_DIR = _env_or_first_existing("POLARIS_DATA_DIR",
                                  REPO_ROOT / "data" / "samples",
                                  LEGACY_EXPERIMENTS)
MODELS_DIR = _env_or_first_existing("POLARIS_MODELS_DIR",
                                    REPO_ROOT / "models",
                                    LEGACY_EXPERIMENTS)
FIGURES_DIR = _env_or_first_existing("POLARIS_FIGURES_DIR",
                                     REPO_ROOT / "figures",
                                     LEGACY_DIR / "Figures")
RESULTS_DIR = _env_or_first_existing("POLARIS_RESULTS_DIR",
                                     REPO_ROOT / "results",
                                     LEGACY_EXPERIMENTS)

# Raw Alibaba GPU trace (cluster-trace-gpu-v2020); only the EDA notebooks
# need it — not the profiler runs on the sampled datasets.
TRACE_DIR = Path(os.environ.get("POLARIS_TRACE_DIR",
                                "/data/clusterdata/cluster-trace-gpu-v2020/data"))

# --- Default artifact locations --------------------------------------------
HDBSCAN_MODEL = MODELS_DIR / "hdbscan_300_power_transform_euclidean.pkl"
XGBOOST_MODEL = MODELS_DIR / "xgboost_final_model.json"
ONEHOT_ENCODER = MODELS_DIR / "onehot_enc_train_data.pkl"
TRAIN_DATA = DATA_DIR / "100_001_sampled_workload_data.csv"
TEST_DATA = DATA_DIR / "10_000_sampled_test_data.csv"

# --- Feature groups ---------------------------------------------------------
# Categorical metadata used to classify a workload into a profile.
METADATA_CATEGORIES = ["job_name", "user", "task_name", "group", "workload"]

# Numerical features the HDBSCAN clustering (and silhouette score) run on.
CLUSTERING_FEATURES = [
    "cpu_usage", "gpu_wrk_util", "avg_mem", "max_mem",
    "avg_gpu_wrk_mem", "max_gpu_wrk_mem",
    "read", "write", "read_count", "write_count",
]

# Resources the profiler predicts, in canonical order.
RESOURCE_FEATURES = ["max_mem", "gpu_wrk_util", "cpu_usage", "duration"]

# Pretty names used in result tables and figures.
RESOURCE_LABELS = {
    "max_mem": "Memory",
    "gpu_wrk_util": "GPU",
    "cpu_usage": "CPU",
    "duration": "Duration",
}


@dataclass
class FeedbackConfig:
    """Tunable knobs of the online feedback loop."""
    estimator: str = "q05"              # see profiles.ESTIMATORS
    rmse_violation_threshold: float = 50.0
    violations_per_check: int = 1000    # ACQUIRES check cadence (in violations)
    recluster_below_score: float = 0.2  # re-run HDBSCAN below this ACQUIRES
    optimal_cluster_size: int = 26
    f1_score_threshold: float = 0.75    # warn when retrained XGBoost drops below
    initial_acquires_score: float = 0.717785
    metadata_categories: list = field(default_factory=lambda: list(METADATA_CATEGORIES))
    clustering_features: list = field(default_factory=lambda: list(CLUSTERING_FEATURES))
    resource_features: list = field(default_factory=lambda: list(RESOURCE_FEATURES))
