"""Loading of the sampled workload datasets exported by the EDA notebooks."""
from pathlib import Path

import pandas as pd

from . import config


def load_sampled_workload(path: Path = config.TRAIN_DATA) -> pd.DataFrame:
    """Load a sampled workload CSV (index in first column, spurious 'index' dropped)."""
    data = pd.read_csv(path, index_col=0)
    if "index" in data.columns:
        data = data.drop(columns="index")
    return data


def load_test_workload(path: Path = config.TEST_DATA) -> pd.DataFrame:
    """Load a test workload CSV and derive the duration from start/end timestamps."""
    data = load_sampled_workload(path)
    data["duration"] = data.end_time - data.start_time
    return data
