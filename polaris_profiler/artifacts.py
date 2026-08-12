"""Loading and saving of persisted models: HDBSCAN, XGBoost, one-hot encoder."""
import pickle
from pathlib import Path

import joblib
import xgboost as xgb

from . import config


def load_clusterer(path: Path = config.HDBSCAN_MODEL):
    """Load the fitted HDBSCAN clusterer."""
    with open(path, "rb") as f:
        return pickle.load(f)


def load_classifier(path: Path = config.XGBOOST_MODEL) -> xgb.XGBClassifier:
    """Load the XGBoost profile classifier."""
    model = xgb.XGBClassifier()
    model.load_model(path)
    return model


def load_encoder(path: Path = config.ONEHOT_ENCODER):
    """Load the fitted OneHotEncoder for the metadata categories."""
    return joblib.load(path)


def save_clusterer(clusterer, path: Path) -> None:
    with open(path, "wb") as f:
        pickle.dump(clusterer, f)


def save_classifier(model: xgb.XGBClassifier, path: Path) -> None:
    model.save_model(path)
