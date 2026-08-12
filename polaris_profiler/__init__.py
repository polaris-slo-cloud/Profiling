"""PolarisProfiler — ML workload profiling on GPU cluster traces.

Reusable library extracted from the original research scripts
(`feedback_loop.py`, `plot-features-distribution.py`, `utils.py`).

Modules
-------
config    : paths, feature groups, and feedback-loop thresholds
data      : loading of the sampled workload datasets
trace     : raw Alibaba GPU-trace loading/merging helpers
artifacts : persisted model loading/saving (HDBSCAN, XGBoost, encoder)
profiles  : profile prediction and per-profile resource estimation
metrics   : RMSE-percentage metrics and the ACQUIRES score
feedback  : the online feedback loop (violations -> re-cluster -> retrain)
plotting  : all figure generation (CDFs, skewness bars, RMSE plots)
"""

__version__ = "0.1.0"
