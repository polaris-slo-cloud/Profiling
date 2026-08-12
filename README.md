# PolarisProfiler

Metadata-based profiling for cluster scheduling: predict a workload's resource behavior **before it runs**, 
from the static metadata it arrives with (user, job name, task, model type). 
Workloads are clustered into *profiles* with HDBSCAN from historical runtime traces, new workloads are assigned a
profile from their metadata with an XGBoost classifier, and the profile's history predicts their resource usage (memory, GPU, CPU, duration). 
An online feedback loop monitors prediction error and re-clusters/retrains when quality degrades.

Validated on two public traces: 
1. The [Alibaba PAI GPU trace](https://github.com/alibaba/clusterdata)
(cluster-trace-gpu-v2020) 
2. The [Google cluster data](https://github.com/google/cluster-data).

## Papers

- **Formal and Empirical Study of Metadata-Based Profiling for Resource
  Management in the Computing Continuum.**
  A. Morichetta, S. Nastic, V. Casamayor Pujol, S. Dustdar.
  *ACM Transactions on Internet Technology*, accepted 2026 (in press).
  Preprint: [arXiv:2504.20740](https://arxiv.org/abs/2504.20740)
- **PolarisProfiler: A Novel Metadata-Based Profiling Approach for
  Optimizing Resource Management in the Edge-Cloud Continuum.**
  A. Morichetta, V. Casamayor Pujol, S. Nastic, S. Dustdar, D. Vij,
  Y. Xiong, Z. Zhang. *IEEE SOSE 2023*.


## Repository layout

```
polaris_profiler/        reusable library (config, data, trace, profiles,
                         metrics, feedback, plotting, artifacts, service)
case-studies/alibaba/    Alibaba PAI case study (TOIT paper, Sec. 3):
                         notebooks 01-sampling ... 05-final-plots,
                         plus experiment result CSVs
case-studies/google/     Google cluster data extension (TOIT paper, Sec. 4):
                         DBSCAN + autoencoder pipeline notebooks
scripts/                 command-line entry points
tests/                   unit tests for the pure logic (pytest)
deploy/kubernetes/       manifests for running the profiler as a service
results/, figures/       runtime output (created on demand)
```

Note on data: the sampled datasets used in the paper (e.g., the 100,001-job Alibaba sample) are larger than what belongs in git (~55–65 MB each).
They will be published with a DOI; until then, open an issue or contact the authors to obtain them.

## Setup

```bash
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev]"
```

## Usage

Run the feedback loop (the second phase described in the paper):

```bash
python scripts/run_feedback_loop.py --estimator q05
```

Generate per-profile skewness figures and per-feature RMSE results:

```bash
python scripts/plot_feature_distributions.py
```

Estimator strategies (`--estimator`): `mean`, `median`, `q05` (5th quantile,
default), `iqr`, `skew-adaptive` (quantile chosen from each feature's
skewness).

From a notebook, import the library instead of copy-pasting helpers:

```python
from polaris_profiler import artifacts, data, profiles, plotting
workload = data.load_sampled_workload()
workload["profile"] = artifacts.load_clusterer().labels_
plotting.plot_skewness_bars(profiles.compute_skewness_by_profile(workload), "figures/")
```

Paths resolve automatically and can be overridden with `POLARIS_DATA_DIR`,
`POLARIS_MODELS_DIR`, `POLARIS_FIGURES_DIR`, `POLARIS_RESULTS_DIR`,
`POLARIS_TRACE_DIR`.

## HTTP service

The profiler runs as a service for orchestration environments: other
components (schedulers, controllers) POST workload metadata to `/predict`
at schedule time and POST observed usage to `/observations` at completion
time, which drives the online feedback loop (violation counting, ACQUIRES
re-checks, re-clustering/retraining). Probes at `/healthz` and `/readyz`,
Prometheus metrics at `/metrics`.

```bash
make api            # service on http://localhost:8080 (docker compose up -d api)
curl -X POST localhost:8080/predict -H 'Content-Type: application/json' \
  -d '{"job_name":"j1","user":"u1","task_name":"t1","group":"g1","workload":"w1"}'
```

Locally without Docker: `pip install -e ".[service]"` then
`uvicorn polaris_profiler.service:app`. The estimator is selected with
`POLARIS_ESTIMATOR` (see `.env.example`). Kubernetes manifests live in
[deploy/kubernetes](deploy/kubernetes/README.md) (single replica and single
uvicorn worker — the feedback state is in-process; see the scaling caveat
there).

## Docker

The image carries only code and dependencies; data, models, results, and
figures are mounted at `/data`, `/models`, `/results`, `/figures`
(host locations configurable via `.env` — see `.env.example`).

```bash
make build          # service image  (docker build -t polaris-profiler .)
make build-cli      # CLI image      (docker build --target runtime ...)
make test           # unit tests inside the image (build --target test)
make run            # feedback loop:  docker compose run --rm profiler
make plots          # figures:        docker compose run --rm plots
make notebook       # JupyterLab on http://localhost:8888
```

Pass CLI flags straight through Compose, e.g.
`docker compose run --rm profiler --estimator skew-adaptive`.
Inside the container LaTeX figure styling is disabled (`POLARIS_USETEX=0`);
figures fall back to a serif font chain rendered by mathtext.

## Tests

```bash
pytest                # locally
make test             # inside the container
```

## Citation

```bibtex
@article{morichetta2026metadata,
  author  = {Morichetta, Andrea and Nastic, Stefan and Casamayor Pujol, Victor and Dustdar, Schahram},
  title   = {Formal and Empirical Study of Metadata-Based Profiling for Resource Management in the Computing Continuum},
  journal = {ACM Transactions on Internet Technology},
  year    = {2026},
  note    = {In press. Preprint: arXiv:2504.20740}
}

@inproceedings{morichetta2023polarisprofiler,
  author    = {Morichetta, Andrea and Casamayor Pujol, Victor and Nastic, Stefan and Dustdar, Schahram and Vij, Deepak and Xiong, Ying and Zhang, Zhaobo},
  title     = {PolarisProfiler: A Novel Metadata-Based Profiling Approach for Optimizing Resource Management in the Edge-Cloud Continuum},
  booktitle = {2023 IEEE International Conference on Service-Oriented System Engineering (SOSE)},
  year      = {2023},
  pages     = {27--36}
}
```

## License

Apache 2.0 — see [LICENSE](LICENSE).
