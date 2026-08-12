"""HTTP service exposing the profiler to other orchestration components.

Endpoints
---------
GET  /healthz       liveness (process is up)
GET  /readyz        readiness (models and reference data loaded)
GET  /info          version, estimator, dataset/profile counts
GET  /profiles      per-profile size and current resource estimates
POST /predict       metadata -> profile + resource estimates (read-only)
POST /observations  completed workload -> error record; feeds the loop
GET  /metrics       Prometheus metrics

Run locally:   uvicorn polaris_profiler.service:app
In the image:  the `service` Docker target starts this on :8080.

State model: the feedback loop lives in process memory and mutates on every
observation, so run ONE replica AND one uvicorn worker per profile store
(`--workers N` would give each worker a divergent loop and a separate
Prometheus registry). A lock serializes mutations. Artifacts load in a
background thread with retries, so the socket binds immediately and
/readyz flips to 200 once loading succeeds.
"""
import logging
import math
import os
import threading

from contextlib import asynccontextmanager

import pandas as pd
from fastapi import FastAPI, HTTPException, Response
from fastapi.exceptions import RequestValidationError
from fastapi.responses import JSONResponse
from prometheus_client import (CONTENT_TYPE_LATEST, Counter, Gauge, Histogram,
                               generate_latest)
from pydantic import BaseModel, ConfigDict, Field

from . import __version__, artifacts, config, data
from .feedback import FeedbackLoop

logger = logging.getLogger(__name__)

LOAD_RETRY_SECONDS = 15

# --- Prometheus metrics ------------------------------------------------------
PREDICTIONS = Counter("polaris_predictions_total",
                      "Profile predictions served")
OBSERVATIONS = Counter("polaris_observations_total",
                       "Completed workloads folded into the reference dataset")
VIOLATIONS = Counter("polaris_violations_total",
                     "Observations whose overall RMSE percentage exceeded the threshold")
RECLUSTERINGS = Counter("polaris_reclusterings_total",
                        "Times HDBSCAN was re-run after quality degradation")
RMSE_OVERALL = Histogram("polaris_rmse_percentage",
                         "Overall RMSE percentage of each observation",
                         buckets=(1, 5, 10, 25, 50, 100, 250, 1000, float("inf")))
WORKLOAD_SIZE = Gauge("polaris_reference_workload_size",
                      "Rows in the reference dataset")
PROFILE_COUNT = Gauge("polaris_profile_count",
                      "Distinct non-outlier profiles")


# --- Request/response schemas ------------------------------------------------
_finite = dict(allow_inf_nan=False)


class WorkloadMetadata(BaseModel):
    """The metadata categories the profile classifier was trained on."""
    model_config = ConfigDict(extra="forbid")

    job_name: str
    user: str
    task_name: str
    group: str
    workload: str

    def as_row(self):
        return [getattr(self, f) for f in config.METADATA_CATEGORIES]


class Prediction(BaseModel):
    profile: int
    estimates: dict
    estimator: str


class Observation(WorkloadMetadata):
    """A completed workload: metadata plus observed resource usage.

    The optional fields cover the remaining clustering features; providing
    them makes the folded observation more useful to future re-clustering.
    """
    max_mem: float = Field(ge=0, **_finite)
    gpu_wrk_util: float = Field(ge=0, **_finite)
    cpu_usage: float = Field(ge=0, **_finite)
    duration: float = Field(gt=0, **_finite)
    avg_mem: float | None = Field(default=None, ge=0, **_finite)
    avg_gpu_wrk_mem: float | None = Field(default=None, ge=0, **_finite)
    max_gpu_wrk_mem: float | None = Field(default=None, ge=0, **_finite)
    read: float | None = Field(default=None, ge=0, **_finite)
    write: float | None = Field(default=None, ge=0, **_finite)
    read_count: float | None = Field(default=None, ge=0, **_finite)
    write_count: float | None = Field(default=None, ge=0, **_finite)


class ObservationRecord(BaseModel):
    profile: int
    rmse_percentage_overall: float
    rmse_percentage_per_feature: dict
    violation: bool
    n_violations: int
    n_reclusterings: int


# --- Application --------------------------------------------------------------
class ProfilerState:
    """Holds the feedback loop and a lock serializing mutations."""

    def __init__(self):
        self.loop: FeedbackLoop | None = None
        self.lock = threading.Lock()
        self.load_error: str | None = None
        self._loader: threading.Thread | None = None
        self._stop_loading = threading.Event()

    def load(self) -> None:
        """One artifact-load attempt (idempotent when already loaded)."""
        if self.loop is not None:
            return
        estimator = os.environ.get("POLARIS_ESTIMATOR", "q05")
        try:
            loop = FeedbackLoop(
                clusterer=artifacts.load_clusterer(),
                classifier=artifacts.load_classifier(),
                encoder=artifacts.load_encoder(),
                workload=data.load_sampled_workload(),
                cfg=config.FeedbackConfig(estimator=estimator),
            )
            self.loop = loop
            self.load_error = None
            self._refresh_gauges()
            logger.info("profiler ready: %d reference rows, estimator=%s",
                        len(loop.workload), estimator)
        except Exception as exc:  # keep serving /healthz; /readyz reports it
            self.load_error = f"{type(exc).__name__}: {exc}"
            logger.exception("failed to load models/reference data")

    def start_loading(self) -> None:
        """Load in a background thread, retrying transient failures (slow or
        briefly unavailable volume mounts) so a bad first attempt does not
        brick the pod until a human deletes it."""
        if self.loop is not None or (self._loader and self._loader.is_alive()):
            return

        def _run():
            while self.loop is None and not self._stop_loading.is_set():
                self.load()
                if self.loop is None:
                    self._stop_loading.wait(LOAD_RETRY_SECONDS)

        self._stop_loading.clear()
        self._loader = threading.Thread(target=_run, name="artifact-loader",
                                        daemon=True)
        self._loader.start()

    def stop_loading(self) -> None:
        self._stop_loading.set()

    def _refresh_gauges(self) -> None:
        WORKLOAD_SIZE.set(len(self.loop.workload))
        labels = self.loop.workload["profile"].dropna()
        PROFILE_COUNT.set(labels[labels >= 0].nunique())


state = ProfilerState()


@asynccontextmanager
async def lifespan(app: FastAPI):
    state.start_loading()
    yield
    state.stop_loading()


app = FastAPI(title="PolarisProfiler", version=__version__, lifespan=lifespan)


def _json_safe(value):
    """Replace non-finite floats so the 422 body itself stays valid JSON."""
    if isinstance(value, float) and not math.isfinite(value):
        return repr(value)
    if isinstance(value, dict):
        return {k: _json_safe(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_safe(v) for v in value]
    return value


@app.exception_handler(RequestValidationError)
async def _validation_error(request, exc: RequestValidationError):
    # a NaN/Infinity in the request body is echoed inside the error detail;
    # without sanitizing, serializing the 422 response would itself fail
    return JSONResponse(status_code=422,
                        content={"detail": _json_safe(exc.errors())})


def _loop() -> FeedbackLoop:
    if state.loop is None:
        raise HTTPException(status_code=503,
                            detail=state.load_error or "profiler still loading")
    return state.loop


# probes are async so they never wait on the shared threadpool: a slow
# re-clustering that queues up threadpool workers must not fail liveness
@app.get("/healthz")
async def healthz():
    return {"status": "ok"}


@app.get("/readyz")
async def readyz():
    _loop()
    return {"status": "ready"}


@app.get("/info")
def info():
    loop = _loop()
    with state.lock:
        return {
            "version": __version__,
            "estimator": loop.cfg.estimator,
            "reference_rows": int(len(loop.workload)),
            "n_violations": loop.n_violations,
            "n_reclusterings": loop.n_reclusterings,
            "best_acquires_score": loop.best_acquires_score,
        }


@app.get("/profiles")
def profiles():
    with state.lock:
        return _loop().profile_summary()


@app.post("/predict", response_model=Prediction)
def predict(metadata: WorkloadMetadata):
    loop = _loop()
    with state.lock:
        label, estimates = loop.predict(metadata.as_row())
    if any(pd.isna(v) for v in estimates.values()):
        raise HTTPException(status_code=409,
                            detail=f"profile {label} has no reference data yet")
    PREDICTIONS.inc()
    return Prediction(profile=label, estimates=estimates,
                      estimator=loop.cfg.estimator)


@app.post("/observations", response_model=ObservationRecord)
def observe(observation: Observation):
    loop = _loop()
    row = pd.Series(observation.model_dump(exclude_none=True))
    with state.lock:
        reclusterings_before = loop.n_reclusterings
        try:
            record = loop.observe(row)
        finally:
            # even a failed observation may have mutated loop state
            state._refresh_gauges()
        # snapshot + metric updates inside the lock so concurrent
        # observations cannot interleave between state and metrics
        snapshot_violations = loop.n_violations
        snapshot_reclusterings = loop.n_reclusterings
        OBSERVATIONS.inc()
        RMSE_OVERALL.observe(record["Overall"])
        if record["violation"]:
            VIOLATIONS.inc()
        if snapshot_reclusterings > reclusterings_before:
            RECLUSTERINGS.inc(snapshot_reclusterings - reclusterings_before)
    per_feature = {f: record[config.RESOURCE_LABELS[f]]
                   for f in loop.cfg.resource_features}
    return ObservationRecord(
        profile=record["Profile"],
        rmse_percentage_overall=record["Overall"],
        rmse_percentage_per_feature=per_feature,
        violation=record["violation"],
        n_violations=snapshot_violations,
        n_reclusterings=snapshot_reclusterings,
    )


@app.get("/metrics")
def metrics():
    return Response(generate_latest(), media_type=CONTENT_TYPE_LATEST)
