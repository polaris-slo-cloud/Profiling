"""Service-layer tests with stubbed models — no artifacts needed."""
import json

import numpy as np
import pandas as pd
import pytest

fastapi = pytest.importorskip("fastapi")
from fastapi.testclient import TestClient

from polaris_profiler import config, service
from polaris_profiler.feedback import FeedbackLoop


class FakeClusterer:
    labels_ = np.array([0] * 5 + [1] * 5)


class FakeClassifier:
    def predict(self, X):
        return [0]


class FakeEncoder:
    def transform(self, X):
        return np.zeros((len(X), 3))


def _workload():
    return pd.DataFrame({
        "job_name": [f"j{i}" for i in range(10)],
        "user": ["u"] * 10,
        "task_name": ["t"] * 10,
        "group": ["g"] * 10,
        "workload": ["w"] * 10,
        "max_mem": np.linspace(1, 10, 10),
        "gpu_wrk_util": np.linspace(0, 1, 10),
        "cpu_usage": np.linspace(5, 9, 10),
        "duration": np.linspace(10, 100, 10),
    })


METADATA = {"job_name": "j-new", "user": "u", "task_name": "t",
            "group": "g", "workload": "w"}


@pytest.fixture()
def client():
    # inject a stub loop BEFORE the lifespan runs; state.load() then no-ops
    service.state.loop = FeedbackLoop(
        clusterer=FakeClusterer(), classifier=FakeClassifier(),
        encoder=FakeEncoder(), workload=_workload(),
        cfg=config.FeedbackConfig(estimator="median"),
    )
    service.state.load_error = None
    with TestClient(service.app) as c:
        yield c


def test_healthz(client):
    assert client.get("/healthz").json() == {"status": "ok"}


def test_readyz_when_loaded(client):
    assert client.get("/readyz").status_code == 200


def test_readyz_503_when_not_loaded(client):
    service.state.loop = None
    service.state.load_error = "boom"
    response = client.get("/readyz")
    assert response.status_code == 503
    assert "boom" in response.json()["detail"]


def test_predict_returns_profile_and_estimates(client):
    response = client.post("/predict", json=METADATA)
    assert response.status_code == 200
    body = response.json()
    assert body["profile"] == 0
    assert set(body["estimates"]) == set(config.RESOURCE_FEATURES)
    assert body["estimator"] == "median"


def test_observation_scores_and_grows_reference(client):
    n_before = len(service.state.loop.workload)
    observation = dict(METADATA, max_mem=3.0, gpu_wrk_util=0.2,
                       cpu_usage=6.0, duration=30.0)
    response = client.post("/observations", json=observation)
    assert response.status_code == 200
    body = response.json()
    assert body["profile"] == 0
    assert body["rmse_percentage_overall"] >= 0
    assert set(body["rmse_percentage_per_feature"]) == set(config.RESOURCE_FEATURES)
    assert len(service.state.loop.workload) == n_before + 1


def test_observation_validates_payload(client):
    assert client.post("/observations", json=METADATA).status_code == 422


def test_observation_rejects_nan_inf_negative_and_extra_fields(client):
    base = dict(METADATA, max_mem=1.0, gpu_wrk_util=0.1,
                cpu_usage=1.0, duration=10.0)
    for bad in (dict(base, max_mem=float("nan")),
                dict(base, duration=float("inf")),
                dict(base, cpu_usage=-1.0),
                dict(base, typo_field=1.0)):
        payload = json.dumps(bad)  # allows NaN/Infinity literals in the body
        response = client.post("/observations", content=payload,
                               headers={"Content-Type": "application/json"})
        assert response.status_code == 422, bad


def test_predict_rejects_extra_fields(client):
    assert client.post("/predict",
                       json=dict(METADATA, oops="x")).status_code == 422


def test_profiles_summary(client):
    body = client.get("/profiles").json()
    assert {p["profile"] for p in body} == {0, 1}
    assert all(p["size"] == 5 for p in body)


def test_metrics_exposed(client):
    client.post("/predict", json=METADATA)
    text = client.get("/metrics").text
    assert "polaris_predictions_total" in text
    assert "polaris_reference_workload_size" in text


def test_info(client):
    body = client.get("/info").json()
    assert body["estimator"] == "median"
    assert body["reference_rows"] == 10
