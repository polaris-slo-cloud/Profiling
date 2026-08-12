# syntax=docker/dockerfile:1
# Multi-stage build for PolarisProfiler.
#
#   docker build -t polaris-profiler .                      # HTTP service (default)
#   docker build --target runtime  -t polaris-profiler-cli .  # CLI image
#   docker build --target test     -t pp-test .               # runs unit tests
#   docker build --target notebook -t pp-notebook .           # JupyterLab
#
# Data, models, and results are NOT baked into the image — mount them
# (see docker-compose.yml / deploy/kubernetes/). The image only carries
# code + dependencies.

ARG PYTHON_VERSION=3.11

# --- base: shared runtime libraries ----------------------------------------
FROM python:${PYTHON_VERSION}-slim AS base
# libgomp1: OpenMP runtime required by xgboost and hdbscan wheels
RUN apt-get update \
    && apt-get install -y --no-install-recommends libgomp1 \
    && rm -rf /var/lib/apt/lists/*
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    # headless matplotlib; no TeX in the image, so disable usetex styling
    MPLBACKEND=Agg \
    MPLCONFIGDIR=/tmp/matplotlib \
    POLARIS_USETEX=0 \
    # canonical mount points for artifacts (see docker-compose.yml)
    POLARIS_DATA_DIR=/data \
    POLARIS_MODELS_DIR=/models \
    POLARIS_RESULTS_DIR=/results \
    POLARIS_FIGURES_DIR=/figures

# --- builder: compile/install the package and its dependencies -------------
FROM base AS builder
# toolchain only needed if a dependency (e.g. hdbscan) lacks a wheel here
RUN apt-get update \
    && apt-get install -y --no-install-recommends build-essential \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY pyproject.toml README.md ./
COPY polaris_profiler ./polaris_profiler
RUN pip install --no-cache-dir --prefix=/install ".[service]"

# --- test: run the unit suite (docker build --target test .) ----------------
FROM base AS test
COPY --from=builder /install /usr/local
WORKDIR /src
COPY tests ./tests
RUN pip install --no-cache-dir pytest httpx && pytest tests -q

# --- runtime: CLI image ------------------------------------------------------
FROM base AS runtime
COPY --from=builder /install /usr/local

RUN useradd --create-home --uid 1000 polaris \
    && mkdir -p /data /models /results /figures /tmp/matplotlib \
    && chown -R polaris:polaris /results /figures /tmp/matplotlib
USER polaris
WORKDIR /home/polaris

ENTRYPOINT ["polaris-feedback-loop"]
CMD ["--help"]

# --- notebook: JupyterLab for interactive work ------------------------------
FROM runtime AS notebook
USER root
RUN pip install --no-cache-dir jupyterlab && mkdir -p /home/polaris/notebooks \
    && chown -R polaris:polaris /home/polaris
USER polaris
EXPOSE 8888
# token auth stays enabled; the login token is printed in the container log
ENTRYPOINT ["jupyter", "lab", "--ip=0.0.0.0", "--no-browser", "--notebook-dir=/home/polaris"]
CMD []

# --- service: HTTP API (default target — keep this stage LAST) ---------------
FROM runtime AS service
EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=3s --start-period=30s --retries=3 \
    CMD ["python", "-c", "import urllib.request,sys; sys.exit(0 if urllib.request.urlopen('http://127.0.0.1:8080/healthz', timeout=2).status == 200 else 1)"]
# SINGLE worker only: the feedback-loop state and the Prometheus registry are
# per-process, so `--workers N` gives N divergent profilers behind one port.
ENTRYPOINT ["uvicorn", "polaris_profiler.service:app", "--host", "0.0.0.0", "--port", "8080", "--workers", "1"]
CMD []
