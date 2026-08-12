"""Raw Alibaba GPU-trace (cluster-trace-gpu-v2020) loading and merging.

Ported from the legacy utils.py; no import-time side effects, pandas-2.x safe,
and the trace location comes from config.TRACE_DIR (POLARIS_TRACE_DIR env var).
"""
from pathlib import Path

import numpy as np
import pandas as pd

from . import config

TRACE_TZ = "Asia/Shanghai"


def get_df(file, header=None) -> pd.DataFrame:
    """Read one trace CSV, taking column names from its .header sidecar file."""
    file = Path(file)
    df = pd.read_csv(file, header=None)
    if header is None:
        header = pd.read_csv(file.with_suffix("").with_suffix(".header")).columns \
            if file.suffix == ".csv" else None
    df.columns = header if header is not None else df.columns
    return df


def load_all_df(trace_dir=config.TRACE_DIR):
    """Load the seven trace tables: job, task, instance, sensor, group, spec, metric."""
    trace_dir = Path(trace_dir)
    names = ["pai_job_table", "pai_task_table", "pai_instance_table",
             "pai_sensor_table", "pai_group_tag_table", "pai_machine_spec",
             "pai_machine_metric"]
    return tuple(get_df(trace_dir / f"{n}.csv") for n in names)


def get_dfiw(dfi: pd.DataFrame) -> pd.DataFrame:
    """Deduplicate instances per worker and derive their runtime."""
    dfiw = dfi.sort_values(["status", "start_time", "end_time"]).copy()
    dfiw.drop_duplicates(subset=["worker_name"], keep="last", inplace=True)
    dfiw.dropna(subset=["worker_name"], inplace=True)
    valid = (dfiw.start_time > 0) & (dfiw.end_time > 0)
    dfiw["runtime"] = dfiw[valid]["end_time"] - dfiw[valid]["start_time"]
    dfiw.loc[dfiw.start_time == 0, ["start_time", "end_time"]] = np.nan
    return dfiw


def get_dfw(dfi, dft, dfg) -> pd.DataFrame:
    """Worker-level view: instances joined with tasks and group tags."""
    dfw = get_dfiw(dfi)
    dfw["start_date"] = dfw.start_time.apply(pd.Timestamp, unit="s", tz=TRACE_TZ)
    dfw = dfw.merge(dft, on=["job_name", "task_name"], how="left", suffixes=["", "_t"])
    dfw = dfw.merge(dfg, on="inst_id", how="left")
    dfw.loc[dfw.group.isnull(), "group"] = dfw.loc[dfw.group.isnull(), "user"]
    return dfw


def get_dfia(dfi: pd.DataFrame) -> pd.DataFrame:
    """Aggregate instances per (job, task): start, end, mean runtime, status."""
    dfi_s = dfi[dfi.start_time > 0][["job_name", "task_name", "start_time"]] \
        .groupby(["job_name", "task_name"]).min()
    dfi_e = dfi[dfi.end_time > 0][["job_name", "task_name", "end_time"]] \
        .groupby(["job_name", "task_name"]).max()
    dfi_m = dfi[(dfi.start_time > 0) & (dfi.end_time > 0)][
        ["job_name", "task_name", "end_time", "start_time"]].copy()
    dfi_m["runtime"] = dfi_m.end_time - dfi_m.start_time
    dfi_m = dfi_m.groupby(["job_name", "task_name"]).mean()[["runtime"]].reset_index()
    dfia = dfi[["job_name", "task_name", "status"]].drop_duplicates() \
        .groupby(["job_name", "task_name"]).max()
    for df in [dfi_s, dfi_e, dfi_m]:
        dfia = dfia.merge(df, on=["job_name", "task_name"], how="left")
    return dfia


def get_dfa(dft, dfj, dfi, dfg) -> pd.DataFrame:
    """Task-level view joined with jobs, instance aggregates, and group tags."""
    dfa = dft.merge(dfj, on=["job_name"], suffixes=["", "_j"])
    dfa.loc[dfa.start_time == 0, ["start_time", "end_time"]] = np.nan
    dfa["runtime"] = dfa.end_time - dfa.start_time
    dfa = dfa.merge(get_dfia(dfi), on=["job_name", "task_name"], suffixes=["", "_i"])
    dfa["duration_min"] = dfa.runtime_i / 60
    dfa["wait_time"] = dfa.start_time_i - dfa.start_time
    dfa["start_date"] = dfa.start_time.apply(pd.Timestamp, unit="s", tz=TRACE_TZ)
    dfa = dfa.merge(dfg[[c for c in dfg.columns if c != "user"]],
                    on="inst_id", how="left")
    dfa.loc[dfa.group.isnull(), "group"] = dfa.loc[dfa.group.isnull(), "user"]
    return dfa


def get_dfwitm(dfwit, csv_file) -> pd.DataFrame:
    """Join worker view with per-machine metrics exported to `csv_file`."""
    res_df = pd.read_csv(csv_file, index_col=0)
    keep = ~res_df.columns.isin(["start_time", "end_time", "machine"])
    return dfwit.merge(res_df.loc[:, keep], on="worker_name", how="left")


def add_hour_date(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure start_date/date/hour columns exist, deriving them if needed."""
    if "start_date" not in df:
        target_col = next((c for c in ("start_time_t", "start_time") if c in df), None)
        if target_col is None:
            raise KeyError("start_time / start_time_t not found in dataframe")
        df["start_date"] = df[target_col].apply(
            lambda x: pd.Timestamp(x, unit="s", tz=TRACE_TZ))
    if "date" not in df:
        df["date"] = df["start_date"].apply(lambda x: x.date())
    if "hour" not in df:
        df["hour"] = df["start_date"].apply(lambda x: x.hour)
    return df


def get_hourly_task_request(df: pd.DataFrame) -> pd.DataFrame:
    """Tasks submitted per hour, one row per day (days with gaps dropped)."""
    df = add_hour_date(df.copy())
    per_day = []
    for date in sorted(df.date.unique()):
        res = df[df.date == date].groupby("hour").count()[["job_name"]]
        per_day.append(res.rename(columns={"job_name": date}).T)
    return pd.concat(per_day).dropna()


def get_hourly_task_resource_request(df: pd.DataFrame, metrics: str = "cpu") -> pd.DataFrame:
    """Planned resource per hour, one row per day. metrics: cpu | gpu | mem."""
    df = add_hour_date(df.copy())
    scale = {"cpu": ("plan_cpu", 100), "gpu": ("plan_gpu", 100),
             "mem": ("plan_mem", 1000)}
    if metrics not in scale:
        raise ValueError(f"metrics must be one of {sorted(scale)}, got {metrics!r}")
    col, div = scale[metrics]
    df["plan_resource"] = df[col] / div
    per_day = []
    for date in sorted(df.date.unique()):
        # select before summing: pandas >= 2 refuses to sum datetime columns
        res = df[df.date == date].groupby("hour")[["plan_resource"]].sum()
        # the legacy code renamed a nonexistent 'job_name' column here, so
        # every row ended up labeled 'plan_resource' — label by date instead
        per_day.append(res.T.rename(index={"plan_resource": date}))
    return pd.concat(per_day).dropna()


def get_inst_task_num_ratio(dfa, inst_num_list=(2, 8, 20, 64, 100, 256, 512)) -> pd.DataFrame:
    """Share of tasks/instances covered by tasks with at least N instances."""
    total_tasks, total_insts = len(dfa), dfa["inst_num"].sum()
    rows = []
    for n in inst_num_list:
        subset = dfa[dfa["inst_num"] >= n]
        rows.append([len(subset) / total_tasks, subset["inst_num"].sum() / total_insts])
    out = pd.DataFrame(rows, columns=["num_task_ratio", "num_inst_ratio"])
    return out.T.rename(columns=dict(zip(range(len(inst_num_list)), inst_num_list)))


def plan_minus_usage_over_capacity(dfas: pd.DataFrame):
    """Per-task (plan - usage) / capacity for cpu/gpu/mem, split by sign."""
    dfas = dfas.copy()
    dfas["plan_gpu_minus_usage_over_capacity"] = \
        (dfas["plan_gpu"] - dfas["gpu_wrk_util"]) / (100 * dfas["cap_gpu"])
    dfas["plan_cpu_minus_usage_over_capacity"] = \
        (dfas["plan_cpu"] - dfas["cpu_usage"]) / (100 * dfas["cap_cpu"])
    dfas["plan_mem_minus_usage_over_capacity"] = \
        (dfas["plan_mem"] - dfas["avg_mem"]) / dfas["cap_mem"]

    cols = [f"plan_{d}_minus_usage_over_capacity" for d in ("gpu", "cpu", "mem")]
    per_task = dfas.groupby(["job_name", "task_name"])[cols].mean()

    over_data, over_label, under_data, under_label = [], [], [], []
    for device in ("cpu", "gpu", "mem"):
        col = f"plan_{device}_minus_usage_over_capacity"
        known = per_task[~per_task[col].isnull()]
        over = per_task[per_task[col] > 0]
        under = per_task[per_task[col] < 0]
        over_label.append("{} {:.2f}%".format(device, 100 * len(over) / len(known)))
        over_data.append(over[col])
        under_label.append("{} {:.2f}%".format(device, 100 * len(under) / len(known)))
        under_data.append(-under[col])
    return over_data, under_data, over_label, under_label
