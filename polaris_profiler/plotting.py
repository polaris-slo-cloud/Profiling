"""All figure generation, consolidated from utils.py, feedback_loop.py and
plot-features-distribution.py.

Figures are written wherever the caller says (`save_path`); nothing is
hard-coded to /tmp or a paper-specific folder anymore.
"""
import os
from contextlib import contextmanager
from pathlib import Path
from typing import Dict, List, Optional, Sequence

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.patches import Ellipse
from matplotlib.ticker import LogLocator, MultipleLocator, NullFormatter
from statsmodels.distributions.empirical_distribution import ECDF

# Paper color scheme for the four predicted resources.
RESOURCE_COLORS = {
    "cpu": "#1F4EBA",
    "gpu": "#5B8E4D",
    "memory": "#D94D47",
    "duration": "#EBC151",
}

LINESTYLES = [
    "solid", "dotted", "dashed", "dashdot",
    (0, (3, 1, 1, 1, 1, 1)), (0, (3, 1, 1, 1)), (0, (1, 1)), (0, (5, 1)),
    (0, (3, 5, 1, 5, 1, 5)), (0, (5, 10)), (0, (3, 10, 1, 10)),
    (0, (3, 10, 1, 10, 1, 10)), (0, (1, 10)), (0, (5, 5)), (0, (3, 5, 1, 5)),
]


@contextmanager
def paper_style(usetex: bool = None):
    """Temporarily switch matplotlib to the paper's LaTeX/Times style.

    LaTeX rendering needs a TeX installation; environments without one
    (e.g. the Docker image) set POLARIS_USETEX=0 to fall back to mathtext.
    """
    if usetex is None:
        usetex = os.environ.get("POLARIS_USETEX", "1") != "0"
    rc = {"text.usetex": usetex}
    if usetex:
        rc["font.family"] = "Times"
    else:
        # serif fallback chain: first Times-like font present wins,
        # DejaVu Serif ships with matplotlib so there is always a hit
        rc["font.family"] = "serif"
        rc["font.serif"] = ["Times", "Times New Roman", "Nimbus Roman",
                            "Liberation Serif", "DejaVu Serif"]
    with plt.rc_context(rc):
        yield


def uniquify(path) -> Path:
    """Return `path`, or `path (n)` if it already exists — never overwrites."""
    path = Path(path)
    candidate, counter = path, 1
    while candidate.exists():
        candidate = path.with_name(f"{path.stem} ({counter}){path.suffix}")
        counter += 1
    return candidate


def _finish(save_path=None) -> None:
    if save_path is not None:
        Path(save_path).parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(save_path, bbox_inches="tight")
        plt.close()
    else:
        plt.show()


# --- Generic CDF plots (from utils.py) --------------------------------------

def get_cdf(data, inverse: bool = False):
    sorted_data = sorted(data)
    p = 100.0 * np.arange(len(sorted_data)) / (len(sorted_data) - 1)
    return sorted_data, (100.0 - p if inverse else p)


def plot_cdf(data, inverse=False, datalabel=None, xlabel=None, ylabel=None,
             title=None, xlog=False, ylog=False, xlim=None, xticks=None,
             figsize=(4, 3), dpi=120, save_path=None):
    """CDF/CCDF of one series."""
    plot_cdfs([data], datalabel=[datalabel] if datalabel else None,
              inverse=inverse, xlabel=xlabel, ylabel=ylabel, title=title,
              xlog=xlog, ylog=ylog, xlim=xlim, xticks=xticks,
              figsize=figsize, dpi=dpi, save_path=save_path)


def plot_cdfs(data: Sequence, datalabel: Optional[List[str]] = None,
              inverse=False, xlabel=None, ylabel=None, title=None,
              xlog=False, ylog=False, xlim=None, ylim=None,
              xticks=None, yticks=None, figsize=(4, 3), dpi=120,
              save_path=None, loc="best", fontsize=None):
    """CDFs/CCDFs of several series on one axis."""
    plt.figure(figsize=figsize, dpi=dpi)
    for i, d in enumerate(data):
        d = pd.Series(d).dropna()
        x, y = get_cdf(d, inverse)
        label = datalabel[i] if datalabel is not None else None
        plt.plot(x, y, label=label, linestyle=LINESTYLES[i % len(LINESTYLES)])
    if datalabel is not None:
        plt.legend(loc=loc, fontsize=fontsize)
    if xlog:
        plt.xscale("log")
    if ylog:
        plt.yscale("log")
    plt.ylim(ylim if ylim is not None else (0, 100))
    if xlim is not None:
        plt.xlim(xlim)
    if xlabel is not None:
        plt.xlabel(xlabel)
    plt.ylabel(ylabel if ylabel is not None else ("CCDF" if inverse else "CDF"))
    if title is not None:
        plt.title(title)
    if xticks is not None:
        plt.xticks(xticks)
    if yticks is not None:
        plt.yticks(yticks)
    plt.grid(alpha=0.3, linestyle="--")
    _finish(save_path)


def draw_bar_plot(odf: pd.DataFrame, col: str, figsize=(4, 4), dpi=120,
                  portion=False, limit=30) -> pd.DataFrame:
    """Horizontal bar plot of the most frequent values of `col`."""
    dfout = (odf.reset_index().groupby(col).count()[["index"]]
             .sort_values("index", ascending=False).head(limit))
    dfout["portion"] = 100 * dfout["index"] / dfout["index"].sum()
    plt.figure(figsize=figsize, dpi=dpi)
    if portion:
        plt.barh(y=dfout.index, width=dfout["portion"])
        plt.xlabel("Percentage (total: %.2f)" % dfout["index"].sum())
    else:
        plt.barh(y=dfout.index, width=dfout["index"])
    plt.grid(alpha=0.3, linestyle="--")
    return dfout


# --- Profiling result plots -------------------------------------------------

def plot_skewness_bars(skewness_by_profile: Dict[int, Dict[str, float]],
                       out_dir, prefix: str = "skewness",
                       sort_values: bool = True, mean_line: bool = True):
    """One bar chart per resource: skewness of that resource across profiles.

    Replaces the four copy-pasted CPU/GPU/memory/duration blocks in
    plot-features-distribution.py.
    """
    df = pd.DataFrame(skewness_by_profile).astype(float)
    mean_skews = df.mean(axis=1)
    feature_keys = {  # skew-dict key -> (short name, color)
        "cpu_usage": ("cpu", RESOURCE_COLORS["cpu"]),
        "gpu_wrk_util": ("gpu", RESOURCE_COLORS["gpu"]),
        "max_mem": ("memory", RESOURCE_COLORS["memory"]),
        "duration": ("duration", RESOURCE_COLORS["duration"]),
    }
    out_dir = Path(out_dir)
    with paper_style():
        for feature in df.index:
            key, color = feature_keys.get(feature, (feature, None))
            plt.subplots(figsize=(12, 8))
            series = df.loc[feature]
            series = series.sort_values() if sort_values else series
            series.plot(kind="bar", color=color, label="_nolegend_")
            if mean_line:
                plt.axhline(mean_skews[feature], color="gray",
                            linestyle="dashed", linewidth=2, label="Mean")
                plt.legend(fontsize=27)
            plt.xlabel(r"Profiles", fontsize=50)
            plt.ylabel(r"Skewness values", fontsize=50)
            plt.tick_params(which="major", width=1, length=7, labelsize=31)
            plt.tick_params(which="minor", width=1, length=4)
            plt.xlim(-0.5, len(series) - 0.5)
            plt.tight_layout()
            _finish(out_dir / f"{prefix}-{key}.pdf")


def plot_rmse_cdf(rmse_values: Sequence[float], save_path,
                  threshold: float = 50.0, color: str = "#AD5689"):
    """Log-x CDF of RMSE-percentage values, shading the area below `threshold`."""
    rmse_values = [v for v in rmse_values if v > 0]
    ecdf = ECDF(rmse_values)
    x_pos, y_pos = ecdf.x[ecdf.x > 0], ecdf.y[ecdf.x > 0]
    x = np.logspace(np.log10(x_pos.min()), np.log10(x_pos.max()), 1000)
    y = np.interp(np.log10(x), np.log10(x_pos), y_pos)

    plt.figure(figsize=(12, 8))
    with paper_style():
        plt.plot(x, y, color=color, linewidth=3)
        plt.axvline(x=threshold, color="black", linewidth=3, linestyle="--")
        plt.fill_between(x, y, where=(x <= threshold), interpolate=True,
                         color=color, alpha=0.5)
        plt.grid(axis="both", which="major", color="k", alpha=0.7, linestyle="-")
        plt.grid(axis="y", which="minor", color="grey", linestyle="--")
        plt.xlabel(r"$RMSE_{perc}$ values", fontsize=50)
        plt.ylabel(r"CDF", fontsize=50)
        plt.tick_params(which="major", width=1, length=7, labelsize=35)
        plt.tick_params(which="minor", width=1, length=4)
        plt.xscale("log")
        plt.tight_layout()
        _finish(save_path)


def plot_rmse_boxplot_by_profile(predictions: Sequence, rmse_values: Sequence[float],
                                 save_path, good_zone: float = 100.0):
    """Boxplot of RMSE% per predicted profile; outliers beyond `good_zone` circled."""
    import seaborn as sns

    order = [str(x) for x in sorted(set(predictions))]
    df_rmse = pd.DataFrame(dict(profile=predictions, rmse_perc=rmse_values))
    df_rmse.profile = df_rmse.profile.astype("string")

    fig, ax = plt.subplots(figsize=(14, 10))
    with paper_style():
        plt.axvline(good_zone, color="green")
        plt.axvline(5, color="green", linestyle="--", linewidth=1)
        plt.fill_between(range(int(good_zone)), [len(order) + 10] * int(good_zone),
                         color="green", alpha=0.1)
        sns.boxplot(y="profile", x="rmse_perc", data=df_rmse, order=order)
        for profile, value in df_rmse[df_rmse.rmse_perc > good_zone].values:
            ax.add_patch(Ellipse((value, order.index(profile)), 30, 1,
                                 edgecolor="red", linewidth=2, facecolor="white"))
        ax.xaxis.set_major_locator(MultipleLocator(200))
        ax.xaxis.set_major_formatter("{x:.0f}")
        ax.xaxis.set_minor_locator(MultipleLocator(100))
        ax.xaxis.set_minor_formatter("{x:.0f}")
        plt.grid(axis="x", which="major", color="k", alpha=0.7, linestyle="-")
        plt.grid(axis="x", which="minor", color="grey", linestyle="--")
        plt.xlabel(r"$RMSE_{perc}$ values", fontsize=50)
        plt.ylabel(r"Profiles", fontsize=50)
        plt.tick_params(which="major", width=1, length=7, labelsize=35)
        plt.tick_params(which="minor", width=1, length=4, labelsize=25)
        _finish(save_path)
