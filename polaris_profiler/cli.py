"""Command-line entry points, installed as console scripts:

    polaris-feedback-loop   run the online profiling feedback loop
    polaris-plot-features   per-profile skewness figures + per-feature RMSE
"""
import argparse
import logging
from pathlib import Path

from . import artifacts, config, data, plotting
from .feedback import FeedbackLoop
from .profiles import ESTIMATORS, compute_skewness_by_profile


def _common_parser(description: str) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("--train", type=Path, default=config.TRAIN_DATA,
                        help="sampled workload CSV used as the reference dataset")
    parser.add_argument("--test", type=Path, default=config.TEST_DATA,
                        help="test workload CSV streamed through the loop")
    parser.add_argument("--estimator", choices=sorted(ESTIMATORS), default="q05",
                        help="per-profile resource estimator")
    parser.add_argument("--out-dir", type=Path, default=config.RESULTS_DIR,
                        help="where result CSVs are written")
    parser.add_argument("--fig-dir", type=Path, default=config.FIGURES_DIR,
                        help="where figures are written")
    parser.add_argument("--log-file", type=Path, default=None,
                        help="log destination (default: <out-dir>/feedback_loop.log)")
    return parser


def _setup(args) -> None:
    args.out_dir.mkdir(parents=True, exist_ok=True)
    Path(args.fig_dir).mkdir(parents=True, exist_ok=True)
    log_file = args.log_file or args.out_dir / "feedback_loop.log"
    logging.basicConfig(level=logging.INFO,
                        format="%(asctime)s - %(levelname)s - %(message)s",
                        filename=log_file, filemode="w")


def _build_loop(args, workload=None) -> FeedbackLoop:
    return FeedbackLoop(
        clusterer=artifacts.load_clusterer(),
        classifier=artifacts.load_classifier(),
        encoder=artifacts.load_encoder(),
        workload=workload if workload is not None
        else data.load_sampled_workload(args.train),
        cfg=config.FeedbackConfig(estimator=args.estimator),
    )


def run_feedback_loop(argv=None) -> None:
    """Run the feedback loop and export overall-RMSE results (replaces feedback_loop.py)."""
    args = _common_parser(run_feedback_loop.__doc__).parse_args(argv)
    _setup(args)

    loop = _build_loop(args)
    result = loop.run(data.load_test_workload(args.test))

    csv_path = plotting.uniquify(args.out_dir / "feedback_loop_rmse.csv")
    result.per_workload.to_csv(csv_path)
    plotting.plot_rmse_cdf(
        result.per_workload["Overall"],
        plotting.uniquify(Path(args.fig_dir) / "overall_RMSE_perc_cdf.pdf"),
        threshold=loop.cfg.rmse_violation_threshold)

    print(f"results written to {csv_path}")
    print(f"number of violations: {result.n_violations}")
    print(f"number of reclusterings: {result.n_reclusterings}")


def plot_feature_distributions(argv=None) -> None:
    """Per-profile skewness figures and per-feature RMSE evaluation
    (replaces plot-features-distribution.py)."""
    args = _common_parser(plot_feature_distributions.__doc__).parse_args(argv)
    _setup(args)

    workload = data.load_sampled_workload(args.train)
    clusterer = artifacts.load_clusterer()
    workload["profile"] = clusterer.labels_

    plotting.plot_skewness_bars(compute_skewness_by_profile(workload), args.fig_dir)

    loop = _build_loop(args, workload=workload)
    result = loop.run(data.load_test_workload(args.test))

    csv_path = plotting.uniquify(args.out_dir / "features_rmse.csv")
    result.per_workload.to_csv(csv_path)
    plotting.plot_rmse_cdf(
        result.per_workload["Overall"],
        plotting.uniquify(Path(args.fig_dir) / "overall_RMSE_perc_cdf.pdf"))

    print(f"results written to {csv_path}")
    print(f"number of violations: {result.n_violations}")


if __name__ == "__main__":
    run_feedback_loop()
