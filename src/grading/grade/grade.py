#!/usr/bin/env python3
"""Run the Hazel batch grader on a single submission JSON and write a report.

The Hazel grader is a js_of_ocaml binary (gradingReport.bc.js) that consumes a
submission JSON and emits a JSON array of per-exercise sections with shape::

    [{"name": "...", "report": {"summary": "...", "overall": [earned, max]}}, ...]

This script is a thin wrapper that:

* executes the grader via node with the required worker/dom polyfill,
* and writes the result to ``--output`` in either JSON (raw) or REPORT
  (human-readable text) form.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path


def run_hazel_grader(submission_json: str, hazel_path: Path) -> list:
    """Run the Hazel grader on the given submission JSON string and return the
    parsed list of exercise sections."""

    polyfill_file = hazel_path / "src/web/www/polyfill_worker.js"
    grading_file = hazel_path / "_build/default/src/web/gradingReport.bc.js"
    www_cwd = hazel_path / "_build/default/src/web/www/"

    if not polyfill_file.exists():
        raise FileNotFoundError(f"Hazel polyfill not found: {polyfill_file}")
    if not grading_file.exists():
        raise FileNotFoundError(
            f"Hazel gradingReport.bc.js not found: {grading_file}. "
            "Did you run `make release` (or `make dev`)?"
        )

    with tempfile.TemporaryDirectory() as tmpdir:
        input_file = Path(tmpdir) / "input.json"
        output_file = Path(tmpdir) / "output.json"

        with open(input_file, "w") as f:
            # Round-trip through json.loads/dump to normalize formatting.
            json.dump(json.loads(submission_json), f, indent=2)

        cmd = [
            "node",
            "-r", str(polyfill_file),
            str(grading_file),
            str(input_file),
            str(output_file),
        ]

        try:
            subprocess.run(
                cmd,
                cwd=www_cwd,
                capture_output=True,
                text=True,
                check=True,
            )
        except subprocess.CalledProcessError as e:
            print(f"[error] Hazel grader failed:\n{e.stderr}", file=sys.stderr)
            raise

        if not output_file.exists():
            raise RuntimeError("Hazel grader did not produce an output file")

        with open(output_file) as f:
            return json.load(f)


def render_report(sections: list) -> str:
    """Render the list of grading sections as a human-readable report."""
    lines: list[str] = []
    total_earned = 0.0
    total_max = 0.0

    for section in sections:
        name = section.get("name", "<unnamed>")
        report = section.get("report", {})
        overall = report.get("overall", [0.0, 0.0])
        earned, maximum = float(overall[0]), float(overall[1])
        total_earned += earned
        total_max += maximum

        header = f"{name}  —  {earned:.1f}/{maximum:.1f}"
        underline = "=" * len(header)
        lines.append(header)
        lines.append(underline)
        summary = report.get("summary", "").rstrip()
        if summary:
            lines.append(summary)
        lines.append("")

    lines.append("-" * 40)
    lines.append(f"Total: {total_earned:.1f}/{total_max:.1f}")
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Run the Hazel batch grader on a submission JSON and write the "
            "result to a file (either raw JSON or a human-readable report)."
        ),
    )
    parser.add_argument("submission_file", help="Path to the submission JSON")
    parser.add_argument(
        "--hazel-dir",
        default=".",
        help="Path to the Hazel project root (default: current directory)",
    )
    parser.add_argument(
        "--output", "-o", required=True, help="Path to write the output to"
    )
    parser.add_argument(
        "--format",
        choices=("json", "report"),
        default="json",
        help="Output format: 'json' (raw grader output) or 'report' "
             "(human-readable text). Default: json.",
    )

    args = parser.parse_args()

    submission_path = Path(args.submission_file).resolve()
    hazel_path = Path(args.hazel_dir).resolve()
    output_path = Path(args.output).resolve()

    if not submission_path.exists():
        print(f"Error: Submission file {submission_path} not found", file=sys.stderr)
        return 1
    if not hazel_path.exists():
        print(f"Error: Hazel directory {hazel_path} not found", file=sys.stderr)
        return 1

    try:
        with open(submission_path) as f:
            submission = f.read()
        sections = run_hazel_grader(submission, hazel_path)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    output_path.parent.mkdir(parents=True, exist_ok=True)
    if args.format == "json":
        with open(output_path, "w") as f:
            json.dump(sections, f, indent=2, sort_keys=True)
            f.write("\n")
    else:
        with open(output_path, "w") as f:
            f.write(render_report(sections))

    print(f"Wrote {args.format} output to {output_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
