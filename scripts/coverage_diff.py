#!/usr/bin/env python3
"""List the lines a branch changed that a coverage session never ran.

Reviewing a behavioral change, the question is whether anyone actually ran the
code it touches. A coverage report answers that for the whole program; this
narrows it to the diff. It reads a Cobertura report (from `bisect-ppx-report
cobertura`) and `git diff <base>...HEAD`, and prints, per changed file, the
changed lines that are instrumented but have zero hits.

    make coverage-web-diff COVERAGE_DIR=~/hazel-coverage/session1 [BASE=origin/dev]

Only lines bisect instruments count: blank lines, comments, type definitions
and the like are neither covered nor uncovered. A changed file that never
appears in the report under its own name (not instrumented, not linked into
the bundle, or copied to another name by a dune rule, like CoverageHook's
variants) is listed separately.

The exit status is 0 either way: uncovered lines are something to look at, not
a failure.
"""
import re
import subprocess
import sys
import xml.etree.ElementTree as ET


def changed_lines(base: str) -> dict[str, set[int]]:
    """New-side line numbers of every added or modified line, per file."""
    diff = subprocess.run(
        ["git", "diff", "--unified=0", "--no-color", f"{base}...HEAD", "--", "src"],
        check=True, capture_output=True, text=True,
    ).stdout
    lines: dict[str, set[int]] = {}
    path = None
    for line in diff.splitlines():
        if line.startswith("+++ "):
            path = None if line.endswith("/dev/null") else line[len("+++ b/"):]
        elif line.startswith("@@") and path is not None:
            m = re.match(r"@@ -\S+ \+(\d+)(?:,(\d+))? @@", line)
            start, count = int(m.group(1)), int(m.group(2) or "1")
            lines.setdefault(path, set()).update(range(start, start + count))
    return lines


def hits(cobertura: str) -> dict[str, dict[int, int]]:
    """Hit count of every instrumented line, per source file."""
    out: dict[str, dict[int, int]] = {}
    for cls in ET.parse(cobertura).iter("class"):
        per_line = out.setdefault(cls.get("filename"), {})
        for ln in cls.iter("line"):
            n = int(ln.get("number"))
            per_line[n] = max(per_line.get(n, 0), int(ln.get("hits")))
    return out


def ranges(nums: list[int]) -> str:
    """[3, 4, 5, 9] -> '3-5, 9'"""
    spans, start = [], None
    for i, n in enumerate(nums):
        if start is None:
            start = n
        if i + 1 == len(nums) or nums[i + 1] != n + 1:
            spans.append(str(start) if start == n else f"{start}-{n}")
            start = None
    return ", ".join(spans)


def main() -> None:
    cobertura, base = sys.argv[1], sys.argv[2]
    covered = hits(cobertura)

    # bisect's paths may differ from git's by a leading directory
    def lookup(path: str):
        if path in covered:
            return covered[path]
        for f, v in covered.items():
            if f.endswith("/" + path) or path.endswith("/" + f):
                return v
        return None

    total = ran = 0
    not_in_report = []
    for path, changed in sorted(changed_lines(base).items()):
        if not path.endswith((".re", ".ml")):
            continue
        file_hits = lookup(path)
        if file_hits is None:
            not_in_report.append(path)
            continue
        instrumented = sorted(n for n in changed if n in file_hits)
        missed = [n for n in instrumented if file_hits[n] == 0]
        total += len(instrumented)
        ran += len(instrumented) - len(missed)
        if missed:
            print(f"{path}: not run: {ranges(missed)}")

    if not_in_report:
        print("\nChanged, but not in the report under this name (not instrumented,"
              " not linked into this bundle, or copied by a dune rule first):")
        for path in not_in_report:
            print(f"  {path}")
    pct = f" ({100 * ran / total:.0f}%)" if total else ""
    print(f"\nChanged instrumented lines run: {ran}/{total}{pct} (vs {base})")


if __name__ == "__main__":
    main()
