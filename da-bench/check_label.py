#!/usr/bin/env python3
"""Grade a DA-Bench solution's output against the InfiAgent reference label.

Usage: check_label.py <task_id> <hazel_output> <labels.jsonl>
Exit 0 if the output matches the task's reference answer, 1 if not, 2 if no label.

This file contains NO answer values — it reads them from the external
da-dev-labels.jsonl at runtime (the InfiAgent clone, which is not committed here),
so the harness never restates the benchmark's gold answers.

We compare the *values* of the answer, not Hazel's exact output string: the label's
`common_answers` is a list of [metric, value] pairs whose order, quoting, sign-spacing
and metric names don't line up with Hazel's printed tuple, and the two sides don't even
always have the same metrics — some labels grade fewer metrics than the solution emits
(extra correct fields), others carry extra detail the solution omits (e.g. da528 lists
every outlier index; da423 adds a relationship label). So we require one value-multiset
to be a subset of the other (this is exact equality when the cardinalities match, and
only tolerates legitimate extras on one side). Element equality is numeric (rounded to
the label's precision, absorbing Hazel's %f float noise) or case-insensitive string.
"""
import sys
import json
import math
import re

# value after '=' : a full quoted string (commas/parens inside are fine) or a bare run
_VAL = re.compile(r'=\s*("(?:[^"\\]|\\.)*"|[^,)\]]*)')
_DECS = re.compile(r"-?\d+\.(\d+)\Z")


def unquote(v):
    v = v.strip()
    for _ in range(2):                       # da550 label values are pre-quoted
        if len(v) >= 2 and v[0] == '"' and v[-1] == '"':
            v = v[1:-1].strip()
    # strip trailing zeros from embedded decimals so "0.00, 1.00" == "0, 1" inside
    # string answers (whole-value float noise is handled numerically in _eq)
    return re.sub(r"\d+\.\d+", lambda m: m.group().rstrip("0").rstrip("."), v)


def values(text):
    """The comparable values in a Hazel output line (or label value list)."""
    text = text.strip()
    found = _VAL.findall(text)               # tuple / list-of-tuples: each =value
    if not found:                            # scalar / "string" / "{dict}"
        found = [text]
    return [unquote(v) for v in found]


def _as_float(s):
    try:
        f = float(s)
    except ValueError:
        return None
    return f if math.isfinite(f) else None    # "nan"/"inf" compared as strings


def _eq(a, b):
    a = "0" if a == "" else a                # empty-list answer ≡ count 0
    b = "0" if b == "" else b
    fa, fb = _as_float(a), _as_float(b)
    if fa is not None and fb is not None:    # round to the coarser stated precision
        da = len(m.group(1)) if (m := _DECS.match(a)) else 0
        db = len(m.group(1)) if (m := _DECS.match(b)) else 0
        d = min(da, db)
        return round(fa, d) == round(fb, d)
    return a.casefold() == b.casefold()


def _subset(small, big):
    """Every value in `small` matches a distinct value in `big` (fuzzy)."""
    pool = list(big)
    for a in small:
        for i, b in enumerate(pool):
            if _eq(a, b):
                del pool[i]
                break
        else:
            return False
    return True


def load_answers(task_id, labels_path):
    with open(labels_path) as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            o = json.loads(line)
            if str(o.get("id")) == str(task_id):
                return o.get("common_answers")
    return None


def grade(task_id, got, labels_path):
    answers = load_answers(task_id, labels_path)
    if answers is None:
        return 2, f"NO-LABEL for id {task_id} in {labels_path}"
    want = [unquote(v) for _k, v in answers]
    have = values(got)
    if _subset(want, have) or _subset(have, want):
        return 0, None
    return 1, f"  id {task_id}: want {sorted(want)}  got {sorted(have)}"


def main(argv):
    if len(argv) != 4:
        print(__doc__, file=sys.stderr)
        return 2
    task_id, got, labels_path = argv[1], argv[2], argv[3]
    try:
        code, msg = grade(task_id, got, labels_path)
    except FileNotFoundError:
        print(f"labels file not found: {labels_path}\n"
              f"set LABELS=/path/to/da-dev-labels.jsonl", file=sys.stderr)
        return 2
    if msg:
        print(msg, file=sys.stderr)
    return code


if __name__ == "__main__":
    sys.exit(main(sys.argv))
