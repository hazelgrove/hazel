#!/usr/bin/env bash
# Regression test for the DA-Bench solutions.
#
# Runs every solution through run.sh (prelude + solution) and compares the
# output to the InfiAgent reference answer, after normalizing every decimal
# number to 2 places (Hazel prints floats as %f / 6 decimals). Catches silent
# breakage from changes to prelude.hz, the CLI, or a solution.
#
# Usage:  da-bench/test.sh            # run all
#         da-bench/test.sh da0-mean-fare.hz   # run one
#         TABLES=/path da-bench/test.sh       # override data dir
# Exits non-zero if any case fails.
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# "<solution> | <expected>"  — floats given to 2 dp; ints/strings verbatim.
# Expected values are the InfiAgent da-dev-labels.jsonl reference answers.
CASES=(
  "da0-mean-fare.hz"
  "da5-corr-familysize-fare.hz"
  "da6-fare-by-agegroup.hz"
  "da9-mean-close.hz"
  "da14-price-range.hz"
  "da18-mar2019-iqr.hz"
  "da24-mean-age.hz"
  "da26-corr-charges-children.hz"
  "da27-charges-outliers.hz"
  "da56-max-deaths.hz"
  "da58-missing-pct.hz"
  "da174-fare-skew.hz"
)

# Round every decimal number in the result to 2 dp so 34.650000 == 34.65.
normalize() {
  python3 -c 'import sys,re; t=sys.stdin.read().strip(); print(re.sub(r"\d+\.\d+", lambda m: "%.2f"%float(m.group()), t))'
}

run_one() {
  "$DIR/run.sh" "$1" 2>&1 | grep -vE 'joo_global_object|deprecat|Terminated' | tail -1 | normalize
}

only="${1:-}"
pass=0; fail=0
for c in "${CASES[@]}"; do
  file="${c%%|*}"; want="${c#*|}"
  [ -n "$only" ] && [ "$only" != "$file" ] && continue
  got="$(run_one "$file")"
  if [ "$got" = "$want" ]; then
    printf 'PASS  %-30s %s\n' "$file" "$got"; pass=$((pass+1))
  else
    printf 'FAIL  %-30s\n        got  [%s]\n        want [%s]\n' "$file" "$got" "$want"; fail=$((fail+1))
  fi
done
echo "----"
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
