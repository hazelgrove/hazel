#!/usr/bin/env bash
# Headless integration eval: real main model + real Jev, control vs all-Jev.
# Meant to run unattended with a real key; prints a verdict per task x arm.
#
#   bench/jev-eval.sh                 # asks before spending
#   bench/jev-eval.sh --yes           # unattended
#   DRY_RUN=1 bench/jev-eval.sh       # print the commands; no key needed
#   STUB=1 bench/jev-eval.sh --yes    # no network; tests the whole pipeline
#
# Env:
#   MODEL       main model (default openai/gpt-6-luna, pinned so runs compare)
#   TASKS       task names (default: every bench/tasks/nav-*.json + fix-middle;
#               with the -sym twins that is more than MAX_RUNS, so pick a set)
#   ARMS        default "control jev" (jev = --jev-prepass --jev-view-tool
#               --jev-edit-tool --jev-edit-builds, the UI's /jev-mode)
#   REPS        default 1
#   MAX_RUNS    spend rail: refuse to start above this many runs (default 12)
#   EST_USD_PER_TURN  for the pre-flight upper bound only (default 0.02)
#   HAZEL       CLI command (default ./hazel)
#   RESULTS_DIR where the eval folder goes (default bench/results; under
#               STUB/DRY_RUN a fresh temp dir, so tests never touch real results)
#
# Nothing here ever deletes results: each run gets its own new timestamped
# folder, and old folders are left alone.
#
# KEY. Read from the environment, else from ~/.config/hazel-jev/openrouter.env
# (one line OPENROUTER_API_KEY=...). The key never enters the repo: this
# folder is synced to OneDrive. It is only ever passed through the
# environment (never argv, where `ps` shows it), never printed, and every
# file this eval writes is checked for it afterwards; a hit fails the run.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
KEY_FILE="$HOME/.config/hazel-jev/openrouter.env"

export MODEL="${MODEL:-openai/gpt-6-luna}"
export TASKS="${TASKS:-$(cd "$ROOT/bench/tasks" && ls nav-*.json | sed 's/\.json$//' | tr '\n' ' ')fix-middle}"
export ARMS="${ARMS:-control jev}"
export REPS="${REPS:-1}"
MAX_RUNS="${MAX_RUNS:-12}"
EST_USD_PER_TURN="${EST_USD_PER_TURN:-0.02}"
DRY_RUN="${DRY_RUN:-0}"
STUB="${STUB:-0}"
ASSUME_YES=0

die() { echo "error: $*" >&2; exit 1; }

for arg in "$@"; do
  case "$arg" in
    --yes) ASSUME_YES=1 ;;
    *) die "unknown argument: $arg" ;;
  esac
done

# Octal permission bits, on macOS (BSD stat) and Linux (GNU stat).
file_mode() { stat -f '%Lp' "$1" 2>/dev/null || stat -c '%a' "$1"; }

load_key() {
  [ -n "${OPENROUTER_API_KEY:-}" ] && return
  [ -f "$KEY_FILE" ] || die "OPENROUTER_API_KEY is not set and $KEY_FILE does not exist"
  local mode
  mode="$(file_mode "$KEY_FILE")"
  # Any group/other bit means another account can read the key.
  if [ $((8#$mode & 8#077)) -ne 0 ]; then
    die "$KEY_FILE is readable by others (mode $mode); run: chmod 600 $KEY_FILE"
  fi
  set -a
  # shellcheck disable=SC1090
  . "$KEY_FILE"
  set +a
  [ -n "${OPENROUTER_API_KEY:-}" ] || die "$KEY_FILE does not set OPENROUTER_API_KEY"
}

# Fails (without printing it) if the key appears in any file under $1. The
# pattern goes to grep through a pipe from a builtin, never through argv.
assert_key_absent() {
  [ -n "${OPENROUTER_API_KEY:-}" ] || return 0
  if printf '%s\n' "$OPENROUTER_API_KEY" | grep -rqF -f - "$1"; then
    die "the API key appears in output under $1; do not share or commit anything from it, and move it out of the synced folder"
  fi
}

max_turns_of() {
  node -e '
    const t = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
    process.stdout.write(String(t.max_turns || 24));
  ' "$ROOT/bench/tasks/$1.json"
}

preflight() {
  local runs=0 turns=0 task arm rep
  for task in $TASKS; do
    [ -f "$ROOT/bench/tasks/$task.json" ] || die "no such task: $task"
    for arm in $ARMS; do
      for rep in $(seq 1 "$REPS"); do
        runs=$((runs + 1))
        turns=$((turns + $(max_turns_of "$task")))
      done
    done
  done
  echo "jev-eval: $runs run(s) = tasks [$TASKS] x arms [$ARMS] x $REPS rep(s)" >&2
  echo "          model $MODEL; at most $turns main-model replies" >&2
  awk -v t="$turns" -v p="$EST_USD_PER_TURN" \
    'BEGIN { printf "          worst-case main-model spend ~$%.2f (at $%s/reply; Jev extra)\n", t * p, p }' >&2
  [ "$runs" -le "$MAX_RUNS" ] || die "$runs runs exceeds MAX_RUNS=$MAX_RUNS; narrow TASKS/ARMS/REPS or raise MAX_RUNS"
}

confirm() {
  [ "$ASSUME_YES" = 1 ] || [ "$DRY_RUN" = 1 ] || [ "$STUB" = 1 ] && return
  local reply
  read -r -p "Spend real money on this? [y/N] " reply
  [[ "$reply" == [yY]* ]] || die "aborted"
}

main() {
  [ "$STUB" = 1 ] || [ "$DRY_RUN" = 1 ] || load_key
  preflight
  confirm

  local results_dir="${RESULTS_DIR:-}"
  if [ -z "$results_dir" ]; then
    if [ "$STUB" = 1 ] || [ "$DRY_RUN" = 1 ]; then
      results_dir="$(mktemp -d "${TMPDIR:-/tmp}/jev-eval-test.XXXXXX")"
    else
      results_dir="$ROOT/bench/results"
    fi
  fi
  local out_dir="$results_dir/jev-eval-$(date +%Y%m%d-%H%M%S)"
  # Never reuse a folder: a second eval in the same second must not append
  # to (or be confused with) another run's results.
  [ ! -e "$out_dir" ] || die "$out_dir already exists; wait a second and rerun"
  local rows="$out_dir/rows.jsonl"
  local study_status=0
  OUT="$rows" "$ROOT/bench/run-nav-study.sh" || study_status=$?
  [ "$DRY_RUN" = 1 ] && return

  assert_key_absent "$out_dir"
  [ -s "$rows" ] || die "no metrics rows were written (see logs in $out_dir)"

  echo
  node "$ROOT/bench/summarize-nav.js" "$rows"
  echo
  echo "=== integration verdict ($out_dir) ==="
  local verdict_status=0
  node "$ROOT/bench/summarize-nav.js" --verdict "$rows" || verdict_status=$?

  # One greppable line for reports to cite. Spend is billed cost only; runs
  # whose main model reported none (e.g. STUB) are counted, not guessed.
  node -e '
    const rows = require("fs").readFileSync(process.argv[1], "utf8").split("\n").filter(Boolean).map(JSON.parse);
    const sum = (f) => rows.reduce((acc, r) => acc + (f(r) || 0), 0);
    const main = sum((r) => r.main.cost_usd), nav = sum((r) => r.jev.cost_usd);
    const edit = sum((r) => r.jev_edit && r.jev_edit.cost_usd);
    const unbilled = rows.filter((r) => r.main.cost_usd == null).length;
    const met = rows.filter((r) => r.outcome.goal_met === true).length;
    console.log(`SUMMARY runs=${rows.length} goal_met=${met}/${rows.length} spend_usd=${(main + nav + edit).toFixed(4)} ` +
      `(main ${main.toFixed(4)}, jev_nav ${nav.toFixed(4)}, jev_edit ${edit.toFixed(4)}; ${unbilled} run(s) without billed main cost)`);
  ' "$rows"

  # A run that crashed wrote no row, so count rows too, not only errors.
  local expected written
  expected=$(( $(wc -w <<<"$TASKS") * $(wc -w <<<"$ARMS") * REPS ))
  written=$(wc -l <"$rows" | tr -d ' ')
  local status=0
  [ "$written" -eq "$expected" ] || { echo "only $written of $expected runs wrote a row" >&2; status=1; }
  [ "$study_status" -eq 0 ] || { echo "at least one run exited non-zero (logs in $out_dir)" >&2; status=1; }
  [ "$verdict_status" -eq 0 ] || status=1
  exit "$status"
}

main
