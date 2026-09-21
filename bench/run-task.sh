#!/usr/bin/env bash
# Turn a task into a trace, then time that trace under every calculus.
#
#   bench/run-task.sh tune-threshold              # record a fresh trace, then bench it
#   bench/run-task.sh tune-threshold --record     # record only, do not bench
#   bench/run-task.sh tune-threshold --bench      # bench the trace already on disk
#   bench/run-task.sh --all                       # every task in bench/tasks
#
# A task is a pair of files in bench/tasks/:
#   <name>.hz     the program the agent starts from
#   <name>.json   { name, program, prompt, feedback, goal?, expectation }
#
# Recording calls a real model and costs money, which is why it is separate
# from benching: the trace is the artifact. Once recorded, a trace can be
# re-benched as often as you like, deterministically and for free. That split
# is also what makes the numbers reproducible -- the agent is nondeterministic,
# the replay is not.
#
# Env:
#   OPENROUTER_API_KEY   required to record (not needed to bench)
#   MODEL                override the model id used for recording
#   REPS / WARMUP        override the benchmark's sampling (default 5 / 1)
#   POLICIES             space-separated id-policies to sweep (default: default)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TASKS="$ROOT/bench/tasks"
TRACES="$ROOT/bench/traces"
RESULTS="$ROOT/bench/results"

REPS="${REPS:-5}"
WARMUP="${WARMUP:-1}"
POLICIES="${POLICIES:-default}"

mkdir -p "$TRACES" "$RESULTS"

die() { echo "error: $*" >&2; exit 1; }

# jq is the only external dependency, and only for reading task files. Node is
# already required by ./hazel, so fall back to it rather than making jq a hard
# requirement.
read_field() {
  local file="$1" field="$2"
  # readFileSync rather than require: require resolves a bare relative path
  # against node's module paths, not the cwd, and silently fails for callers
  # that pass one.
  node -e '
    const fs = require("fs");
    const t = JSON.parse(fs.readFileSync(process.argv[1], "utf8"));
    const v = t[process.argv[2]];
    process.stdout.write(v === undefined || v === null ? "" : String(v));
  ' "$file" "$field"
}

record() {
  local name="$1"
  local task="$TASKS/$name.json"
  [ -f "$task" ] || die "no such task: $name (expected $task)"
  [ -n "${OPENROUTER_API_KEY:-}" ] || die "OPENROUTER_API_KEY is not set; recording needs it"

  local program prompt feedback goal max_turns
  program="$(read_field "$task" program)"
  prompt="$(read_field "$task" prompt)"
  feedback="$(read_field "$task" feedback)"
  goal="$(read_field "$task" goal)"
  max_turns="$(read_field "$task" max_turns)"
  [ -n "$program" ] || die "$task has no \"program\" field"

  local args=("$ROOT/$program" "$prompt" --trace "$TRACES/$name.json" --trace-name "$name")
  [ -n "$feedback" ] && args+=(--feedback "$feedback")
  [ -n "$goal" ] && args+=(--goal "$goal")
  # A search task needs a bigger turn budget than a fix-it task: every probe
  # the agent makes costs a turn, and running out mid-search truncates the
  # trace rather than failing loudly.
  [ -n "$max_turns" ] && args+=(--max-turns "$max_turns")
  [ -n "${MODEL:-}" ] && args+=(--model "$MODEL")

  echo "=== recording $name ===" >&2
  echo "    program:  $program" >&2
  echo "    feedback: ${feedback:-0} round(s)   goal: ${goal:-<none>}" >&2
  echo "    model:    ${MODEL:-<default>}" >&2
  # Tee so the agent's turn-by-turn output is visible live AND kept: the
  # transcript is how you tell a trace that reflects real iteration from one
  # where the model guessed right first try.
  "$ROOT/hazel" agent "${args[@]}" 2>&1 | tee "$RESULTS/$name.agent.log"

  [ -f "$TRACES/$name.json" ] || die "$name: agent produced no trace"
  local steps
  steps="$(node -e 'const fs=require("fs");process.stdout.write(String(JSON.parse(fs.readFileSync(process.argv[1],"utf8")).steps.length))' "$TRACES/$name.json")"
  echo "=== recorded $name: $steps step(s) -> bench/traces/$name.json ===" >&2
}

bench() {
  local name="$1"
  local trace="$TRACES/$name.json"
  [ -f "$trace" ] || die "no trace for $name; run with --record first"

  local policy_args=()
  for p in $POLICIES; do policy_args+=(--id-policy "$p"); done

  echo "=== benching $name (reps=$REPS warmup=$WARMUP policies='$POLICIES') ===" >&2
  "$ROOT/hazel" bench-incr \
    --reps "$REPS" --warmup "$WARMUP" \
    "${policy_args[@]}" \
    --json "$RESULTS/$name.bench.json" \
    "$trace" | tee "$RESULTS/$name.bench.txt"

  # The task file says what the trace was supposed to demonstrate. Printing it
  # next to the numbers is the cheapest guard against reading a win into a
  # trace that was built as a negative control.
  local expectation
  expectation="$(read_field "$TASKS/$name.json" expectation 2>/dev/null || true)"
  if [ -n "$expectation" ]; then
    echo "" | tee -a "$RESULTS/$name.bench.txt"
    echo "expected of this task:" | tee -a "$RESULTS/$name.bench.txt"
    echo "$expectation" | fold -s -w 76 | sed 's/^/  /' | tee -a "$RESULTS/$name.bench.txt"
  fi
}

all_tasks() {
  for f in "$TASKS"/*.json; do basename "$f" .json; done
}

main() {
  local do_record=1 do_bench=1
  local names=()

  for arg in "$@"; do
    case "$arg" in
      --record) do_bench=0 ;;
      --bench)  do_record=0 ;;
      --all)    while read -r t; do names+=("$t"); done < <(all_tasks) ;;
      -*)       die "unknown flag: $arg" ;;
      *)        names+=("$arg") ;;
    esac
  done

  [ ${#names[@]} -gt 0 ] || die "usage: bench/run-task.sh <task>... | --all  [--record|--bench]"

  for name in "${names[@]}"; do
    [ "$do_record" = 1 ] && record "$name"
    [ "$do_bench" = 1 ] && bench "$name"
  done

  echo "" >&2
  echo "results in bench/results/: $(printf '%s ' "${names[@]}")" >&2
}

main "$@"
