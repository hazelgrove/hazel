#!/usr/bin/env bash
# Jev navigation study: every task x arm x rep, one metrics row per run.
#
#   bench/run-nav-study.sh                       # all nav-* tasks, 4 arms, REPS reps
#   TASKS="nav-fleet nav-parts" REPS=1 bench/run-nav-study.sh
#   DRY_RUN=1 bench/run-nav-study.sh             # print the commands only
#   STUB=1 bench/run-nav-study.sh                # no network: --stub, for testing the plumbing
#
# Arms (plan.md §5, v3-jev-implementor.md). Each is a set of orthogonal flags,
# named by joining its parts with "+":
#   control             no flags (dev today)
#   prepass             --jev-prepass        Jev pre-selects the view per message
#   view                --jev-view-tool      modify_view replaces expand/collapse
#   edit                --jev-edit-tool      jev_edit replaces the editor tools
#   build               --jev-edit-builds    jev_edit takes no sketch; Jev builds
#                                            the code's shape too. Implies edit.
#   prepass+view, view+edit ("Jev as nav + editor"), prepass+view+edit,
#   edit+build, view+edit+build
#   jev                 all of Jev at once (prepass + view + edit + build), the
#                       UI's /jev-mode
#
# Arms are interleaved within each task and rep (control, prepass, view, ...,
# control, ...) so model or provider drift over a long session lands on every
# arm alike instead of on whichever arm ran last.
#
# Env:
#   OPENROUTER_API_KEY   required unless STUB=1
#   MODEL                main-model id (default: hazel agent's default)
#   REPS                 reps per task x arm (default 5)
#   TASKS                space-separated task names (default: every bench/tasks/nav-*.json)
#   ARMS                 subset of arms (default: all nine above)
#   BATCHES              space-separated --jev-batch values to sweep; each arm
#                        with prepass or view runs once per value (control and
#                        edit ignore it). Default: the built-in batch size.
#   OUT                  metrics JSONL (default bench/results/nav-study.jsonl);
#                        per-run logs and transcripts go next to it. Under
#                        STUB=1 the default is a fresh temp dir instead.
#                        Rows are only ever appended; nothing is deleted.
#   HAZEL                command that runs the CLI (default ./hazel)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TASK_DIR="$ROOT/bench/tasks"
RESULTS="$ROOT/bench/results"

REPS="${REPS:-5}"
ARMS="${ARMS:-control prepass view prepass+view edit view+edit prepass+view+edit edit+build view+edit+build}"
BATCHES="${BATCHES:-}"
# Stub runs are plumbing tests; by default they write to a fresh temp dir so
# they can never mix into (or tempt anyone to clean up) real results.
if [ -z "${OUT:-}" ] && [ "${STUB:-0}" = 1 ]; then
  OUT="$(mktemp -d "${TMPDIR:-/tmp}/nav-study-stub.XXXXXX")/nav-study.jsonl"
fi
OUT="${OUT:-$RESULTS/nav-study.jsonl}"
HAZEL="${HAZEL:-$ROOT/hazel}"
DRY_RUN="${DRY_RUN:-0}"
STUB="${STUB:-0}"

die() { echo "error: $*" >&2; exit 1; }

# Reject before any run starts: a study that dies on its first paid call after
# printing a plan is worse than one that never starts.
if [ "$STUB" != 1 ] && [ "$DRY_RUN" != 1 ] && [ -z "${OPENROUTER_API_KEY:-}" ]; then
  die "OPENROUTER_API_KEY is not set (use STUB=1 to test without the network)"
fi

# Same field reader as bench/run-task.sh: node is already required by ./hazel.
read_field() {
  node -e '
    const t = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
    const v = t[process.argv[2]];
    process.stdout.write(v === undefined || v === null ? "" : Array.isArray(v) ? v.join(",") : String(v));
  ' "$1" "$2"
}

# One flag per "+"-joined part, so any combination is an arm without a new case.
arm_flags() {
  [ "$1" = control ] && return
  local part
  for part in ${1//+/ }; do
    case "$part" in
      prepass) echo "--jev-prepass" ;;
      view)    echo "--jev-view-tool" ;;
      edit)    echo "--jev-edit-tool" ;;
      # Building is a mode of jev_edit; `hazel agent` turns the tool on for
      # it, so "edit+build" and "build" run the same arm.
      build)   echo "--jev-edit-builds" ;;
      jev)     echo "--jev-prepass --jev-view-tool --jev-edit-builds" ;;
      *)       die "unknown arm part: $part (in $1)" ;;
    esac
  done
}

default_tasks() {
  for f in "$TASK_DIR"/nav-*.json; do basename "$f" .json; done
}

run_one() {
  local task="$1" arm="$2" batch="$3" rep="$4"
  local spec="$TASK_DIR/$task.json"
  [ -f "$spec" ] || die "no such task: $task"

  local program prompt feedback goal max_turns nav_targets
  program="$(read_field "$spec" program)"
  prompt="$(read_field "$spec" prompt)"
  feedback="$(read_field "$spec" feedback)"
  goal="$(read_field "$spec" goal)"
  max_turns="$(read_field "$spec" max_turns)"
  nav_targets="$(read_field "$spec" nav_targets)"

  local args=(agent "$ROOT/$program" "$prompt" --task "$task" --arm "$arm" --metrics-out "$OUT")
  # shellcheck disable=SC2207 # arm_flags emits whitespace-separated flags by design
  args+=($(arm_flags "$arm"))
  [ -n "$batch" ] && args+=(--jev-batch "$batch")
  # Under --stub there is no model to feed back to; each round would only
  # re-evaluate the unchanged program and slow the plumbing test down.
  [ "$STUB" = 1 ] && feedback=0
  [ -n "$feedback" ] && args+=(--feedback "$feedback")
  [ -n "$goal" ] && args+=(--goal "$goal")
  [ -n "$max_turns" ] && args+=(--max-turns "$max_turns")
  [ -n "$nav_targets" ] && args+=(--nav-targets "$nav_targets")
  [ -n "${MODEL:-}" ] && args+=(--model "$MODEL")
  # The stub's one canned edit lands on the task's first nav target, so it is
  # a real edit (and, in the edit arms, a real Jev fill) rather than a call on
  # a path that does not exist. Under --jev-edit-builds --stub-code is the
  # planner's names; otherwise it is the code (or sketch): a bare hole.
  if [ "$STUB" = 1 ]; then
    args+=(--stub --stub-path "${nav_targets%%,*}")
    if [[ "$arm" == *build* || "$arm" == jev ]]; then args+=(--stub-code x); else args+=(--stub-code "?"); fi
  fi

  local run_id="$(dirname "$OUT")/$task.$arm${batch:+.b$batch}.$rep"
  local log="$run_id.log"
  args+=(--transcript-out "$run_id.transcript.json")
  if [ "$DRY_RUN" = 1 ]; then
    printf '%q ' "$HAZEL" "${args[@]}"; echo
    return
  fi
  echo "=== $task / $arm${batch:+ / batch $batch} / rep $rep ===" >&2
  # A failed run still leaves its log; keep going so one bad run does not
  # void the rest of the study (its missing row shows up in the summary's n).
  if ! "$HAZEL" "${args[@]}" >"$log" 2>&1; then
    echo "  run failed; see $log" >&2
    FAILED_RUNS=$((FAILED_RUNS + 1))
  fi
}

FAILED_RUNS=0

main() {
  local tasks
  tasks="${TASKS:-$(default_tasks | tr '\n' ' ')}"
  [ -n "$tasks" ] || die "no tasks"
  [ "$DRY_RUN" = 1 ] || mkdir -p "$(dirname "$OUT")"

  for task in $tasks; do
    for rep in $(seq 1 "$REPS"); do
      for arm in $ARMS; do
        # --jev-batch only sizes Jev's navigation requests; sweeping it on an
        # arm that never navigates with Jev would just repeat that arm.
        if [ -z "$BATCHES" ] || [[ "$arm" != *prepass* && "$arm" != *view* ]]; then
          run_one "$task" "$arm" "" "$rep"
        else
          for batch in $BATCHES; do run_one "$task" "$arm" "$batch" "$rep"; done
        fi
      done
    done
  done
  [ "$DRY_RUN" = 1 ] || echo "rows appended to $OUT; summarize with: node bench/summarize-nav.js $OUT" >&2
  # Keep going past a failed run (the rest of the study is still worth
  # having) but say so in the exit status, so callers cannot miss it.
  if [ "$FAILED_RUNS" -gt 0 ]; then
    echo "$FAILED_RUNS run(s) failed" >&2
    exit 1
  fi
}

main "$@"
