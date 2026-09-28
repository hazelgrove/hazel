# Eval harness integration

How the team's headless-agent work was brought onto `russ/jev-nav-subagent`, what was
left behind, and how the before/after study runs. Nothing here is committed yet.

## What existed (2026-09-22)

| Where | Author | What | Taken? |
|---|---|---|---|
| `origin/incr-eval-agent-bench` (no PR) | Matt Keenan | `src/CLI/AgentRun.re` — `hazel agent`: headless run of the real `Agent.Update` loop, goal oracle, feedback rounds, DONE detection, `--stub`; `src/CLI/xhrNode.js` async Node XHR (streams SSE); `bench/tasks/*.{hz,json}` five agent tasks; `bench/run-task.sh` | **yes** (these files only) |
| same branch | Matt Keenan | `BenchIncr.re`, `IdMatch.re`, `Calculus.re`, `IncrEval` changes, 5k lines of tests, `bench/traces`, `bench/results`, `summarize.js`, `CompositionGo` IdMatch hook | no — incremental-evaluation research, separate concern |
| PR #2571 `harness-eval-plan` | russell-rozenbaum | `docs/agent-harness-eval/{README,plan,findings,bugs}.md` — structural-vs-text-edit study plan; bug list (B1–B5 block a fair scorer) | **yes** |
| PR #2423 `cost-display-refinements` | russell-rozenbaum | `src/CLI/AgentEval.re` — scripted 3-turn cache study; per-turn `usage` JSONL, cost payload, credits ledger; sync curl XHR polyfill | not yet — port `usage_rows` / cost JSONL into `AgentRun` (§ next) |
| PR #2353 `da-bench` | 7h3kk1d | InfiAgent-DABench language benchmark | no — not an agent harness |

All three branches merge clean onto ours (`git merge-tree` dry run), but only the
files above were pulled, via `git checkout <ref> -- <paths>`, so nothing was committed
and no merge commit exists. `Cli.re` received only `agent_cmd` and the deferred-exit
tail; `bench-incr` is **not** on this branch, so `bench/run-task.sh --bench` will fail
(record mode works).

## Verified

- `dune build ./src/CLI/cli.bc.js` builds with the new files.
- `./hazel agent --stub --stub-path billed --stub-code '…' bench/tasks/fix-middle.hz "fix it"`
  runs the real reducer offline and applies the edit. No network, no key.
- Task format: `bench/tasks/<name>.json` = `{name, program, prompt, feedback, goal?,
  max_turns?, expectation}`; `<name>.hz` = starting program. Five tasks, 10–15 bindings
  each, goals are literal values.

## Running a real recording

```
export OPENROUTER_API_KEY=…          # personal key; never in the repo
./hazel agent bench/tasks/fix-middle.hz "$(jq -r .prompt bench/tasks/fix-middle.json)" \
    --feedback 6 --goal 30700 --model google/gemini-3-flash-preview
# or: bench/run-task.sh fix-middle --record
```

Default model is `google/gemini-3-flash-preview`; `--model` overrides. Spend rail:
`--max-turns 24`.

## What to add for the Jev study

1. **Flags through the CLI:** `--jev-prepass`, `--jev-view-tool` → `Settings.agent_globals`.
2. **Metrics JSONL:** port `AgentEval.usage_rows` + `cost_payload` into `AgentRun`
   `report_and_exit`; add the Jev fields from `plan.md` §6; append one
   row per run to `bench/results/nav-study.jsonl`.
3. **`bench/run-nav-study.sh`:** for each task × arm × rep, run `hazel agent` with the
   flags, capturing stdout to `bench/results/<task>.<arm>.<rep>.log`. Interleave arms
   within a task so model drift does not confound (Matt's `--id-policy` sweep does the
   same for calculi).
4. **`bench/summarize-nav.js`:** group rows by arm; report median navigation turns,
   main cost, Jev requests, `goal_met` rate, wall time; ratio vs the control arm.
5. **Before touching Jev code:** record the **control** arm on all five tasks once, so
   the baseline exists before the intervention (pre-registration, cheap).

## Open points (decide, not code)

- Feedback-loop asymmetry (plan.md §14): Filbert sees test/probe output only in the
  next snapshot. Same for all arms here, so it does not confound this study.
- B1–B3 (`hazel test` exit codes) matter for the structural-vs-text study, not for
  this one — `goal_met` uses `Run.evaluate`, not `hazel test`.
- Reps: agents are nondeterministic; 5 reps × 5 tasks × 4 arms = 100 recordings per
  model. At Gemini Flash prices that is a few dollars; at Sonnet, tens.
