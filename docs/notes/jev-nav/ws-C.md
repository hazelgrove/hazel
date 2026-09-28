# Workstream C: eval harness and tasks (2026-09-22)

## Built
- `hazel agent` flags: `--jev-prepass`, `--jev-view-tool`, `--jev-batch N` (sets `jev_batch_max_tokens`), plus the labels `--task`, `--arm`, `--nav-targets a,b`, and `--metrics-out FILE` (appends JSONL).
- `AgentRun.Metrics.row` is pure. It takes the chat, the outcome, the run info and `JevNav.Log.drain()` and returns one row: flags, wall_ms, main model (turns, expand/collapse/modify_view/total calls from tool results, prompt/completion/cached tokens, billed `usage.cost`), outcome (goal_met, said_done, feedback rounds, static error count), Jev (selections plus summed requests, questions, input_tokens and cost) and selection recall/precision.
- Recall counts targets in `yes` plus `closure_added` (what the agent sees). Precision counts Jev's `yes` answers only, so closure ancestors don't lower it. Both are per ws-A. A failed selection counts as opening nothing, per ws-A. If a selection was attempted but opened nothing, recall is 0 and precision is null. The field is null for the control arm and for tasks without targets.
- `goal_met` and `static_errors` were factored out, so the settle loop and the report share one oracle. The trace output is unchanged.
- New scripts:
  - `bench/run-nav-study.sh`: runs tasks × reps × interleaved arms (× `BATCHES`). Options: `DRY_RUN`, `STUB` (forces `--feedback 0`), `HAZEL`, `OUT`. It refuses to run without a key unless `STUB=1` or `DRY_RUN=1`.
  - `bench/summarize-nav.js`: prints medians per task × arm@batch and the ratio against control.
  - `bench/obfuscate-task.js`: generates the `-obf` twin of a task.

## Tasks (approximate binding counts)
| task | bindings | targets |
|---|---|---|
| nav-fleet | 61 | `Fuel/fuel_cost`, `Driver/idle_penalty`, `Maintenance/needs_service` |
| nav-warranty (callee + callers) | 43 | `Policy/covered_amount`, `Settlement/payout`, `Audit/exposure` |
| nav-parts (3 levels deep) | 62 | `Demand/forecast/weekly`, `Replenish/policy/safety_stock`, `Pricing/bulk/qualifies` |
| nav-parts-obf | 62 | `M06/v25/v27`, `M07/v31/v33`, `M08/v39/v42` |
| nav-emissions (flat) | 98 | `co2_grams`, `extra_high_samples` |

## Verified
- Every starting `.hz` runs with no static errors.
- For every task, the fixed copy (in the scratchpad) evaluates exactly to `goal`. Each bug changes the result on its own.
- Every target path resolves: a `--stub` `update_definition` edit on it applies.
- `STUB=1` run on all 5 tasks, plus 14 rows on nav-emissions with 4 arms × 2 batches × 2 reps: rows were written and the summarizer ran.
- A stub run that completes the last fix reports `goal_met: true`.
- The summarizer's ratio and recall columns were checked on synthetic rows.

## Open issues
- Bindings inside `{ let a; let b }` module literals are not `HighLevelNodeMap` nodes, so `M/a` paths don't resolve for them. The tasks use `module M = let … in { exports } in` so every binding has a path. ws-A has been told.
- nav-warranty needs `update_binding_clause` to change an annotated signature. The type guard rejects `update_definition` for this, and the stub can't exercise that tool.
- `Metrics.row` has no unit tests. Its tests would go in `test/`, which this workstream doesn't own.
- `--jev-batch` is a token budget (the Stage 0 field), not a binding count. The plan sweeps {1, 4, 16, all} bindings, so it needs a mapping or a new field.
- Early on I ran `dune build @src/fmt --auto-promote` once over all of `src`. That may have reformatted other workstreams' files. After that, only my own files were formatted, with `refmt`.

# Round 2 (C2): eval arms for the Jev edit tool (2026-09-23)

## Built
- **`--jev-edit-tool`** sets `agent_globals.jev_edit_tool`. `run_info` and the row's `flags` now include it.
- **Metrics row** (the builder stays pure):
  - New `jev_edit` section, built from `JevEdit.Log.drain()`. It holds the raw per-edit records plus totals: count, failed, rounds, holes_seen, filled, escalated (a count), fill_rate = filled / holes_seen (null when 0), requests, input_tokens, cost_usd, latency_ms.
  - `main.tool_calls` gains `jev_edit`, `refused_nav` and `refused_edit`.
  - A refused call is `success=false` and not skipped, and it only counts when the arm hides that tool: `refused_nav` needs `jev_view_tool`, `refused_edit` needs `jev_edit_tool`. Outside those arms, a failed call is an ordinary failure (bad path, type guard).
  - Editor calls are identified with `CompositionUtils.Public.action_of`, the same classification the trace uses, so there is no second list of tool names.
- **Arms:** `control prepass view prepass+view edit view+edit prepass+view+edit`, all by default. `ARMS` picks a subset.
  - Flags come from the `+`-joined parts, so any combination works as an arm without editing the script. Unknown parts abort the run.
  - Arms are still interleaved.
  - The `BATCHES` sweep only applies to arms containing prepass or view. Control and edit run once.
  - Rename: round-1 `tool`/`both` are now `view`/`prepass+view`.
- **Summarizer:** new columns total_cost (main + Jev navigation + Jev edit; null when the main model reported no cost), wall_s, refused, edit_fill, edit_esc, edit_lat_s. It also reports ratios vs control for total cost and wall time (`x_total`, `x_wall`). Batch goes into the arm label only when Jev navigates. Round-1 rows without `jev_edit` still load.

## Verified
- Build green in build_C. It was briefly blocked by B2's work in progress and was retried, with no edits to their files.
- `STUB=1 REPS=1 TASKS=nav-parts BATCHES="4000 8000"` produced 12 runs and 12 rows.
  - Every row has `jev_edit` and the new tool counters.
  - `refused_edit` is 1 in edit arms only: the stub's `update_definition` call. It is 0 in every other arm.
  - The summarizer printed all columns.
- A dry run showed the flag mapping per arm, and an unknown arm aborts.

## C2b: stub coverage for jev_edit
- **`--stub --jev-edit-tool`:** the canned reply now calls `jev_edit` on `--stub-path`, with `--stub-code` as the sketch. Without the flag, the stub still calls `update_definition`.
- **Fake Jev:** Jev is faked through B2's seam `AgentJev.edit_code`. It calls the real `JevEdit.edit` with `first_candidate_decider`, which picks each hole's first candidate that isn't escalate. The whole engine runs offline, with no new flag and no edits to other workstreams' files.
- **Verified on fix-middle:** the sketch `fun rows -> map(rows, fun r -> volume(?) * cost(r))` became `volume(r) * cost(r)` and the program evaluated to 30466.
  - The metrics row showed `jev_edit` count 1, holes_seen 1, filled 1, fill_rate 1.0, latency_ms 72, and `tool_calls.jev_edit` 1.
  - The summarizer printed edit_fill 1 and edit_lat_s 0.072.
- **Also covered:** `STUB=1` study runs now exercise `jev_edit` in the edit arms.
- **Open:** the `--trace` (bench-incr) output doesn't include `jev_edit` edits, because they are not `EditorAction`s. Metrics are unaffected.

# Round 3 (C3): eval arm for "Jev builds" (2026-09-24)

## Built
- **`--jev-edit-builds`** sets `agent_globals.jev_edit_builds` and implies `--jev-edit-tool`, so the two can never disagree. The row's `flags` now include `jev_edit_builds`.
- **Runner:** new arm token `build`, which emits `--jev-edit-builds`. The default arms now also include `edit+build` and `view+edit+build`.
- **Summarizer:** new columns `holes/edit` and `rounds/edit`, the medians of per-run holes_seen and rounds divided by the jev_edit count. Per call rather than per run, so a planner making more calls doesn't blur how much each call cost.
- **Stub, builds mode:** the canned `jev_edit` call has no sketch, and `--stub-code` is the planner's comma-separated names.
- **Stub decider:** renamed to `stub_decider` and made just smart enough to finish a build. Each hole's expected type is read from Jev's state. A function-typed hole takes the `fun` form. Any other hole prefers a complete term (no `?`), and falls back to the first option. The old first-option picker chose planner name `x` for an `Int -> Int` hole, which escalated.

## Verified (A3's build engine landed)
- **Builds mode:** `let double : Int -> Int = ? in double(21)` with `--stub --jev-edit-builds --stub-path double --stub-code x` built `fun x -> x` and evaluated to 21. Metrics: rounds 2, holes_seen 2, filled 2, fill_rate 1.0, escalated 0.
- **Sketch mode:** fix-middle still gives 30466.
- **STUB study on fix-middle:** 9 arms, 9 rows. The edit arms have `jev_edit_tool`, the build arms also have `jev_edit_builds`, and the summarizer printed holes/edit and rounds/edit.
- **Build:** green after A3/B3 finished `signature` (it was briefly red in CompositionUtils, which isn't my file).

## Open
- The stub never passes `signature`, A3's typed spec. It isn't needed while the program's annotation types the hole.

# Round 4 (C4): headless integration eval (2026-09-24)

## Built
- **`bench/jev-eval.sh`** runs control vs `jev` (`--jev-prepass --jev-view-tool --jev-edit-builds`, where builds implies edit). Defaults: MODEL `openai/gpt-6-luna`, TASKS nav-* + fix-middle, REPS 1, MAX_RUNS 12.
  - It reuses `run-nav-study.sh` for the runs, which also sets `--max-turns` from each task's json (else AgentRun's 24).
  - Before spending it prints a pre-flight: run count, the most main-model replies possible, and a worst-case cost using `EST_USD_PER_TURN`.
  - Real runs need `--yes` or a y/N confirmation. `DRY_RUN=1` and `STUB=1` need no key.
  - It writes to `bench/results/jev-eval-<ts>/rows.jsonl` with per-run logs in the same folder, then prints the summarizer table and a per-run verdict.
  - It exits 1 if any run exited non-zero, wrote no row, or reported an error.
- **Key safety:**
  - The key comes from the environment, else from `~/.config/hazel-jev/openrouter.env`. The script refuses to use that file if any group or other permission bit is set (chmod 600).
  - It is only passed through the environment, never argv.
  - After the runs, `grep -F -f -` (pattern piped in from a builtin) checks every output file for the key. A hit fails the run without printing it.
  - The metrics row never contains the key: `Metrics.row` takes no settings.
  - The main model, the pre-pass (`AgentSend`, `settings.agent_globals.api_key`) and jev_edit/modify_view (`AgentJev.resolve` via `globals.api_key`) all get the key from that same env var.
- **`bench/.gitignore`:** `results/`. bench/results was not ignored before.
- **Row:** new `outcome.api_failures`, the reducer's ApiFailure messages.
- **`summarize-nav.js --verdict`:** one line per run with goal_met, turns, main cost, jev requests, edit fill rate, refused calls and the first error (API failure, else a failed jev_edit, else a failed Jev selection). Exits 1 when any run has an error.
- **`run-nav-study.sh`:**
  - New arm token `jev`.
  - Exits 1 if any run failed.
  - Under STUB, the canned edit targets the task's first nav target (`x` names in build arms, a bare `?` otherwise), so jev_edit really fills something.
- **fix-middle.json:** gained `nav_targets` [billed, with_surcharge].

## Verified (no real key)
- **DRY_RUN:** 12 commands printed. The jev arm had the right flags and `--model openai/gpt-6-luna`.
- **STUB, all 6 tasks × 2 arms:** 12 rows, and the table and verdict printed. Jev arms show edit fill 1.0 (0.5 on fix-middle), 2 holes and 2 rounds per edit.
  - Every row's verdict shows "An API key is required": after the canned call the reducer continues and asks the main model again, with no key. That is expected under STUB, and it exercises the error path (exit 1).
- **Perms guard:** a 644 key file was refused with the chmod hint.
- **MAX_RUNS:** 2 runs against MAX_RUNS=1 was refused.
- **Scrub:** a fake key planted in a log was detected and not printed; a different key passed. The test output directory was deleted afterwards.

## Open
- The clean exit-0 path can only be shown with a real key.
- `EST_USD_PER_TURN` (0.02) is a placeholder; set it from gpt-6-luna's real price.

# C5: per-run transcripts (2026-09-24)
- **`--transcript-out FILE`:** writes the output of `AgentRun.Transcript.of_run`. Like `Metrics.row`, it is a pure function and never sees Settings. Contents:
  - task, arm, model, starting program and prompt;
  - `events` in chat order:
    - agent: turn, text cut to 600 chars, usage (prompt/completion/cached/cost) and tool_calls. Each call has its name, full args, and a result with success, skipped, content cut to 400 chars, and the diff's old/new printed as code;
    - user/feedback messages;
    - api_failure messages;
  - `jev_selections` and `jev_edits`, in call order (the logs have latency but no timestamps);
  - final program, final value, goal, goal_met, said_done.
- **Shared logs:** the Jev logs are now drained once in `report_and_exit`, so the row and the transcript report the same calls.
- **Runner:** `run-nav-study.sh` (and so `jev-eval.sh`) writes `<results dir>/<task>.<arm>[.b<batch>].<rep>.transcript.json` next to each log. jev-eval's existing leak check scans these files too.
- **Fixed:** `--stub` now ignores OPENROUTER_API_KEY. With a key set, the reducer's follow-up turn after the canned reply would otherwise have sent a real request.
- **Verified:** STUB jev-eval on fix-middle with a fake key in the env. Both transcripts were written with agent, tool-call and api_failure events; the jev arm has its jev_edit record (2 holes seen, 1 filled). The fake key was found in no output, and there was no network call.

# Incident (2026-09-24 ~11:50): real eval results deleted by my test cleanup
- **What happened:** after a STUB test for C5 I ran `rm -rf bench/results/jev-eval-*` by hand. The glob also matched the lead's paid run `bench/results/jev-eval-20260924-114421/`, which was deleted. No script had a delete in it. Recovery: the OneDrive web recycle bin, since `rm` bypasses the macOS Trash.
- **Fix:**
  - STUB and DRY_RUN runs of `jev-eval.sh` and STUB runs of `run-nav-study.sh` now default to a fresh `mktemp -d` dir, so tests never write under bench/results.
  - `jev-eval.sh` has a `RESULTS_DIR` override and refuses to reuse an existing output folder.
  - No code path deletes results.
- **Rule for myself:** never `rm` under bench/results or any results dir. Test output goes to mktemp or the scratch dir.

# C5b: symptom-style nav tasks + SUMMARY line (2026-09-24)
- **New tasks:** `nav-fleet-sym`, `nav-parts-sym`, `nav-warranty-sym`.
  - Each `.hz` is a byte-identical copy of the original. goal, feedback, max_turns and nav_targets are unchanged.
  - The prompts describe observable symptoms and expected behaviour only.
  - Checked in code: no nav_targets path segment (module names and binding names) appears in any prompt, as a whole word, case-insensitive.
  - The originals are untouched, for comparison.
- **nav-warranty-sym** keeps its dependency-test wording as "the function that works out how much of a repair is covered … every place that uses it", without naming it.
- **`jev-eval.sh` SUMMARY line:** `SUMMARY runs=N goal_met=k/N spend_usd=X (main, jev_nav, jev_edit; M run(s) without billed main cost)`, summed from the rows' billed costs.
  - Checked on synthetic rows: 0.12 + 0.01 + 0.002 = 0.1320.
  - A STUB eval of the three -sym tasks ran in a temp dir; bench/results was untouched.
- **Note:** the default TASKS (nav-* + fix-middle) now includes the -sym twins, so it exceeds MAX_RUNS=12. Set TASKS explicitly.

# C6: Jev lab notebook generator (2026-09-24)
- **Usage:** `node bench/notebook/build.js [OUT.html]` writes one self-contained page, to a new temp dir by default. It refuses to write under `docs/` without `--allow-docs`.
- **Key check:** the build fails without writing if the output contains `sk-or-` or `$OPENROUTER_API_KEY`.
- **Metadata, hand-maintained in `bench/notebook/`:**
  - `evals.json`: id, title, round, models, measured spend, data file, results dirs.
  - `modes.json`: 6 modes, each with a plain-English description and flow steps.
  - `tasks-meta.json`: plain name, what the program is, what is broken, prompt style, bug count, definitions.
  - `findings.json`: seeded from eval-001/002 and discussion-log #23–#24.
- **Page files:** `body.html`, `notebook.css` and `client.js`. The references' `<style>` blocks are pulled in verbatim at build time, so the look, tokens and dark mode are the Eval 001/002 reports'. `hbars`, the timeline cells, chips and `hl` from those reports are generalised and shared by every tab.
- **Runs, one normalised shape from three sources:**
  - Eval 001: its data file.
  - Eval 002: its data file, replaced by the matching results-folder copy (same task, arm and wall_ms) because that copy has the full row and transcript.
  - Any `bench/results/jev-eval-*` folder not claimed by an eval: listed as "new HHMM" until written up. Empty folders (evals still running) are skipped.
  - Mode comes from the row's flags; old trimmed rows fall back to the arm label.
- **Row change:** new runs now carry `transcript` (basename) in the metrics row. Older rows are matched to transcripts by task, arm and turn count.
- **Tabs:** Overview (tiles, headline findings, a test × mode board; a mode only counts as a win if it is correct and at least 10% cheaper and faster than control), Modes, Tests, All runs (mode/test filter chips, click to open details), Run details, Findings log, Spend.
- **Verified:**
  - Built from Evals 001–002 plus the lead's in-progress Eval 003 folders: 7 runs.
  - A node DOM stand-in ran the page script with no errors.
  - Headless Chrome screenshots in light and dark look right. Headless Chrome won't go below 500px wide, so a 360px content width was emulated: only tables and code overflow, inside their scroll wrappers.
  - No `sk-or-` in the output. The docs guard and the key guard both fail cleanly.
  - bench/results was only read.
