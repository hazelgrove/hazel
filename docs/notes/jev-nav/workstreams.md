# Workstreams — parallel agents

Planning only; no code starts without an explicit go. Each workstream owns disjoint
files so agents never edit the same file. They meet at **Stage 0 contracts**.

## Code principles (all agents)

- **Ponytail first:** reuse `HighLevelNodeMap`, `AgentContext`, `API.request`,
  `AgentRun`, the `defer_dispatch_send` seam pattern. New code only where nothing exists.
- **Modular = ReasonML modules + module signatures**, not classes. "OOP" here means
  encapsulation behind `.rei` interfaces and swappable implementations (e.g. a
  `Decider` signature with `Jev` and `Fake` implementations). No objects, no mutation
  outside the existing seams.
- **Functional core, thin shell:** pure functions for everything decidable
  (summaries, questions, thresholds, frontier); HTTP and state updates only at the edge.
- **DRY:** one `curate_view`, two callers. One metrics record, one JSONL writer.
- Tests with every module; flags-off ⇒ existing 3,982 tests unchanged.

## Stage 0 — contracts (one agent, first, small)

Freeze the interfaces everyone codes against, as `.rei` files + one doc:
- `Decider` signature: `decide(state, questions) → answers + usage` (async callback).
- `NavMetrics.t` record (fields from `plan.md` §6) + `to_json`.
- Flag names: `jev_prepass`, `jev_view_tool` (`AgentGlobals`), `--jev-prepass`,
  `--jev-view-tool` (CLI).
- Task file schema v2 (extends Matt's: adds `nav_targets` = bindings a good run opens).

## Workstream A — Jev-facing subagent (transport + walker)

Owns: `src/util/OpenRouter.re` (new `SystemOne` module only), new
`CompositionCore/JevNav.re` (pure), new `agentCore/AgentNav.re` (walker).
- `SystemOne`: POST `/api/v1/systemone`, reuse `API.request` + Bearer header.
- `JevNav`: `summary_of_node`, `questions_of_frontier`, `decide_of_answers`,
  `frontier_of_opened`. Question wording rules from `plan.md` §3.
- `AgentNav`: monotone frontier walk over an injected `Decider`; returns
  `open_paths` + `NavMetrics.t`.
- Tests: pure `JevNav` tests; walker tests with a `Fake` decider.

## Workstream B — agent integration (prompts, tools, wiring)

Owns: `ToolJsonDefinitions/ViewTools.re` (add `modify_view`), `CompositionUtils.re`,
`CompositionActions.re`, `AgentToolCallHandler.re`, `AgentSend.re` (pre-pass hook),
`AgentGlobals.re` (flags), `CompositionPrompt.re` (one short `modify_view` section).
- Tool description teaches *concrete* intents.
- Pre-pass in `handle_dispatch_send`, before `update_context`; wire order unchanged.
- Tests: `Test_AgentControlFlow`-style — flags on/off, `expanded_paths` set, prompt
  prefix byte-identical.

## Workstream C — evals (tasks, runner, analysis)

Owns: `bench/tasks/`, `src/CLI/AgentRun.re`, `Cli.re` `agent_cmd` flags,
new `bench/run-nav-study.sh`, new `bench/summarize-nav.js`.
- Write ~5 **navigation-hard** tasks: 30–100+ bindings, nested modules, each touching
  2–4 bindings; record `nav_targets`.
- Stop dropping nav tool calls from logs; port `usage_rows` / cost from `AgentEval.re`;
  write one `NavMetrics` JSONL row per run.
- Runner: tasks × 4 arms × reps, arms interleaved. Summarizer: medians vs control.
- Can start **now** (no dependency on A/B): tasks + control-arm logging.

## Ordering

```
Stage 0 contracts ─┬─> A (Jev subagent) ─┐
                   ├─> B (integration)  ─┼─> merge ─> record all 4 arms ─> analyze
                   └─> C (evals) ── record control baseline early ──────┘
```

B depends on A only through the Stage 0 `Decider` signature (codes against `Fake`).
C records the control baseline before A/B merge — pre-registration.

## Integrator pass (last, one agent)

Merge A/B/C, dedupe helpers that grew twice, run full `make test` + `dune build @src/fmt`,
then the 4-arm recording. Each agent is briefed with its section here + the Coding
Standards in `~/.claude/CLAUDE.md`. Worktrees if any file ownership turns out to overlap.

## Why not more agents

A prompts agent separate from B would edit the same files (`ViewTools`,
`CompositionPrompt`) → merge conflicts. Prompt/tool text stays with B.

## Stage 0 — done (2026-09-22)

Contracts written as compiling stubs (CLI builds green):
- `src/util/OpenRouter.re` → `module SystemOne`: `noul_question`, `answer {id, p_yes}`,
  `usage {input_tokens, cost_usd}`, `reply = Answers | Failed`, `decide(~key, ~model_id=?,
  ~state, ~questions, ~handler, ())` (stub).
- `src/haz3lcore/CompositionCore/JevNav.re` → `node`, `thresholds`, `metrics`,
  `decide` (injected function type), `selection {open_paths, metrics}`; pure stubs
  `nodes_of`, `question_of`, `state_of`, `batches`, `classify`, `close_ancestors`;
  `select(~decide, ~intent, ~on_done, z)`; `jev_decide(~key)`; `Log.record/drain`.
- `AgentGlobals.Model`: `jev_prepass`, `jev_view_tool` (default false),
  `jev_batch_max_tokens` (8000).
- View ownership (#10 accepted): `AgentContext.Model` gains `suggested_paths`
  (Jev-owned); `expanded_paths` stays model-owned. Implemented by B.

## File ownership (enforced)

| Agent | Owns | Build dir |
|---|---|---|
| A | `OpenRouter.re` (`SystemOne` only), `JevNav.re`, new `test/Test_JevNav.re`, `test/haz3ltest.re` | `$SCRATCH/build_A` |
| B | `AgentContext.re`, `CompositionView.re`, `ViewTools.re`, `CompositionUtils.re`, `CompositionActions.re`, `AgentToolCallHandler.re`, `CompositionPrompt.re`, all `src/web/view/agentCore/*` except `AgentGlobals.re` fields already added; tests inside existing `Test_AgentTools.re` / `Test_AgentControlFlow.re` | `$SCRATCH/build_B` |
| C | `src/CLI/AgentRun.re`, `src/CLI/Cli.re`, `bench/**` | `$SCRATCH/build_C` |

Each agent logs its work in `docs/notes/jev-nav/ws-<A|B|C>.md`. Nobody commits.

## Round 2 — V3 (Jev implements), 2026-09-23

Variants are orthogonal flags: `jev_prepass`, `jev_view_tool` (built), `jev_edit_tool`
(new). Edit arm: main model loses edit tools, gains `jev_edit(path, sketch, names,
literals, intent, mode)`; Jev fills the sketch's `?` holes from `TyDi.suggest`
candidates + planner vocab, one Choice per hole per round.

Stage 0 (done): `SystemOne.choice_question / choice_answer / choice_reply /
decide_choices` (stub); `JevEdit.re` (`request`, `hole`, `metrics`, `outcome`,
`decide_choices`, `holes_in`, `question_of`, `state_of`, `edit`, `jev_decide_choices`,
`Log`); `AgentGlobals.jev_edit_tool`.

| Agent | Owns (round 2) |
|---|---|
| A2 | `SystemOne` choice section, `JevEdit.re`, `test/Test_JevEdit.re`, `test/haz3ltest.re` |
| B2 | `jev_edit` tool + wiring (same files as round 1), `/jev-edit-tool` slash command |
| C2 | `AgentRun.re`, `Cli.re` agent_cmd, `bench/**`: `--jev-edit-tool`, arms, edit metrics |
