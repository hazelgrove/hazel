# Jev navigation + editing — working notes

Research-lab record for the experiment: pair a fast System-1 decision model (Jev) with a
typed structure editor (Hazel). The planner (Claude) does the System-2 thinking; Jev makes
local picks; Hazel guarantees every state is valid. Theory: [`theory.md`](./theory.md).

**Findings so far: [`findings-summary.md`](./findings-summary.md).**

## Current state (2026-09-24)

Built, tested (full suite green), uncommitted on `russ/jev-nav-subagent`. Real Jev calls go
through OpenRouter (`typesafe/jev-1.13`); first live call reached the API. Every mode is a
flag; flags combine; all off = today's agent.

| Mode | Flag / slash command | What the agent can do |
|---|---|---|
| Control | — | expand/collapse + direct edits |
| Pre-pick | `jev_prepass` · `/jev-prepass` | Jev opens relevant code before each turn; expand still available |
| Navigator | `jev_view_tool` · `/jev-view-tool` | only `modify_view(intent)`; expand/collapse hidden + refused |
| Editor (sketch) | `jev_edit_tool` · `/jev-edit-tool` | only `jev_edit(path, sketch with ?, names, literals, intent)`; Jev fills holes |
| Editor (Jev builds) | `jev_edit_builds` · `/jev-builds` | `jev_edit` without sketch; Jev builds from one `?` via typed forms |
| Full Jev | `/jev-mode` | pre-pick + navigator + editor (sketch) |

Priorities (#20–#22): navigation and fan-out edits first (clearly System 1); building from a
spec + search is the novel research claim. Sketch mode is a baseline only.

## Files

| File | What |
|---|---|
| [`theory.md`](./theory.md) | The research claim: typed structure editor + System-1 policy + planner + search |
| [`plan.md`](./plan.md) | Navigation design (selection, what Jev sees, metrics) |
| [`nav-encodings.md`](./nav-encodings.md) | Frontier walk vs per-arm depth vs one-shot selection: evidence and decision |
| [`v3-jev-implementor.md`](./v3-jev-implementor.md) | Jev as editor: holes, candidates, rounds, names/literals |
| [`discussion-log.md`](./discussion-log.md) | Every question, option, pushback, decision (#1–#22) |
| [`research.md`](./research.md) | Every source read, with what it changed |
| [`workstreams.md`](./workstreams.md) | Parallel-agent split, contracts, file ownership per round |
| [`integration.md`](./integration.md) | Integrator passes: fixes, verification, open issues |
| [`eval-harness-integration.md`](./eval-harness-integration.md) | Team's headless runner; how the before/after study runs |
| [`agent-subsystem-map.md`](./agent-subsystem-map.md) | Where the existing agent code lives |
| `ws-A.md` / `ws-B.md` / `ws-C.md` | Per-workstream build logs |

Convention: `docs/plans/` is gitignored upstream, so these live under `docs/notes/`.
