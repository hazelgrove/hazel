# Jev as a navigation subagent — the plan (v2, converged)

Status: **converged design, pre-implementation.** Branch `russ/jev-nav-subagent` off
`dev` @ `761b865b`. Why each choice was made: `discussion-log.md` (#1–#14),
`nav-encodings.md`, `research.md`. Who builds what: `workstreams.md`.

## 1. One paragraph

The main agent pays a full big-model turn every time it opens or hides code. We add
one pure function, `select_view(intent) → open set`: Jev answers one calibrated yes/no
per definition — *"is this binding relevant to the intent?"* — seeing each definition's
real code. Our code then opens every "yes" plus its parents. Two callers: an automatic
**pre-pass** on each user message, and a **`modify_view(intent)`** tool the agent can
call. Both flag-gated, default off ⇒ identical to `dev`. Jev never edits, never picks
tools, never decides "done".

## 2. Selection (E3: one-shot + ancestor closure)

1. Build `HighLevelNodeMap` (exists).
2. One node per binding: `{path, name, type, code, refs, used_by}` where `code` = its own
   full definition with nested bindings folded (`⋱`), comments stripped; `refs`/`used_by`
   from statics (`GeneralTreeUtils.get_refs_to`, exists). Every line of the program
   appears once.
3. One Noul per node, all in one request if < ~8k tokens, else split and sent in parallel
   (batch size is a swept parameter).
4. `p ≥ 0.7` → yes; `p ≤ 0.35` → no; between → no, logged (the escape hatch).
5. Open set = yes-set ∪ all ancestors. Parents show their own code; their other nested
   bindings stay folded. Siblings are folded, not expanded.

One round trip. No walk, no loop, nothing to undo. Scale fallback (top-level prune, then
flat) only above ~256 bindings; not built until a task needs it.

## 3. What Jev sees

No system prompt. JSON `state` + `questions`:

```json
{
  "model": "typesafe/jev-1.13",
  "state": {
    "intent": "fix how billed amounts are computed",
    "bindings": [
      {"path": "billed", "name": "billed", "type": "[(Int,Int,Int)] -> [Int]",
       "code": "fun rows -> map(rows, fun r -> volume(r) + cost(r))",
       "refs": ["volume", "cost"], "used_by": ["total"]}
    ]
  },
  "questions": {
    "billed": {"type": "noul",
      "instructions": "Is binding `billed` relevant to the intent?",
      "criteria": {"true": "Reading or changing `billed` helps achieve the intent.",
                   "false": "The intent can be achieved with `billed` folded."}}
  }
}
```

Question wording rules (Jev's documented weak spots): name the path explicitly; positive
criteria on both sides; no counting, ranking, negation, or "is the view enough?".

## 4. View ownership (proposed — #10, awaiting Russ)

`pinned` = opened by the model (`expand`), closed only by the model. `suggested` =
chosen by Jev, replaced on each new intent. View = `pinned ∪ suggested ∪ ancestors`.
"Whoever opened it closes it." No blind reset.

## 5. Callers and arms

| | pre-pass off | pre-pass on |
|---|---|---|
| **tool off** | control (`dev` today) | view pre-selected each user message |
| **tool on** | agent can call `modify_view(intent)` | both |

- Pre-pass: `AgentSend.handle_dispatch_send`, before `update_context`.
- Tool: `modify_view(intent)` in `ViewTools.re`; result is one line (`open: …`);
  snapshot refresh shows the view. Tool description asks for concrete intents.
  **In this arm the agent is blind to `expand`/`collapse`** (hidden + refused, #15).
- Wire order unchanged (`prompt, dev notes, history, snapshot`) → prompt cache intact.
  Jev traffic is a separate HTTP call, never in the chat.
- Flags: `jev_prepass`, `jev_view_tool` (`AgentGlobals`); `--jev-prepass`,
  `--jev-view-tool` (CLI); `--jev-batch N`.

## 6. Metrics (one JSONL row per run)

- **Jev:** requests, questions, input tokens, cost, latency, batch size, band count,
  `closure_added`, `dep_added`.
- **Main model:** turns, `expand`/`collapse`/`modify_view` calls, prompt / completion /
  cached tokens, cost.
- **Outcome:** `goal_met`, `said_done`, feedback rounds, final static errors, wall time.
- **Selection quality:** recall / precision vs each task's `nav_targets`.

Primary: main-model navigation turns and cost at equal `goal_met`. Bar to explain:
Forge/Intent.Lisp's 33–44% fewer navigation steps.

## 7. Eval design

Runner = Matt's `hazel agent` (already extracted, verified with `--stub`). Needed:
- **Navigation-hard tasks** (30–100+ bindings, nested modules, fix touches 2–4 bindings
  across subtrees), each with `nav_targets`.
- Variants: obfuscated names; a caller+callee fix in different subtrees (dependency test).
- Sweeps: 4 arms × batch size {1, 4, 16, all}; optional E1 frontier-walk arm.
- Control baseline recorded **before** Jev code lands.

## 8. Code shape (functional core, modular shell)

- `OpenRouter.SystemOne` — HTTP only (reuse `API.request`).
- `JevNav` (pure) — nodes from the map, questions, thresholds, closure.
- `Decider` signature with `Jev` and `Fake` implementations — tests never touch HTTP.
- `select_view` — composes the above; returns open set + metrics record.
- Callers wire it in; everything else reused. Build split: `workstreams.md`.

## 9. Status since v2

Built: selection (E3), `modify_view`, pre-pass, view ownership, and the navigator arm is
**blind** to expand/collapse (#15). Editing modes are in `v3-jev-implementor.md`. Model pinned
to `typesafe/jev-1.13`.

## 10. Open

- Accept #4 view ownership?
- Pre-pass on every user message vs only the first (measure).
- `modify_view` tool description wording.
- V3 later: Jev picks and executes closed-set tool calls (probes, statics, projectors).
