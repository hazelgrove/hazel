# Integrator pass — 2026-09-22

Merged A (Jev transport + selection core), B (agent wiring), C (eval harness) in one
working tree. Nothing committed.

## Review verdict

- **Flags off ⇒ dev behaviour.** `modify_view` filtered out of the tool list (the list
  is part of the cached prefix); pre-pass only when `jev_prepass`; no prompt text added.
- **Async handled without changing the synchronous tool fold.** Pre-pass holds
  `pending_dispatch_send` (Stop reuses the phase-gap cancel; `jev_prepass_seq` drops
  stale results). `modify_view` resolves all intents first, then re-enters
  `handle_llm_response` via `HandleLLMResponseWithViews`; flight-seq gate covers Stop.
- **Fail-safe everywhere.** Any failed batch ⇒ `failed`, view unchanged; missing key ⇒
  `Failed(401)` through the same path.
- **Selection quality** (C, after A's review): recall over what the agent sees
  (`yes` ∪ closure-added ancestors), precision over `yes` only; failed selections count
  as opening nothing. Control arm / no `nav_targets` ⇒ null.

## Fixed during integration

- **Shadowed bindings collided** (A's open issue): duplicate question ids made every
  selection on such a program fail. `JevNav.Nodes.path_of` now suffixes `#k` using
  `HighLevelNodeMap.matches_for_path` — the same numbering `path_to_id` resolves, so the
  renderer opens exactly the binding Jev picked. Test added (`n#1`, `n#2`).

## Left as-is (deliberate, documented)

- `AgentViewSelect.select_all` and `JevNav.select` both gather N callbacks with a
  counter. Two 6-line uses; a shared helper would be a speculative abstraction.
- `--jev-batch` is a token budget, not a binding count; sweep token budgets
  (e.g. 250 ≈ 1–2 bindings, 1000, 4000, 8000) instead of {1, 4, 16, all}.
- `AgentRun.Metrics.row` has no unit tests: `src/CLI` is an executable, not a library
  `test/` can link. Covered by the STUB end-to-end run.

## Open (for Russ)

1. Pre-pass blocks each send on Jev with no timeout. Add one (fallback: old view)?
2. A stopped pre-pass cannot abort its HTTP call; result is discarded. Acceptable.
3. `modify_view` selects against the program as it was before the reply's other tools
   ran; a binding inserted in the same batch is selectable next turn.
4. Bindings inside `module M = { … }` literals are not in `HighLevelNodeMap`, so Jev
   (and `expand`) can't address them. Tasks use nested `let`s. Upstream limitation.
5. Untyped parameters show as `?` in `typ`.
6. Everyone's format runs (`--auto-promote`) touched each other's files — layout only.

## UI toggles (added for manual testing)

Slash commands `/jev-prepass` and `/jev-view-tool` flip the `AgentGlobals` flags
(persisted with settings). `/show-thinking` now shares the same `toggle_with_notice`
helper in `ChatBottomBar.re`. Final suite: **4016 tests, exit 0**; agent tests re-run
green after the toggles (275).

# Integrator pass — round 2 (V3), 2026-09-23

Merged A2 (Choice + JevEdit engine), B2 (`jev_edit` tool, edit-arm blindness,
`AgentJev` resolver), C2 (7 arms, edit metrics, offline `jev_edit` stub). Combined
suite before fixes: 4037 tests, exit 0.

## Fixed during integration

- **Fills could break types.** `JevEdit.Holes.fill` used `overwrite_term` with no
  static check, so a planner name/literal of the wrong type could be written in —
  contradicting "well-typed by construction". `fill` now rejects any fill that
  raises the static-error count (the hole escalates). Rendering of hole markers
  uses the unguarded `overwrite` (markers are unbound names, never applied). Test:
  an ill-typed literal escalates and leaves 0 static errors.

## Verified

- No dependency cycle (`CompositionActions` → `JevEdit`; JevEdit uses only
  CompositionGo/View, HighLevelNodeMap, JevNav, TyDi).
- Chained `jev_edit`s: the second starts from the first's applied (normalized)
  zipper, so B2's program-text stale guard compares equal texts.
- Offline end-to-end: `hazel agent --stub --jev-edit-tool` on `fix-middle`:
  `volume(?) * cost(r)` → `volume(r) * cost(r)`, row shows 1/1 filled, 72 ms.

## Open (for Russ)

1. Builtins (`Some`, `None`, library functions) only offered if the planner lists
   them in `names` — TyDi candidates are limited to program bindings to avoid flooding.
2. Holes of unknown type get almost every binding, unranked.
3. A probe placed between two `jev_edit`s in one reply is lost (stale guard is
   text-only).
4. `--trace` (bench-incr) omits `jev_edit` edits; metrics unaffected.
5. The 40KB system prompt still teaches the direct edit tools and expand/collapse;
   in the Jev arms they are hidden + refused. Separate Jev-arm prompt = separate
   cache; decide after seeing refusal counts.

## Fix from first live UI test (2026-09-23)

**Bug:** in the edit arm the planner could not create code. `jev_edit` only revised an
existing binding (`Update(Definition)`), and the direct insert tools are hidden, so on an
empty program the agent looped on `jev_edit` failures (`Cant_derive_local_AST_information`)
and finally asked the user to type the code.

**Fix — create mode:** if `path` does not exist, `jev_edit` creates code. `JevEdit.Holes.create`
inserts the sketch after the last top-level binding (so earlier bindings stay in scope), or
prepends it when there are none (the old program becomes the body — a trailing `let … in`
needs one). Jev fills only holes the new code introduced (`scope = NewCode(ids_before)`), so
a pre-existing `?` is never asked. The no-path insert logic moved into
`CompositionGo.Public.insert_at_boundary`, now shared by the handler and JevEdit. Tool
description documents both modes with an example. Tests: empty program; after an existing
binding (candidate uses it). Suite: 4040, exit 0.

**Observed, not fixed:** the planner wrote a full program with zero holes, so Jev had
nothing to fill. `holes_seen` in the metrics measures this; whether to require ≥1 hole is a
research decision for Russ.

# Integrator pass — round 3 (Jev builds), 2026-09-24

Merged A3 (forms catalogue, pattern holes, build mode with round/holes caps, typed
`signature` spec with validation), B3 (`jev_edit_builds` flag, `/jev-builds`, schema
without `sketch` + with `signature`), C3 (`--jev-edit-builds`, `build` arms, per-edit
columns, builds-mode stub). Mid-round failure (partial signature `Int ->` accepted by the
error-tolerant parser) fixed by A: signatures must parse as complete types with no hole
and must not raise static errors. Final suite: **4054 tests, exit 0**. Web rebuilt; server
on localhost:8001 (8000 was in use by another local server).

Framing (#20–#22): build mode is a research arm, not a goal; priorities are navigation,
fan-out edits, and spec + search (`theory.md`).
