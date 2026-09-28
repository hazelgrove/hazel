# Workstream B — agent integration (log)

Branch `russ/jev-nav-subagent`, 2026-09-22. Codes against the Stage 0 `JevNav`
contract (`select`, `selection`, `decide`, `jev_decide`); never against A's internals.

## What landed

| Piece | Where |
|---|---|
| `suggested_paths` (Jev-owned, `[@yojson.default []]`), `SetSuggested`, `open_paths` (union), `view_summary`, freshen both sets | `AgentContext.re` |
| Renderer leaves `expanded ∪ suggested` unfolded | `CompositionView.re` (one line) |
| `modify_view(intent)` schema | `ViewTools.re` |
| `ModifyView(string)` action + parse | `CompositionActions.re`, `CompositionUtils.re` |
| Registry: `modify_view` = View / Ungated (allowed in converse) | `AgentToolUtils.re` |
| Exposed only when `jev_view_tool` | `AgentSend.enabled_tools(~jev_view_tool)` |
| Seam + shared helpers (`select_view` ref, `modify_view_intents`, `select_all`) | new `agentCore/AgentViewSelect.re` |
| Pre-pass | `AgentSend.handle_dispatch_send` → `JevPrepassDone` → `finish_dispatch_send` |
| modify_view async resolution | `AgentResponse.resolve_views_then_handle` → `HandleLLMResponseWithViews` |
| Handler: apply selection / fail cleanly | `AgentToolCallHandler.update(~view_selections)` |
| Tool result `opened: a, M/inner · kept: b` | `AgentToolExec` (from post-update `agent_view`) |

## Async design (the one real choice)

Both callers reuse the existing "hold, then re-enter" shape of the loop instead of
making tool execution async.

- **Pre-pass.** `DispatchSend` starts `select_view` and returns, leaving
  `pending_dispatch_send` set. That keeps the chat busy (queued sends wait) and makes
  Stop cancel it exactly like a phase-1/phase-2 gap stop — no new Stop branch.
  `JevPrepassDone(chat_id, seq, selection)` applies `SetSuggested` (skipped if
  `metrics.failed`) then runs the old phase-2 body (`finish_dispatch_send`:
  `update_context` → `dispatch_send`). Wire order untouched. A new transient
  `jev_prepass_seq` (bumped on each pre-pass and on a gap Stop) drops a stale result
  that lands after Stop + resend.
- **modify_view.** `HandleLLMResponse` first collects distinct `modify_view` intents;
  if the flag is on it runs them all through Jev in parallel and schedules
  `HandleLLMResponseWithViews(..., intent → selection)`, which runs the **unchanged**
  synchronous fold with the answers in hand. During the wait `awaiting_response` and
  `main_llm_seq` are untouched, so Stop is caught by the existing flight-seq gate.
  Tool order, stop-on-first-failure, and skipped rows are unchanged.
- Flags off: no `select_view` call, no new action scheduled, `modify_view` absent from
  the tool list → identical payload to `dev`.

## Deviations

1. **No `CompositionPrompt` paragraph.** The system prompt is baked into each chat at
   creation and is the cache prefix, so any text there changes the control arm
   (breaks "flags-off byte-identical"). The guidance lives in the `modify_view` tool
   description instead, which only ships when the flag is on. Conditional prompt text
   would need settings plumbed into `Chat.init` — not worth it.
2. **`collapse` also removes a path from `suggested_paths`.** Otherwise the model
   can't hide a Jev-opened binding and burns a turn. Model outranks Jev.
3. modify_view is resolved against the zipper **before** the reply's tool batch runs;
   an edit earlier in the same batch that adds a binding won't be selectable in that
   call. Acceptable (next turn sees it); documented in code.
4. Tool-count test bumped 36 → 37 (new tool).

## Tests — full suite green: 4015 tests, exit 0 (`dune build @runtest --profile dev`)

- `Test_AgentTools`: SetSuggested replace/keeps pins, union dedup, collapse closes
  suggestion, freshen drops stale suggestions, `view_summary`, old-JSON load default,
  suggested binding renders unfolded, `modify_view` parse + missing-intent failure,
  registry category.
- `Test_AgentControlFlow` (fake `select_view`, no HTTP): tool hidden/present by flag
  (off list = on list minus `modify_view`), modify_view applies selection + one-line
  result, flag off ⇒ failure without Jev call, Stop during modify_view resolution,
  pre-pass sets suggested and snapshot before the request, flags off ⇒ no Jev call and
  a failed pre-pass leaves `api_messages_for_openrouter` identical, Stop during
  pre-pass, stale pre-pass cannot hijack the next send.

## Open

- Jev latency now sits on the critical path of every user message when the pre-pass is
  on (UI shows the user message but no "thinking" state). Measure; consider a
  timeout that proceeds with the old view.
- `select_view` has no abort handle; a stopped pre-pass still finishes its HTTP call
  (result dropped). Fine for cost at Jev prices.
- JevNav `Log.record` is left to A's `select`; the eval runner (C) drains it.

## Round 2 — `jev_edit` arm (V3), 2026-09-23

Full suite green: 4037 tests, exit 0 (`dune build @runtest --profile dev`, build_B).

**Built**
- `jev_edit(path, sketch, names?, literals?, intent)` schema in `EditTools.re`;
  `CompositionActions.JevEdit(JevEdit.request)`; parse in `CompositionUtils`
  (vocab lists optional → `[]`). Registry: Edit / EditGated, so Plan and Converse block it.
- Arms are orthogonal flags. `AgentSend.jev_arms_allow(globals, name)` is one pure filter:
  view arm swaps expand/collapse for modify_view, edit arm swaps the 8 direct edit tools
  (`edit_tools_replaced_by_jev`) for jev_edit. `enabled_tools(~globals, prompting)` now
  takes the globals record instead of one bool per arm (`dispatch_send` too).
- Handler refuses `EditorAction`/`InsertAtProgramBoundary` when `jev_edit_tool` is on
  (same blindness as expand/collapse). `JevEdit(request)` → rebuilds the editor from
  Jev's zipper like a boundary insert (fresh statics/dynamics) → normal success path:
  `update_context`, whole-program diff + before/after segments, `is_edit`.
- Result line: `filled N/M holes` + ` · unfilled: <hole_id> : <expected type>, …`, or the
  error on failure (program unchanged).
- `/jev-edit-tool` slash command: `ChatSlashCommands`, `AgentGlobals.ToggleJevEditTool`,
  `ChatBottomBar` (`toggle_with_notice`), `Test_AgentUX` expected list.

**Async: one pre-resolution step for every Jev-backed call**
- `AgentViewSelect.re` → renamed `AgentJev.re`. `AgentJev.resolve` collects the reply's
  modify_view intents and jev_edit requests (only for arms that are on), runs views in
  parallel and edits as a **chain** (each edit runs on the previous edit's zipper), and
  fires once with `resolved = {views, edits}`. `HandleLLMResponseWithViews` became
  `HandleLLMResponseResolved(…, resolved)`; the synchronous fold is unchanged.
- Each edit result records the program text it was computed from (`base_text`). The
  handler applies it only over that exact program, so an edit can never silently drop a
  change made earlier in the reply. Chaining keeps multi-edit replies working.
- Context string = `CompositionView.print` of the chat's agent view on that zipper.
- Seam `AgentJev.edit_code` (like `select_view`); no key → `ChoiceFailed(401)` through `JevEdit.edit`.

**Tests** (fake `edit_code`, no HTTP): arm swaps tools and control = all tools minus the
Jev ones; arms combine; Plan blocks jev_edit; direct edits refused; outcome becomes the
editor (text, `is_edit`, diff, snapshot); unfilled holes listed with types; failure
leaves the program; two edits in one reply chain; jev_edit parse; golden edit list;
tool count 38.

**Deviations / notes**
- Edited `agentView/ChatBottomBar.re` and `test/Test_AgentUX.re` (outside the round-1
  list; needed for the slash command, as asked).
- `CompositionActions` now depends on `JevEdit`: `JevEdit.re` must not use
  `CompositionActions`/`CompositionUtils` (would be a cycle).
- The base-program guard is a program-text comparison (`print_zipper`): cheap and
  robust to id churn; a probe placed between two jev_edits in one reply is not part
  of that text and would be lost by the second edit. Rare; noted.
- `StringUtil.plain_search` misses matches after multibyte chars (`⋱`); the new tests
  use `Str`. Pre-existing helper, not changed.

## Round 3 — "Jev builds" switch, 2026-09-24

Full suite green: 4050 tests, exit 0 (build_B, `--profile dev`).

- Flag `AgentGlobals.jev_edit_builds` (yojson/sexp default false) + `ToggleJevEditBuilds`;
  `/jev-builds` via `toggle_with_notice` (ChatSlashCommands, ChatBottomBar, Test_AgentUX
  list). `/jev-mode` untouched (sketch mode).
- Schema: `EditTools.mk_jev_edit(~builds)` is the one source for both variants; shared
  `path/names/literals/intent`, `sketch` only when `builds=false`. `jev_edit` (registry)
  is byte-identical to round 2; `jev_edit_builds` has its own description (path + precise
  intent + names incl. parameters + literals; Jev constructs). `AgentSend.jev_schema`
  swaps the variant inside `enabled_tools`, next to the arm filter.
- Parse: `sketch` optional → `""`.
- A sketch sent anyway while builds is on is **dropped**: `AgentJev.effective_request`
  blanks it, used both when asking Jev and in `find_edit(~globals)`, so the lookup key
  matches. Why: the arm must measure Jev building, not the planner's code.
- Builds alone (edit arm off) changes nothing: tool JSON byte-identical to control (tested).
- Result line unchanged.
- Tests: schema has/lacks sketch by flag (rest shared), builds-alone JSON identical to
  control, sent sketch ignored (Jev sees `""`, edit applies, result line), parse without sketch.
- Note: `dune build @src/fmt --auto-promote` also reformatted A3's `JevEdit.re` and
  `test/Test_JevEdit.re` (whitespace only). No content change from me.

### Round 3 add-on — `signature` (2026-09-24)

- Builds schema: `signature` param (Hazel type text) replaces `sketch`; description says
  always give it ("it restricts what the selector may build"); fib example uses
  `signature="Int -> Int"`. Sketch schema unchanged (byte-identical to round 2); it does
  not advertise `signature`, but a sent one is parsed in both modes.
- Parse: `get_optional_string` → `""` when absent. Tests: signature parsed; absent → `""`;
  builds schema keys = `path, signature, names, literals, intent`.
- Full suite: 4054 run, 1 failure in A3's `JevEdit.build` #4 (`"Int ->"` signature
  expected to fail, didn't): A3's in-progress code, not this change. All agent suites green.
- Formatted only my files this time (`dune promote <file>`); A3's `Test_JevEdit.re` has a
  pending fmt diff I left alone.

## Round 4 — spec-only jev-mode, `add_tests`, clearer jev_edit (2026-09-24)

Full suite green: 4059 tests, exit 0 (build_B, `--profile dev`).

- `/jev-mode` (`SetJevMode`) now also sets `jev_edit_builds`; `jev_mode_on` requires all
  four, so builds-off reads as off and the next `/jev-mode` turns everything on. Notice:
  "Jev mode (pre-selection + modify_view + jev_edit builds from spec)".
- `add_tests(tests)`: `CompositionActions.AddTests(list(string))`, parse rejects `[]`.
  Registry Edit/EditGated (blocked in plan/converse). Exposed only in the edit arm:
  `AgentSend.jev_edit_arm_tools = ["jev_edit", "add_tests"]`, hidden when the arm is off,
  so the control list is unchanged (tested). Handler refuses it when the arm is off.
- Placement: `AgentToolCallHandler.final_expression` descends through Let/TyAlias/
  ModuleExp bodies and `;` tails; `test e end;` lines go right before that term via the
  existing `PerformUtils.insert_term(…, Left)` (same path as `insert_before`), then the
  usual statics veto + `normalize_top_level`. Fib program → `let fib = … in` /
  `test fib(0) == 0 end;` / `test fib(10) == 55 end;` / `fib(10)` (tested). New tests go
  after existing ones.
- Descriptions: both jev_edit variants say an existing path REPLACES the right-hand side
  only (never `let <name> =`, other bindings, tests, or the final expression); new code
  goes to a NEW path; tests go through add_tests.
- DRY note for the integrator: the statics-veto + normalize tail of `insert_tests`
  mirrors `CompositionGo.Public.insert_at_boundary` (not my file); a shared
  `insert_at(z, caret_move, code)` there would remove ~8 duplicated lines.

## Round 5 — additive modify_view (2026-09-24)

Full suite green: 4066 tests, exit 0 (build_B). Trigger: eval-002 (18 replacing
modify_view calls, +6 turns).

- `ModifyView(intent, replace)`; `replace` optional bool, parse default `false`.
- Default adds: new `AgentContext.AddSuggested` (union, order kept, no dupes);
  `replace=true` → `SetSuggested` (exactly Jev's picks). Pre-pass still `SetSuggested`
  (resets per user message). Collapse/expand blindness unchanged.
- Result: `AgentContext.Utils.view_change_summary(~before, after)` →
  `open: x, a · added: a` (`added: (none)` when the call opened nothing, so the planner
  sees a no-op). `AgentToolExec` captures the view before the handler runs.
- Description rewritten per lead's text (ask for everything in one call; mentioned
  paths open exactly; calls add; replace only for an unrelated focus). Ships only
  with the flag, so the control prefix is unchanged.
- Tests: additive union over an existing suggestion, replace resets, explicit false adds,
  parse replace=true, parse without replace → false, schema has optional `replace`.
- `dune promote` limited to my files; JevNav.re / Test_JevNav.re have pending fmt diffs (A's).
