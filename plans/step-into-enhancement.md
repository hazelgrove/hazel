# Step Into Enhancement Plan

This plan describes enhancements to the "step into" feature for probes, making it work in the context of specific samples (execution traces) rather than just syntax.

## Background & Motivation

### Current Behavior
Step into (`Refractors.step_into`) currently:
1. Finds the function binding site via `is_jump_target`
2. Gets the function body via `Statics.Map.enclosing_let_of_binding`
3. Adds an auto probe on the function body
4. Updates `dyn_cursor` by prepending `ap_id` to `trimmed_stack(dc)`
5. Sets `pinned_stack` to the new call stack
6. Jumps the editor cursor to the function body (parameters if function literal)

Triggered via: Context menu, F8, Arrow Up in probe mode

### The Problem
Step into operates at the **syntax level**, not the **sample level**. When `f(x)` was called 5 times during evaluation, stepping into should take you to the function body while maintaining your position in the execution trace - you should see the body's evaluation **for that specific invocation**, not all invocations blended together.

### The Solution
Move step into to the **sample context menu** (environment dropdown). Being in that dropdown means you've already selected a specific sample, so step into can use that sample's exact `call_stack` to maintain execution context.

Additionally, after stepping into, the relevant sample at the destination should receive **focus** - both visually and for keyboard navigation.

---

## Architecture Overview

### Key Data Structures

**Zipper.Refractor.t** (in `src/haz3lcore/zipper/ZipperBase.re`):
```ocaml
type t = {
  manuals: Map.t,              (* Manual probes *)
  autos: list(Id.t),           (* Auto-probe anchors *)
  ephemerals: Map.t,           (* Derived probes from autos *)
  dyn_cursor: Language.DynCursor.t,
};
```

**DynCursor.t** (in `src/language/dynamics/Sample.re`):
```ocaml
type t = {
  stack: Probe.call_stack,           (* Full call stack *)
  index: int,                        (* Effective depth *)
  pinned_stack: option(Probe.call_stack),
  indicated_call: option(Id.t),
  time: option(float),
  iter: int,
  step_range: option((int, int)),
};
```

**Sample.t** (in `src/language/dynamics/Sample.re`):
```ocaml
type t = {
  id: int,
  syntax_id: Id.t,
  value: DHExp.t,
  env: Env.t,
  call_stack: Probe.call_stack,  (* Key: identifies this specific execution *)
  time: float,
  iter: int,
  step_start: int,
  step_end: int,
};
```

### Key Files

| File | Purpose |
|------|---------|
| `src/haz3lcore/Refractors.re` | `step_into` implementation (lines 240-290) |
| `src/haz3lcore/projectors/implementations/ProbeProj.re` | Probe UI, environment dropdown |
| `src/haz3lcore/zipper/action/DynCursorPerform.re` | Dynamic cursor actions |
| `src/haz3lcore/zipper/action/Action.re` | Action type definitions |
| `src/language/dynamics/Sample.re` | Sample types, cursor relations |
| `src/haz3lcore/projectors/ProjectorBase.re` | Focusable interface |
| `src/haz3lcore/projectors/ProjectorPerform.re` | Focus action execution |
| `src/web/view/ScratchMode.re` | Worker result handling, `schedule_action` pattern |
| `src/web/www/style/projectors/proj-probe.css` | Probe styling |

### Projector Focus System

ProbeProj currently has:
- **Pointer focus**: `Some(id => JsUtil.get_elem_by_id(Id.cls(id))##focus)`
- **Keyboard focus**: `None` (can't arrow-key into probe from editor)

Once focused (via click), the probe handles keyboard events via `key_handler`.

To "focus a sample":
1. Give DOM focus to the probe element
2. Trigger `DynCursor(Capture(sample, ap_id))` to select the specific sample

### Timing Considerations

**probe_all ON**: All samples are collected by the worker, BUT the projector DOM element won't exist until after a view cycle (adding auto probe → model update → render → DOM exists).

**probe_all OFF**: Must wait for both:
1. Projector to render (view cycle)
2. Worker to return with samples

Both cases require some form of deferred action for focus.

---

## Implementation Phases

### Phase 1: Refactor Environment Dropdown → Sample Context Menu

**Goal**: Clean separation of context actions from environment display.

**Current structure** (ProbeProj.re `env_view` function, lines 519-566):
```
div.sample-dropdown
├── Pin Header (ad-hoc)
└── Live Environment (variable bindings)
```

**Target structure**:
```
div.sample-context-menu
├── Context Actions Section
│   ├── Pin / Unpin
│   └── Step Into (NEW)
└── Environment Section
    └── Variable bindings
```

**Tasks**:
1. Create `sample_context_actions` function that renders the action buttons
2. Create `sample_environment` function that renders variable bindings
3. Refactor `env_view` to compose these two sections
4. Update CSS classes for the new structure
5. Ensure SafeTriangle behavior still works

**Files to modify**:
- `src/haz3lcore/projectors/implementations/ProbeProj.re`
- `src/web/www/style/projectors/proj-probe.css`

---

### Phase 2: Move Step Into to Sample Context Menu

**Goal**: Step into appears in sample context menu, uses sample's call_stack.

**Tasks**:

#### 2.1 Add Step Into UI
In `ProbeProj.re`, add step into button to context actions:
- Show "Step into" when `is_jump_target(ap_id)` returns `Some(_)`
- Icon suggestion: `→` or `↓` or similar
- Handler: `step_into_call(~parent, ~sample, ~ap_id, ~di)`

#### 2.2 Create Step Into Handler
```ocaml
let step_into_call = (~parent, ~sample: Sample.t, ~ap_id: option(Id.t), ~di) =>
  switch (ap_id) {
  | Some(ap_id) =>
    parent(Refractor(StepIntoSample(sample, ap_id)))
  | None => Effect.Ignore
  };
```

#### 2.3 Add New Action Variant
In `Action.re`, add to refractor actions:
```ocaml
| StepIntoSample(Language.Sample.t, Id.t)
```

#### 2.4 Modify Refractors.step_into
Change signature to accept `Sample.t`:
```ocaml
let step_into = (~sample: Sample.t, ~ap_id: Id.t, z: Zipper.t, statics): result(Zipper.t, Action.Failure.t)
```

Key change - use `sample.call_stack` instead of `trimmed_stack(dc)`:
```ocaml
(* OLD *)
let trimmed = DynCursor.trimmed_stack(dc);
let call_stack = [ap_id, ...trimmed];

(* NEW *)
let call_stack = [ap_id, ...sample.call_stack];
```

#### 2.5 Ensure Step Into Pins
The current implementation already sets `pinned_stack`. Verify this is preserved.

#### 2.6 Remove from Syntax Context Menu
In `ContextMenu.re`, remove the "Step into" entry (lines 123-144).
Keep the `is_jump_target` function as it's still needed.

**Files to modify**:
- `src/haz3lcore/projectors/implementations/ProbeProj.re`
- `src/haz3lcore/zipper/action/Action.re`
- `src/haz3lcore/Refractors.re`
- `src/web/app/editors/code/ContextMenu.re`

---

### Phase 3: Immediate Focus (Synchronous Case)

**Goal**: After step into, give focus to the matching sample at the destination.

**Challenge**: Even when samples are available (probe_all on), the projector DOM element doesn't exist until after a view cycle.

**Approach**: Use `schedule_action` to defer focus until after render.

**Tasks**:

#### 3.1 Add Pending Focus State
In `Sample.re`, extend `Cursor.t`:
```ocaml
type pending_focus = {
  probe_id: Id.t,
  target_stack: Probe.call_stack,
};

type t = {
  (* existing fields *)
  pending_focus: option(pending_focus),  (* NEW *)
};
```

#### 3.2 Set Pending Focus in Step Into
At the end of `step_into`, set `pending_focus`:
```ocaml
let body_id = ...; (* the function body we're jumping to *)
let target_stack = [ap_id, ...sample.call_stack];
let dc = {...dc, pending_focus: Some({probe_id: body_id, target_stack})};
```

#### 3.3 Create Focus Resolution Action
Add new action in `Action.re`:
```ocaml
type dyn_cursor =
  | Capture(...)
  | TogglePinCall(...)
  | Reset
  | ResolvePendingFocus  (* NEW *)
```

#### 3.4 Handle Focus Resolution
In `DynCursorPerform.re`, handle `ResolvePendingFocus`:
```ocaml
| ResolvePendingFocus =>
  switch (z.refractors.dyn_cursor.pending_focus) {
  | None => Ok(z)
  | Some({probe_id, target_stack}) =>
    (* Give DOM focus to probe *)
    JsUtil.get_elem_by_id(Id.cls(probe_id))##focus;
    (* Clear pending_focus *)
    let z = update_dyn_cursor(z, dc => {...dc, pending_focus: None});
    (* Find and capture matching sample - done via existing selection logic *)
    Ok(z)
  }
```

#### 3.5 Schedule Focus After Step Into
In the action handler for `StepIntoSample`, after performing `step_into`:
```ocaml
(* Use Effect.Many or schedule_action to queue focus resolution *)
schedule_action(DynCursor(ResolvePendingFocus))
```

**Note**: Need to investigate exact mechanism - may need to use `Effect.Many` in the view layer or pass `schedule_action` through.

**Files to modify**:
- `src/language/dynamics/Sample.re`
- `src/haz3lcore/zipper/action/Action.re`
- `src/haz3lcore/zipper/action/DynCursorPerform.re`
- `src/haz3lcore/Refractors.re`
- Potentially view layer files for scheduling

---

### Phase 4: Deferred Focus (Async Case)

**Goal**: Handle focus when probe_all is off and we must wait for worker results.

**Tasks**:

#### 4.1 Add "Evaluating" Status Indicator
In `Sample.re`, extend `empty_status`:
```ocaml
type empty_status =
  | NoSamplesExist
  | HiddenByPin
  | NotAligned
  | Evaluating  (* NEW *)
```

#### 4.2 Add Animated Indicator UI
In `ProbeProj.re`, add case in `empty_status_view`:
```ocaml
| Evaluating =>
  div(
    ~attrs=[Attr.classes(["empty-status", "evaluating"]), ...],
    [text("⟳")]  (* or similar spinning symbol *)
  )
```

#### 4.3 Add CSS Animation
In `proj-probe.css`:
```css
.empty-status.evaluating {
  animation: spin 1s linear infinite;
}

@keyframes spin {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
```

#### 4.4 Detect Evaluating State
Need logic to determine when a probe is waiting for evaluation:
- Probe exists (in manuals/ephemerals)
- No samples yet
- Evaluation is in progress (worker hasn't returned)

May need to track "evaluation in flight" state.

#### 4.5 Resolve Focus When Worker Returns
In `ScratchMode.re` (and similar mode files) where worker results are handled:
```ocaml
(* After updating result *)
schedule_action(DynCursor(ResolvePendingFocus))
```

The `ResolvePendingFocus` handler will:
1. Check if `pending_focus` is set
2. Check if samples now exist for that probe
3. If so, focus the probe and capture the matching sample
4. Clear `pending_focus`

**Files to modify**:
- `src/language/dynamics/Sample.re`
- `src/haz3lcore/projectors/implementations/ProbeProj.re`
- `src/web/www/style/projectors/proj-probe.css`
- `src/web/view/ScratchMode.re` (and other mode files)

---

### Phase 5: Polish

**Tasks**:

#### 5.1 Keyboard Shortcut
Consider adding keyboard shortcut for step into when sample is focused.
In ProbeProj `key_handler`, add case for step into (e.g., Enter or specific key).

#### 5.2 Edge Cases
- No matching sample found after focus resolution → clear pending_focus, don't error
- Multiple samples match target_stack → pick first (by iter) or use existing selection logic
- Step into on non-function → should be prevented by UI (only show when `is_jump_target` succeeds)

#### 5.3 Testing
- Test step into from sample context menu
- Test focus works with probe_all on
- Test focus works with probe_all off (async)
- Test evaluating indicator appears and animates
- Test pin is set correctly
- Test cursor stack is set correctly

#### 5.4 Documentation
Update any user-facing documentation about step into feature.

---

## Implementation Notes

### Using schedule_action

The `schedule_action` pattern is used throughout the codebase for async operations:

```ocaml
(* In calculate function *)
let calculate = (~settings, ~schedule_action, ~is_edited, model) => {
  (* ... *)
  Worker.send(
    ~onmessage=result => {
      schedule_action(ResultAction(UpdateResult(result)));
    },
    request
  );
  (* ... *)
};
```

For our use case, we can schedule `ResolvePendingFocus` after the step into action completes.

### Call Stack Semantics

When stepping into function application `ap_id` from a sample with `call_stack = [a, b, c]`:
- New call stack should be `[ap_id, a, b, c]`
- This matches what samples inside the function body will have
- The evaluator adds `ap_id` to call_stack when `RecordStackFrame` is processed

### Focus vs Capture

- **Focus**: DOM focus on probe element (`##focus`), enables keyboard navigation
- **Capture**: Updates `dyn_cursor` to select a specific sample (`Capture(sample, ap_id)`)

Both are needed for full "focus a sample" behavior.

---

## Open Questions

1. **Exact schedule_action mechanism**: Need to trace through how to schedule an action from within `Refractors.step_into` or the action handler. May need to return an effect or use existing patterns.

2. **Evaluating state detection**: How to know evaluation is "in flight"? May need to track pending worker requests.

3. **ProbeText.re**: Should the evaluating indicator be added to text output for LLM consumption?

---

## Summary

| Phase | Scope | Complexity |
|-------|-------|------------|
| 1 | Refactor environment dropdown | Low - pure refactoring |
| 2 | Move step into, use sample.call_stack | Medium - new action, modified logic |
| 3 | Immediate focus with schedule_action | Medium - new state, deferred action |
| 4 | Async focus, evaluating indicator | Medium - worker integration |
| 5 | Polish, testing | Low |

The core semantic change is Phase 2: using `sample.call_stack` directly instead of `trimmed_stack(dc)`. Phases 3-4 add the focus enhancement which makes the feature feel more polished and maintains context through the navigation.

---

## Notes for Implementation

*From the planning conversation:*

1. **The semantic goal matters**: This isn't just a UI reorganization. The user is building toward a more algebraic notion of "stepping" through execution traces. Step into should feel like following a specific execution path, not just jumping to a definition. Keep this in mind when making UX decisions.

2. **Why the environment dropdown**: Putting step into there *enforces* that a sample is selected - you can't even see the menu without having indicated a sample. This sidesteps edge cases about "what if no sample is selected." The user mentioned they might revisit putting it back in the syntax context menu later, but wants the simpler case first.

3. **The focus requirement is important but subtle**: The user specifically wants the destination sample to receive focus so you can immediately keyboard-navigate within it. This is about continuity - you're following an execution path, not just jumping around. Don't skip phases 3-4 thinking they're optional polish.

4. **probe_all ≠ projector exists**: Even with probe_all on (samples available), the projector DOM element won't exist until after a view cycle. Both cases need deferred focus, the difference is just whether you're also waiting for samples.

5. **Build incrementally**: Each phase should leave the system working. Phase 1 is pure refactoring with no behavior change. Phase 2 is the core feature. Phases 3-4 are enhancements. Test at each phase.

6. **The user thinks out loud**: If you get clarification requests, expect some stream-of-consciousness. The key information is usually there, just interleaved with thinking.

7. **Check the tests**: There are probe-related tests in `test/evaluator/`. Run `node _build/default/test/haz3ltest.bc.js test 'Probe'` to find them. The plan doesn't add new tests but you should make sure existing ones pass.
