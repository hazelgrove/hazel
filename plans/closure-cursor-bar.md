# Closure Cursor Bar (Dynamic Cursor Bar)

## Status: Partially working

Working: name clicks for user-defined functions (e.g. `update`), separator
clicks for direct app_ids, separator fallback (rounding down).

**Not working:** Name clicks for built-in functions (e.g. `fold_left`).
See "Known issue" section below.

## What was done

1. **Created `ClosureCursorBar.re`** - New component that displays call stack breadcrumbs
   - Shows function names from applications in the call stack
   - Uses arrow separators between entries
   - Highlights the "focused" entry (at sample_cursor.index)
   - Shows ghosted entries beyond current index
   - Only appears when probes exist
   - Shows "⌀" when at top level (empty call stack)
   - Click on entry jumps to that syntax location

2. **CSS in `style.css`** - Added closure cursor bar styling
   - Positioned in row 2 of a 4-row grid layout
   - Matches top bar aesthetics
   - Hidden when no probes exist (via `.hidden` class)

3. **Modified `Page.re`** - Integrated closure cursor bar into page layout
   - Bar is rendered between top bar and main content

4. **Modified `Transition.re`** - Added `RecordStackFrame` to built-in function applications
   - Both print and other built-in functions now record stack frames

5. **Fixed grid layout** - Added grid positioning to `#sidebars` in `style.css`

## Click behavior overview

The bar alternates between **separator chevrons** (`❯`) and **function names**.
- Chevrons jump to the **call site** (the application expression)
- Names jump to the **definition** (the function's binding site)

When a click target isn't navigable (because the ID points to built-in
internal code or a non-zipper tile), we want to fall back to the nearest
navigable target by walking down the call stack.

### Current click handler structure

**Name clicks** use `definition_target` (tiers 1-2) and `fallback_target` (tier 3):
- **definition_target** (tier 1: `get_fn_info` body_id, tier 2: `fn_def_id`):
  Jump to definition + set cursor index. Works for user-defined functions.
- **fallback_target** (tier 3: `find_nearest_user_app`):
  Just jump, no cursor index change (target is at a different depth).
- **Neither**: Just set cursor index (no jump).

**Separator clicks** use direct vs fallback:
- **Direct** (app_id in user code): Jump + set cursor index. Works.
- **Fallback** (rounded down to nearest user call site): Just jump.

### Cursor index policy

`set_cursor_index` is only dispatched when the jump target is at the
same depth as the clicked entry. For fallback targets (which are at a
different, shallower depth), we only jump — no cursor index change.
This avoids mismatches between the syntax cursor position and the
dynamic cursor depth.

## Known issue: built-in function name clicks

### Symptom

Clicking on `fold_left` (or any HazelFn built-in) in the breadcrumb
shows "can't move" in the console. The separator chevron next to it
works fine.

### Root cause

`get_fn_info` calls `Info.get_binding_site` for the `Var("fold_left")`
reference. This returns a real-looking UUID (not `Id.invalid`), even
though `fold_left` is a built-in. The ID changes between page loads
(freshly generated each time).

This ID is NOT navigable — it doesn't correspond to a tile in the
user's zipper. When `jump_to(id)` dispatches `Move(Goal(TileId(id)))`,
`jump_to_id_indicated` in Move.re can't find the tile and returns
`None`, which becomes `Cant_move`.

**Why `get_binding_site` returns a real ID for built-ins:**

Built-in context entries are created with `id: Id.invalid` in
`BuiltinsUtil.ctx_entry_of_builtin`. The `get_binding_site` function
checks `entry.id == Id.invalid` and returns `None` for invalid IDs.

However, `fold_left` is a `HazelFn` — its implementation is actual
Hazel code (a `FixF` expression with `Pat.var("fold_left")`). Somewhere
during statics/elaboration, the FixF pattern binding creates a context
entry for `fold_left` with a fresh (non-invalid) ID. This entry shadows
or replaces the original `Id.invalid` entry, so `get_binding_site`
returns the fresh ID.

**What we tried that didn't work:**
- Checking if the binding site ID is in `info_map`: The ID IS in
  info_map (the built-in implementation goes through statics or the
  IDs end up there through some other mechanism).
- Checking `Builtins.env_init` for the name: Would work but has a
  shadowing edge case (user-defined `fold_left` would also be skipped).
- Swapping effect ordering: Both orderings fail for the same reason.

### Proposed fix

The core problem is: `get_fn_info` returns a body_id that looks valid
but isn't a navigable tile. We need a way to check navigability.

**Option A (recommended):** Pass the measured map or a tile-existence
check function to `ClosureCursorBar.view`. Then in `get_fn_info` (or
in the click handler), verify the body_id corresponds to an actual
tile before using it. If not, fall through to the fallback.

**Option B:** In `get_fn_info`, for `Var(name)`, look up the binding
site and also verify it's a `Pat` entry that was created by user code
(not by built-in FixF elaboration). This requires understanding exactly
how the FixF pattern IDs end up in the context.

**Option C (simplest):** Skip `get_binding_site` entirely for names
that are in `Builtins.env_init`. Accept the edge case that user-
shadowed built-in names also won't get jump-to-definition. (In
practice, users rarely shadow built-in names.)

## Example scenario

```
let update : (Ledger, Action) -> Ledger =
  fun (ledger, action) -> ...
in
let run : (Ledger, [Action]) -> Ledger =
  fun (ledger, actions) ->
    fold_left(actions, update, ledger)
in
```

When probing inside `update` during a `fold_left` iteration, the
dynamic cursor bar shows:

```
                                              app_ids from:
  λ ❯ run ❯ fold_left ❯ fold_left ❯ update
  ^   ^      ^            ^            ^
  |   user   user code    built-in     built-in
  |   code   (fold_left   internal     internal
  |          (...))       (recursive)  (callback)
  top-level
```

### Current click behavior

| Click target               | Works? | What happens                           |
|----------------------------|--------|----------------------------------------|
| `update` (name)            | YES    | Jumps to `update` definition (tier 2, fn_def_id) |
| `fold_left` (name, outer)  | NO     | "can't move" — body_id is non-navigable |
| `fold_left` (name, inner)  | NO     | Same issue (fallback finds outer app_id, but definition_target takes priority) |
| `run` (name)               | YES    | Jumps to `run` definition (tier 1)     |
| `❯` before `run`           | YES    | Jumps to `run(...)` call site          |
| `❯` before outer fold_left | YES    | Jumps to `fold_left(...)` call site    |
| `❯` before inner fold_left | YES    | Rounds down to `fold_left(...)` call site |
| `❯` before `update`        | YES    | Rounds down to `fold_left(...)` call site |

### Desired behavior for fold_left names

When clicking a built-in function name, since there's no navigable
definition, fall back to the nearest user-visible call site (same as
the separator fallback). All fold_left names would jump to the
`fold_left(actions, update, ledger)` application in user code.

## Future idea: step-into through built-in higher-order functions

**Status:** Not implemented. Recording for future consideration.

**Problem:** When debugging a chain like `test → run → fold_left → update`,
step-into can get you from the test into `run`, but then you hit
`fold_left(actions, update, ledger)`. Step-into on this call lands inside
`fold_left` (a built-in), which isn't useful. You want to get to `update`.

Manually eta-expanding — replacing `update` with `fun a -> update(a)` — gives
you a user-visible application site that step-into can target. This works
because the lambda body has an explicit `update(a)` call you can step into,
which places a probe on `update`'s body and sets the dynamic cursor to the
right call context.

**Idea:** The system could simulate eta-expansion at the cursor level. When
step-into targets a built-in call:
1. Identify function-typed arguments (e.g. `update` in `fold_left(xs, update, acc)`)
2. Resolve the argument to its definition via statics/binding site
3. Place an auto-probe on the callback's body
4. Set the dynamic cursor to include intermediate built-in frames

This would behave as if the user had eta-expanded and stepped through manually.

**Challenges:**
- Identifying which argument is the callback (type-based or heuristic)
- Multiple function arguments: which one to step into? May need user choice.
- Constructing cursor state for frames the user never explicitly navigated through
- Non-trivial interaction with pin system and sample filtering

## `fn_def_id` pipeline

The `fn_def_id` field is populated during evaluation:

1. `Exp.get_fn_def_id` extracts the definition-site `Id.t` from
   Fun/TypFun expressions (looking through FixF/Parens wrappers)
2. `Transition.get_fn_def_id_from_expr` handles the DHExp wrapper,
   looking inside Closures to call `Exp.get_fn_def_id`
3. `RecordStackFrame(fn_name, arg, fn_def_id)` carries it as a side effect
4. `EvaluatorState.update` stores it in the `stack_frame` record
5. `ClosureCursorBar` uses it as the second fallback tier (definition_target)

For built-in functions, `fn_def_id` is always `None` (they have no
user-visible definition site).

## Files changed

- `src/web/app/probesystem/ClosureCursorBar.re` (new, with fallback system)
- `src/web/www/style.css`
- `src/web/www/style/sidebar.css`
- `src/web/app/Page.re`
- `src/language/dynamics/transition/Transition.re`
- `src/language/term/Exp.re` (added `get_fn_def_id`)
- `src/language/dynamics/Sample.re` (added `fn_def_id` to `stack_frame`)
- `src/language/dynamics/state/EvaluatorState.re` (updated `RecordStackFrame` effect)
- `src/haz3lcore/ProbePerform.re` (updated stack_frame constructions)
- `src/haz3lcore/projectors/implementations/ProbeProj.re` (updated pin construction)
- `src/web/app/probesystem/ProbeSidebar.re` (updated legend constructions)
- `test/Test_SampleSelection.re` (updated test helpers)
- `test/evaluator/Test_Evaluator_ProbeSelection.re` (updated test constructions)
