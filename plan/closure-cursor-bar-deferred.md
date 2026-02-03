# Closure Cursor Bar - Deferred Ideas

## Collapsing Recursive Runs

**Concept:** When you have a run of recursive function applications (e.g., `map` called repeatedly), collapse them into something like `map ↻5` with a count.

**Considerations:**
- For built-in HOFs (map, fold, filter): Makes more sense since intermediate calls are structurally identical
- For general recursion: Trickier - different recursive calls might represent different states/arguments
- UX question: What does clicking a collapsed run do? Which call do you jump to?
  - Name click → outer call site (the actual calling of the function, not inner recursive calls)
  - Number/arrow click → expand to show all?
- Could be a toggleable option, perhaps off by default
- The inner recursive calls don't correspond to the same application site

**Status:** Holding off. Current display with blue color + 0.7 opacity already visually subordinates ghost entries. Revisit if deep recursive stacks prove cluttered in practice.

---

## Ghost Entry Toggle

**Concept:** Double chevron (`❯❯`) before first ghost entry that toggles visibility of ghost entries.

**What we tried:**
- Double chevron replacing the separator before first ghost entry
- Clicking it would: jump to location + set index + toggle visibility

**Why we removed it:**
- Conflicting behaviors on single click (navigate vs toggle)
- Modifier key (shift+click) felt awkward
- Separate toggle button added clutter

**Current state:** Double chevron visual indicator disabled. Ghost entries always shown with 0.7 opacity + blue color coding.

**Status:** Removed for now. Could revisit with a cleaner interaction model if needed.

---

## Indicated Entry Highlighting (Red Outline)

**Concept:** Show a red outline/border on a breadcrumb entry when the editor cursor is "related" to that function call - similar to how indicated samples get a red border.

### Option 1: Cursor on the app expression
- Check if `indicated_id == app_id`
- **Difficulty: Easy** - we already have `app_id` for each entry, just need to pass `indicated_id` to the view function

### Option 2: Cursor on the function name/variable
- Check if `indicated_id == fn_id` (the ID of the function expression in the app)
- **Difficulty: Easy** - we already extract `fn_id` in `get_fn_info`

### Option 3: Cursor inside the function body
- Get the binding site of the function via `Info.get_binding_site`
- Check if `indicated_id` is a descendant/contained within that body
- **Difficulty: Medium** - needs parent traversal or containment check
- Note: There may already be helpers for this - auto-probe mechanism likely uses something similar. Check `ProbePerform.re` or related files for existing "is inside function" helpers. May want to extract to a shared helper location.

### Option 4: Cursor inside lexical extent of the let binding
- Similar to #3 but includes the pattern side too
- **Difficulty: Medium** - same containment check approach

### Implementation Notes:
- `Indicated.piece(zipper)` or similar gives the indicated ID
- For #3-4, look for existing helpers in auto-probe code before writing new ones
- Could start with #1 (easiest) and expand later

**Status:** Option 1 implemented. Options 2-4 deferred.

---

## Separator Click / Sample Indication Sync (TODO - Investigate)

**Observed issue:** When clicking a separator in the closure cursor bar, the breadcrumb entry gets the red "indicated" outline (because the syntax cursor moves to the app_id), but the corresponding **sample** in the probe doesn't get indicated.

**Possible cause:** Sample indication is tied to `indicated_call` in `sample_cursor`, which only gets set when clicking directly on a sample (via `Capture` action). Clicking the separator only calls `SetIndex` + `jump_to` - it doesn't set `indicated_call`.

**Possible off-by-one aspect:** The sample cursor index might be at a different depth than expected, causing the "wrong" sample to appear indicated (or none at all).

**To investigate:**
- How does `indicated_call` interact with sample display?
- Should separator clicks set `indicated_call`?
- Is there an index mismatch between breadcrumb position and sample filtering?

**Status:** Needs investigation. Noted late at night when too tired to fully diagnose.

---

## Dropdown Menus Showing Branching Paths

**Concept:** Dropdown menus on breadcrumb entries that show different call paths from the sample data.

### Example
```
let double = fun x -> x * 2
let process = fun y -> double(y) + double(y + 1)
process(5)
```

Call stacks from samples inside `double`:
- `[process_app, double_app_1]` → first call, `double(5)`
- `[process_app, double_app_2]` → second call, `double(6)`

Dropdown on `double` entry could show:
```
┌─────────────┐
│ double(5)   │  ← from first call site
│ double(6)   │  ← from second call site
└─────────────┘
```

### For map/fold iterations
```
map(fun x -> x + 1, [1, 2, 3])
```

Dropdown could show iteration index or input value:
```
┌───────────┐
│ map @ [1] │
│ map @ [2] │
│ map @ [3] │
└───────────┘
```

### Tree Structure Observation
All sample call stacks form a tree - they share common prefixes. The closure cursor bar shows one **path** through this tree. Dropdowns would show **siblings** at each node.

### Data Source
- Use all collected samples from evaluation
- Each sample has call_stack, step_start, step_end (temporal ordering)
- The step ranges give partial order on expression evaluation durations

**Limitations:**
- Works well when there are distinct, meaningful branches
- For deep recursion with many similar calls, could get unwieldy
- Best for: small numbers of distinct call sites, built-in HOFs where iteration context is meaningful

**Status:** Deferred. Explore if dropdown navigation proves useful for debugging workflows.

---

## Investigation Breadcrumbs / Backtracking Debugging

**Concept:** A debugging history system separate from the call stack, allowing you to mark "investigation points" and backtrack when pursuing one path doesn't pan out.

### Motivation
When debugging, you often:
1. Trace down through function calls following a wrong value
2. Find an expression that's giving the wrong result
3. That expression might have multiple variable references - each potentially the bug source
4. You want to pursue one, and if it doesn't pan out, backtrack and try another

### Example
```
let a = 5
let b = 10
let c = a + b  // result is 15, but expected 20
```

Debugging flow:
1. Mark `a + b` as an "investigation point"
2. Pursue `a` → trace back, see it's 5, looks fine
3. **Backtrack** to investigation point
4. Pursue `b` → trace back, find the bug

### Conceptual Model
An "investigation stack" separate from call stack:
```
Investigation Stack:
├─ [1] a + b at line 3  (problem is in dynamic extent of this)
│   ├─ [1.1] pursuing `a` → checked, looks fine
│   └─ [1.2] pursuing `b` → (current investigation)
```

### Key Distinction from Current Pin
- Pin filters which samples are shown
- Investigation breadcrumbs track **your debugging path** (where you've looked), not just the **program's call path** (what executed)

### Possible UI
- Mark expressions as "investigation points" (separate from pin)
- Small panel or dropdown showing investigation history
- Keyboard shortcuts: mark point, go back, go forward
- Closure cursor bar indicator if you're "inside" an investigation

### Related Concept: Multiple Pins
Could also be implemented as multiple pins with a stack/tree structure, rather than just one pin.

**Status:** Deferred. This is a more general debugging workflow enhancement. Could be valuable for complex debugging scenarios.

---

## Keyboard Navigation

**Concept:** Navigate the closure cursor bar with keyboard.

### MVP (Implemented)
- Left/Right arrow keys to move between breadcrumb entries
- Enter to jump to the current entry's definition (then refocuses main editor)

### Focus Shortcut (TODO)
Need a keyboard shortcut to focus the closure cursor bar from the main editor.

**Candidate shortcuts:**
- `Ctrl+;` - ergonomic, usually unbound
- `Ctrl+Shift+K` - mnemonic for "call stacK"
- `Ctrl+Shift+D` - mnemonic for "Dynamic cursor"

**TODO:** Check with team on preferred shortcut to avoid conflicts with existing bindings.

### Extension: Dropdown Navigation
- Down arrow opens a dropdown showing function applications within the current function body
- Could show applications that are lexically inside the function definition
- Selecting one would jump to it (and possibly add a probe if needed)
- This would allow keyboard-based navigation through the entire call structure

### Data Source Questions
- Should dropdowns be populated from dynamics (what actually executed) or lexically (what apps exist in the function body)?
- Lexical: Always shows all possible calls, even if not executed this run
- Dynamic: Only shows calls that actually happened, with sample data
- Could be a combination: lexical structure, annotated with dynamic info

**Status:** MVP implemented. Focus shortcut pending. Dropdown extension deferred.

---

## Bidirectional Hover Highlighting

**Concept:** Highlight matching elements when hovering.

### Breadcrumb → Sample
When hovering a breadcrumb entry, highlight the corresponding sample(s) in probes.

### Sample → Breadcrumb
When hovering a sample in a probe, highlight the matching breadcrumb entry.

### Implementation Thoughts
- Would likely need a `hover_cursor` field separate from the committed `sample_cursor`
- Both probes and closure cursor bar would check this for highlighting
- Similar infrastructure to indicated highlighting, but triggered by hover instead of syntax cursor

**Status:** Deferred. Useful for understanding correspondence between bar and samples.

---

## Argument Values in Tooltips

**Concept:** Show argument values in breadcrumb tooltips, e.g., `map([1,2,3])` instead of just `map`.

### Implementation Challenges
- Need to probe the argument expressions to capture their values
- Would require adding app argument IDs to sample targets before evaluation
- Closure cursor bar currently only receives statics info, would need dynamics too
- Need to fish out the argument values from samples and format them

### Alternative: Lighter Version
- Just show the argument expressions (syntax), not their values
- This is available from statics without additional probing

**Status:** Deferred. Requires infrastructure changes. Consider lighter syntax-only version first.

---

## Copy Call Stack

**Concept:** Button to copy the current call stack for debugging/sharing.

### Possible Formats
- Human-readable: `λ > map > filter > f`
- With locations: `λ > map (line 5) > filter (line 12) > f (line 3)`
- JSON for tooling integration

**Status:** Deferred. Low priority but potentially useful for bug reports.

---

## Multiple Sample Selection

**Concept:** Allow selecting multiple samples for comparison/analysis.

### Use Cases
- Compare two execution paths: "Why did `process(5)` work but `process(6)` fail?"
- Call stack diff: highlight where two samples' call stacks diverge
- Side-by-side value comparison

### Connection to Investigation Breadcrumbs
- Investigation breadcrumbs track exploration history (where you've looked)
- Multiple selection tracks comparison targets (what you're analyzing)
- Could potentially unify: investigation points become selectable samples

**Status:** Deferred. Interesting direction but adds complexity. Revisit when single-cursor workflows feel limiting.

---

## Time Travel Slider

**Concept:** Scrub through execution using step_start/step_end data.

### How It Would Work
- Slider representing evaluation steps
- As you scrub, highlight samples whose step ranges contain the current step
- Shows "program state at step N"

### Limitations
- With probe_all: many samples, rich scrubbing experience
- Without probe_all: limited to collected samples only
- May need UI to indicate "no data at this step"

**Status:** Deferred. Interesting but depends on probe density.

---

## Evaluation Context Stepping (Exploratory)

**Concept:** Capture evaluation context within function calls, enabling finer-grained stepping.

### Current State
Call stack entries represent function application boundaries. Within a function body, we don't track intermediate evaluation steps in the cursor.

### Possible Extension
- Capture evaluation context frames (what's being evaluated within a function)
- Alternating sequence: call stack entry → eval context → call stack entry → ...
- Would allow stepping through entire program at expression granularity

### Open Questions
- What exactly constitutes an "evaluation context frame"?
- How to represent in UI without overwhelming?
- Relationship to step_start/step_end data we already have?

**Status:** Very exploratory. Needs more thought on what this would look like concretely.

---

## Pin from Dynamic Cursor UI

**Concept:** Allow pinning directly from the closure cursor bar, not just from sample dropdowns.

### Possible Affordances
- Right-click or long-press on breadcrumb entry shows context menu with "Pin here"
- Small pin icon appears on hover (like current unpin icon, but for pinning)
- Clicking would pin the call stack up to and including that entry

### Could We Show Full Sample Context Menu?

Analysis of what we'd need:

| Feature | Required Data | Currently Available? |
|---------|--------------|---------------------|
| Pin action | `app_id`, call stack | ✅ Yes - construct from `sample_cursor.call_stack` trimmed to entry index |
| Step Into | Full `Sample.t` | ❌ No - needs dynamics passed in |
| Environment | `sample.env`, `view_seg`, `utility` | ❌ No - needs dynamics + rendering utilities |

**Current ClosureCursorBar inputs:** `globals`, `refractors` (has `sample_cursor`), `info_map` (statics only)

**What's missing:** `Dynamics.Info.t`, samples list, `view_seg`/`utility`

**Verdict:** Simplified context menu with just Pin is doable now. Full menu (Step Into + Environment) requires threading dynamics through.

**Status:** Deferred. Pin-only version could be quick; full menu needs more plumbing.
