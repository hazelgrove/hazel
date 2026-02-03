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

**Status:** Deferred. Add when we want richer feedback between editor cursor and closure cursor bar.

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
