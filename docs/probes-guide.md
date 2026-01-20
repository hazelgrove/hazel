# Hazel Probes System: A User Guide

Probes are Hazel's live debugging and introspection system. They allow you to see the runtime values of expressions as your program executes, providing immediate feedback on how code behaves.

## Table of Contents

1. [Adding Probes](#adding-probes)
2. [Understanding Sample Display](#understanding-sample-display)
3. [Single vs Many Mode](#single-vs-many-mode)
4. [Empty Status Indicators](#empty-status-indicators)
5. [Environment Variables on Hover](#environment-variables-on-hover)
6. [Pinning](#pinning)
7. [Step-Into Navigation](#step-into-navigation)
8. [Sample Coloring](#sample-coloring)
9. [Keyboard Navigation](#keyboard-navigation)
10. [Auto Probes](#auto-probes)
11. [Internals Overview](#internals-overview)

---

## Adding Probes

There are two ways to add probes to expressions:

### Manual Probes

Wrap any expression with `^^probe(...)` syntax:

```
let x = ^^probe(1 + 2) in x
```

This creates a probe on the expression `1 + 2`, which will display its evaluated value (`3`) in the offside region next to the code.

### Pattern Probes

You can also probe patterns to see what values are bound:

```
let ^^probe(x) = 42 in x                    -- Shows: 42
let (^^probe(a), b) = (1, 2) in a + b       -- Shows: 1
let f = fun ^^probe(x) -> x * 2 in f(5)     -- Shows: 5
```

### Auto Probes (REPL Mode)

In REPL mode, probes are automatically placed on strategic expressions. The system intelligently selects:
- The largest rightmost expression on each line
- Function bodies rather than function values
- Individual elements of multi-line containers rather than the container itself

---

## Understanding Sample Display

When you add a probe to an expression, the **samples** are the collected values from each time that expression was evaluated during program execution.

### Text Representation

In text-only contexts (like LLM/agent interactions), probes are displayed with the `≡` divider:

```
let x = ^^probe(1 + 2)     ≡ 3
let f = fun x -> x + 1     ≡ [Function omitted]
```

Multiple samples are separated by `⫽`:

```
let double = fun x -> ^^probe(x * 2)
in [double(1), double(2), double(3)]     ≡ 2 ⫽ 4 ⫽ 6
```

### Why Multiple Samples?

A single probe can produce multiple samples when:

1. **Function bodies**: Each call to the function produces a sample
   ```
   let f = fun x -> ^^probe(x + 1)
   in [f(1), f(2), f(3)]     -- 3 samples: 2, 3, 4
   ```

2. **Recursive functions**: Each recursive call produces a sample
   ```
   let fact = fun n ->
     if n <= 1 then 1
     else ^^probe(n) * fact(n - 1)
   in fact(5)     -- 4 samples: 5, 4, 3, 2
   ```

3. **List operations**: When mapping over collections
   ```
   let items = [1, 2, 3]
   in map(items, fun x -> ^^probe(x * 2))     -- 3 samples: 2, 4, 6
   ```

---

## Single vs Many Mode

Probes have two display modes, toggled with **Space** when a probe is focused:

### Single Mode (Default)

Shows **at most one sample** per probe, aligned with the current cursor position in the execution trace.

```
Probe Display (Single Mode):
┌─────────────────────┐
│ fun x -> ^^probe(x) │  ≡ 2
└─────────────────────┘
         ↑ Shows only the sample at the current cursor depth
```

**Use case**: Following a specific execution path through nested function calls.

### Many Mode

Shows **up to 30 samples** in a scrollable window.

```
Probe Display (Many Mode):
┌─────────────────────┐
│ fun x -> ^^probe(x) │  ≡ 1  2  3  4  5 ...
└─────────────────────┘
         ↑ Shows multiple samples, scrollable with arrows
```

**Use case**: Seeing all values an expression takes across different executions.

When there are more than 30 samples, navigation arrows appear to scroll through different windows of samples.

---

## Empty Status Indicators

When no samples are shown for a probe, a status icon indicates why:

| Icon | Name | Meaning | Interaction |
|------|------|---------|-------------|
| `∅` | No Samples | Expression was never evaluated | None |
| `⍟` | Hidden by Pin | Samples exist but filtered by current pin | Click to clear pin |
| `⊖` | Not Aligned | Single mode: samples exist but none align with cursor | Click to jump to closest sample |
| `⟳` | Evaluating | Waiting for evaluation after step-into | Animated spinner |

### Common Scenarios for Empty Probes

**Branch not taken:**
```
if false then ^^probe(1) else 2     -- Probe shows ∅ (never evaluated)
```

**Case branch not matched:**
```
case 2
| 1 => ^^probe(10)     -- Shows ∅
| _ => 20              -- This branch taken instead
end
```

**Function never called:**
```
let f = fun x -> ^^probe(x + 1)     -- Shows ∅ if f is never called
in 42
```

---

## Environment Variables on Hover

When you hover over or focus on a sample, a context menu appears showing:

1. **Actions**: Pin/Unpin, Step Into (for function applications)
2. **Environment**: Variable bindings captured at sample time

```
Sample Context Menu:
┌────────────────────────┐
│ Pin                    │
│ Step Into              │
├────────────────────────┤
│ x ≡ 5                  │
│ y ≡ [1, 2, 3]          │
│ name ≡ "hello"         │
└────────────────────────┘
```

**Note**: Function values are filtered out (shown as "Opaque") since they don't display well in this context.

### When Environment is Most Useful

Environment display is particularly valuable when probing expressions that reference variables:

```
let outer = 10 in
let f = fun x ->
  ^^probe(x + outer)     -- Hover shows: x ≡ 5, outer ≡ 10
in f(5)
```

---

## Pinning

Pinning filters sample display to show only samples that occurred within a specific call context.

### Why Pin?

When debugging a function called many times, you may want to focus on samples from a specific invocation:

```
let process = fun item ->
  let result = ^^probe(complex_calculation(item))
  in result
in map([a, b, c, d, e], process)
```

Without pinning: Shows 5 samples (one per call)
With pin on `process(b)`: Shows only the sample from when `item = b`

### How to Pin

1. **Context Menu**: Click "Pin" on a sample
2. **Keyboard**: Press `p` when a sample is indicated
3. **Meta/Cmd + Click**: Toggle pin on a sample

### Pinned Samples Indicator

Pinned samples display a pin icon. Samples not matching the pinned call stack are filtered out (showing `⍟` status if all are filtered).

---

## Step-Into Navigation

Step-into allows you to navigate from a function application into its body while maintaining the execution context of a specific sample.

### What Makes Step-Into Special

Unlike traditional debugger step-into which blends all invocations:

**Traditional step-into:**
```
f(1); f(2); f(3)     -- Step into f: you see all 3 invocations mixed
```

**Hazel sample-level step-into:**
```
f(1); f(2); f(3)     -- Step into from sample for f(2): you see ONLY that invocation
```

### How to Step Into

1. Probe a function application:
   ```
   let f = fun x -> x + 1
   in ^^probe(f(5))     -- Sample shows: 6
   ```

2. Hover/focus on the sample to show context menu

3. Click "Step Into" (or press **Enter**)

4. Cursor jumps to function body, probe added there automatically:
   ```
   let f = fun x -> ^^probe(x + 1)     -- Now shows: 6 (in context of f(5) call)
   in f(5)
   ```

### Call Stack Context

After step-into:
- The pin is automatically set to the call context
- Samples at other depths are colored to show their relationship
- You can continue stepping into nested calls

---

## Sample Coloring

When you select (indicate) a sample, other samples are colored based on their relationship to it in the call stack:

### Color Categories

| Relationship | Visual | Meaning |
|--------------|--------|---------|
| **Cursor** | Green background | The selected/indicated sample |
| **Caller (Above)** | Magenta/Pink text | Samples from parent/ancestor calls |
| **Callee (Below)** | Cyan text | Samples from child/descendant calls |
| **Unrelated** | Gray/muted | Samples with incompatible call stacks |

### Visual Example

```
let grid = [[1, 2], [3, 4]] in
map(grid, fun ^^probe(row) ->           -- row samples: [1,2], [3,4]
  map(row, fun ^^probe(x) -> x))        -- x samples: 1, 2, 3, 4

When [1,2] is indicated (green):
  - x=1 and x=2 are cyan (below [1,2])
  - x=3 and x=4 are gray (unrelated - they're below [3,4])
  - [3,4] is pink (sibling at same depth)
```

### Direct vs Indirect

The coloring further distinguishes:
- **Direct**: One level up/down the call stack
- **Indirect**: Multiple levels up/down

This helps visualize how deeply nested you are in the execution trace.

---

## Keyboard Navigation

When a probe is focused:

| Key | Action |
|-----|--------|
| **Space** | Toggle Single ↔ Many mode |
| **←** / **→** | Navigate between samples |
| **Shift + ←** / **→** | Adjust display length of sample value |
| **p** | Toggle pin on indicated sample |
| **Enter** | Step into (for function applications) |
| **Escape** | Blur probe focus |
| **Shift + Escape** | Reset all probe state and clear pin |

### Navigating Large Sample Sets

In Many mode with more than 30 samples:
- Arrow buttons appear at probe edges
- Use ← / → to scroll through sample windows
- Sample count overlay shows total (e.g., "1k+")

---

## Auto Probes

In REPL mode, probes are automatically placed using intelligent heuristics.

### Placement Rules

1. **Rightmost-largest**: On each line, probe the largest expression ending at the rightmost position

2. **Avoid function types**: Functions display poorly, so probe the body instead
   ```
   let f =
     fun x -> x + 1     -- Probes: x + 1 (not f)
   in f(5)
   ```

3. **Multi-line containers**: Probe elements rather than the container
   ```
   let items = [
     a,     -- Probes: a
     b,     -- Probes: b
     c      -- Probes: c (not the whole list)
   ] in ...
   ```

4. **Let with hole body**: Probe the definition, not the hole
   ```
   let x = 2 + 1 in ?     -- Probes: 2 + 1 (not the ?)
   ```

5. **If expressions**: Probe individual branches
   ```
   if condition then     -- Probes: condition
     branch1             -- Probes: branch1
   else branch2          -- Probes: branch2
   ```

6. **Avoid redundant variable references**: Don't re-probe a variable that just refers to an already-probed binding

---

## Internals Overview

### Sample Data Structure

Each sample captures:

```
{
  id: int,              // Hash for deduplication
  syntax_id: Id.t,      // Which expression was probed
  value: DHExp.t,       // The evaluated value
  env: Env.t,           // Captured variable bindings
  call_stack: list(Id.t), // Function application context
  time: float,          // When evaluation occurred
  seq: int,             // Sequence number for ordering
  step_start: int,      // Step count when evaluation started
  step_end: int         // Step count when evaluation finished
}
```

### Call Stack Semantics

The `call_stack` is a list of function application IDs, with the most recent (innermost) call at the head:

```
// For: outer(inner(x))
// Inside inner's body: call_stack = [inner_ap_id, outer_ap_id]
// Inside outer's body: call_stack = [outer_ap_id]
// At top level: call_stack = []
```

### Sample Cursor vs Sample

**Sample**: A specific evaluation moment (value, environment, call stack, timing)

**Sample Cursor**: Which evaluation moment the user is focused on across ALL probes

The cursor stores coordinates (call_stack, index) rather than a sample reference because:
1. Many probes share one cursor
2. Samples are recomputed on every edit
3. Intent preservation requires remembering deeper call context

### Sample Targets

The evaluator is told which expressions to sample via a `targets` map:

```
type targets = Id.Map.t(capture_spec)
type capture_spec = { refs: Binding.s }  // Which variables to capture
```

When evaluation encounters a targeted expression, it creates a sample with the current call stack and specified environment bindings.

---

## Quick Reference

### Syntax

| Syntax | Purpose |
|--------|---------|
| `^^probe(expr)` | Manual probe on expression |
| `^^probe(pattern)` | Pattern probe on binding |

### Status Icons

| Icon | Meaning |
|------|---------|
| `∅` | Never evaluated |
| `⍟` | Hidden by pin |
| `⊖` | Not aligned (Single mode) |
| `⟳` | Evaluating |

### Text Output Symbols

| Symbol | Meaning |
|--------|---------|
| `≡` | Separates expression from values |
| `⫽` | Separates multiple values |

---

## Notes and Caveats

### Implementation Observations

**Sample Coloring Variations**: You mentioned there are "two other categories of sample coloring specifically for applications" - I found CSS classes for `.ap` and special handling for `is_below_indicated_call` in the cursor relation logic. The implementation distinguishes:
- `cursor-callee` with `direct` vs `indirect` (for depth inside indicated call)
- `cursor-caller` with `direct` vs `indirect` (for calls above)
- There's also step-range based coloring (`StepContainedWithin`, `StepContains`, `StepDisjoint*`) but this appears to be for the Steps sample base mode rather than the default Calls mode.

The CSS has some styling variations but several blocks are commented out. The primary visible distinction appears to be caller (pink/magenta) vs callee (cyan) vs unrelated (gray).

**Sample Base Modes**: The implementation has three sample filtering strategies (`Calls`, `Steps`, `StepRange`) with various cutoff options (`before_cutoff`, `after_cutoff`, `caller_cutoff`, `callee_cutoff`), but these settings appear to be suppressed in the current UI - the default Calls mode is active.

**Applications vs Calls Distinction**: The guide mentions this but could be clearer: a single function application syntax node (like `f(x)`) can produce multiple *calls* at runtime (if inside a loop/recursion), and each call is a separate sample. The call_stack uses the application's syntax ID, so samples from the same application share that ID in their stack.

**ProbeText Limitations**: The text-only representation (`ProbeText.re`) is simplified compared to the full GUI:
- Shows max 5 samples in Many mode (vs 30 in GUI)
- Max 40 chars per value (vs dynamic length adjustment)
- No cursor alignment checking currently implemented (TODO in code)
- No coloring or navigation

**Closure Cursor**: You mentioned "call cursor" and "closure cursor" - the implementation uses `Sample.Cursor.t` which has the call_stack. The cursor relates to closures in that when you're inside a function body, the call_stack reflects which closure/function execution context you're in. The CSS styling uses this to highlight relationships but doesn't explicitly track "closure ID" separately from the call stack.

### Potential Areas for Clarification

1. **Step-range coloring in GUI**: Is this currently exposed? The implementation supports it but I couldn't confirm it's actively used in the default mode.

2. **"Contained in" vs "Contains" for applications**: The CSS has comments suggesting special styling for samples that "contain" vs "are contained in" the indicated application, but these appear partially commented out.

3. **Sample count display**: The overlay shows sample count (e.g., "1k+") - is this the total count or the filtered count after pin?
