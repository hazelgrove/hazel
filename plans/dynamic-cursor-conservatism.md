# Dynamic Cursor Conservatism

How the sample cursor preserves the user's probe selection across
navigation between probes at different call depths.

## The problem

When probes appear at multiple call depths, each probe has multiple
samples — one per dynamic invocation. The sample cursor determines
which sample each probe displays. As the user navigates between probes,
the cursor must decide: which sample should the target probe show?

This has two aspects: **consistency** (samples at different depths must
be in the same execution context) and **intent preservation** (when
multiple samples are consistent, prefer the user's prior selection over
resetting to a default).

## Running example: doubly nested map

```
let f : [Int] -> [Int] = fun xs ->
  map(fun x ->
    map(fun y -> ^^probe(y) + 1, [10, 20])
  , ^^probe(xs))
in f([1, 2, 3])
```

Three levels of probes:

- **Top** (`xs`): 1 sample, call stack `[F]`
- **Mid** (`x`): 3 samples, call stacks `[M0,F]`, `[M1,F]`, `[M2,F]`
  (one per outer map iteration)
- **Inner** (`y`): 6 samples, call stacks `[N0,M0,F]`, `[N1,M0,F]`,
  `[N0,M1,F]`, `[N1,M1,F]`, `[N0,M2,F]`, `[N1,M2,F]`
  (two per inner map × three outer iterations)

The call stack is a list with the **innermost frame first**: `[N0,M1,F]`
means "inside inner-map iteration 0, inside outer-map iteration 1,
inside the call to f."

### Consistency

If the user is viewing mid sample `[M1,F]` (x during the second outer
iteration), the inner probe must show a sample under M1 — either
`[N0,M1,F]` or `[N1,M1,F]`. Showing `[N0,M0,F]` would be inconsistent:
it's from a different outer iteration.

Consistency is enforced by the call stack's suffix structure. A sample
is consistent with the cursor if its call stack shares the same suffix
(i.e., is on the same path through the call tree).

### Intent preservation (two levels)

1. User arrows the inner probe from `y` sample `[N0,M1,F]` to
   `[N1,M1,F]` (choosing the second inner iteration)
2. User clicks the mid probe on `[M1,F]` (which is already consistent)
3. User clicks back on the inner probe

At step 3, both `[N0,M1,F]` and `[N1,M1,F]` are consistent with
`[M1,F]`. Without intent preservation, the inner probe resets to N0
(the first). With intent preservation, it stays at N1 — the user's
prior selection.

### Intent preservation (three levels)

The deeper challenge:

1. User is at the mid level, viewing `[M1,F]`
2. User arrows down to the inner probe (cursor picks `[N0,M1,F]`)
3. User arrows up back to mid — still shows `[M1,F]` ✓
4. User arrows up again to the top probe (`[F]`)

At step 4, the mid probe re-renders. Without multi-level intent
preservation, the cursor at the top level only knows about `[F]`, so
all mid samples (`[M0,F]`, `[M1,F]`, `[M2,F]`) are equally "below F"
and the first one wins — resetting to M0.

With multi-level preservation, the cursor retains the full path
`[N0,M1,F]` even at index=0 (top level). The mid probe recovers M1
by checking whether its samples are suffixes of this stored path.

## Mechanism: cursor as call path

The cursor stores a `(call_stack, index)` pair:

- **`call_stack`**: the full path through the call tree, representing
  the user's chosen branch at every depth. E.g., `[N0, M1, F]` means
  "I chose N0 at the inner level, M1 at the mid level, inside call F."

- **`index`**: which depth is currently "active" (being viewed).
  Index 0 = outermost call frame, index 2 = innermost in a 3-deep stack.

- **`effective_stack`** (derived): `call_stack` sliced to `index + 1`
  elements from the outer end. Represents the cursor's effective
  position. E.g., `[N0,M1,F]` at index=0 → effective stack `[F]`.

### Write side: `SampleCursorPerform.capture`

When the user clicks a sample or navigates to a probe (via ArrowUp/Down),
`capture` updates the cursor. The key logic: if the new sample's call
stack is a **suffix** of the current stack, we **keep the deeper stack**
but lower the index.

```
Before: call_stack = [N0, M1, F], index = 2   (viewing inner level)
Navigate to mid probe, sample [M1, F]:
  [M1, F] is suffix of [N0, M1, F]  →  keep stack
After:  call_stack = [N0, M1, F], index = 1   (viewing mid level)

Navigate to top probe, sample [F]:
  [F] is suffix of [N0, M1, F]  →  keep stack
After:  call_stack = [N0, M1, F], index = 0   (viewing top level)
```

Both N0 and M1 are preserved in the stack even though the cursor's
effective depth has moved to the top level. The write side has been
stable throughout the history of this mechanism — the bugs have always
been on the read side.

### Read side: current intent vs. historical intent

The cursor's two fields answer two different questions:

- **`effective_stack`** answers: "what is the user currently looking at?"
  This is the active depth, the thing they just clicked on or arrowed to.

- **`call_stack`** (full) answers: "what was the user looking at here
  before?" This is the historical path — deeper selections preserved
  by the write side for potential recovery later.

When a probe decides which sample to display, it should prefer the
answer to the first question. Only when the first question has no
answer (because the probe lives at a different nesting level than the
cursor's effective depth) should it fall back to the second.

### Why both are needed: distinct call sites vs. recursive calls

The distinction between these two questions is invisible when every
call site has a **distinct ID** — which is true for the nested-map
example above. There, each frame (N0, N1, M0, M1, F) is unique, so
suffix matching against the full stack produces the same result as
matching against the effective stack: only one sample per probe can
possibly match either way.

But in **recursive functions**, all calls go through the same syntactic
call site, so every frame shares a single ID. Consider factorial:

```
let fact = fun ^^probe(x) ->
  case x | 1 => 1 | _ => fact(x-1) * x end
in fact(5)
```

The probe on `x` has 5 samples with stacks of repeated `F` frames:

```
x=5: [F]            (depth 1)
x=4: [F, F]         (depth 2)
x=3: [F, F, F]      (depth 3)
x=2: [F, F, F, F]   (depth 4)
x=1: [F, F, F, F, F] (depth 5)
```

Because all frames are identical, **every shallower stack is a suffix
of every deeper stack**: `[F]` is a suffix of `[F, F, F]`, `[F, F]`
is a suffix of `[F, F, F]`, etc. The suffix structure that perfectly
discriminates distinct call sites degenerates into "all depths match."

If alignment only looks at the full `call_stack`, the longest suffix
always wins — the system snaps to the deepest preserved sample,
overriding the user's active selection. This means:

- **Clicking** on a shallower sample doesn't stick (re-render snaps
  back to the deepest match).
- **Arrow keys** get stuck (each step shallower is immediately undone
  by the read side recovering the deeper historical selection).

The `index` field is the disambiguator. It encodes which depth the
user is currently looking at. The effective stack (trimmed to `index+1`)
limits the suffix scan so that samples deeper than the active depth
can't match, breaking the degeneracy.

### Alignment in the recursive case

For cursor `(call_stack=[F,F,F], index=1)` — the user navigated from
x=3 to x=4:

**Effective stack** = `[F, F]` (2 elements, the outer portion):

| Sample | Suffix of `[F,F]`? | Length |
|---|---|---|
| x=5: `[F]` | yes | 1 |
| x=4: `[F, F]` | yes | 2 ← best |
| x=3: `[F, F, F]` | **no** (too long) | — |

Result: x=4. Correct — the user's active selection.

### Alignment in the non-recursive case (intent preservation)

For cursor `(call_stack=[N0, M1, F], index=0)` — the user navigated
from inner to top, and we're now reading the mid probe:

**Effective stack** = `[F]` (1 element):

| Sample | Suffix of `[F]`? |
|---|---|
| `[M0, F]` | no (too long) |
| `[M1, F]` | no (too long) |
| `[M2, F]` | no (too long) |

No match. **Fall through to full stack** `[N0, M1, F]`:

| Sample | Suffix of `[N0,M1,F]`? | Length |
|---|---|---|
| `[M0, F]` | no (M0 ≠ M1) | — |
| `[M1, F]` | yes | 2 ← best |
| `[M2, F]` | no (M2 ≠ M1) | — |

Result: M1. Intent preserved — the user's prior selection recovered.

### Implementation: `most_aligned_index`

The function `Selection.most_aligned_index(~ap_id, cursor, samples)`
uses a two-tier suffix scan:

1. **Effective match** (current intent): suffix scan against
   `effective_stack(cursor)`. This answers "what sample is at the
   depth the user is currently looking at?" Handles same-probe
   navigation (arrow between recursive depths) and cross-probe
   depth consistency.

2. **Full match** (historical intent): if tier 1 finds nothing,
   suffix scan against the full `cursor.call_stack`. This answers
   "what sample was the user looking at here before?" Recovers
   preserved selections when returning to a probe at a different
   nesting level.

3. **Indicated call match** (step-into): find a sample that is a
   direct callee of the cursor's `indicated_call`.

4. **Any related sample** (fallback): find any sample that shares
   call context with the cursor's effective stack.

Tier 1 takes priority over tier 2 because the user's current intent
takes priority over their historical intent. This is the key
principle: **where you are beats where you've been**.

### Navigation: `most_aligned_sample`

When the user presses ArrowUp/Down to navigate between probes,
`resolve_pending_probe_cursor` calls `most_aligned_sample` to pick
which sample to capture at the target probe. This delegates to
`most_aligned_index`, using the same two-tier scan to find the sample
in the same call branch, ensuring that `capture` preserves the
deeper stack via its `is_suffix_of` check.

## The suffix principle and its limits

The cursor's `call_stack` is a path through the call tree. Each entry
identifies a specific function invocation. For a sample at depth K to
be "on the same path," its K-element call stack must equal the last K
elements of the cursor's stack — i.e., it must be a suffix.

This is true regardless of how many nesting levels exist. The suffix
check is the general principle; the earlier "full-then-trimmed" pattern
was a special case that handled only the two endpoints (full length and
effective length) but missed intermediate depths.

However, suffix matching against the full stack alone is not sufficient.
The `call_stack` records syntactic call site IDs, not dynamic invocation
identifiers. In recursive functions, all invocations share one ID, so
the suffix structure degenerates — all depths are mutual suffixes. The
`index` field (via `effective_stack`) provides the necessary
disambiguation by limiting the suffix scan to the user's active depth.

## History

This mechanism was discovered incrementally:

1. **Original**: compared samples against the full cursor stack only.
   Broke shallower probes: an outer probe can't match a stack deeper
   than its own samples.

2. **Trimmed-only fix** (babf1c6e25): compared against the effective
   stack (trimmed to `index + 1`). Fixed shallower probes but broke
   intent preservation: trimming discards the preserved deeper info,
   so all samples under the same outer call become indistinguishable.

3. **Full-then-trimmed** (fb475e9803): try full stack first (recovers
   preserved intent), fall back to trimmed (matches shallower probes).
   Fixed the two-level case but still failed at three levels: the mid
   probe's samples matched neither the full stack (wrong length) nor
   the trimmed stack (too short).

4. **Intermediate depth match** (7987a166f2): added a tier checking
   whether a sample's stack is a proper suffix of the full stack.
   Fixed the three-level case.

5. **Suffix unification** (5dee4854c1): recognized that tiers 1-4 are
   all instances of the same operation — suffix matching at different
   lengths. Unified into a single scan that finds the longest suffix
   match against the full `call_stack`, eliminating the `~trimmed`
   parameter and the multi-tier structure. This broke recursive
   functions (see step 6).

6. **Effective-first, full-fallback** (current): the full-stack-only
   scan from step 5 always picks the deepest suffix match, which in
   recursive functions (where all depths share the same frame ID)
   means the cursor snaps to the deepest preserved sample, overriding
   the user's active selection. Fixed by scanning against the effective
   stack first (respects `index`, the user's current depth) and only
   falling back to the full stack when the effective scan finds nothing
   (recovers intent for probes at different nesting levels).

The write side (`capture`'s `is_suffix_of` check) has been correct
throughout — it preserves deeper stacks for potential recovery. The
read side needed two adjustments: first recognizing suffix matching
as the general principle (step 5), then recognizing that the suffix
scan must be scoped by `index` to handle same-ID frames in recursive
functions (step 6).

## Testing

Unit tests in `Test_SampleSelection.re`:

**`intent_preservation_tests`** (two-level, distinct IDs):
- `most_aligned_index` preserves inner selection from deeper cursor
- No preserved info falls back correctly to first related sample
- Outer probe unaffected by preserved inner info
- `Selection.select` pipeline with preserved cursor
- `move_cursor` simulation: arrow navigation from preserved position

**`three_level_tests`** (three-level, distinct IDs):
- Mid probe finds correct sample from 3-level cursor via suffix match
- Inner probe still found via full-stack match (longest suffix)
- Top probe unaffected by deeper info in cursor
- `most_aligned_sample` picks correct mid sample for navigation
- Full `capture` chain through three levels preserves all info

**`recursive_tests`** (same-ID frames, like factorial):
- Click on shallower sample: effective stack limits scan, respects index
- Click on shallowest sample: same, from deeply preserved stack
- Arrow left twice (x=3→x=4→x=5): each step advances correctly
- Arrow right then left (x=3→x=2→x=3): return to original depth
- `Selection.select` Single mode returns active-depth sample
- Cross-probe consistency: second probe follows depth change
- Intent recovery: returning from non-recursive probe recovers depth

Integration tests in `Test_Evaluator_ProbeSelection.re`:
- Real evaluation with `f` called 3 times, verifying preserved index
- Arrow navigation starting from preserved position

## Key invariant

**The cursor's `call_stack` is always at least as deep as any sample
it was captured from.** The `index` field selects the effective depth.

The read side answers two questions in priority order:

1. **"Where am I?"** — suffix scan against `effective_stack` (the
   active depth). This governs direct interaction: clicks, arrow keys,
   cross-probe consistency.

2. **"Where was I?"** — suffix scan against full `call_stack` (the
   historical path). This governs recovery: returning to a probe at
   a different nesting level after navigating away.

Current intent beats historical intent. The full stack is a fallback,
not the primary signal.
