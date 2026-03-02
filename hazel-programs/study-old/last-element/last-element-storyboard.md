# last_element Storyboard

## Task Overview
- **Error pattern**: Fold accumulator/initial value errors
- **Probe insight**: See fold result immediately; catch wrong accumulator logic
- **Difficulty**: Tiny (1 line of logic)

## Setup
- Tests provided: 5 (multi-element, singleton, two singletons, empty with different defaults)
- Functions provided: fold_left, fold_right, rev, length (more than needed)
- Key insight needed: Each step should REPLACE acc with current element x

## CLI Development Session

### Step 1: Incomplete fold (mid-typing)

**User writes:**
```hazel
let last = fun (xs, default) ->
  fold_left(xs, fun (acc, x) ->
in
last([1, 2, 3], 0)
```

**Probe output:**
```
let last = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0)
  ⟦fold_left(xs, fun (acc, x) -> ⟧     ≡ ([1, 2, 3], ^^fold(fun (acc, x):((?, ...
```

**Issue**: Without closing the fold_left call, it gets parsed incorrectly. The result is a tuple containing the list and an incomplete function.

**Lesson**: Incomplete delimiters in fold_left lead to confusing output. The probe shows strange values, signaling something is wrong with the syntax.

### Step 2: Complete but wrong - returning acc

**User writes:**
```hazel
let last = fun (xs, default) ->
  fold_left(xs, fun (acc, x) -> acc, default)
```

**Probe output:**
```
  ⟦fold_left(xs, fun (acc, x) -> acc, default)⟧     ≡ 0
...
  ⟦⟦last([1, 2, 3], 0)⟧     ≡ 0
  == 3⟧     ≡ false
```

**Insight**: The fold returns `0` (the initial value) because `acc` never changes. Probe immediately shows the bug - the fold isn't accumulating anything meaningful.

### Step 3: Wrong initial value

**User writes:**
```hazel
fold_left(xs, fun (acc, x) -> x, 0)  # Uses 0 instead of default
```

**Probe output with empty list test:**
```
  ⟦fold_left(xs, fun (acc, x) -> x, 0)⟧     ≡ 3
...
  ⟦⟦last([], 99)⟧     ≡ 0
  == 99⟧     ≡ false
```

**Insight**: For `last([], 99)`, probe shows result is `0`, not `99`. The `default` parameter is being ignored!

### Step 4: Correct solution

**User writes:**
```hazel
fold_left(xs, fun (acc, x) -> x, default)
```

**Probe output (--many):**
```
let last = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0) ⫽ ([42], 0) ⫽ ([], 99) ⫽ ([], 0)
  ⟦fold_left(xs, fun (acc, x) -> x, default)⟧     ≡ 3 ⫽ 42 ⫽ 99 ⫽ 0
```

**Insight**: Results match expectations across all test cases. For empty lists, returns the default; for non-empty, returns last element.

## Common Mistake Paths

### Mistake A: `fun (acc, x) -> acc` (accumulator unchanged)

**What happens**: Fold always returns initial value.

**Probe shows**: `fold_left(...) ≡ 0` for input `[1, 2, 3]` with default `0`.

**How probe helps**: Immediately see the fold isn't doing anything useful.

### Mistake B: `fun (acc, x) -> acc + x` (sum instead of last)

**What happens**: Computes sum, not last element.

**Probe shows**: `fold_left([1,2,3], ..., 0) ≡ 6` instead of `3`.

**How probe helps**: Value `6` is clearly not the last element of `[1,2,3]`.

### Mistake C: Hardcoded initial value `0`

**What happens**: Empty list returns `0` regardless of `default` parameter.

**Probe shows**: `last([], 99) ≡ 0` when expecting `99`.

**How probe helps**: See that `default` is being ignored.

### Mistake D: Wrong parameter order in fold

**User might try**:
```hazel
fold_left(fun (acc, x) -> x, xs, default)  # Wrong order
```

**What happens**: Type error or unexpected behavior.

**How probe helps**: Would show function being used as list argument.

## Incomplete Syntax Findings

| Syntax Issue | Probe Behavior | Analysis |
|--------------|----------------|----------|
| Missing `)` on fold_left | Shows tuple `(list, function)` | Parsed as curried application |
| Missing `in` after let | All probes show `∅` | Call absorbed into function body |
| Mid-typing fold body | Confusing partial results | Parser makes best guess |

## Key Probe Benefits for Folds

1. **Immediate result visibility**: See what fold computes without print statements
2. **Accumulator debugging**: Would benefit from step-by-step view (not yet available in CLI)
3. **Multiple test comparison**: `--many` shows behavior across all inputs at once
4. **Initial value verification**: Catch hardcoded vs parameterized initial values
