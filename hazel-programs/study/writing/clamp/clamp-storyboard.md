# Clamp Storyboard

## Task Overview

**Goal**: Constrain a number to be within a given range [lo, hi].

**Error Pattern**: Condition boundary errors - `<` vs `<=`, `>` vs `>=`.

**Probe Benefit**: User sees which branch is taken for each test case, including boundary values. This reveals whether their condition logic handles edge cases correctly.

## Setup

**Tests provided**:
```hazel
test clamp(5, 0, 10) == 5 end;     # in range
test clamp(-3, 0, 10) == 0 end;    # below min
test clamp(15, 0, 10) == 10 end;   # above max
test clamp(0, 0, 10) == 0 end;     # at min boundary
test clamp(10, 0, 10) == 10 end    # at max boundary
```

**Initial sketch**:
```hazel
let clamp = fun (x, lo, hi) ->
  ?
in
```

## Writing Steps (Correct Path)

### Step 1: User writes first condition

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else ?
in
```

**Probe output** (auto-probe on conditions):
```
x < lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
```

**Insight**: User sees that `x < lo` is only `true` for the second test case (`clamp(-3, 0, 10)`). Importantly, the fourth test case (`clamp(0, 0, 10)`) shows `false` - the boundary value 0 is NOT less than lo=0, which is correct behavior.

### Step 2: User adds second condition

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else if x > hi then hi
  else ?
in
```

**Probe output**:
```
x < lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
x > hi     ≡ false ⫽ true ⫽ false ⫽ false
```

**Insight**: The second probe only shows 4 values (not 5) because the second test case takes the first branch and doesn't reach this condition. User sees `x > hi` is true for third test (15 > 10). The fifth test (10 at boundary) shows `false` - 10 is NOT greater than hi=10.

### Step 3: User completes with else branch

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else if x > hi then hi
  else x
in
```

**Tests**: All pass.

## Common Mistake Paths

### Mistake A: Using `<=` instead of `<`

User writes overly inclusive conditions.

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x <= lo then lo
  else if x >= hi then hi
  else x
in
```

**Probe output**:
```
x <= lo    ≡ false ⫽ true ⫽ false ⫽ true ⫽ false
x >= hi    ≡ false ⫽ true ⫽ true
```

**Observation**: With `<=`, the fourth test case (`clamp(0, 0, 10)`) now shows `true` for `x <= lo`. The boundary value takes the first branch instead of falling through to `else x`.

**How probe helps**: User can see which branch handles boundary cases. Even though tests pass (result is still 0 either way), the probe reveals the control flow differs from the spec.

**Note**: For integers, this "bug" produces correct output - the distinction only matters for understanding which case handles boundaries. This could be a discussion point about whether probes help with understanding vs just correctness.

### Mistake B: Conditions in wrong order

User checks `x > hi` first.

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x > hi then hi
  else if x < lo then lo
  else x
in
```

**Probe output**:
```
x > hi     ≡ false ⫽ false ⫽ true ⫽ false ⫽ false
x < lo     ≡ false ⫽ true ⫽ false ⫽ false
```

**How probe helps**: Still works correctly! Probes show the logic is fine - just in a different order. User might notice and stick with their version, or reorder to match the spec.

### Mistake C: Forgetting the else branch

User leaves a hole in the else case.

**User writes**:
```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else if x > hi then hi
  else ?
in
```

**Probe output**:
```
x < lo     ≡ false ⫽ true ⫽ false ⫽ false ⫽ false
x > hi     ≡ false ⫽ true ⫽ false ⫽ false
?          ≡ ? ⫽ ? ⫽ ?
```

**How probe helps**: The hole `?` shows that 3 test cases reach this branch and need a value. The user knows they need to fill in what happens for values in range.

## Key Observations

1. **Branch visibility**: Probes on condition expressions show exactly which tests take which branches, making control flow explicit.

2. **Boundary case clarity**: The boundary tests (0 and 10) show whether inclusive/exclusive comparisons behave as expected.

3. **Semantic vs syntactic correctness**: Even when tests pass, probes can reveal whether the *reason* for correctness matches the intended logic.

4. **Partial evaluation**: When one branch short-circuits, subsequent probes show fewer values - this reveals the control flow structure.

## Potential Downsides

- **Information density**: With 5 test cases and multiple conditions, there are many boolean values to interpret. For more complex functions, this could become overwhelming.

- **Correct output hides subtle issues**: Both `<` and `<=` versions pass all tests for this input set. Without probes showing branch taken, user might not notice the conceptual difference.
