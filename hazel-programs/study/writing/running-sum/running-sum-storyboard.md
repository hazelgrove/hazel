# Running Sum Storyboard

## Task Overview

**Goal**: Compute a list where each element is the cumulative sum up to that position.
`[1, 2, 3] -> [1, 3, 6]`

**Error Patterns**:
- Fold accumulator mistakes (wrong initial value, wrong update logic)
- List construction errors (forgetting to append, wrong order)
- Tuple handling (tracking two values through the fold)

**Probe Benefit**: User sees the accumulator evolve step-by-step at each iteration, making it clear whether the running total and result list are being built correctly.

## Setup

**Tests provided**:
```hazel
test running_sum([1, 2, 3]) == [1, 3, 6] end;
test running_sum([5]) == [5] end;
test running_sum([]) == [] end;
test running_sum([1, 1, 1, 1]) == [1, 2, 3, 4] end
```

**Functions provided**:
- `fold_left(list, fn, init) -> result` — fn takes `(accumulator, element)`
- `append(list1, list2) -> list`
- `rev(list) -> list`
- `map(list, fn) -> list`

**Initial sketch**:
```hazel
let running_sum = fun nums ->
  ?
in
```

## Writing Steps (Correct Path)

### Step 1: User realizes they need to track two things

The user needs both the running total AND the result list. They decide to use a tuple accumulator.

**User writes**:
```hazel
let running_sum = fun nums ->
  fold_left(nums,
    fun ((total, acc), x) -> ?,
    (0, [])
  )
in
```

**Probe output** (auto-probe on fold):
```
fun ((total, acc), x) -> ?     ≡ (0, ?) ⫽ (0, ?) ⫽ ...
```

**Insight**: The initial accumulator `(0, [])` is visible. The `?` in the function body shows evaluation hasn't progressed past the hole.

### Step 2: User computes the new running total

**User writes**:
```hazel
let running_sum = fun nums ->
  fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      ?,
    (0, [])
  )
in
```

**Probe output**:
```
let new_total = total + x     ≡ 1 ⫽ 3 ⫽ 6   (for [1,2,3])
```

**Insight**: User sees the running total evolving: 0+1=1, 1+2=3, 3+3=6. This confirms their addition logic is correct.

### Step 3: User builds the result tuple

**User writes**:
```hazel
let running_sum = fun nums ->
  fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      (new_total, append(acc, [new_total])),
    (0, [])
  )
in
```

**Probe output**:
```
(new_total, append(acc, [new_total]))     ≡ (1, [1]) ⫽ (3, [1, 3]) ⫽ (6, [1, 3, 6])
```

**Insight**: User sees the accumulator evolve through all iterations. The tuple shows both the running total and the growing result list. This matches expectations perfectly.

### Step 4: User extracts just the result list

**User writes**:
```hazel
let running_sum = fun nums ->
  let (_, result) = fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      (new_total, append(acc, [new_total])),
    (0, [])
  ) in
  result
in
```

**Probe output**:
```
result     ≡ [1, 3, 6] ⫽ [5] ⫽ [] ⫽ [1, 2, 3, 4]
```

**Tests**: All pass.

## Common Mistake Paths

### Mistake A: Wrong initial accumulator

User starts with `(0, [0])` instead of `(0, [])`, thinking the first element should be 0.

**User writes**:
```hazel
let running_sum = fun nums ->
  let (_, result) = fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      (new_total, append(acc, [new_total])),
    (0, [0])  # Wrong!
  ) in
  result
in
```

**Probe output**:
```
result     ≡ [0, 1, 3, 6] ⫽ [0, 5] ⫽ [0] ⫽ [0, 1, 2, 3, 4]
```

**How probe helps**: Every result starts with an extra `0`. The probe immediately shows the result structure is wrong before running tests. User can trace back and see the initial accumulator included `[0]`.

### Mistake B: Using cons instead of append (reversed result)

User builds the list with `::` which prepends, giving reversed order.

**User writes**:
```hazel
let running_sum = fun nums ->
  let (_, result) = fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      (new_total, new_total::acc),
    (0, [])
  ) in
  result
in
```

**Probe output**:
```
(new_total, new_total::acc)     ≡ (1, [1]) ⫽ (3, [3, 1]) ⫽ (6, [6, 3, 1])
result     ≡ [6, 3, 1] ⫽ ...
```

**How probe helps**: User sees `[6, 3, 1]` instead of `[1, 3, 6]`. The step-by-step probe shows how cons is prepending each new value, building the list in reverse. They can fix by either adding `rev(result)` at the end or switching to `append`.

### Mistake C: Forgetting to update the total

User returns the old total instead of the new one.

**User writes**:
```hazel
let running_sum = fun nums ->
  let (_, result) = fold_left(nums,
    fun ((total, acc), x) ->
      let new_total = total + x in
      (total, append(acc, [new_total])),  # Returns old total!
    (0, [])
  ) in
  result
in
```

**Probe output**:
```
(total, append(acc, [new_total]))     ≡ (0, [1]) ⫽ (0, [1, 2]) ⫽ (0, [1, 3])
```

**How probe helps**: The first element of the tuple stays `0` throughout! The user can see that `total` never updates between iterations because they're returning the old value. The result list also shows the problem: `[1, 2, 3]` instead of `[1, 3, 6]` because each iteration starts fresh from 0.

### Mistake D: Computing the sum, not the running sum

User misunderstands and computes a single sum instead of a list.

**User writes**:
```hazel
let running_sum = fun nums ->
  fold_left(nums, fun (acc, x) -> acc + x, 0)
in
```

**Probe output**:
```
acc + x     ≡ 1 ⫽ 3 ⫽ 6
running_sum([1, 2, 3])     ≡ 6
```

**How probe helps**: The final result is `6` (a number), not `[1, 3, 6]` (a list). The probe clearly shows they're computing a single value, not building a list. Tests will also fail with a type mismatch.

## Key Observations

1. **Accumulator evolution visible**: The tuple accumulator `(total, acc)` is shown at each fold iteration, making it clear how state evolves.

2. **Complex state tracking**: This task requires tracking TWO values through the fold. Probes show both values at each step, helping debug which one is wrong.

3. **Order matters**: Building a list with cons vs append produces reversed results. Probes show this happening step by step.

4. **Progressive debugging**: When something is wrong, probes help isolate WHICH part of the accumulator update is incorrect.

## Complexity Notes

This is a "small" task (5-10 lines) that exercises several concepts:
- Higher-order functions (fold_left)
- Tuple destructuring
- List construction
- Compound accumulator state

It may be challenging for participants unfamiliar with fold. Consider providing a hint about needing to track "two pieces of information" through the fold.

## Potential Downsides

- **Information density**: With tuple accumulators, each probe shows multiple values. For complex accumulator structures, this could become hard to parse.

- **Fold unfamiliarity**: If participants aren't comfortable with fold_left, even seeing the probe output might not help them understand how to fix their code.
