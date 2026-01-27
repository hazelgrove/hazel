# Base Route Storyboard

## Task Overview

**Goal**: Extract the first path segment from a URL path string.

**Error Pattern**: Parameter order ambiguity - `string_split` takes `(sep, str)` but both are strings, so the order isn't obvious from the type signature.

**Probe Benefit**: User sees actual split result immediately, can correct wrong order without consulting documentation.

## Setup

**Tests provided**:
```hazel
test base_route("/api/v1") == "api" end;
test base_route("/api/actions/rm") == "api" end;
test base_route("/") == "" end
```

**Functions provided** (names + signatures):
- `string_split(sep, str) -> [String]`
- `nth(list, index) -> element`
- (plus several others as distractors)

**Initial sketch**:
```hazel
let base_route = fun path ->
  ?
in
```

## Writing Steps (Correct Path)

### Step 1: User realizes they need to split the path

**User writes**:
```hazel
let base_route = fun path ->
  let parts = string_split("/", path) in
  ?
in
```

**Probe output** (auto-probe on `base_route`):
```
let parts = string_split("/", path)     ≡ ["", "api", "v1"] ⫽ ["", "api", "actions", "rm"] ⫽ ["", ""]
```

**Insight**: User sees that splitting "/api/v1" by "/" gives `["", "api", "v1"]`. The empty string at the start makes sense because the path starts with "/". They can see the structure they're working with.

### Step 2: User extracts the base route segment

**User writes**:
```hazel
let base_route = fun path ->
  let parts = string_split("/", path) in
  nth(parts, 1)
in
```

**Probe output**:
```
let parts = string_split("/", path)     ≡ ["", "api", "v1"] ⫽ ["", "api", "actions", "rm"] ⫽ ["", ""]
nth(parts, 1)                           ≡ "api" ⫽ "api" ⫽ ""
```

**Insight**: User sees that `nth(parts, 1)` returns "api" for the first two tests and "" for the third. This matches the expected test values. The probe confirms their understanding is correct before even running the tests.

**Tests**: All pass.

## Common Mistake Paths

### Mistake A: Wrong `string_split` parameter order

User assumes `string_split(str, sep)` (string first, separator second).

**User writes**:
```hazel
let base_route = fun path ->
  let parts = string_split(path, "/") in
  ?
in
```

**Probe output**:
```
let parts = string_split(path, "/")     ≡ ["/"]
```

**How probe helps**: Instead of the expected list of path segments, user sees `["/"]` - the entire path as a single-element list (splitting "/" by "/api/v1" doesn't find the separator). This immediately signals something is wrong. Without probes, they'd only discover this after completing the function and seeing test failures.

**Recovery**: User swaps the arguments:
```hazel
let parts = string_split("/", path)     ≡ ["", "api", "v1"] ⫽ ...
```
Now they see the expected structure.

### Mistake B: Wrong index for `nth`

User tries `nth(parts, 0)` thinking indices are 0-based for "first segment".

**User writes**:
```hazel
let base_route = fun path ->
  let parts = string_split("/", path) in
  nth(parts, 0)
in
```

**Probe output**:
```
let parts = string_split("/", path)     ≡ ["", "api", "v1"] ⫽ ["", "api", "actions", "rm"] ⫽ ["", ""]
nth(parts, 0)                           ≡ "" ⫽ "" ⫽ ""
```

**How probe helps**: User sees that `nth(parts, 0)` returns `""` for all tests - the empty string that appears before the first "/". This reveals that they need index 1, not 0, to skip the leading empty string.

### Mistake C: Both parameter order AND index wrong

User gets both wrong simultaneously.

**User writes**:
```hazel
let base_route = fun path ->
  let parts = string_split(path, "/") in
  nth(parts, 0)
in
```

**Probe output**:
```
let parts = string_split(path, "/")     ≡ ["/"]
nth(parts, 0)                           ≡ "/"
```

**How probe helps**: User sees `"/"` as the result - clearly not "api". They can diagnose step by step: first fix the `string_split` order to see the correct list structure, then adjust the index.

## Key Observations

1. **Progressive feedback**: Each line the user writes gets immediate feedback. They don't have to complete the entire function to learn if their understanding is correct.

2. **Multiple test cases visible simultaneously**: The `⫽` separator shows values for all three test inputs at once. User can verify their logic handles edge cases (like "/") without running tests separately.

3. **Error localization**: When something goes wrong, the probe output shows exactly which expression produced unexpected values, reducing the "where is the bug?" search.

4. **API discovery without docs**: User can experiment with parameter orders and see results immediately, learning the API through direct observation rather than documentation lookup.

## Time Estimate

- Without probes: ~3-5 minutes (including doc lookup for `string_split` order, running tests, debugging)
- With probes: ~1-2 minutes (immediate feedback at each step)

## Potential Downsides

- **None significant for this task**: The task is simple enough that information overload isn't a concern.
- **Possible**: User might rely on trial-and-error with probes rather than thinking about what `string_split` should do conceptually first. However, for an unfamiliar API, this empirical approach may be faster anyway.
