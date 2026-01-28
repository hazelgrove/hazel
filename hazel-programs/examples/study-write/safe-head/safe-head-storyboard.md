# safe_head Storyboard

## Task Overview
- **Error pattern**: Pattern match coverage
- **Probe insight**: See which branch is taken for each test case
- **Difficulty**: Tiny (2-3 lines)

## Setup
- Tests provided: 4 (non-empty list, singleton, empty with non-zero default, empty with zero default)
- Syntax hint: case/pattern syntax provided in sketch

## CLI Development Session

### Step 1: Function skeleton (no body)

**User writes:**
```hazel
let safe_head = fun (xs, default) ->
```

**Probe output:**
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ∅
⟦ ⟧     ≡ ∅
```

**Insight**: Empty hole shown as `⟦ ⟧`, `∅` because no calls yet.

### Step 2: Add case structure

**User writes:**
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  | x::rest => x
  end
in
safe_head([1, 2, 3], 0)
```

**Probe output (--many):**
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0)
  ⟦case ⟦xs⟧     ≡ [1, 2, 3]
  | [] => ⟦default⟧     ≡ ∅
  | x::rest => ⟦x⟧     ≡ 1
  end⟧     ≡ 1
```

**Insight**:
- `xs ≡ [1, 2, 3]` shows input
- `default ≡ ∅` because empty branch not taken
- `x ≡ 1` shows first element extracted
- Case result `≡ 1` confirms correct branch

### Step 3: Add empty list test

**With both tests and --many:**
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0) ⫽ ([], 99)
  ⟦case ⟦xs⟧     ≡ [1, 2, 3] ⫽ []
  | [] => ⟦default⟧     ≡ 99
  | x::rest => ⟦x⟧     ≡ 1
  end⟧     ≡ 1 ⫽ 99
```

**Insight**: Now we see both branches taken across different test cases. The `⫽` separator shows multiple samples.

## Incomplete Syntax Experiments

### Missing `end` keyword

**User writes:**
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  | x::rest => x

in
safe_head([1, 2], 0)
```

**Result:**
```
⟦safe_head([1, 2], 0)⟧     ≡ xs [] default x::rest x
```

**Issue**: Without `end`, patterns become the "result". Analyze shows `Variable x is not bound`.

**Lesson**: The missing `end` causes confusing output. Probes won't help debug this - need to complete the case syntax.

### Missing `in` keyword

**User writes:**
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  | x::rest => x
  end

safe_head([1, 2], 0)
```

**Result:** All probes show `∅`

**Format shows:**
```
let safe_head = fun (xs, default) -> (case xs | ... end safe_head([1, 2], 0)) in ?
```

**Issue**: The call gets absorbed INTO the function body! Very unexpected parsing.

**Lesson**: Missing `in` leads to bizarre behavior where subsequent code becomes part of the binding.

## Common Mistake Paths

### Mistake A: Forgetting empty list case

**User writes:**
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | x::rest => x
  end
```

**With test `safe_head([], 99)`:**
The case has an inexhaustive match error for `[]`.

**How probe helps**: Static analysis catches this immediately.

### Mistake B: Wrong pattern (missing rest)

**User writes:**
```hazel
| x => x   # Instead of x::rest => x
```

**How probe helps**: `x` matches the WHOLE list `[1, 2, 3]`, not the first element. Probe would show `x ≡ [1, 2, 3]` instead of `x ≡ 1`.

## Key Probe Benefits

1. **Branch visibility**: See which case branch was taken for each test
2. **Variable binding**: See exactly what `x` and `default` are bound to
3. **Multiple samples**: `--many` shows behavior across all test inputs
4. **Empty branch detection**: `∅` indicates untaken paths
