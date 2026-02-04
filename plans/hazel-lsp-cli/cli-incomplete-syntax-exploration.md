# CLI Incomplete Syntax Exploration

This document records experiments with the Hazel CLI (`./hazel probe --auto`) using incomplete syntax, to understand how the parser and evaluator handle partially-written code.

## Context

When users write code incrementally, they often have incomplete syntax:
- Missing delimiters (`end`, `in`, `)`)
- Missing expressions (holes)
- Partial function bodies

Understanding how probes behave in these cases helps design study tasks and set expectations.

---

## Experiment 1: `case` without `end`

### Input
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  | x::rest => x

in
safe_head([1, 2], 0)
```

### Probe Output
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ([1, 2], 0)
  case ⟦xs⟧     ≡ ∅
  | [] => ⟦default⟧     ≡ ∅
  | x::rest => ⟦x⟧     ≡ ∅

in
⟦safe_head([1, 2], 0)⟧     ≡ xs [] default x::rest x
```

### Analyze Output
```
Found 1 static error:

error: Variable x is not bound
  --> /tmp/case-no-end.hz:4:18
  |
4 |   | x::rest => x
  |                  ^
```

### Format Output (shows parser interpretation)
```
let safe_head = fun (xs, default) -> (xs [] default x::rest x) in
safe_head([1, 2], 0)
```

### Analysis
Without `end`, the parser doesn't recognize the case expression. The patterns and results become a bizarre expression `(xs [] default x::rest x)`. The variable `x` from the pattern is now unbound because it's not inside a proper pattern match.

**Verdict**: Very confusing. Missing `end` leads to nonsensical output.

---

## Experiment 2: `let` without `in`

### Input
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  | x::rest => x
  end

safe_head([1, 2], 0)
```

### Probe Output
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ∅
  ⟦case ⟦xs⟧     ≡ ∅
  | [] => ⟦default⟧     ≡ ∅
  | x::rest => ⟦x⟧     ≡ ∅
  end⟧     ≡ ∅

 ⟦safe_head([1, 2], 0)⟧     ≡ ∅
```

### Analyze Output
```
No static errors found.
```

### Format Output
```
let safe_head = fun (xs, default) -> (case xs
| [] => default
| x::rest => x
end safe_head([1, 2], 0)) in
?
```

### Analysis
Without `in`, the parser absorbs the following code (`safe_head([1, 2], 0)`) into the function body! The format shows the call became part of the case expression. The outer let then has no body, becoming `?`.

All probes show `∅` because nothing actually evaluates - the program is just a let with a hole body.

**Verdict**: Very surprising. Missing `in` causes code to be "eaten" by the binding.

---

## Experiment 3: Incomplete `fold_left` (mid-typing)

### Input
```hazel
let last = fun (xs, default) ->
  fold_left(xs, fun (acc, x) ->
in
last([1, 2, 3], 0)
```

### Probe Output
```
let last = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0)
  ⟦fold_left(xs, fun (acc, x) -> ⟧     ≡ ([1, 2, 3], ^^fold(fun (acc, x):((?, ...
in
⟦last([1, 2, 3], 0)⟧     ≡ ([1, 2, 3], ^^fold(fun (acc, x):((?, ...
```

### Format Output
```
let last = fun (xs, default) -> (fold_left (xs, fun (acc, x) -> ?)) in
last([1, 2, 3], 0)
```

### Analysis
The parser fills in `?` for the missing function body. But note: `fold_left (xs, fun ...)` - it's a curried call with ONE tuple argument, not `fold_left(list, fn, init)`. The missing initial value and closing paren mean the structure is wrong.

The result is a tuple `([1, 2, 3], function)` because `fold_left` partially applied returns... something weird.

**Verdict**: Confusing output signals structural problem.

---

## Experiment 4: `fold_left` with all args but missing `)`

### Input
```hazel
let last = fun (xs, default) ->
  fold_left(xs, fun (acc, x) -> x, default
in
last([1, 2, 3], 0)
```

### Probe Output
```
let last = fun (⟦xs, default⟧) ->     ≡ ([1, 2, 3], 0)
  ⟦fold_left(xs, fun (acc, x) -> x, default⟧     ≡ ([1, 2, 3], ^^fold(fun (acc, x):((?, ...
in
⟦last([1, 2, 3], 0)⟧     ≡ ([1, 2, 3], ^^fold(fun (acc, x):((?, ...
```

### Format Output
```
let last = fun (xs, default) -> (fold_left (xs, fun (acc, x) -> x, default)) in
last([1, 2, 3], 0)
```

### Analysis
Same issue - `fold_left (tuple)` instead of `fold_left(a, b, c)`. The missing `)` makes the parser think the whole `(xs, fun ..., default)` is a single tuple argument.

**Verdict**: Missing closing delimiter causes structural misparse.

---

## Experiment 5: `if-then-else` with incomplete `else` (USEFUL)

### Input
```hazel
let clamp = fun (x, lo, hi) ->
  if x < lo then lo
  else if x > hi then hi
  else
in
clamp(5, 0, 10);
clamp(-5, 0, 10);
clamp(15, 0, 10)
```

### Probe Output (--many)
```
let clamp = fun (⟦x, lo, hi⟧) ->     ≡ (5, 0, 10) ⫽ (-5, 0, 10) ⫽ (15, 0, 10)
  if x < lo then ⟦lo⟧     ≡ 0
  else ⟦if x > hi then ⟦hi⟧     ≡ 10
  else ⟧     ≡ ? ⫽ 10
in
⟦clamp(5, 0, 10)⟧;     ≡ ?
⟦clamp(-5, 0, 10)⟧;     ≡ 0
⟦clamp(15, 0, 10)⟧     ≡ 10
```

### Analysis
Even with incomplete `else`, we get useful information:
- `clamp(-5, 0, 10) ≡ 0` - works (hits `x < lo` branch)
- `clamp(15, 0, 10) ≡ 10` - works (hits `x > hi` branch)
- `clamp(5, 0, 10) ≡ ?` - shows THIS input needs the missing else branch

The branch probes show:
- `lo ≡ 0` (only for -5 case)
- `hi ≡ 10` (only for 15 case)

**Verdict**: USEFUL! Shows which test cases need the incomplete branch.

---

## Experiment 6: `case` with only one branch (USEFUL)

### Input
```hazel
let safe_head = fun (xs, default) ->
  case xs
  | [] => default
  end
in
safe_head([1, 2], 0);
safe_head([], 99)
```

### Probe Output (--many)
```
let safe_head = fun (⟦xs, default⟧) ->     ≡ ([1, 2], 0) ⫽ ([], 99)
  ⟦case ⟦xs⟧     ≡ [1, 2] ⫽ []
  | [] => ⟦default⟧     ≡ 99
  end⟧     ≡ case [1, 2] | [] => default end ⫽ 99
in
⟦safe_head([1, 2], 0)⟧;     ≡ case [1, 2] | [] => default end
⟦safe_head([], 99)⟧     ≡ 99
```

### Analysis
With only the `[]` branch:
- `safe_head([], 99) ≡ 99` - works correctly
- `safe_head([1, 2], 0) ≡ case [1, 2] | [] => default end` - shows STUCK expression

The stuck case expression in the output indicates which input pattern isn't handled.

**Verdict**: USEFUL! Stuck expressions show which patterns need branches.

---

## Experiment 7: Inner `let` without `in`

### Input
```hazel
let f = fun x ->
  let y = x + 1
in
f(5)
```

### Probe Output
```
let f = fun ⟦x⟧ ->     ≡ ∅
  let y = ⟦x + 1⟧     ≡ ∅
in
⟦f(5)⟧     ≡ ∅
```

### Format Output
```
let f = fun x -> (let y = x + 1 in
f(5)) in
?
```

### Analysis
Same absorption problem - `f(5)` became the body of the inner `let y`, and the outer let has no body.

**Verdict**: Same issue as Experiment 2.

---

## Summary Table

### Problematic (Confusing/Misleading)

| Scenario | What Happens | Symptom |
|----------|--------------|---------|
| `case` without `end` | Patterns become garbled expression | Unbound variable errors, nonsense values |
| `let` without `in` | Following code absorbed into binding | All `∅`, code "eaten" |
| `func(args` without `)` | Curried call with tuple argument | Returns weird tuples |

### Useful (Gives Meaningful Feedback)

| Scenario | What You See | Why Useful |
|----------|--------------|------------|
| Expression hole `?` | `≡ ?` in output | Clear placeholder, structure intact |
| `if-then-else` missing branch | `?` for unhandled, values for handled | See which inputs need work |
| `case` with subset of branches | Stuck expression for unmatched | Identify needed patterns |

---

## Key Insight

**Delimiter completeness matters more than expression completeness.**

- Missing `end`/`in`/`)` → parser reinterprets structure badly
- Missing expression (hole `?`) → parser fills in hole, evaluation shows `?` or stuck term

**Recommendation for incremental development**: Close delimiters first, then fill in expressions. Holes show `?` but structure stays correct.

---

## Stdin vs File Issue

Multi-line `case` expressions don't parse correctly from stdin:
```bash
echo 'case [1] | [] => 0 | x::xs => x end' | ./hazel probe --auto -
# Output: ⟦ ⟧     ≡ ?
```

But work fine from files:
```bash
echo 'case [1] | [] => 0 | x::xs => x end' > /tmp/test.hz
./hazel probe --auto /tmp/test.hz
# Output: ⟦case ⟦[1]⟧ ... end⟧     ≡ 1
```

This may be a line-ending or buffering issue worth investigating.
