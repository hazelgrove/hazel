# Probe Test Coverage Gaps

Priority list of untested probe/sample functionality. Created after fixing the
stack_frame None-vs-Some regression and adding Test_SampleSelection.re (unit)
and Test_Evaluator_ProbeSelection.re (integration).

## Current test files (for reference)

- `test/evaluator/Test_Evaluator_Probes.re` — probe values (integration)
- `test/evaluator/Test_Evaluator_ProbeSteps.re` — step ranges (integration)
- `test/evaluator/Test_Evaluator_ProbeCallStack.re` — call stacks (integration)
- `test/evaluator/Test_Evaluator_ProbeSelection.re` — eval→select pipeline (integration)
- `test/Test_SampleSelection.re` — Selection/Cursor pure logic (unit)
- `test/Test_AutoProbe.re` — auto-probe placement (unit)
- `test/Test_RefractorSerialization.re` — ^^probe syntax round-trip (unit)

## 1. SampleCursorPerform actions (HIGH PRIORITY)

**Why**: These are the actual functions called when users interact with probes.
Most likely to regress as UI evolves. Currently zero test coverage.

**What to test** (`src/haz3lcore/zipper/action/SampleCursorPerform.re`):
- `capture(z, data, id)` — updates cursor from a sample capture event.
  Sets call_stack, index, seq, time, step_range. Uses `is_suffix_of` to
  decide whether to keep existing deeper stack or adopt new one.
- `toggle_pin_call(z, call_stack)` — toggles pin on/off. Uses `ids_of_stack`
  to compare. Should unpin if same ids, pin if different.
- `set_index(z, i)` — changes cursor depth in breadcrumb bar.
- `reset(z)` — resets cursor to `Cursor.init`.

**Approach**: Unit tests. Create a zipper with some refractors/sample_cursor
state, apply the action, check resulting sample_cursor fields. Could use
`Zipper.next_blank()` as starting point.

## 2. Environment capture (HIGH PRIORITY)

**Why**: Core user-visible feature — probes show environment bindings.
No tests verify the evaluator captures the right bindings.

**What to test**:
- `sample.env` field is populated based on `capture_spec.refs`
- Correct bindings captured for let-bound variables in scope
- Correct values for those bindings at time of sample collection
- Nested scopes: inner probe captures both inner and outer bindings

**Approach**: Integration tests in `test/evaluator/`. Parse with probes,
evaluate, check `sample.env` fields. Similar pattern to ProbeCallStack tests.

**Example test cases**:
```
let x = 5 in ^^probe(x + 1)        → env should contain x=5
let x = 5 in let y = x + 1 in ^^probe(y)  → env should contain x=5, y=6
let f = fun x -> ^^probe(x) in f(5)       → env should contain x=5
```

## 3. Function names in stack frames (MEDIUM)

**Why**: Verifies evaluator populates `stack_frame.name` correctly.
Names drive ClosureCursorBar breadcrumb display.

**What to test**:
- `let f = fun x -> ^^probe(x) in f(5)` → stack frame name should be Some("f")
- Anonymous function (fun applied directly) → name should be None or similar
- Nested calls → each frame should have correct function name

**Approach**: Integration test. Evaluate, check `sample.call_stack[i].name`.

## 4. App args / sample.args (MEDIUM)

**Why**: When probe wraps `f(x)`, the argument value is recorded.
Powers argument display in probe UI.

**What to test** (`EvaluatorState.lookup_app_arg`):
- `^^probe(f(5))` → sample.args should contain the argument value 5
- Multiple args → correct arg captured
- Nested application → correct arg for each level

**Approach**: Integration test. Similar to ProbeCallStack pattern.

## 5. Recursive call stacks at depth (MEDIUM)

**Why**: Current tests only cover depth-1 call stacks. Deep recursion
is a common use case and call stacks grow with each recursive call.

**What to test**:
- `let f : Int -> Int = fun n -> if n == 0 then 0 else ^^probe(f(n - 1)) in f(3)`
  → should produce samples at depths 1, 2, 3
- Selection works correctly at depth > 1
- Pin at depth 2 correctly filters to that recursive invocation

**Approach**: Integration test in ProbeSelection or ProbeCallStack.

## 6. Cursor.trimmed_stack / index interaction (MEDIUM)

**Why**: `index` controls effective cursor depth by trimming the stack.
Core to breadcrumb navigation. Trimming + selection must interact correctly.

**What to test** (`Sample.Cursor.trimmed_stack`):
- cursor with call_stack=[C,B,A] and index=1 → trimmed_stack=[B,A]
  (reversed, sliced to index+1, reversed back)
- `relation` with trimmed=true uses trimmed stack
- Selection with trimmed cursor at depth 1 of a depth-3 stack

**Approach**: Unit tests in Test_SampleSelection.re. Pure logic, no eval needed.
