# CLI Test Command Plan

**Status: MVP IMPLEMENTED**

See `src/CLI/Cli.re` for the implementation.

## Overview

Add a `./hazel test` command to run Hazel programs and report test results. Tests in Hazel are expressions that evaluate to booleans, using the `test expr end` or `hint "name" test expr end` syntax.

## Current State

### Test Infrastructure (Already Exists)
- **AST Forms**: `Test(exp)` and `HintedTest(exp, hint_string)` in `Grammar.re`
- **Syntax**: `test expr end` and `hint "test name" test expr end`
- **Effects**: `RecordTest(instance_report)` emitted during evaluation
- **Results**: `TestMap.t` collects `(Id.t, list(instance_report))` pairs
- **instance_report**: `{exp: DHExp.t, status: TestStatus.t, hint: string}`
- **TestStatus**: `Pass | Fail | Indet`
- **TestResults**: Has `test_summary_str` for human-readable output

### What We Have Access To
- Test ID (can map back to source location via `Measured.find_by_id`)
- Test status (Pass/Fail/Indet)
- Hint string (for HintedTests, otherwise "No hint available.")
- Evaluated test expression (`DHExp.t`)

## MVP Design

### Command Interface
```bash
./hazel test program.hz           # Run tests and report results
./hazel test program.hz --verbose # Show all tests, not just failures
./hazel test -                    # Read from stdin
```

### Output Format

**Default (compact) - only show failures:**
```
Test Results: 3 passing, 2 failing

FAIL [line 12]: test x == 5 end
FAIL [line 18, "addition test"]: test 2 + 2 == 5 end
```

**Verbose - show all:**
```
Test Results: 3 passing, 2 failing

PASS [line 3]: test true end
PASS [line 7]: test 1 < 2 end
PASS [line 9, "basic math"]: test 1 + 1 == 2 end
FAIL [line 12]: test x == 5 end
FAIL [line 18, "addition test"]: test 2 + 2 == 5 end
```

### Information to Display

For each test (especially failing):
1. **Status**: PASS / FAIL / INDET
2. **Location**: Line number (from Measured)
3. **Hint** (if HintedTest): The test name in quotes
4. **Source text**: The original test syntax from the source file

### Exit Codes
- `0`: All tests pass (or no tests)
- `1`: One or more tests fail
- `2`: Parse or evaluation error

## Implementation Plan

### 1. Add `evaluate_with_tests` to Run.re
Return both the result and test results from evaluation:
```reasonml
let evaluate_with_tests = (exp: Exp.t): (Exp.t, TestResults.t) => {
  let (result, state) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  let test_results = TestResults.mk_results(state.tests);
  (result, test_results);
};
```

### 2. Add source text extraction
Extract the source text for a test given its ID and the source file:
```reasonml
let extract_source_text = (
  ~source: string,
  ~measured: Measured.t,
  id: Id.t
): option(string) => {
  switch (Measured.find_by_id(id, measured)) {
  | Some({origin, last}) =>
    let lines = lines_of_string(source);
    /* Extract text from origin to last */
    ...
  | None => None
  };
};
```

### 3. Add `test_hazel` function to Cli.re
Main logic:
1. Parse program to zipper (to get Measured positions)
2. Get term and evaluate with tests
3. Process TestMap to build output
4. For each test in TestMap:
   - Get ID → lookup in Measured for line number
   - Get hint from instance_report
   - Get status
   - If failing (or verbose): extract and display source text
5. Print summary

### 4. Add test command
```reasonml
let test_cmd = {
  let doc = "Run tests in a Hazel program and report results.";
  let verbose_arg = {
    let doc = "Show all tests, not just failures.";
    Arg.(value & flag & info(["verbose", "v"], ~doc));
  };
  let info = Cmd.info("test", ~doc);
  Cmd.v(info, Term.ret(Term.(const(test_hazel) $ verbose_arg $ input_arg)));
};
```

## Design Decisions

### Why show source text, not evaluated expression?
The evaluated `DHExp.t` is the fully-reduced form. For debugging, you want to see what you wrote, not `false`. The source text tells you which test failed.

### Why use line numbers?
Test IDs are opaque. Line numbers let you jump to the failing test.

### Why hints matter for AI workflows
With hinted tests, the AI can:
1. Write tests with descriptive names: `hint "factorial of 5" test factorial(5) == 120 end`
2. See which named test failed
3. Quickly locate and debug the issue

## Future Ideas (Not MVP)

### Auto-probe failing equality tests
For tests like `test x == 5 end` that fail, automatically instrument with probes to show actual values:
```
FAIL [line 12]: test x == 5 end
  Left:  3
  Right: 5
```

This would require:
1. Detecting equality comparisons in the test AST
2. Re-running with probes on both sides
3. Special output formatting

Add to `plans/hazel-lsp-cli/experience-log.md` after testing the basic flow.

### JSON output mode
For machine-readable results:
```json
{
  "summary": {"total": 5, "passing": 3, "failing": 2, "indet": 0},
  "tests": [
    {"line": 12, "status": "fail", "hint": null, "source": "test x == 5 end"},
    ...
  ]
}
```

### Combined analyze + test
Run static analysis and tests in one command, failing early on type errors.

## Documentation Updates

After implementation:
1. Update `cli-api.md` with test command documentation
2. Update `hazel-primer.md` debugging section to mention:
   - Use `./hazel test` to run tests
   - When a test fails, add probes to debug values
   - Use hinted tests for better error messages

## Test Plan

Create test files in `hazel-programs/`:
- `test-all-pass.hz`: All tests pass
- `test-some-fail.hz`: Mix of passing/failing
- `test-hinted.hz`: Tests with hint names
- `test-indet.hz`: Tests with indeterminate results
