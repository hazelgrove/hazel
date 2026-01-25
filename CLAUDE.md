# Hazel Project Guide

Hazel is a live functional programming environment with typed holes and live evaluation.

## Build & Test

### Building
```bash
make
```

### Running Tests

**Full test suite (very slow, ~4 minutes):**
```bash
make test
```
**Important test suite (slow, ~2 minutes):**
```bash
make test-quick
```

**Running specific tests (fast, ~0.1s):**

```bash
# Build first
dune build

# Run by test group name (regex)
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps'
node _build/default/test/haz3ltest.bc.js test 'Evaluator'

# Run specific test number(s) within a group
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '3'
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '0..3'
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '0,2,5'

# Show errors inline
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' --show-errors

# List all available tests
node _build/default/test/haz3ltest.bc.js list
```

**Test output locations:**
- Logs: `_build/_tests/HazelTests/<TestGroup>.<number>.output`
- Check failures: `grep -l "FAIL" _build/_tests/HazelTests/*.output`

### Test Framework
Uses [Alcotest](https://github.com/mirage/alcotest) with js_of_ocaml. Tests in `test/` use `` `Quick `` or `` `Slow `` annotations.

### Expected failures:

Tests in Pattern Coverage Checker may fail regilarly locally; this is a known node issue. There is an intermittant failure in the Mehnir property test; re-run.


## Code Style

- **Language:** ReasonML (`.re` files) compiled with js_of_ocaml
- **Comments:** `/* ... */` style

## Key Files

- `src/haz3lcore/` - Core Hazel library
- `src/haz3lcore/zipper/` - Zipper/editing infrastructure
- `src/haz3lcore/lang/` - Language definitions (MakeTerm, Grammar, etc.)
- `src/haz3lcore/derived/` - Derived computations (Indentation, Measured, etc.)
- `src/haz3lcore/statics/` - Type checking (ExpToSegment, Statics, etc.)
- `test/` - Test files (Test_*.re)

## Test File Conventions

- When possibly (it's not always possible) tests should use textual concrete Hazel syntax directly: `"let x = 1 in x"`
- Cursor position can be printed with appropriate Printer options, commonly: `"¦"` character
- Convex holes (explicit) can be printed with appropriate Printer options, commonly: `"?"`
- Concave grout can be printed with appropriate Printer options, commonly: `"~"`
- Parse with: `Parser.to_term(s)`, `Parser.to_segment(s)`, `Parser.to_zipper(s)`
- Print segments: `Printer.of_segment(~holes="?", ~refractors=Id.Map.empty, seg)`
