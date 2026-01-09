# Hazel Project Guide

Hazel is a live functional programming environment with typed holes and live evaluation.

## Build & Test

### Building
```bash
dune build
```

### Running Tests

**Full test suite (slow, ~4 minutes):**
```bash
dune runtest
dune build @runtest --force  # Force rebuild
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

## Code Style

- **Language:** ReasonML (`.re` files) compiled with js_of_ocaml
- **Comments:** `/* ... */` style

## Key Directories

```
src/
├── language/           # Core language implementation
│   ├── dynamics/       # Evaluation, dynamic semantics
│   │   └── transition/ # Step-by-step evaluation
│   └── statics/        # Type checking, elaboration
├── haz3lcore/          # Core utilities, zipper, projectors
test/
└── evaluator/          # Evaluator tests
```

## Current Work

See `plans/` directory for in-progress project plans.
