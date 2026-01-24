# Claude Code Development Notes

## Running Tests

Build first:
```bash
dune build
```

Run specific test groups (much faster than running all tests):
```bash
# Run by test group name (regex)
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps'
node _build/default/test/haz3ltest.bc.js test 'Evaluator'
node _build/default/test/haz3ltest.bc.js test 'CanonicalCompletion'

# Run specific test number(s) within a group
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '3'
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '0..3'
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' '0,2,5'

# Show errors inline
node _build/default/test/haz3ltest.bc.js test 'ProbeSteps' --show-errors

# List all available tests
node _build/default/test/haz3ltest.bc.js list
```

Run all tests (slow):
```bash
dune runtest
```

## Key Files

- `src/haz3lcore/` - Core Hazel library
- `src/haz3lcore/zipper/` - Zipper/editing infrastructure
- `src/haz3lcore/lang/` - Language definitions (MakeTerm, Grammar, etc.)
- `src/haz3lcore/derived/` - Derived computations (Indentation, Measured, etc.)
- `src/haz3lcore/statics/` - Type checking (ExpToSegment, Statics, etc.)
- `test/` - Test files (Test_*.re)

## Test File Conventions

- Tests use textual Hazel syntax directly: `"let x = 1 in x"`
- Cursor position: `"¦"` character
- Convex holes (explicit): `"?"`
- Concave grout: `"~"`
- Parse with: `Parser.to_term(s)`, `Parser.to_segment(s)`, `Parser.to_zipper(s)`
- Print segments: `Printer.of_segment(~holes="?", ~refractors=Id.Map.empty, seg)`
