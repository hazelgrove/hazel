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
│   │   ├── transition/ # Step-by-step evaluation
│   │   └── Sample.re   # Probe sample types and utilities
│   └── statics/        # Type checking, elaboration
├── haz3lcore/          # Core utilities, zipper, projectors
│   ├── projectors/     # Projector implementations (probes, sliders, etc.)
│   ├── zipper/         # Zipper data structure for editing
│   └── Refractors.re   # Manual/ephemeral probe management
├── CLI/                # Command-line interface
│   ├── Cli.re          # Main CLI entry point and commands
│   ├── Run.re          # Evaluation helpers
│   └── Print.re        # Output formatting
├── web/                # Web UI (not relevant for CLI work)
└── util/               # Shared utilities

test/
├── evaluator/          # Evaluator tests
└── haz3ltest.re        # Test runner entry point

hazel-programs/         # Sample Hazel programs (.hz files)
├── docs/               # Documentation examples (extracted from src/web/init/docs/)
├── b2t2/               # B2T2 slide examples (extracted from src/b2t2/slides/)
└── study/              # User study programs and documentation
    ├── debugging/      # Programs with intentional bugs for debugging tasks
    └── writing/        # Sketch/solution pairs for program writing tasks

plans/                  # Project plans and documentation
├── hazel-lsp-cli/      # CLI/LSP development plan
│   ├── README.md       # Main plan and TODOs
│   ├── cli-api.md      # CLI documentation for AI agents
│   ├── hazel-primer.md # Hazel syntax reference
│   └── experience-log.md # AI model experience reports
└── [other plans]/      # Other in-progress plans

scripts/
└── extract-docs.py     # Extract .hz programs from ML backup_text fields
```

## Hazel CLI

Run Hazel programs from the command line:

```bash
./hazel run program.hz          # Execute and print result
./hazel analyze program.hz      # Type check (errors with line numbers)
./hazel format program.hz       # Normalize code
./hazel probe program.hz        # Run with probe values inline
./hazel probe --many program.hz # Show multiple probe samples
```

Use `-` to read from stdin: `echo 'let x = 5 in x + 1' | ./hazel run -`

See `plans/hazel-lsp-cli/cli-api.md` for detailed CLI documentation.

**Known CLI limitations:**
- No JSON output mode yet
- No combined analysis command (must run analyze + probe separately)

**Important Hazel notes:**
- Hazel has NO `let rec` keyword. Recursion works automatically with arrow type annotations.
- Use `?` for type holes instead of type variables like `'a`
- Probes: `^^probe(expr)` syntax shows runtime values

## AI Development Workflow

When developing Hazel programs as an AI model:

1. **Write code** in `.hz` files or use stdin with `./hazel run -`
2. **Check types** with `./hazel analyze` - errors are human-readable
3. **Debug** with `^^probe(expr)` and `./hazel probe --many`
4. **Reference** `plans/hazel-lsp-cli/hazel-primer.md` for syntax

**Experience logging:** Document issues, suggestions, and learnings in `plans/hazel-lsp-cli/experience-log.md` to help improve the tooling.

## Current Work

See `plans/` directory for in-progress project plans.

### Active: Hazel LSP/CLI for AI Development
Building out CLI tools for AI-assisted Hazel development.
See `plans/hazel-lsp-cli/README.md` for the full plan.
