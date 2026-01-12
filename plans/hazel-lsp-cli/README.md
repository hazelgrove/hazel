# Hazel Language Server CLI for AI-Assisted Development

This plan outlines the development of Hazel's CLI/language server capabilities to enable AI-assisted Hazel program development.

**See [vision.md](./vision.md) for the long-term direction: semantic views, structural queries, and bidirectional editing.**

## Goals

### 1. Enable AI Development of Hazel Programs
Build out CLI tools that allow AI agents (Claude Code, etc.) to develop Hazel code via standard text files, with semantic feedback comparable to what humans get in the Hazel editor.

### 2. Build a Corpus of Sample Hazel Programs
Use AI development to create sample programs for:
- Documentation examples
- User study tasks (probe mechanism evaluation)
- Test cases and benchmarks

### 3. Process Development
Establish workflows for AI-assisted Hazel development that can be documented and reused.

## Current State Assessment

### Existing CLI Commands (via `./hazel`)

| Command | Status | Description |
|---------|--------|-------------|
| `run` | **Working** | Evaluates program, prints final value |
| `format` | **Working** | Reconstructs code from AST with explicit holes (`?`) |
| `analyze` | **Working** | Reports static errors with line numbers and erroneous terms |
| `probe` | **Working** | Shows probe values inline; use `--many` for multiple samples |

### Example Usage (Current)

```bash
# Run a program
echo 'let x = 5 in x + 3' | ./hazel run -
# Output: 8

# Format code (holes become explicit)
echo 'let  = 5 in  + 3' | ./hazel format -
# Output:
# let ? = 5 in
# ? + 3

# Static analysis (type error)
echo 'let x = true in x + 3' | ./hazel analyze -
# Output: Static errors (raw format showing Inconsistent Expectation)

# Test form evaluates but results not exposed
echo 'test true end' | ./hazel run -
# Output: ()
```

### Remaining Issues

1. **No test results reporting**: Tests execute (we can see `()` result) but pass/fail status isn't surfaced via CLI.

2. **No syntax error feedback**: Parser failures just say "Failed to parse" without details about what went wrong.

## Implementation Plan

### Phase 1: Fix Critical Bugs ✅ COMPLETED

#### 1.1 Fix Probe Command ✅
- Added `evaluate_with_probe_map` to `src/CLI/Run.re`
- Updated `src/CLI/Cli.re` to build probe_map from zipper's refractors
- Probe command now correctly shows values inline

#### 1.2 Improve Static Error Formatting ✅
- Implemented Rust-style error format with source context
- Shows line numbers, column positions, and carets pointing to errors
- Uses existing `ErrorPrint.re` for human-readable messages

### Phase 2: Add Missing Features

#### 2.1 Test Results Command
**Priority: Medium**

Add a `test` command that:
- Runs the program
- Reports test pass/fail status with counts
- Optionally shows which tests failed and their locations

The infrastructure exists in `TestResults.re` and `EvaluatorState.re`.

#### 2.2 Better Syntax Error Reporting
**Priority: Medium**

Investigate what feedback is available when parsing fails:
- Tylr's recovering parser should provide partial results
- Could show where grout (implicit holes) was inserted

### Phase 3: Enhanced Debugging Support

#### 3.1 Probe Output Refinement
**Priority: Medium**

The probe text output currently uses Unicode symbols (`≡`, `∅`, etc.). Consider:
- Alternative ASCII-only mode for broader compatibility
- Structured output option (JSON) for programmatic consumption
- More context about probe locations

#### 3.2 Combined Analysis Mode
**Priority: Low**

A single command that runs all analyses and outputs comprehensive feedback:
- Static errors
- Test results
- Probed values

This would reduce round-trips for AI agents.

### Phase 4: Edit Actions (Future)

Currently agents edit Hazel as plain text. Future work could expose:
- Structure-aware edit operations
- Refactoring commands
- Code completion suggestions

This is explicitly out of scope for the initial implementation but noted for planning.

## File Structure

```
plans/hazel-lsp-cli/
├── README.md           # This file
├── vision.md           # Long-term direction: semantic views, structural queries
├── cli-api.md          # CLI API documentation for agents
├── hazel-primer.md     # Comprehensive Hazel syntax guide
├── hazel-builtins.md   # Complete list of built-in functions
├── experience-log.md   # AI model experience reports (add entries here!)
└── implementation/     # Detailed implementation notes (as needed)

hazel-programs/
├── docs/               # Programs from documentation slides
├── b2t2/               # Programs from B2T2 slides
├── study/              # User study programs for probe evaluation
└── examples/           # General example programs

scripts/
└── extract-docs.py     # Extract .hz programs from ML backup_text fields
```

## Agent-Facing Documentation

See [cli-api.md](./cli-api.md) for documentation intended to be consumed by AI agents writing Hazel code.

## Hazel Language Notes (for AI Agents)

### Unique Characteristics

1. **Gradual Typing**: Programs with type errors can still run
2. **Structure Editing**: Holes are first-class; partial programs are valid
3. **Live Evaluation**: Probes show runtime values inline with code
4. **Test Forms**: `test <bool-expr> end` records pass/fail during evaluation

### Syntax Highlights

```hazel
# Let bindings
let x = 5 in x + 1

# Functions
let f = fun x -> x + 1 in f(5)

# Recursive Functions (arrow type annotation required, NO 'rec' keyword)
let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl => 1 + length(tl)
end
in length([1,2,3])

# Pattern matching
case x
| None => 0
| Some(y) => y
end

# Type annotations
let x: Int = 5 in x

# Probes (for debugging)
let x = 5 in ^^probe(x + 1)

# Tests
test 2 + 2 == 4 end
```

**Important**: Hazel does NOT have `let rec`. Recursion works automatically when a function has an arrow type annotation.

## Success Criteria

1. AI agents can write Hazel programs in `.hz` files
2. Agents can get static feedback (type errors) with useful information
3. Agents can run programs and observe output
4. Agents can use probes to inspect intermediate values
5. Agents can run tests and see results
6. A corpus of 20+ sample programs exists for documentation/study

## Current TODOs

### Medium Priority
- [ ] Add test results CLI command
- [ ] Better syntax error reporting

### Low Priority
- [ ] Combined analysis mode (all feedback in one command)
- [ ] JSON output option for programmatic consumption
- [ ] ASCII-only output mode
- [ ] Add column numbers to error locations

### Completed
- [x] Add line numbers to static errors (uses Measured.find_by_id)
- [x] Fix probe command - now passes probe_map to evaluator correctly
- [x] Improve static error formatting - uses ErrorPrint.re (shows erroneous term)
- [x] Create consolidated Hazel syntax primer for agents (hazel-primer.md)
- [x] Extract documentation programs from ML files (9 files in hazel-programs/docs/)
- [x] Extract B2T2 programs from ML files (39 files in hazel-programs/b2t2/)
- [x] Create initial CLI API documentation (cli-api.md)
- [x] Set up hazel-programs folder structure
- [x] Create extraction script with documentation (scripts/extract-docs.py)
- [x] Create experience log workflow (experience-log.md)
- [x] Document file structure centrally in CLAUDE.md
