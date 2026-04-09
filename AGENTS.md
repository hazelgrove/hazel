# AGENTS.md - Hazel Development Guide

This document provides essential information for agentic coding agents working on the Hazel codebase.

## Project Overview

Hazel is a web-based programming environment written in **ReasonML** (OCaml with syntactic sugar), compiled to JavaScript via `js_of_ocaml`. The project uses **Dune** as its build system.

## Build Commands

```bash
# Install dependencies (run once after cloning or pulling)
make deps

# Build for development (includes source maps, auto-formatting)
make dev

# Build for release (optimized, no source maps)
make release

# Watch mode - automatically rebuild on file changes
make watch

# Watch mode for release build
make watch-release

# Format code (uses refmt/dune fmt)
make fmt

# Clean build artifacts
make clean
```

## Testing

```bash
# Run all tests
make test

# Run quick tests (skip slow/property-based tests)
make test-quick

# Run tests with filtering
./run_tests test 'Evaluator' 19      # Run test 19 from Evaluator group
./run_tests test 'Statics.*' -q    # Run all Statics tests, quiet mode
./run_tests test -q                 # Run all quick tests
```

### Running a Single Test

Use Alcotest's filtering syntax via the `run_tests` script:
```bash
./run_tests test '<GroupName>' <test_number>
```

Examples:
```bash
./run_tests test 'Evaluator' 1           # First test in Evaluator
./run_tests test 'Evaluator'             # All tests in Evaluator group
./run_tests test 'Evaluator.*' -q        # All Evaluator tests, quiet
```

Coding-agent–related groups (deterministic; no live LLM):

```bash
./run_tests test 'AgentControlFlow' -q   # Stop, flight ignore, send queue flush
./run_tests test 'AgentMultiTool' -q     # Multi-tool assistant replies, skip-on-failure
./run_tests test 'Agent UX' -q           # Chat utils, compaction slice, workbench, OpenRouter JSON
./run_tests test 'GeneralTreeRefs' -q    # get_refs_to_after_pattern_edit sanity
```

### Manual QA (coding agent UI — not exercised in Node tests)

Run through these in the browser after substantive agent UI changes:

- **Stop**: While a main or compaction request is in flight, Stop clears busy state; a late HTTP reply must not append assistant text or a compaction summary (cancel line should remain the visible outcome for that turn).
- **Send queue**: While busy, Enter queues the draft; after idle or Stop, the queue drains in order; cancel messaging stays ordered above flushed queued sends when both apply.
- **Context meter**: With no compaction on the branch, the bar reflects the last assistant message’s reported `prompt_tokens`; after compaction, expect an em dash until the next assistant reply supplies usage again.
- **Copy LLM context**: From the Agent Context panel, copy uses the clipboard shim and shows the copy toast (depends on browser APIs).
- **Compaction display**: Compaction summary content renders as Markdown in the chat transcript where applicable.

### Test Coverage

```bash
make coverage                    # Run tests with coverage instrumentation
make generate-coverage-html     # Generate HTML coverage report
```

## Code Style Guidelines

### General Principles

- **Language**: ReasonML (`.re` files). Avoid raw OCaml (`.ml` files) unless necessary.
- **Formatting**: Code is automatically formatted via `dune fmt` (uses refmt). Always run `make fmt` before committing.
- **Deriving**: Most datatypes use `[@deriving (show({with_path: false}), sexp, yojson)]` for debugging and serialization.

### Naming Conventions

- **Files**: `CamelCase.re` (e.g., `Evaluator.re`, `StringUtil.re`)
- **Modules**: `CamelCase` (e.g., `module MyModule`)
- **Types**: `CamelCase` (e.g., `type myType`, `type MyRecord`)
- **Functions/variables**: `snake_case` (e.g., `let my_function`, `my_variable`)
- **Constants**: `SCREAMING_SNAKE_CASE`

### Imports

```reason
// Direct module include (for re-exporting)
include SomeModule;

// Explicit import
module M = SomeModule;

// Open locally (use sparingly)
open OptionStorage;
```

### Error Handling

- Use `Result.t` type for functions that can fail
- Follow patterns: `Ok(value)` for success, `Error(error)` for failures
- Avoid suppressing exceptions silently

### Type Annotations

- Prefer explicit type annotations for function signatures
- Use type inference for let bindings when clear from context

### Testing Best Practices

- Test files in `test/` directory named `Test_<Feature>.re`
- Use Alcotest for unit tests
- Use QCheck for property-based tests (see `test/QCheck_Util.re`)
- Group tests by module/component using Alcotest's test suite organization

### Debugging

- Use `print_endline` to print to browser console
- Use derived `show` functions: `show_myType(value)` or `show` for type `t`
- Append `#debug` to URL for debug mode with reset options

## Project Structure

```
src/
  haz3lcore/     # Core editor functionality
  web/           # Web frontend (UI, exercises)
  util/          # Utility modules
  pretty/        # Pretty-printing/layout
  menhirParser/  # Parser (Menhir-generated)
  language/      # Language definitions
  b2t2/          # B2T2 table slides
  CLI/           # Command-line interface
test/            # Test suite
  statics/       # Type system tests
  evaluator/    # Evaluator tests
```

## Common Issues

- **Build fails after pulling**: Run `make deps` to update dependencies
- **Opam switch out of sync**: Recreate switch with `opam switch create ./ 5.2.0`
- **Tests failing**: Ensure `make setup-zarith` has been run for BigInt runtime

## Dependencies

- OCaml 5.2.0
- Dune (build system)
- Node.js (for running compiled JavaScript tests)
- npm packages: esbuild, vite (for web bundling)
