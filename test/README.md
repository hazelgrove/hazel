# Hazel Test Suite

This directory contains the test suite for the Hazel project.

## Overview

- **Test Framework:** [Alcotest](https://github.com/mirage/alcotest)
- **Execution:** Tests are run via `make` commands or the `run_tests` script.

## How to Run Tests

### 1. Using Make

- **Run all tests (including slow and property-based):**
    ```sh
    make test
    ```

- **Run only quick tests (skip slow/property-based):**
    ```sh
    make test-quick
    ```

### 2. Using the run_tests Script

The `run_tests` script builds with dune and then invokes the test runner,
forwarding any CLI arguments. Use this when you need to filter tests by
group or number:

- **Run all tests:**
    ```sh
    ./run_tests
    ```

- **Run all tests in the "Statics" group with quick output:**
    ```sh
    ./run_tests test 'Statics.*' -q
    ```

- **Run only test 19 from the "Evaluator" group:**
    ```sh
    ./run_tests test 'Evaluator' 19
    ```

- For more CLI options, refer to the [Alcotest documentation](https://github.com/mirage/alcotest).

### 3. Scaling QCheck Counts at Runtime

Every QCheck property test declares a `~count` in source, but those counts can
be multiplied at runtime via environment variables — no source edits needed:

```sh
QCHECK_LONG=1 QCHECK_LONG_FACTOR=10 ./run_tests test 'Statics.Properties'
QCHECK_LONG=1 QCHECK_LONG_FACTOR=100 make test
```

- `QCHECK_LONG=1` enables QCheck's "long" mode in `QCheck_alcotest`.
- `QCHECK_LONG_FACTOR=N` multiplies every test's `count` by `N` while long
  mode is active.

Useful when chasing rare counterexamples without bumping the per-test
defaults (which would slow down regular CI/dev runs).

Other useful QCheck env vars:

- `QCHECK_SEED=<int>` — reproduce a previous run by reusing its random seed
  (the seed is printed at the start of each run).
- `QCHECK_VERBOSE=1` — print each generated case.

### 4. The Weekly Extended Run

`make ci-extended` is the whole suite in long mode: it sets `QCHECK_LONG=1` and
leaves `QCHECK_LONG_FACTOR` to the caller, so

```sh
QCHECK_LONG_FACTOR=100 make ci-extended
```

runs every property a hundred times over. The `Extended Tests` workflow
(`.github/workflows/extended-tests.yml`) does exactly that against `dev` every
Sunday — long enough to turn a property that fails on one seed in a few hundred
into one you can actually see.

Be aware of what the factor does *not* touch when you size it. Only the QCheck
properties scale; the other `Slow`-tagged tests and the three "stats only"
harnesses in `Test_Statics_Properties` are a fixed floor. Runtime is linear in
the factor on top of that constant, and so falls well short of N times the
normal suite at factor N. To size a factor against a time budget, time the
suite at two factors and divide the difference by the gap between them.

It fixes the seed rather than letting QCheck pick one, and prints the whole
`QCHECK_*` command to the run's job summary, so a failure there reproduces
locally verbatim. It gates nothing; a red run is a bug report, not a broken
build.

## Architecture

- **`test/dune`** defines the test executable and two dune aliases (`@runtest` for all tests, `@test-quick` for quick tests). The `Makefile` targets invoke these aliases.
- **`test/run_node.sh`** is a shared wrapper that runs the compiled JS with the correct Node.js flags (`--stack-size=8192` for deeply recursive tests, `--require idb_stub.js` for IndexedDB globals). Both dune rules and `run_tests` use this script so the flags are defined in one place.
- **`run_tests`** builds with dune, then calls `run_node.sh` directly, which allows it to forward arbitrary CLI arguments to the test runner.

## Test File Structure

- All test files are located in this directory and its `statics/` subdirectory.
- Each file tests a specific module or feature (e.g., `Test_Evaluator.re` for the evaluator).
- Property-based tests use [QCheck](https://github.com/c-cube/qcheck) and are included when running the full test suite.

## Adding New Tests

- To add a new test, create a new file named `Test_<Feature>.re` or add to an existing file.
- Use the [Alcotest](https://github.com/mirage/alcotest) API for defining test cases.
- Utility functions for property-based testing are in `QCheck_Util.re`.

## Performance Budgets

`evaluator/Test_PerfBudgets.re` (quick suite) fails CI when the evaluator does
more work than its budget. It counts the steps the evaluator's trampoline
takes, not time, so a count is the same on every machine and run. It holds:

- a cold run of a small benchmark program (recursion, list traversals, case
  matches), and
- a re-run after editing a literal nothing else reads, which must reuse the
  earlier results: it has its own budget and must also stay under 5% of the
  cold run.

Each budget is a ratchet, and the failure message says which way it tripped:

- **Over budget** means a performance regression. Find it, or, if the extra
  steps are legitimate (e.g. a semantics change), raise the budget in
  `Test_PerfBudgets.re` and give the reason in the commit message.
- **More than a quarter under budget** also fails, as a stale budget. Lower it
  in the same change (the message suggests a value), so the improvement is
  recorded.

The test log prints each count (`PERF …`). Step counts can't see a step that
got slower, such as extra work inside one step, so time evaluator hot-path
changes separately (e.g. `hazel bench-eval` before and after).

## Troubleshooting

- If you encounter build errors, ensure all dependencies are installed:
    ```sh
    make deps
    ```
- For issues with Node.js execution, check your Node.js version (>=14 recommended).

## Code Coverage

- **Run tests with coverage instrumentation:**
    ```sh
    make coverage
    ```
  This will run the test suite and collect coverage data.

- **Generate an HTML coverage report:**
    ```sh
    make generate-coverage-html
    ```
  This will produce an HTML file with a coverage overview, which you can open in your browser to inspect which parts of the codebase are covered by tests.

## Continuous Integration

- Tests are automatically run on each pull request via GitHub Actions.
- CI status can be viewed on the repository main page.
- The test suite uses [junit_alcotest](https://github.com/Khady/ocaml-junit) to generate a `junit.xml` report, which is picked up by CI for test result reporting.
- Coverage information is uploaded to [Codecov](https://about.codecov.io/) to provide coverage metrics on pull requests.
