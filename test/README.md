# Hazel Test Suite

This directory contains the test suite for the Hazel project.

## Overview

- **Test Framework:** [Alcotest](https://github.com/mirage/alcotest)
- **Execution:** Tests can be run via `make` commands or directly with Node.js.

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

### 2. Using Node.js Directly

- **Run the test suite:**
    ```sh
    node _build/default/test/haz3ltest.bc.js
    ```

- **Filter tests by group or number:**
    - Run all tests in the "Statics" group with quick output:
        ```sh
        node _build/default/test/haz3ltest.bc.js test 'Statics.*' -q
        ```
    - Run only test 19 from the "Evaluator" group:
        ```sh
        node _build/default/test/haz3ltest.bc.js test 'Evaluator' 19
        ```

## Additional Information

- You can pass CLI arguments to filter and control test execution.
- For more CLI options, refer to the [Alcotest documentation](https://github.com/mirage/alcotest).

## Test File Structure

- All test files are located in this directory and its `statics/` subdirectory.
- Each file tests a specific module or feature (e.g., `Test_Evaluator.re` for the evaluator).
- Property-based tests use [QCheck](https://github.com/c-cube/qcheck) and are included when running the full test suite.

## Adding New Tests

- To add a new test, create a new file named `Test_<Feature>.re` or add to an existing file.
- Use the [Alcotest](https://github.com/mirage/alcotest) API for defining test cases.
- Utility functions for property-based testing are in `QCheck_Util.re`.

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
