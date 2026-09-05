# Hazel CLI

The Hazel CLI is a command-line interface for working with Hazel programs. It provides tools for running, formatting, and analyzing Hazel code, as well as batch grading of student submissions. This tool is designed to streamline the development and debugging process for Hazel developers and instructors.

## Features

- **Run Hazel Programs**: Execute Hazel programs and print their evaluated results.
- **Format Hazel Code**: Reconstruct Hazel code from its abstract syntax tree (AST), using tylr to ensure syntactic correctness. Comments and refractor trigger syntax are preserved; original whitespace is replaced with structured formatting.
- **Static Analysis**: Perform static analysis on Hazel code to identify and report errors (with Rust-style source locations). Optionally also report warnings such as unused variables.
- **Grade Submissions**: Grade an exported submission JSON and emit either the raw grading data as JSON (`grade-json`) or a human-readable text report (`grade-report`).
- **Slide tooling**: List, decode, and encode the documentation slides linked into the binary, so they can be edited as plaintext and re-emitted as `.ml` modules.

## Usage
The Hazel CLI can be invoked from the command line using the `hazel` script located in the root of the repository. The script accepts various commands and options to perform different tasks.

### A note on memory

The CLI is compiled with `js_of_ocaml` and runs under Node, which defaults to a fairly small old-space heap. On bigger inputs (e.g. the larger documentation slides) commands like `analyze` can blow the default heap with no helpful error. If you see a sudden silent failure, bump the heap:

```sh
$ NODE_OPTIONS="--max-old-space-size=4096" ./hazel analyze path/to/program.hz
```

`4096` (= 4 GB) is plenty for anything in this repo.

### Running a Hazel Program
To run a Hazel program, use the `run` command followed by the path to the Hazel file. For example:

```sh
$ ./hazel run path/to/hazel_file.hz
```

This command will execute the Hazel program and print the evaluated result to the console.

Alternatively, you can use `-` instead of a file path to read from standard input. For example:

```sh
$ echo "let x = 5 in x + 3" | ./hazel run -
8
```

### Formatting Hazel Code
To format a Hazel program, use the `format` command followed by the path to the Hazel file. For example:

```sh
$ ./hazel format path/to/hazel_file.hz
```
This command will reformat the Hazel code, inserting line breaks to fit within a target width (`-w`/`--width`, default 60). Comments are preserved, but original whitespace is replaced with structured formatting. Manual refractors written with `^^probe(...)` / `^^statics(...)` trigger syntax round-trip through the formatter. Implicit holes (Grout) are rendered using the marker character set by `--implicit-hole` (default `¿`); see [Slides — round-trip](#round-tripping-a-slide-through-the-formatter) for why a marker is needed.

You can also use `-` instead of a file path to read from standard input. For example:

```sh
$ echo "let  = 5 in  + 3" | ./hazel format -
let ¿ = 5 in
¿ + 3
```
### Static Analysis
To perform static analysis on a Hazel program, use the `analyze` command followed by the path to the Hazel file. For example:

```sh
$ ./hazel analyze path/to/hazel_file.hz
```
This command will analyze the Hazel program and report any errors found, with Rust-style source locations and a caret under the whole offending term (for `m.x` or `a + b`, the operands included), as the editor highlights it. Pass `-W` / `--warnings` to also report warnings (e.g. unused variables):

```sh
$ echo "let unused = 5 in 42" | ./hazel analyze -W -
Found 1 warning:

warning: unused variable: unused
  --> -:1:5
  |
1 | let unused = 5 in 42
  |     ^^^^^^
```

You can also use `-` instead of a file path to read from standard input. For example:

```sh
$ echo "let x = 5 in x + 3" | ./hazel analyze -
No static errors found.
```
