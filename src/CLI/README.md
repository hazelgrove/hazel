# Hazel CLI

The Hazel CLI is a command-line interface for working with Hazel programs. It provides tools for running, formatting, and analyzing Hazel code, as well as batch grading of student submissions. This tool is designed to streamline the development and debugging process for Hazel developers and instructors.

## Features

- **Run Hazel Programs**: Execute Hazel programs and print their evaluated results.
- **Format Hazel Code**: Reconstruct Hazel code from its abstract syntax tree (AST), using tylr to ensure syntactic correctness while removing original whitespace and comments.
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
This command will reformat the Hazel code, inserting line breaks to fit within a target width (`-w`/`--width`, default 60). Comments are preserved, but original whitespace is replaced with structured formatting. Manual refractors written with `^^probe(...)` / `^^statics(...)` trigger syntax round-trip through the formatter. The formatter performs explicit hole insertion using `?` instead of grout.

You can also use `-` instead of a file path to read from standard input. For example:

```sh
$ echo "let  = 5 in  + 3" | ./hazel format -
let ? = 5 in                            
? + 3
```
### Static Analysis
To perform static analysis on a Hazel program, use the `analyze` command followed by the path to the Hazel file. For example:

```sh
$ ./hazel analyze path/to/hazel_file.hz
```
This command will analyze the Hazel program and report any errors found, with Rust-style source locations and a caret pointing at the offending span. Pass `-W` / `--warnings` to also report warnings (e.g. unused variables):

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

### Slides

The CLI exposes the documentation slides that ship with Hazel (the `let out : ... PersistentSegment.t` modules under `src/web/init/docs` and `src/b2t2/slides`) by **name**. The slides are looked up out of the in-binary slide list, so you don't need to point the CLI at a particular `.ml` file to read one.

#### Listing slides

```sh
$ ./hazel slide-list
Basic Reference
Projectors
ADTs
...
Probes
Livelits
B2T2: example tables
...
```

#### Decoding a slide to plaintext

`slide-decode` prints a slide's program as plaintext. Manual refractors are rendered with `^^probe(...)` / `^^statics(...)` trigger syntax so the output is reparseable:

```sh
$ ./hazel slide-decode "Probes" | head -3
#  _____           _                #
# |  __ \         | |               #
# | |__) | __ ___ | |__   ___  ___  #
```

#### Encoding a slide back to `.ml`

`slide-encode` builds a slide `.ml` module from a title and a plaintext program. Pass `-` to read the program from stdin, and `-o` to write the result to a file (otherwise it goes to stdout). Refractors written using trigger syntax in the input are rebuilt on parse:

```sh
$ ./hazel slide-encode --title "Probes" path/to/program.hz -o src/web/init/docs/Probes.ml
```

#### Round-tripping a slide through the formatter

Because `slide-decode` produces reparseable plaintext and `format` preserves refractors, you can pretty-print a slide in place by composing the three commands:

```sh
$ ./hazel slide-decode "Probes" \
    | ./hazel format -w 60 - \
    | ./hazel slide-encode --title "Probes" - -o src/web/init/docs/Probes.ml
```

### Grading Submissions

The CLI can grade Hazel exercise submissions. The input is a submission JSON file (the JSON export produced by Hazel's export feature in exercise mode), which includes persisted state for every exercise the student worked on. The grader dispatches on each exercise kind (Code, Derivation, Theorem) and produces a per-exercise score and summary.

#### Raw JSON output

Use `grade-json` to produce the raw grader output as JSON:

```sh
$ ./hazel grade-json path/to/submission.json --output report.json
```

The output is an array of `{ name, report: { summary, overall: [earned, max] } }` objects. Omit `--output` to write to stdout.

#### Human-readable text report

Use `grade-report` to produce a human-readable text summary:

```sh
$ ./hazel grade-report path/to/submission.json --output report.txt
```

The text output lists each exercise with its score and breakdown, followed by a `Total:` line aggregating across all exercises. Omit `--output` to write to stdout.