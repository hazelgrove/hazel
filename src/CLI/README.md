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

### Comprehensive check

The `check` command runs every diagnostic in a single pass and prints one report covering **syntax errors**, **static errors**, **live-typing errors**, **warnings**, and **test results**. It is the one-shot command for validating a plaintext program (e.g. a decoded slide):

```sh
$ ./hazel check path/to/hazel_file.hz
```

The report is emitted to stdout, section by section, using the same Rust-style locations as `analyze` and the same per-test formatting as `test`. Live-typing errors are the ones surfaced only after a live run: the program is evaluated to gather probe samples and type instantiations (the expressions with unknown types are targeted automatically), then statics is re-run with those runtime observations. Any error that appears only in that second pass — not in the pure static analysis — is reported as a `live typing error`.

The exit code is `0` only when there are no syntax errors, no static errors, no live-typing errors, and no failing tests. **Warnings do not fail the run** (matching `analyze -W` and `gcc -Wall`). This makes `check` suitable as a CI/pre-commit gate for `.hz` sources:

```sh
$ echo "1 : ? : String" | ./hazel check -
No static errors found.

Found 1 live typing error:

live typing error: Expecting type String but got inconsistent type Int
  --> -:1:3
  |
1 | 1 : ? : String
  |   ^

No warnings found.

Test Results: No tests available.
```

As with `analyze`, bump the Node heap on large slides: `NODE_OPTIONS="--max-old-space-size=4096" ./hazel check path/to/program.hz`.

### Slides

The CLI exposes the documentation slides that ship with Hazel (the `let out : ... PersistentSegment.t` modules under `src/web/init/docs` and `src/b2t2/slides`) by **name**. The slides are looked up out of the in-binary slide list, so you don't need to point the CLI at a particular `.ml` file to read one.

#### Listing slides

```sh
$ ./hazel slide-list
Basic Reference
Projectors
ADTs
Tuples
...
Probes
Livelits
B2T2 / Datasheet
B2T2 / Example Tables
...
```

#### Decoding a slide to plaintext

`slide-decode` prints a slide's program as plaintext. Manual refractors are rendered with `^^probe(...)` / `^^statics(...)` trigger syntax, projectors as `^^fold(...)` (etc.), and implicit holes (Grout) as a marker token — `¿` by default, overridable with `--implicit-hole`. The marker character is what lets `slide-encode` recover Grout positions on the way back; see [round-trip](#round-tripping-a-slide-through-the-formatter) below for the reasoning. The output is reparseable:

```sh
$ ./hazel slide-decode "Probes" | head -3
#  _____           _                #
# |  __ \         | |               #
# | |__) | __ ___ | |__   ___  ___  #
```

#### Encoding a slide back to `.ml`

`slide-encode` builds a slide `.ml` module from a title and a plaintext program. Pass `-` to read the program from stdin, and `-o` to write the result to a file (otherwise it goes to stdout). Refractors written using trigger syntax in the input are rebuilt on parse, and any `¿` markers (or whatever was passed to `--implicit-hole`) are converted back to Grout via a destruct-and-regrout pass so the round-trip is bit-stable:

```sh
$ ./hazel slide-encode --title "Probes" path/to/program.hz -o src/web/init/docs/Probes.ml
```

#### Round-tripping a slide through the formatter

Because `slide-decode` produces reparseable plaintext and `format` preserves both refractors and the implicit-hole marker, you can pretty-print a slide in place by composing the three commands:

```sh
$ ./hazel slide-decode "Probes" \
    | ./hazel format -w 60 - \
    | ./hazel slide-encode --title "Probes" - -o src/web/init/docs/Probes.ml
```

##### Why `¿` for implicit holes?

A persisted slide segment may contain Grout pieces (implicit holes) sitting in shape positions the parser cares about. If we printed those as nothing — or as a regular space — the re-parsed text wouldn't know they were there, and the round-trip would lose them (or, worse, the Printer's whitespace would glue tokens together). So we print Grout as a marker token.

The marker has to be:

1. **A single, self-contained token** so it parses to one Tile that `slide-encode` can find and destruct, letting `remold_regrout` re-insert the Grout in the canonical place.
2. **Disjoint from identifier characters** so it doesn't glue with adjacent keywords. (An identifier-shaped marker next to `in` would print as `inMARKER` and the parser would read the whole thing as one variable, swallowing the `in` keyword.)
3. **Disjoint from operator characters** so it doesn't glue with adjacent commas, semicolons, etc. (E.g. `[1, ¿, 3]` must tokenize as seven tokens, not five with `¿,` merged.)
4. **Distinct from the parser's `?` empty-hole token** so user-typed `?` Tiles survive round-trip distinct from implicit Grout.

`¿` (U+00BF) satisfies all four. The tokenizer is configured (in `Token.re` / `Form.re`) to treat it as an atomic `ImplicitHoleMarker` form with the same Convex molds as `ExplicitHole`. Override with `--implicit-hole CHAR` if you need a different character (e.g. for testing or downstream tooling); pass the same `--implicit-hole` to every command in the pipe so the marker survives all the way through.

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