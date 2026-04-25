# Hazel CLI

The Hazel CLI is a command-line interface for working with Hazel programs. It provides tools for running, formatting, and analyzing Hazel code, as well as batch grading of student submissions. This tool is designed to streamline the development and debugging process for Hazel developers and instructors.

## Features

- **Run Hazel Programs**: Execute Hazel programs and print their evaluated results.
- **Format Hazel Code**: Reconstruct Hazel code from its abstract syntax tree (AST), using tylr to ensure syntactic correctness while removing original whitespace and comments.
- **Static Analysis**: Perform static analysis on Hazel code to identify and report errors. This currently does not report location information for errors.
- **Grade Submissions**: Grade an exported submission JSON and emit either the raw grading data as JSON (`grade-json`) or a human-readable text report (`grade-report`).

## Usage
The Hazel CLI can be invoked from the command line using the `hazel` script located in the root of the repository. The script accepts various commands and options to perform different tasks.

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
This command will reconstruct the Hazel code from its AST and print the formatted code to the console. Note that this will not preserve original whitespace or comments. Additionally, the formatter performs explicit hole insertion using `?` instead of grout.

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
This command will analyze the Hazel program and report any errors found. Note that this does not provide location information for errors.

You can also use `-` instead of a file path to read from standard input. For example:

```sh
$ echo "let x = 5 in x + 3" | ./hazel analyze -
No static errors found.
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