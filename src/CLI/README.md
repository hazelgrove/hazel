# Hazel CLI

The Hazel CLI is a command-line interface for working with Hazel programs. It provides tools for running, formatting, and analyzing Hazel code. This tool is designed to streamline the development and debugging process for Hazel developers.

## Features

- **Run Hazel Programs**: Execute Hazel programs and print their evaluated results.
- **Format Hazel Code**: Reconstruct Hazel code from its abstract syntax tree (AST), using tylr to ensure syntactic correctness while removing original whitespace and comments.
- **Static Analysis**: Perform static analysis on Hazel code to identify and report errors. This currently does not report location information for errors.

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