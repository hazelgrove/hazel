# HazelTigen

> **Note:** Make sure you have Docker installed and running with plenty of resources.

## Getting Started

- Unzip the `HazelTigen` folder to your local machine. Run `tar -xvzf HazelTigen.tar.gz` to extract the files.
- Open a terminal and navigate to the `HazelTigen` folder. Run `docker build -t hazeltigen .`
- Run `docker run -it hazeltigen` to start the container. You may want to use VSCode Server or somthing similar if you want to use a GUI / IDE to interact with the code.
- Now run `dune test` to test the installation. This will run a series of tests to ensure everything is working correctly.

## Usage

> **Note:** Now cd into the `testgen` directory

``` bash
cat example.hz | dune exec ./Main.exe
```

- We are using the Hazel Menhir parser interface to execute the program. As you may have seen in the example file and the accompanying report, the mark `{{{ exp }}}` is used to indicate a **reachpoint** in the program.

- We may have multiple reachpoints in a single program and the `testgen` tool will try to generate inputs that will reach all of them. A quick sanity check is to see that there is no satisfiable assigment when we mark both the *then* and *else* of an `if` expresssion as reachpoints.

- There are few other programs `sample_input.hz` and `sample_input2.hz` to run as well.
