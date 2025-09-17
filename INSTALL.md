# Hazel Installation Guide

This guide contains step-by-step instructions for building and running Hazel.
If you are unfamiliar with `ocaml` or `opam` or get stuck, we recommend you
follow these instructions instead of the shorter instructions in 
[README.md](README.md).

## Prerequisites

- If you are on Windows, [install the Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/install).

- If you are on MacOS, you probably want to have [Homebrew](https://brew.sh/) installed.

- Make sure `gcc`, `git`, `make`, `nodejs`, and `m4` are installed.

  - If you are on Ubuntu or WSL + Ubuntu, you can do this by running the following
    commands:

    ```sh
    sudo apt update
    sudo apt install gcc git make m4 nodejs pkg-config
    ```

  - If you are on MacOS, we recommend using the built-in `m4` (i.e., not the one from Homebrew).

    You can install the remaining programs (i.e., `gcc`, `git`, `node`, and `make`) by
    running the following commands:

    ```sh
    brew update
    brew install gcc git make node pkg-config
    ```

## Install and Initialize `opam`

- Install the most recent version of `opam` (which must be at least 2.0):

  - If you are on Ubuntu Linux 19.04 (disco) or later, you can do this
    by running the following commands:

    ```sh
    sudo apt update
    sudo apt install opam
    ```

  - If you are on MacOS, you can do this by running the following commands:

    ```sh
    brew update
    brew install opam
    ```

- Check that you have the correct version of `opam` by running the following
  command:

  ```sh
  opam --version
  ```

  This should report version 2.0 or greater. If it does not, the following
  instructions may not work.

- Initialize `opam`, by running:

  - If you are on WSL:

    ```sh
    opam init github git+https://github.com/ocaml/opam-repository.git
    ```

  - If you are on Linux or MacOS:

    ```sh
    opam init
    ```

- Enable `opam` for the current shell with the following:

  ```sh
  eval $(opam env)
  ```

 - You may want to put that line into your `~/.zshrc` file (default on recent Macs) and `~/.bashrc` (used by the VS Code shell) so you don't have to run it again every time you open a new shell.

## Install OCaml
You can use either a global opam switch or a switch local to the Hazel source directory.

### Global Switch

- Update the list of available `opam` packages:

  ```sh
  opam update
  ```

- Install OCaml 5.2.0 (see the
  ["Current version" section of `Updating.md`](docs/Updating-OCaml-Version.md) for
  why we may not use the newest version of OCaml).

  ```sh
   opam switch create 5.2.0 ocaml-base-compiler.5.2.0
  ```

- Update the current switch (OCaml environment):
  ```sh
  opam switch 5.2.0
  eval $(opam env)
  ```

### Local Opam Switch

If you are working on multiple OCaml projects, it may be helpful to set up a local Opam switch. You can do so as follows:

  ```sh
  opam update
  opam switch create ./ 5.2.0
  eval $(opam env)
  make deps
  ```

## Clone the Source Code


- Pick a directory that you want to be the parent of the directory that contains
  the Hazel source code and use the `cd` command to change to that
  directory.

- Clone a copy of the source code.

  You should first make sure you have [GitHub SSH keys setup](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account). Then you can run the following command to clone Hazel:

  ```
  git clone git@github.com:hazelgrove/hazel.git
  ```

  NOTE: We do not recommend cloning using https, because this will not allow you to contribute back to Hazel without going through some even more awkward hoops. If you want to quickly get Hazel cloned without setting up SSH keys, we recommend using the [GitHub Desktop application](https://github.com/apps/desktop) or the Visual Studio Code command "Git Clone", which will set things up for you in the background.

## Install Library Dependencies

- Use the `cd` command to change to the directory containing the Hazel source
  code. If you just ran the `git clone` command, you can do this by running the
  following command:

  ```sh
  cd hazel
  ```

- Run the following to install the necessary OCaml library dependencies:

  ```sh
  make deps
  ```

  It is a good idea to run `make deps` whenever you pull new versions of the code from GitHub so you can be sure that you have the latest dependencies installed.

## Build and Run Hazel

You can now go back to 
[README.md](README.md) and follow the instructions there to build and run Hazel.

## Troubleshooting dependency updates and build failures

- If Hazel fails to compile after you update or pull new dependencies, first run:
  
  ```sh
  make deps
  ```

- If that doesn't work, your opam switch may be out of sync. Remove and recreate it, then reinstall deps:
  
  ```sh
  # Replace 5.2.0 with the name of your switch
  opam switch remove 5.2.0
  ```
  
  Then recreate the switch following the steps in this guide (see "Install OCaml" and "Install Library Dependencies"), run `eval $(opam env)`, `make deps`, and rebuild.
