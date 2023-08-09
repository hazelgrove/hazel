# Hazel Installation Guide

This guide contains step-by-step instructions for building and running Hazel.
If you are unfamiliar with `ocaml` or `opam` or get stuck, we recommend you
follow these instructions instead of the shorter instructions in the
[README.md](README.md).

## Prerequisites

- If you are on Windows, install the Windows Subsystem for Linux (WSL) by doing the
  following.

  - WSL has to be enabled before it can be installed. So, to enable WSL, do the following:

    - From the start menu or task bar, open the "PowerShell" application **as an administrator** (by right-clicking on it when you search for it in the start menu).

    - Run the following command at the PowerShell prompt:

      ```sh
      Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
      ```

    - When this command asks you if you want to reboot, reboot by pressing `y`.

  - After enabling WSL, to install WSL, go the to Microsoft Store, and search for and install "Ubuntu".
    This will install WSL and the Ubuntu Linux distribution.

  - From the start menu or taskbar, open the "Ubuntu" application.  This will
    open a Bash shell on Ubuntu Linux.  Use this when running the commands in
    the rest of these instructions.

- If you are on MacOS, make sure you have [Homebrew](https://brew.sh/) installed.

- Make sure `gcc`, `git`, `make`, and `m4` are installed.

  - If you are on Ubuntu or Windows, you can do this by running the following
    commands:

    ```sh
    sudo apt update
    ```

    ```sh
    sudo apt install gcc git make m4 pkg-config
    ```

  - If you are on MacOS, we recommend using the built-in `m4` (i.e., not the one from Homebrew).

    You can install the remaining programs (i.e., `gcc`, `git`, and `make`) by
    running the following commands:

    ```sh
    brew update
    ```

    ```sh
    brew install gcc git make pkg-config
    ```

## Install and Initialize `opam`

- Install the most recent version of `opam` (which must be at least 2.0):
  - If you are on Ubuntu Linux 19.04 (disco) or later, you can do this
    by running the following commands:

    ```sh
    sudo apt update
    ```

    ```sh
    sudo apt install opam
    ```

  - If you are on MacOS, you can do this by running the following commands:

    ```sh
    brew update
    ```

    ```sh
    brew install opam
    ```

- Check that you have the correct version of `opam` by running the following
  command:

  ```sh
  opam --version
  ```

  This should report version 2.0 or greater.  If it does not, the following
  instructions may not work.

- Initialize `opam`, by running:

  - If you are on Windows:

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

## Install OCaml

- Update the list of available `opam` packages:

  ```sh
  opam update
  ```

- Install OCaml 5.0.0 (some older versions may also work; see the
  ["Current version" section of `Updating.md`](docs/Updating-OCaml-Version.md) for
  why we may not use the newest version of OCaml).

  ```sh
   opam switch create 5.0.0 ocaml-base-compiler.5.0.0
  ```

## Clone the Source Code

- Pick a directory that you want to be the parent of the directory that contains
  the Hazel source code and use the `cd` command to change to that
  directory.

- Clone a copy of the source code by either running the following command:

  ```
  git clone git@github.com:hazelgrove/hazel.git
  ```

  Or running the following command:

  ```
  git clone https://github.com/hazelgrove/hazel.git
  ```

  This will put create a `hazel` directory containing the Hazel source code
  inside the current directory.

  If you plan to `git push` or `git pull` frequently, you may want to consider
  configuring your GitHub account to work with your SSH key.  This will prevent
  you from having type your password every time.  For more information, see the
  GitHub documentation on [Connecting to GitHub with
  SSH](https://help.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh)
  and [Troubleshooting
  SSH](https://help.github.com/en/github/authenticating-to-github/troubleshooting-ssh).

## Install Library Dependencies

- Use the `cd` command to change to the directory containing the Hazel source
  code.  If you just ran the `git clone` command, you can do this by running the
  following command:

  ```sh
  cd hazel
  ```

- If you run `ls`, you should see some files like the following:

  ```sh
  dune-project
  LICENSE
  Makefile
  README.md
  src
  UPDATING.md
  ```

  If you do not see these files, use `cd` to change to the directory containing
  the Hazel source code.

- Run the following to install the necessary OCaml library dependencies:

  ```sh
  make deps
  ```

## Compile Hazel

- You can now compile Hazel by running one of the following.

  - If you want to compile a development version of Hazel, run `make` (or equivalently, `make dev`)

  - If you want to compile a release version of Hazel, which optimizes more aggressively, run `make release`

If the build fails, it sometimes helps to do a `make clean`.

## Run Hazel

- Once Hazel is compiled, you can see it in action by running one of the
  following commands.

  - If you are on Linux, you can launch Hazel with `BROWSER $(make
    echo-html)` where (depending on your installed operating system and browser)
    `BROWSER` is probably one of:

    - `firefox`,
    - `chrome`,
    - `chrome-browser`,
    - `chromium`, or
    - `chromium-browser`.

    As a convenience, we have provided the following shorthand make targets:

    - `make firefox`
    - `make chrome`
    - `make chrome-browser`
    - `make chromium`
    - `make chromium-browser`

    On some setups, you can also use `xdg-open` to open your default browser.
    The `make xdg-open` shorthand invokes `xdg-open $(make echo-html)`.

  - If you are on MacOS, you can use the above or you can launch Hazel with 
    `open $(make echo-html)` or `make open` more concisely.

  - If you are on Windows, the path to the browser may not be so easy to type, so you
    can use the following commands to launch Hazel in your browser:
    - `make win-firefox`
    - `make win-chrome`

You can also launch Hazel directly by opening
`_build/default/src/haz3lweb/www/index.html` in your browser.  The command `make
echo-html` echos that path to the terminal, so that you don't have to remember
it.

You can also run `make repl` to get a REPL in which you can play with the definitions
in `haz3lcore`. The definitions in `haz3lweb` cannot be used in the REPL because that
package needs a browser environment to run.


## (Optional) Install fswatch for automatic reformatting & recompilation

Do this if you want to be able to run `make watch` to automatically watch
the Hazel source for changes, triggering reformatting and recompilation.

  - If you are on Ubuntu/Debian, you can do this by running the following commands:

    ```sh
    sudo apt install fswatch
    opam install fswatch
    ```

  - If you are on MacOS, you can do this by running the following commands:

    ```sh
    brew install fswatch
    opam install fswatch
    ```
