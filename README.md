# Hazel [![Build status: dev](https://img.shields.io/travis/hazelgrove/hazel/dev?label=build:%20dev)](https://travis-ci.org/hazelgrove/hazel) [![Build status: master](https://img.shields.io/travis/hazelgrove/hazel/master?label=build:%20master)](https://travis-ci.org/hazelgrove/hazel)

![Hazel Mascot](src/hazelweb/www/imgs/hazel-logo.png)

Hazel is a live functional-programming environment rooted in the principles of
type theory. You can find the relevant papers and more motivation at [the Hazel
website](http://hazel.org/).

You can try Hazel online with either the
[stable](https://hazel.org/build/master/index.html) or
[development](https://hazel.org/build/dev/index.html) version.

## Screenshot of Hazel in Action

![Screenshot of Hazel](hazel-screenshot.png)

## Building and Running Hazel

### Prerequisites

- If you are on Windows:

  - Go the to Microsoft Store, and search for and install "Ubuntu".  This will
    install the Windows Subsystem for Linux and the Ubuntu Linux distribution.

  - From the start menu or taskbar, open the "Ubuntu" application.  This will
    open a Bash shell on Ubuntu Linux.

- Make sure make sure `gcc`, `git`, `make`, and `m4` are installed.

  - If you are on Linux or Windows, you can do this by running the following
    commands:

    ```sh
    sudo apt update
    ```

    ```sh
    sudo apt install gcc git make m4
    ```

  - If you are on MacOS, you can do this by running the following commands:

    ```sh
    TODO
    ```

### Install and Initialize `opam`

- Install the most recent version of `opam` (which must be at least 2.0):

  - If you are on Linux or Windows, you can do this by running the following
    commands:

    ```sh
    sudo add-apt-repository ppa:avsm/ppa
    ```

    ```sh
    sudo apt update
    ```

    ```sh
    sudo apt install opam
    ```

  - If you are on MacOS, you can do this by running the following commands:

    ```sh
    TODO
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
    opam init --disable-sandboxing
    ```

  - If you are on Linux or MacOS:

    ```sh
    opam init
    ```

- Enable `opam` for the current shell with the following:

  ```sh
  eval $(opam env)
  ```

### Install OCaml

- Update the list of available `opam` packages:

  ```sh
  opam update
  ```

- Install OCaml 4.08.1 (some older versions such as may also work, but see the
  ["Current version" section of `Updating.md`](UPDATING.md#current-version) for
  why we do not use newer versions).

  ```sh
  opam switch create 4.08.1
  ```

### Clone the Source Code

- Pick a directory that you want to be the parent of the directory that contains
  the Hazel source code and use the `cd` command to change to that
  directory.

- Clone a copy of the source code by running the following command:

  ```
  git clone git@github.com:hazelgrove/hazel.git
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

### Install Library Dependencies

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

### Compile Hazel

- You can now compile Hazel by running one of the following.

  - If you want to compile a development version of Hazel, run the following command:

    ```sh
    make dev
    ```

  - If you want to compile a release version of Hazel, run the following command:

    ```sh
    make release
    ```

If the build fails, it sometimes helps to do a `make clean` before running `make
dev` or `make release` again.

### Run Hazel

- Once Hazel is compiled, you can see it in action by running one of the
  following commands.

  - If you are on Linux or MacOS, you can launch Hazel with `BROWSER $(make
    echo-html)` where (depending on your installed operating system and browser)
    `BROWSER` is one of:

    - `firefox`,
    - `chrome`,
    - `chrome-browser`,
    - `chromium`, or
    - `chromium-browser`.

  - If you are on Windows, the path to the browser may not be so easy to type, so you
    can use the following commands to launch Hazel in the browser:
    - `make win-firefox`
    - `make win-chrome`

You can also launch Hazel directly by opening
`_build/default/src/hazelweb/www/index.html` in your browser.  The command `make
echo-html` echos that path to the terminal, so that you don't have to remember
it.

You can also run `make repl` to get a REPL in which you can play with the core
Hazel functions.

## Suggested Extensions for VS Code

Most of our team uses VisualStudio Code to write code.  If you use VS Code, here
are a few extensions that might be helpful.

- These extensions provide support for editing ReasonML and Dune source code:

  - [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode)
  - [Dune](https://marketplace.visualstudio.com/items?itemName=maelvalais.dune)

- Due to Reason's poor parse errors, unbalanced parentheses can be difficult
  to find.  The following extensions help with that.

  - [Bracket Pair Colorizer 2](https://marketplace.visualstudio.com/items?itemName=coenraads.bracket-pair-colorizer-2)
  - [Indenticator](https://marketplace.visualstudio.com/items?itemName=sirtori.indenticator)
  - [indent-rainbow](https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow)

## Build System Details

Hazel is implemented in Reason (a dialect of OCaml) and is compiled to
Javascript for the web browser via the `js_of_ocaml` compiler.

Though, `make` targets are provided as a convenience, they mostly translate to
`dune` commands.

Invoking `make` by itself is equivalent to invoking `make dev`. With these
commands we pass additional flags to `js_of_ocaml` that cause the insertion of
comments that map locations in the generated JS to locations in the source
files. This is useful for debugging purposes.

`make dev` also auto-formats Reason source files using `refmt` (this is what the
`@src/fmt` alias is for). This ensures code from all contributors follows the
same style.

The `make dev` and `make release` commands do three things:

1. Generate some parsers using `menhir`.
2. Compile the Reason code to OCaml bytecode using the OCaml compiler.
3. Compile the OCaml bytecode to JavaScript
   (`_build/default/src/hazelweb/www/hazel.js`) using `js_of_ocaml`.
