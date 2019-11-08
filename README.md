# Hazel

Hazel is a live functional-programming environment rooted in the principles of
type theory. You can find the relevant papers and more motivation at [the Hazel
website](http://hazel.org/).

## Building and Running Hazel

### Build Status

- Development branch (`dev`):
  [![Build Status](https://travis-ci.org/hazelgrove/hazel.svg?branch=dev)](https://travis-ci.org/hazelgrove/hazel)

- Stable branch (`master`):
  [![Build Status](https://travis-ci.org/hazelgrove/hazel.svg?branch=master)](https://travis-ci.org/hazelgrove/hazel)

### Prerequisites

Hazel is implemented in Reason (a dialect of OCaml) and is compiled to
Javascript for the web browser via the `js_of_ocaml` compiler.  An easy way to
install both Reason and the necessary libraries is to use
[opam](https://opam.ocaml.org/) and the following steps.

- If you do not have `opam` installed, install it using the instructions in the
  [Installing `opam`](#installing-opam) section of this document.

- Update the list of available `opam` packages:

  ```sh
  opam update
  ```

- Install OCaml 4.07.1 (some older versions such as may also work, but see the
  ["Current version" section of `Updating.md`](UPDATING.md#current-version) for
  why we do not use newer versions).

  ```sh
  opam switch create 4.07.1
  ```

- Run the Makefile in the `hazel` root folder to install the necessary OCaml
  dependencies:

  ```sh
  make deps
  ```

#### Installing `opam`

- If you are on Windows:

  - Go the to Microsoft Store, and search for and install "Ubuntu".  This will
    install the Windows Subsystem for Linux and the Ubuntu Linux distribution.

  - From the start menu or taskbar, open the "Ubuntu" application.  This will
    open a Bash/Linux shell.

- In the shell, run the following three commands:

  ```sh
  sudo add-apt-repository ppa:avsm/ppa
  ```

  ```sh
  sudo apt update
  ```

  ```sh
  sudo apt install m4 opam
  ```

- Check that the `opam` install worked by running:

  ```sh
  opam --version
  ```

  It should report at version 2.0 or greater.  If you have a version before
  that, the following instructions may not work.

- Initialize `opam`, by running:

  - If you are on windows:

    ```sh
    opam init --disable-sandboxing
    ```

  - On any other platform:

    ```sh
    opam init
    ```

- Enable `opam` for the current shell with the following:

  ```sh
  eval $(opam env)
  ```

### Compiling

You can now build the application with:

```sh
make release
```

The `make release` command does three things:

1. Generates some parsers using `menhir`.
2. Compiles the Reason code to OCaml bytecode using the OCaml compiler.
3. Compiles the OCaml bytecode to JavaScript
   (`_build/default/src/hazelweb/www/hazel.js`) using `js_of_ocaml`.

If something weird is going on, it sometimes helps to do a `make clean`.

### Running

Once Hazel is compiled, you can see it in action by opening
`_build/default/src/hazelweb/www/index.html` in your browser.

The command `make echo-html` echos that path to the terminal, so that you don't
have to remember it.

Thus you can launch Hazel with `BROWSER $(make echo-html)` where (depending on
your installed operating system and browser) `BROWSER` is one of:

- `firefox`,
- `chrome`,
- `chrome-browser`,
- `chromium`, or
- `chromium-browser`.

For Window's users, the path to the browser may not be so easy to type, so the
commands `make win-firefox` and `make win-chrome` will launch Hazel in the
browser.

### Debugging

Invoking `make` by itself is equivalent to invoking `make dev`. With these
commands we pass additional flags to `js_of_ocaml` that cause the insertion of
comments that map locations in the generated JS to locations in the source
files. This is useful for debugging purposes.

`make dev` also auto-formats Reason source files using `refmt` (this is what the
`@src/fmt` alias is for). This ensures code from all contributors follows the
same style.

You can also run `make repl` to get a REPL in which you can play with the core
Hazel functions.
