# Hazel

Hazel is a live functional programming environment rooted in the principles
of type theory. You can find the relevant papers and more motivation at
[the Hazel website](http://hazel.org/).

## Building and Running Hazel

You can build and run Hazel using the following instructions.

### Prerequisites

Hazel is implemented in Reason/OCaml, and is compiled to Javascript for the web browser via the `js_of_ocaml` compiler. An easy way to install both OCaml and the necessary libraries is to install [opam](https://opam.ocaml.org/). After having installed `opam` using the instructions on their website, follow these steps:

  - Make sure you have OCaml 4.07.1+:

    ```sh
    > ocaml --version
    The OCaml toplevel, version 4.07.1
    ```

  - Make sure opam is updated:

    ```sh
    > opam update
    ```

  - Run the Makefile in the `hazel` root folder to install the necessary OCaml dependencies:

    ```sh
    > make deps
    ```

### Compiling

We can now build the application.

```sh
> make release
```

The `make release` command does three things:

1. Generates some parsers using `menhir`.
2. Compiles the Reason code to OCaml bytecode using the OCaml compiler.
3. Compiles the OCaml bytecode to JavaScript (`_build/default/src/hazelweb/www/hazel.js`) using `js_of_ocaml`.

If something weird is going on, it sometimes helps to do a `make clean`.

### Running

You can now open `_build/default/src/hazelweb/www/hazel.html` in a browser to see Hazel in action.

### Debugging

Invoking `make` by itself is equivalent to invoking `make debug`. With these commands we pass additional flags to `js_of_ocaml`, which causes the insertion of comments that map locations in the generated JS to locations in the source files. This is useful for debugging purposes.

`make debug` also auto-formats Reason source files using `refmt` (this is what the `@src/fmt` alias is for). This ensures code from all contributors follows the same style.

You can also run `dune utop src/hazelcore` to get a REPL in which you can play with the core Hazel functions.
