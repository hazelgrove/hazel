# Hazel

Hazel is a live functional programming environment rooted in the principles
of type theory. You can find the relevant papers and more motivation at
[the Hazel website](http://hazel.org/).

## Building Hazel
You can build Hazel using the following instructions.

### Prerequisites

Hazel is implemented in Coq and Reason/OCaml, and is compiled to Javascript for the web browser via the `js_of_ocaml` compiler. An easy way to install both OCaml and the necessary libraries is to install [opam](https://opam.ocaml.org/). After having installed `opam` using the instructions on their website, follow these steps:

  - Make sure you have OCaml 4.06+:

    ```sh
    > ocaml --version
    The OCaml toplevel, version 4.07.1
    ```

  - Make sure opam is updated:

    ```sh
    > opam update
    ```

  - Install the necessary OCaml dependencies:

    ```sh
    > opam install dune reason js_of_ocaml tyxml deriving ppx_deriving reactiveData js_of_ocaml-tyxml camomile menhir oUnit
    ```

  - We also need Coq:

    ```sh
    > opam install coq
    ```

### Compilation

We can now build the application.

```sh
> cd src
> dune build --auto-promote
```

From a fresh checkout, `dune build --auto-promote` does four things:

1. Extracts the semantics from the Coq file, `Semantics.v`.
2. Generates some parsers using `menhir`.
3. Auto-formats all reason code, using `refmt` (this is why we need `auto-promote`).
4. Compiles the reason code to `src/_build/default/www/hazel.js`.

If writing `dune build --auto-promote` is annoying, we put that command into the `Makefile`, so you can also just run `make`.

If something weird is going on, it sometimes helps to do a `dune clean` to delete the various files being generated.

### Debugging
You can use `dune build --profile debug --auto-promote` instead of `dune build --auto-promote`. This differs only in that we pass the `--debuginfo` flag to `js_of_ocaml`, which causes the insertion of comments mapping locations in the generated JS to locations in the source files. This is useful for debugging purposes, but causes a substantial increase in compilation time and file size, so it is disabled by default.

### Results
You can now open `src/_build/default/www/hazel.html` in a browser to see Hazel in action.

