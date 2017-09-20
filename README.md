# Hazel

Hazel is a structure editor rooted in the principles of type theory. It is 
based on Hazelnut, a structure editor calculus described in our paper at
POPL 2017, and is the primary artifact of the research vision outlined in 
our paper at SNAPL 2017. You can find these papers at [the Hazel Grove website](http://www.hazelgrove.org/).

# Building Hazel
You can build Hazel using the following instructions.

## Prerequisites

Hazel is implemented in Reason/OCaml and compiled to Javascript for the web browser via the `js_of_ocaml` compiler. An easy way to install both OCaml and the necessary libraries is to install [opam](https://opam.ocaml.org/). After having installed `opam` using the instructions on their website, follow these steps:

  - If you are using `opam` for the first time, you have to initialize it:

    ```sh
    > opam init
    > eval `opam config env`
    ```

    This will create a `.opam` directory in your home.

  - You need a recent version of the OCaml compiler. First check the current version used by `opam`:

    ```sh
    > opam switch
    --     -- 3.11.2  Official 3.11.2 release
    --     -- 3.12.1  Official 3.12.1 release
    --     -- 4.00.0  Official 4.00.0 release
    --     -- 4.00.1  Official 4.00.1 release
    --     -- 4.01.0  Official 4.01.0 release
    --     -- 4.02.0  Official 4.02.0 release
    --     -- 4.02.1  Official 4.02.1 release
    --     -- 4.02.2  Official 4.02.2 release
    --     -- 4.02.3  Official 4.02.3 release
    --     -- 4.03.0  Official 4.03.0 release
		--     -- 4.04.0  Official 4.04.0 release
    --     -- 4.04.1  Official 4.04.1 release
    --     -- 4.04.2  Official 4.04.2 release
    system  C system  System compiler (4.02.1)
    ```

    The `C` marks the current compiler. Here version 4.02.1 is installed. We can see that a more recent version is available (4.04.2). So we will install it with `opam switch 4.03.0`. This won't remove the system compiler as `opam` will install the files in your `.opam` directory. Hazel requires OCaml 4.04.2+.

    The following command switches out the current compiler with the newly installed one and sets up your path to use it permanently.

    ```sh
    > opam switch 4.03.0
    > eval `opam config env`
    ```

  - We can now install the necessary dependencies.

    ```sh
    > cd ..
    > opam update
    > opam install reason reason-parser js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent camomile js_of_ocaml-tyxml coq
    ```
    
 You now have all the required packages. We can now build the application.

## Compilation

```sh
> cd src
> make
```

From a fresh checkout, `make` does three things:

1. Extracts the semantics from the Coq file, `semantics.v`.
2. Compiles the `hazel.re` file to OCaml bytecode, `hazel.byte`, with the `rebuild` command.
3. Compiles the `www/hazel.js` file from `hazel.byte`, with the `js_of_ocaml` compiler.

## Results
You can now open src/www/hazel.html in a browser to see Hazel in action.

