# Hazel

Hazel is a structure editor rooted in the principles of type theory. It is 
based on Hazelnut, a structure editor calculus described in our paper at
POPL 2017, and is the primary artifact of the research vision outlined in 
our paper at SNAPL 2017. You can find these papers at [the Hazel Grove website](http://www.hazelgrove.org/).

# Running Hazel
You can run Hazel without installing any dependencies by opening /src/www/hazel.html in a browser. We plan to soon have [a hosted version](http://www.hazelgrove.org/hazel/) available.

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
    system  C system  System compiler (4.02.1)
    ```

    The `C` marks the current compiler. Here version 4.02.1 is installed. We can see that a more recent version is available (4.03.0). So we will install it with `opam switch 4.03.0`. This won't remove the system compiler as `opam` will install the files in your `.opam` directory.

    The following command switches out the current compiler with the newly installed one and sets up your path to use it permanently.

    ```sh
    > opam switch 4.03.0
    > eval `opam config env`
    ```

  - We can now install the necessary dependencies.

    First we will install the Reason frontend.

    ```sh
    > git clone https://github.com/facebook/reason.git
    > cd reason
    > opam pin add -y reason-parser reason-parser
    > opam pin add -y reason .
    ```

    Then we can install our library dependencies.

    ```sh
    > cd ..
    > opam install js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent camomile coq js_of_ocaml-tyxml
    ```

  - To make sure you have the latest versions of everything, ask `opam` to upgrade the packages if needed:

    ```sh
    > opam update
    > opam upgrade
    ```

  You now have all the required packages. We can now build the application.

## Compilation

You can execute build.sh to compile Hazel.

```sh
> cd src/
> ./build.sh
```

It consists of two steps:

1. Compile the `hazel.re` file to OCaml bytecode with the `ocamlbuild` command.
2. Build the Javascript file from the `hazel.byte` file with the `js_of_ocaml` command.

## Results
You can now open src/www/hazel.html in a browser to see Hazel in action.

# Implementation Details

The file `hazel_semantics.re` implements the syntax and semantics in a pure functional style, independent of any details of the user interface.

The file `hazel_model.re` gives the signature of the reactive signal that models edit states, which consist of a Z-expression paired with an H-type.

The file `hazel_view.re` transforms Z-expressions to pretty printed documents. 

The file `pretty.re` defines a generic pretty printer, and a translation from pretty printed documents to HTML. This HTML is styled by the `style.css` file in the `www` directory.

The file `hazel.re` (which should be read roughly from the bottom up) is the top-level file. It sets up the reactive signals and constructs the UI. The main logic of interest has to do with the action palette, which controls updates to the model and therefore the view.

