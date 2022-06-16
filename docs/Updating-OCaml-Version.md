# Instructions for Updating Various Components

## Current version

The most recent version that we use is Ocaml 4.13.1. We have not updated past that for the following
reasons (which should be re-confirmed periodically):

- The package [`sexplib`](https://opam.ocaml.org/packages/sexplib/) as of v0.15.0 has a bug preventing
  compilation of expressions of the form

    `[@deriving sexp] type t = u and ...`

  where `u` is an identifier.
- The package [`rtop`](https://opam.ocaml.org/packages/rtop/) did not support 4.13 or higher version.

## How to update Hazel to use a new version of ocaml

There are new releases of OCaml 2-4 times per year.

To update, make sure the current branch compiles and then do the following:

- `opam update`

- Determine the latest version of Ocaml supported by the current branch.

    `opam upgrade --update-invariant --dry-run --no`

  - The curent version (`OLD-VERSION`) of Ocaml and the latest version (`NEW-VERSION`)
     supported by the current toolchain are the versions reported for the `ocaml` or
     `ocaml-base-compiler` package.

- Create a new branch called `update_ocaml_NEW-VERSION`.

    `git checkout -b update_ocaml_NEW-VERSION`

- Create a new opam switch called `update_ocaml_NEW-VERSION`.

    ```
    opam switch create update_ocaml_NEW-VERSION OLD-VERSION
    opam switch set update_ocaml_NEW-VERSION
    ```

  - If there is a local switch folder in the current working directory (`./_opam`),
    it must be overriden manually.

      `eval $(opam env --switch=update_ocaml_NEW-VERSION --set-switch)`

- Install the old toolchain and dependencies.

    `opam switch import opam.export`

- Perform the upgrade.

    `opam upgrade --update-invariant`

- Make sure the REPL loads correctly.

    `make repl`

- Make sure Hazel still runs correctly.

    `make release`

  - Test in Firefox and Chrome.

- Reconstruct `opam.export`.

    `make change-deps`

- Update the version number in `.github/workflows/deploy_branches.yml`

  - Update the version number in step 3: Install dependencies and build hazel.
  - Push to github and check deploy status

- Update the current version number and caveats at the top of this document.

- Update the version numbers in `README.md`.

- Update the version number and instructions in `INSTALL.md`.

- Announce the version change on the `#hazel-dev` channel of the `hazelgrove`
  Slack by sending the following message, with the appropriate VERSION:

      @channel
      We have switched `dev` to OCaml version VERSION. You can update to VERSION by doing the following things.

      - Merge your branch with either `dev` or `update_ocaml_NEW-VERSION` if that is tricky.

      - Update your OCaml installation by running the following:

        ```
        opam update
        opam switch create NEW-VERSION
        make deps
        ```
