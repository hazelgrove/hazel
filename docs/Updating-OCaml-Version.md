# Instructions for Updating Various Components

## Current version

The most recent version that we use is OCaml 4.13.1. We have not updated past that for the following
reasons (which should be re-confirmed periodically):

- The following packages break support for OCaml version 4.14.0 by conflicting on `js_of_ocaml` version 4.0.0:
  - [`incr_dom`](https://github.com/janestreet/incr_dom)
  - [`async_hs`](https://github.com/janestreet/async_js)
  - [`virtual_dom`](https://github.com/janestreet/virtual_dom)
- The package [`sexplib`](https://opam.ocaml.org/packages/sexplib/) as of v0.15.0 has a bug preventing
  compilation of some expressions of the form

    `[@deriving sexp] type t = u and ...`

  where `u` is an identifier. To work around this limitation, `sexplib` has been downgraded from version
  0.15 to 0.14, preventing a "complete" upgrade to OCaml 4.13.1 by downgrading several Jane Street packages
  from version 0.15 to 0.14.

## How to update Hazel to use a new version of OCaml

There are new releases of OCaml 2-4 times per year.

To update, make sure the current branch compiles and then do the following:

- `opam update`

- Determine the latest version of OCaml supported by the current branch.

    `opam upgrade --update-invariant --dry-run --no`

  - The curent version (`OLD-VERSION`) of OCaml and the latest version (`NEW-VERSION`)
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
