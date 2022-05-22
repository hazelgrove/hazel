# Instructions for Updating Various Components

## Current version

The most recent version that we use is Ocaml 4.12.1. We have not updated past that for the following
reasons (which should be re-confirmed periodically):

- The package [`rtop`](https://opam.ocaml.org/packages/rtop/) does not support 4.13 or higher version.

## How to update Hazel to use a new version of ocaml

There are new releases of OCaml 2-4 times per year.

To update do the following:

- `opam update`

- `opam switch list-available`

- Choose the most recent version that does not contain a `+` character (e.g.,
  `4.12.1`)

- Create a new branch called `update_ocaml_VERSION` where VERSION is the 
  version of OCaml you intend to upgrade to. 

    `git checkout -b update_ocaml_VERSION`

- `opam switch create VERSION`, where `VERSION` is the most recent OCaml version
  that does not contain a `+` character (e.g., `4.12.1`).

- `make deps`

- `opam upgrade`

- `make change-deps`

- `make repl`

- Make sure the REPL loads correctly.

- `make release`

- Test in Firefox and Chrome.

- Update the version number in `.github/workflows/deploy_branches.yml`

  - Update the version number in step 3: Install dependencies and build hazel.
  - Push to github and check deploy status

- Update the version numbers in `README.md`.

- Update the version number and instructions in `INSTALL.md`.

- Update the "Current Version" section in `UPDATING.md`

- Announce the version change on the `#hazel-dev` channel of the `hazelgrove`
  Slack by sending the following message, with the appropriate VERSION:

      @channel
      We have switched `dev` to OCaml version VERSION. You can update to VERSION by doing the following things.
      
      - Merge your branch with either `dev` or `update_ocaml_VERSION` if that is tricky.

      - Update your OCaml installation by running the following:

        ```
        opam update
        opam switch create VERSION
        make deps
        ```
