# Instructions for Updating Various Components

## How to update Hazel to use a new version of ocaml

There are new releases of OCaml 2-4 times per year.

To update do the following:

- `opam update`

- `opam switch list-available`

- Choose the most recent version that does not contain a `+` character (e.g.,
  `4.12.1`)

- `opam switch create VERSION`, where `VERSION` is the most recent OCaml version
  that does not contain a `+` character (e.g., `4.12.1`).

- `make deps`

- `make repl`

- TODO: How to test REPL.

- `make release`

- Test in Firefox, Chrome, and Chromium using:

 - `make firefox`
 - `make chrome`
 - `make chromium`

  TODO: Testing protocol

- Update the version number in `.github/workflows/deploy_branches.yml`

  - Update the version number in step 3: Install dependencies and build hazel.
  - Push to github and check deploy status

- Update the version numbers in `README.md`

- Update the "Current Version" section in `UPDATING.md`

- Announce the reversion change on the `#hazel-dev` channel of the `hazelgrove`
  Slack by sending the following message.

      @channel
      We switch to ocaml version 4.12.1. You can update to 4.12.1 by doing the following things.
      
      - Merge your branch with `dev` or `update_ocaml`.
      - Update your OCaml installation by running the following:

        ```
        opam update
        opam switch create 4.08.1
        make deps
        ```

## Current version

The most recent version that we use is Ocaml 4.12.1.

- The package [`rtop`](https://opam.ocaml.org/packages/rtop/) does not support 4.13 or higher version.
