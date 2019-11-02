# Instructions for Updating Various Components

## How to update Hazel to use a new version of ocaml

There are new releases of Ocaml 2-4 times per year.

To update do the following:

- `opam update`

- `opam switch list-available`

- Choose the most recent version that does not contain a `+` character (e.g.,
  `4.09.0`)

- `opam switch create VERSION`, where `VERSION` is the most recent OCaml version
  that does not contain a `+` character (e.g., `4.09.0`).

- `make deps`

- `make repl`

- TODO: How to test REPL.

- `make release`

- Test in Firefox, Chrome, and Chromium using:

  - `chrome $(make echo-html)`,

  - `chromium $(make echo-html)`, and

  - `firefox $(make echo-html)`.

TODO: How to test in browser.

- Update the version number in `.travis.yml`

- Check that travis builds correctly.

- Update the version numbers in `README.md`

- Update the "Current Version" section in `UPDATING.md`

### Current version

The most recent version that we can use is Ocaml 4.08.1.

Using OCaml 4.09.0, will generate the following error when running `make deps`:

```Text
The following dependencies couldn't be met:
  - incr_dom → js_of_ocaml >= 3.0 → ocaml < 4.09.0
      base of this switch (use `--unlock-base' to force)
```

This is known limitation of `js_of_ocaml`, and we are waiting for the
`js_of_ocaml` maintainers to release a fix.

## How to update the `master` branch

TODO: explain how to update (need info from Cyrus)

- Test that everything works

  TODO: explain how to test that everything works
