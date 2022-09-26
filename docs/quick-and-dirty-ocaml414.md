From project root folder:

1. Checkout `haz3l-tests` or pull it into your feature branch.

2. If working on a feature branch, make sure `opam.export` looks exactly
   like the one in `haz3l-tests` plus your own changes, nothing more or less.

3. Run `make update-ocaml`.

That's it! Now `make` should compile the project with OCaml 4.14.
