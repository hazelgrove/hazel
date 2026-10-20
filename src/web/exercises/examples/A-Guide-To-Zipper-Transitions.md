# Zipper transitions

The shipped example modules (`src/web/exercises/examples/Ex_*.ml`,
`src/web/derivation/examples/Ex_*.ml`) store each editor as a
`PersistentZipper.t` literal — `{zipper = "<Zipper sexp>"; backup_text =
"<program text>"}` — and load it through `<Kind>Exercise.of_persistent`.
So when the `Zipper.t` datatype changes, they keep loading: the sexp
decode fails and the loader re-parses `backup_text`, printing
`Warning: using backup text! ...` once per editor. Nothing becomes
ill-typed; what is lost until the sexp is refreshed is whatever the
text does not carry (piece ids, projector state).

To refresh the sexps after such a change, run the exporter described at
the top of `src/web/Migrate_exercises.re`:

1. `dune build src/web/migrate_exercises.bc.js --profile dev`, run it
   under node, and split the dump into files (the header gives the exact
   commands).
2. It reprints every registered module and emits a file only when each
   editor round-trips (parse fixpoint on the text, decode-equivalence on
   the sexp); failures are listed in the summary and leave the old file
   in place.
3. Rebuild and check that the warning is gone (it also shows up in the
   test log).

A module exported from the app with "Export Exercise Module" is a plain
`Zipper.t` record literal instead: it compiles against the current
datatype but becomes ill-typed at the next change. To carry such a module
across a change, use "Export Transitionary Exercise Module" in the old
build (string components, loaded via `<Kind>Exercise.transition`), then
re-export it in the new build or run the exporter above.

NOTE: the text parser is a little quirky with spacing around
holes sometimes, so you may want to check initial states
to make sure they look right after a text-based reload.
