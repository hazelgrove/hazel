# Migrating zipper-embedding exercise modules (tile-datatype flip)

Example modules that embed `Zipper.t` literals break when the tile datatype
changes. At this commit they are all converted to "transitionary" form (code
as strings, re-parsed via `*.transition` at load time), which compiles before
AND after the flip. If your branch has custom exercise/tutorial/derivation
modules, do the same at this commit before merging forward:

1. Check out the commit introducing this README, merge your branch into it.
2. Register your modules in `src/web/Migrate_exercises.re`'s `files` table
   (path + one item per top-level `let`; Blank templates need no conversion).
3. `dune build src/web/migrate_exercises.bc.js --profile dev`
4. `node --stack-size=8192 --require ./test/idb_stub.js _build/default/src/web/migrate_exercises.bc.js > /tmp/migrate_out.txt`
5. Check the `===SUMMARY===`: every file must be PASS (PASS = the emitted
   text survives parse → reprint; a FAIL is left unconverted — fix its
   syntax in-app and re-export, or ask for help).
6. `python3 scripts/split_migrate_output.py /tmp/migrate_out.txt`
7. `dune build @fmt --auto-promote && make test-quick`, commit, merge forward.
