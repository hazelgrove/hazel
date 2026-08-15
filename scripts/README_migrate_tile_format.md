# Migrating zipper-embedding exercise modules (tile-datatype flip)

DISPOSAL: delete this file once tile-datatype has merged to dev and
active feature branches with custom exercise modules have run the
recipe below. `src/web/Migrate_exercises.re` and
`scripts/split_migrate_output.py` outlive the migration: they are the
re-export tool for the example modules' shipped (persistent) format.
Nothing at runtime depends on any of it. (The slide-migration half of
this apparatus — `Migrate_slides.re`, `LegacyBase.re`, and its recipe —
was deleted at the 2026-08-14 dev merge: doc slides ship as
`hazel-programs/**.hz` text on dev, so no branch holds serialized slide
`.ml` files to migrate anymore.)

Example modules that embed `Zipper.t` literals break when the tile datatype
changes. Two module formats are involved:

- **Transitionary** (the migration interchange format): each editor is
  program TEXT, re-parsed via `*.transition` at load time. Compiles before
  AND after the flip (it is independent of the Zipper/Tile representation),
  but the startup re-parse is slow (~2-3s across the registered examples),
  so it is only a vehicle for crossing the migration commit.
- **Persistent** (the shipped format): each editor is a
  `PersistentZipper.t` literal — serialized zipper sexp + `backup_text`
  plaintext fallback — decoded via `*.of_persistent` (same scheme as the
  doc slides). Decode at startup is ~free; a decode failure after a future
  datatype change self-heals by re-parsing `backup_text` (with a printed
  warning).

At the migration commit the examples are all in transitionary form; on
current tile-datatype they have been re-exported as persistent. If your
branch has custom exercise/tutorial/derivation modules:

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
8. Re-export as persistent: once merged past the commit that flipped the
   examples to persistent form (the exporter now emits `of_persistent`
   modules and additionally checks that each emitted zipper sexp decodes
   to a segment structurally equal to the reparse of its `backup_text`),
   repeat steps 3-7 so your modules stop re-parsing at startup. The
   transitionary modules still WORK after the flip — this step is a
   startup-performance fix, not a correctness one.
