# Migrating zipper-embedding exercise modules (tile-datatype flip)

DISPOSAL: disposable migration tooling for the tile FormId change. Delete
this file (together with `src/web/LegacyBase.re`, `src/web/Migrate_slides.re`,
`src/web/Migrate_exercises.re`, and `scripts/split_migrate_output.py`) once
tile-datatype has merged to dev and active feature branches have run the
recipe below. Nothing at runtime depends on it.

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

## Slide migration (serialized-segment .ml files)

The 49 slide modules under `src/web/init/docs/` and `src/b2t2/slides/`
embed segment sexps as strings inside `PersistentSegment.t` records. Old
(label+mold) sexps still compile but fail decode at runtime and fall back
to `backup_text`, which orphans the slide's refractor id references.
`src/web/Migrate_slides.re` decodes each embedded segment with
`LegacyBase.segment_of_sexp` (the pre-FormId types, kept verbatim in
`src/web/LegacyBase.re`), upgrades it id-preservingly via
`LegacyBase.upgrade_segment` (exact reverse lookup (label, mold) ->
FormId), and re-emits the full .ml files (`backup_text`/`refractors`
untouched). Note: the FormId constructor is now named `Compound`; old
sexps with `Form` heads are read via the alias in `FormId.t_of_sexp`. Do NOT use `hazel slide-encode` for this: it re-parses text
and re-mints ids, orphaning refractors.

If your branch has its own slide files, at the migration commit:

1. Merge your branch into the commit introducing this section.
2. Add your slides to the `entries` table in `src/web/Migrate_slides.re`
   (path + `YourSlide.out`). Slides built at load time from source (like
   `B2t2.Datasheet`) need no entry.
3. `dune build src/web/migrate_slides.bc.js --profile dev`
4. `node --stack-size=8192 --require ./test/idb_stub.js _build/default/src/web/migrate_slides.bc.js > /tmp/migrate_slides_out.txt`
5. Check the `===SUMMARY===`: every file PASS; the upgrade-path histogram
   should show `c(any-fallback)=0 d(classified/stale-mold)=0` (non-zero d
   means stale-mold tiles — inspect the logged tiles before proceeding).
   A legacy decode failure aborts the run before emitting anything;
   investigate that slide rather than regenerating it from backup_text.
6. `python3 scripts/split_migrate_output.py /tmp/migrate_slides_out.txt`
7. `dune build @fmt --auto-promote`, then verify: run the Slow reparse
   suite `bash test/run_node.sh test 'DocSlides.ReparseBackuptext'` — it
   must pass with ZERO "using backup text!" warnings in its output (each
   warning is a slide whose segment sexp failed to decode; the suite
   itself can pass trivially through the fallback).
8. `make test-quick`, commit, merge forward.
