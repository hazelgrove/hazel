# Migrating zipper-embedding exercise modules (tile-datatype flip)

DISPOSAL: mostly disposable migration tooling for the tile FormId change.
Delete this file (together with `src/web/LegacyBase.re`,
`src/web/LegacyBaseV1.re`, and `src/web/Migrate_slides.re`) once
tile-datatype has merged to dev and active feature branches have run the
recipe below. `src/web/Migrate_exercises.re` and
`scripts/split_migrate_output.py` outlive the migration: they are the
re-export tool for the example modules' shipped (persistent) format.
Nothing at runtime depends on any of it.

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

## Slide migration (serialized-segment .ml files)

The 49 slide modules under `src/web/init/docs/` and `src/b2t2/slides/`
embed segment sexps as strings inside `PersistentSegment.t` records. Old
(label+mold) sexps still compile but fail decode at runtime and fall back
to `backup_text`, which orphans the slide's refractor id references.
`src/web/Migrate_slides.re` is a RENORMALIZER with a THREE-TIER decode:
CURRENT format first (`Segment.t_of_sexp`: FormId v2 — sort-free
families + `Tok`/`TokInfix`, tile carries an explicit `sort` field),
then FormId v1 (`src/web/LegacyBaseV1.re`: sort-committed form ids
`Compound|Unsorted|Atom|Unmolded`, incl. the `(Form ...)` head alias
that predated the Compound rename), then the pre-FormId label+mold
format (`src/web/LegacyBase.re`). Both legacy tiers are upgraded
id-preservingly (v1: `Compound(cf)` => family + cf's out sort,
`Atom(class,s,t)` => `(Tok(t), s)` — or `TokInfix` for the
InfixDelimiterPrefix class, `Unsorted`/`Unmolded` => sort `Any`;
label+mold: exact reverse lookup with sort = mold.out). It then
re-emits the full .ml files (`backup_text`/`refractors` untouched).
Re-encoding normalizes to v2 heads and drops default-valued
`sort`/`shards`/`children` fields, so the tool is idempotent and usable
by branches in any of the three states. Do NOT use `hazel slide-encode`
for this: it re-parses text and re-mints ids, orphaning refractors.

If your branch has its own slide files, at the migration commit:

1. Merge your branch into the commit introducing this section.
2. Add your slides to the `entries` table in `src/web/Migrate_slides.re`
   (path + `YourSlide.out`). Slides built at load time from source (like
   `B2t2.Datasheet`) need no entry.
3. `dune build src/web/migrate_slides.bc.js --profile dev`
4. `node --stack-size=8192 --require ./test/idb_stub.js _build/default/src/web/migrate_slides.bc.js > /tmp/migrate_slides_out.txt`
5. Check the `===SUMMARY===`: every file PASS (each line notes which
   decode path was taken, `current`, `v1`, or `legacy`); the upgrade-path
   histogram should show `c(any-fallback)=0 d(classified/stale-mold)=0`
   (non-zero d means stale-mold tiles — inspect the logged tiles before
   proceeding; slides decoded as `current` don't touch the histogram).
   A decode failure (both paths) aborts the run before emitting anything;
   investigate that slide rather than regenerating it from backup_text.
6. `python3 scripts/split_migrate_output.py /tmp/migrate_slides_out.txt`
7. `dune build @fmt --auto-promote`, then verify: run the Slow reparse
   suite `bash test/run_node.sh test 'DocSlides.ReparseBackuptext'` — it
   must pass with ZERO "using backup text!" warnings in its output (each
   warning is a slide whose segment sexp failed to decode; the suite
   itself can pass trivially through the fallback).
8. `make test-quick`, commit, merge forward.
