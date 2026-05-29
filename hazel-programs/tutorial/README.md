# Tutorial-mode slides (authored from text)

These `.hz` text files compile into **Tutorial-mode** lessons (the gated,
prompt-panel UI — same machinery as the hand-written `Tu_*.ml` lessons) via
`./hazel gen-tutorial`. There is also an inverse (`tutorial-decode`) that turns
existing hand-written `Tutorial.spec` lessons back into this text format, and a
verifier (`tutorial-verify`).

## The iteration loop

```bash
# 1. Edit / add / reorder .hz files in this directory (subdirs allowed).
# 2. Regenerate the Tutorial.spec .ml files:
./hazel gen-tutorial
# 3. Rebuild and run the app:
make dev
```

`gen-tutorial` reads `.hz` files **recursively** (ordered by relative path),
writes one `TuGen_<Name>.ml` per file into `src/web/exercises/examples/`, plus
an aggregation `TutorialGenerated.ml` (`let all : Tutorial.spec list`). That
`all` is appended to `lessons` in
`src/web/exercises/settings/TutorialSettings_base.re`. `./hazel
gen-tutorial-clean` wipes the generated files.

## File format

A plain Hazel program, optionally split by marker lines that are *exactly*:

| marker | maps to | notes |
|---|---|---|
| `@prompt` | `prompt` | markdown for the instructions panel |
| `@code` | `your_impl` | editor contents (parsed with `TextRoundtrip.of_text`) |
| `@test` | `hidden_tests.tests` | defaults to `test true end` |
| `@hint` | `display_hint` | short one-liner |
| `@reference` | `task_reference` | markdown for the Task Reference sidebar |
| `@hints` | `hidden_tests.hints` | one hint per non-empty line |
| `@flags` | misc | space-separated: `wrapper`, `show_report`, `version=N`, `id=<uuid>` |

- **No markers** → the whole file is `@code`.
- Holes are written as `¿` (the implicit-hole marker). For a hole you want to
  *survive* re-parsing inside a container (e.g. a fillable list element), prefer
  the explicit hole token `?` — `[?]` round-trips, whereas implicit `[¿]` may
  collapse to `[]`. Probes/projectors round-trip as `^^probe(...)`.
- `wrapper` wraps the impl as `let answer = <impl> in …` so the hidden tests
  reference `answer` (used by "write one expression" lessons).

## Importing the hand-written lessons → text

```bash
./hazel tutorial-decode            # writes all hand-written lessons to
                                   #   hazel-programs/tutorial-imported/
./hazel tutorial-decode "Holes"    # prints matching lessons to stdout
```

`tutorial-imported/` is a sibling dir (NOT under this one) so it isn't picked up
by `gen-tutorial` until you deliberately move files in. To make the whole
sequence text-authored: move the imported files in here (e.g. under a `basics/`
subdir), remove the hand-written entries from `TutorialSettings_base.lessons`
(leaving `lessons = TutorialGenerated.all`), and `gen-tutorial`.

## Verifying

```bash
./hazel tutorial-verify            # per-slide OK/MISMATCH + summary
./hazel tutorial-verify --verbose  # also print before/after text for mismatches
```

`tutorial-verify` checks that each slide's impl/tests text is a **fixed point**
of the text round-trip (`to_text` == `to_text ∘ of_text ∘ to_text`). A clean
slide is reproduced faithfully by decode→encode (IDs aside). Known
non-fixed-points are grout-placement quirks in `TextRoundtrip` (e.g. `[¿]`→`[]`,
or a stray `¿` next to the `$==` test operator) — see the `?`-vs-`¿` note above.

## Generator source

`src/CLI/GenTutorial.re` (text→spec), `src/CLI/TutorialDecode.re`
(spec→text + verify). The text round-trip engine is
`src/haz3lcore/zipper/TextRoundtrip.re` (from the `slide-cli` work).
