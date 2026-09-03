# Tutorial-mode slides (authored from text)

These `.hzt` text files ARE the **Tutorial-mode** lessons (the gated,
prompt-panel UI): they are embedded at compile time (ppx_blob, like the
documentation slides) and parsed into `Tutorial.spec` records at startup.
There is also an inverse (`tutorial-decode`) that turns compiled lessons
back into this text format, and a verifier (`tutorial-verify`).

## The iteration loop

```bash
# 1. Edit .hzt files in this directory.
# 2. Rebuild and run the app:
make dev
```

Slide order is the order of the list in `src/tutorialslides/Slides.re`
(filenames are unnumbered; the list is the only ordering). **Adding,
removing, or renaming a slide** means updating that list too — one
`[%blob]` line per file. Renaming also changes the slide's `module_name`,
which keys the per-slide config tables (`TutorialSlideInit`,
`TutorialProbeStrip`) — update those keys in the same pass.

## File format

The `.hzt` extension marks this format: prose plus marker sections, NOT a
Hazel program (a `.hz` file anywhere in the repo must parse, and these do
not).

Marker lines are *exactly*:

| marker | maps to | notes |
|---|---|---|
| `@prompt` | `prompt` | markdown for the instructions panel |
| `@code` | `your_impl` | editor contents (parsed with `MarkerParse.of_text`) |
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

## Decoding lessons → text

```bash
./hazel tutorial-decode            # writes all compiled lessons to
                                   #   hazel-programs/tutorial-imported/
./hazel tutorial-decode "Holes"    # prints matching lessons to stdout
```

`tutorial-imported/` is a sibling dir (NOT under this one) so it isn't picked
up by `gen-tutorial` unless you deliberately move files in.

## Verifying

```bash
./hazel tutorial-verify            # per-slide OK/MISMATCH + summary
./hazel tutorial-verify --verbose  # also print before/after text for mismatches
```

`tutorial-verify` checks that each slide's impl/tests text is a **fixed point**
of the text round-trip (`to_text` == `to_text ∘ of_text ∘ to_text`). A clean
slide is reproduced faithfully by decode→encode (IDs aside). The known
non-fixed-point class is grout-placement quirks (e.g. `[¿]`→`[]`) — see the
`?`-vs-`¿` note above.

## Source pointers

`src/tutorialslides/Slides.re` (the embedded file list),
`src/web/exercises/TutorialText.re` (text→spec, at startup),
`src/CLI/TutorialDecode.re` (spec→text + verify). The text round-trip
engine is `src/haz3lcore/zipper/TextRoundtrip.re`.
