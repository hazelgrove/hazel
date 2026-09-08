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
(filename sort by convention). **Adding, removing, or renaming a slide**
means updating that list too — one `[%blob]` line per file.

## File format

The `.hzt` extension marks this format: prose plus marker sections, NOT a
Hazel program (a `.hz` file anywhere in the repo must parse, and these do
not).

Marker lines are *exactly*:

| marker | maps to | notes |
|---|---|---|
| `@title` | `title` | `"<Folder> / <Lesson>"` (a `SlidePath`, see Folders); defaults to one derived from the filename |
| `@prompt` | `prompt` | markdown for the instructions panel |
| `@code` | `your_impl` | editor contents (parsed with `MarkerParse.of_text`) |
| `@test` | `hidden_tests.tests` | defaults to `test true end` |
| `@hint` | `display_hint` | short one-liner |
| `@reference` | `task_reference` | markdown for the Task Reference sidebar |
| `@hints` | `hidden_tests.hints` | one hint per non-empty line |
| `@flags` | misc | space-separated: `wrapper`, `show_report`, `version=N`, `id=<uuid>` |

- **No markers** → the whole file is `@code`.
- Holes are `¿` — the editor's implicit Grout, and what you are asking the
  student to fill. Probes and projectors round-trip as `^^probe(...)`.
  - **Workaround, remove once hazelgrove/hazel#2518 is fixed:** a lone `¿`
    inside a container is dropped on reload (`[¿]` comes back as `[]`), so a
    lesson wanting a hole as a container's only element has to spell it `?`
    for now. That is an explicit hole *tile*: it survives, but the student
    replaces a piece of syntax instead of filling a gap. Only
    `14-list-literals.hzt` does this today — put it back to `¿` when the bug
    is fixed.
  - In *type* position `?` means the unknown type (as in lesson 22), which is
    neither of the above.
- `wrapper` binds the **whole** `@code` as `answer` — `let answer = <impl> in
  <tests>` — so the tests see `answer` and nothing else the impl defined. Use
  it for "write one expression" lessons, where there is no binding to name.

  Without it the tests are appended *inside* the impl's own `let` chain
  (`EditorUtil.append_exp` recurses into `Let` bodies), so every binding the
  impl introduces is in scope for them. That is how a lesson whose tests
  reference `first_four` works — and it is why a lesson needing more than one
  name must leave `wrapper` off.

## Folders

A lesson's `@title` is a **`SlidePath`**: a `/`-separated hierarchical name, the
same convention Documentation-mode slide names use (`"B2T2 / Table API / ..."`).
The leading segments are folders, the last is the lesson. `src/web/util/SlidePath.re`
owns that encoding.

```
Basics / Holes
Tuple Structural Operations / Labeled Tuple Omission
```

The folder drives two things: the top bar renders one dropdown per segment
(`EditorModeView.indicator_select`), and the prev/next arrows — both the
in-slide pair and the top-bar pair — **iterate only within the current lesson's
folder**, clamping at its edges. The last lesson of a folder shows "Done! 🎉"
instead of a next arrow. Crossing folders is done with the folder dropdown. The
slide's own heading shows just the leaf, since the breadcrumb already names the
folder.

Authoring rules, pinned by `test/Test_Tutorial.re`:

- Give every lesson **exactly one** folder segment.
- Never let one title be a proper prefix of another (`Tables` alongside
  `Tables / Tables`): the shorter one becomes unreachable from the deeper
  dropdown.
- Keep a folder's lessons **contiguous** in `src/tutorialslides/Slides.re` —
  dropdown options appear in first-appearance order.
- Keep `Basics / Holes` first in that list. It is the lesson a fresh profile
  opens on (`TutorialsMode.StoreTutorialKey.default`).

Retitling a lesson is safe: identity comes from the `id=<uuid>` flag, not the
title, so moving a lesson between folders does not reset anyone's progress.

## Decoding lessons → text

```bash
./hazel tutorial-decode            # writes all compiled lessons to
                                   #   hazel-programs/tutorial-imported/
./hazel tutorial-decode "Holes"    # prints matching lessons to stdout
```

`tutorial-imported/` is a sibling dir (NOT under this one, and gitignored) so
its files are not compiled in until you deliberately move them here.

## Verifying

```bash
./hazel tutorial-verify            # per-slide OK/MISMATCH + summary
./hazel tutorial-verify --verbose  # also print before/after text for mismatches
```

`tutorial-verify` checks that each slide's impl/tests text is a **fixed point**
of the text round-trip (`to_text` == `to_text ∘ of_text ∘ to_text`). A clean
slide is reproduced faithfully by decode→encode (IDs aside).

`Test_TextRoundtrip.re`'s `TutorialLessons` group asserts the same property
over every shipped lesson, so the CLI is for *diagnosing* a mismatch
(`--verbose` prints before and after), not for catching one.

## Source pointers

`src/tutorialslides/Slides.re` (the embedded file list),
`src/web/exercises/TutorialText.re` (text→spec, at startup),
`src/CLI/TutorialDecode.re` (spec→text + verify). The text round-trip
engine is `src/haz3lcore/zipper/MarkerParse.re` (`to_text` / `of_text`, and
the `¿` convention) over `src/haz3lcore/zipper/PersistentZipper.re`.
