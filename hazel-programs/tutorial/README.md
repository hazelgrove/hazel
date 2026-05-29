# Tutorial-mode slides (authored from text)

These `.hz` text files are compiled into **Tutorial-mode** lessons (the gated,
prompt-panel UI — same machinery as the hand-written `Tu_*.ml` lessons), via
`./hazel gen-tutorial`. This is the Tutorial-mode counterpart to `gen-slides`
(which makes read-only Documentation slides under `hazel-programs/study/`).

## The iteration loop

```bash
# 1. Edit / add / reorder .hz files in this directory.
# 2. Regenerate the Tutorial.spec .ml files:
./hazel gen-tutorial
# 3. Rebuild and run the app:
make dev      # or: dune build src --profile dev ; then serve
```

`gen-tutorial` writes one `TuGen_<Name>.ml` per file into
`src/web/exercises/examples/`, plus an aggregation `TutorialGenerated.ml`
(`let all : Tutorial.spec list`). That `all` is appended to the `lessons` list
in `src/web/exercises/settings/TutorialSettings_base.re`, so generated slides
appear **after** the hand-written onboarding lessons, in filename order.

To wipe the generated files (restores an empty stub):

```bash
./hazel gen-tutorial-clean
```

## File format

A plain Hazel program. Optionally split into sections with marker lines that
are *exactly* `@prompt`, `@code`, or `@test`:

```
@prompt
Markdown shown in the instructions panel to the left of the editor.
You can use **bold**, lists, `code`, etc.

@code
let x = 1 in
x + 1

@test
test x + 1 == 2 end
```

- **No markers** → the entire file is treated as `@code`. (This is how the
  existing probe-study tutorial slides were brought over verbatim, with their
  instructions still inline as `# comments #`.)
- `@prompt` → markdown for the instructions panel. Defaults to a placeholder.
- `@code` → the editor contents (`your_impl`). Must parse as a Hazel
  expression; `gen-tutorial` prints a WARNING if it doesn't.
- `@test` → a Hazel `test … end` used as the hidden test. **Defaults to
  `test true end`**, which trivially passes — so by default a slide is
  *ungated* but shows a ✔. Put a real condition here to gate the slide.

## Ordering & titles

- Slides are ordered by filename (`01-…`, `02-…`, …). Reorder by renaming.
- Title is the humanized filename: `03-auto-probe.hz` → `"03 Auto Probe"`.
- Module/id are derived deterministically from the filename, so localStorage
  state is stable across regenerations.

## Notes / current limitations (v1)

- Only the **editor content** and a markdown prompt come from text. The richer
  `Tutorial.spec` fields (`display_hint`, `task_reference`, real gating tests)
  are minimal/placeholder and can be filled in by editing the generated `.ml`
  — but that edit will be overwritten on the next `gen-tutorial`. Prefer adding
  an `@test` / `@prompt` section to the source instead.
- These slides duplicate the Documentation versions under
  `hazel-programs/study/tutorial/` for now. The Documentation copies can be
  retired once the Tutorial-mode versions are the canonical ones.
- Generator source: `src/CLI/GenTutorial.re`.
