# Charts

Three programs, all shipped as documentation slides under **Charts / …**.

| File | Slide | What it is |
|---|---|---|
| `charts.hz` | Charts | The charting library — `Svg`, `Scale`, `Chart` — plus a gallery of all five kinds |
| `linked.hz` | Linked Views | An app whose bars are dragged to set their values, with a pie derived from the same model |
| `calculator.hz` | Calculator | A calculator whose keys rewrite their own source, one `|>` stage per press |

There is no chart type or chart projector in Hazel. A chart is an ordinary HTML
value built from `Node` and `Create`, which render SVG. See `docs/charts.md`.

## How to use

Open the **Charts** slide, or copy the `module Svg`/`module Scale`/`module Chart`
block out of `charts.hz` into your own program — Hazel has no cross-file
imports, so the module block is the unit of distribution.

To draw a value, put `^^probe_html` on it: that rich probe renders the
*evaluated* HTML of any expression. `^^html` is the other one, and it renders
*syntax* — right for HTML you wrote out literally and for MVU apps, wrong for a
computed chart like `Chart.bar(sales)`.

In `charts.hz` each chart carries its own `^^probe_html`, so it draws next to
the code that builds it; the gallery `Div` at the bottom is the program's
result, which **Nut menu → HTML** renders in the evaluation output as well.

From the command line, `./hazel run charts.hz` prints the HTML value the program
produces, and `./hazel test calculator.hz` runs that file's inline tests.

## Editing

These files ARE the slides: `src/charts/Slides.re` embeds them at compile time
and the load path parses them, so an edit here changes the slide on the next
build. There is no encoding step.

Two things to keep in mind:

- Indentation here is for humans; the loader flattens it, since Hazel computes
  indentation at layout time. Literal leading spaces would render doubled.
- The load path must reproduce the committed text byte for byte
  (`Test_ReparseDocSlides`). It normalises where newlines sit around a
  projector invocation, so keep `^^probe_html(Div(...` on one line rather than
  breaking after the paren.
- Comments are `#...#`; `../mvu/check-comments.py` catches unterminated ones,
  which otherwise surface as a pile of unrelated static errors.

`charts.hz` is the only full copy of the library. `linked.hz` carries `Svg` and
`Chart` trimmed to what a pie actually needs — no `Scale` at all, since a pie
has no axes — and `calculator.hz` uses no chart code at all, since its
subject is the probe rather than charting. Keeping each program to what it uses
is what stops these from becoming three libraries to keep in sync; if you extend
`charts.hz`, nothing here needs to follow.

`test/Test_Charts.re` reads the library out of the shipped slide and cuts it at
the `# ===== A GALLERY OF ALL FIVE KINDS ===== #` header — renaming that line
will fail the tests with a message saying so.
