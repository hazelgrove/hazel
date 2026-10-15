# Livelit slides

Each `.hz` here is one slide of the **Documentation → Livelits** deck. The
deck's order and titles are the list in `src/livelitdemos/Slides.re`; the
folders `hygiene/`, `expansion-errors/`, `either/` and `advanced/` are
sub-decks. `playground/` is not: it is its own deck, **Documentation →
Livelit Playground**, listed as `playground_slides` in the same file --
livelits made for fun rather than to explain an idea (1990s Face, Kids'
Choice, Tree Care). It lives here so the tests that read every livelit
slide read it too. Each
slide is an ordinary Hazel program that defines a livelit and uses it. On
the command line the `^^livelit(...)` wrappers are inert and the program
runs as written:

```
./hazel run hazel-programs/docs/livelits/defined-slider.hz
```

The slides are the documentation. Start with the Overview slide
(`overview.hz`), which gives the `Livelit` signature and the commands, and
Color (Figure 3) (`color-fig3.hz`), the paper's example rebuilt line by
line. The design as built, with the plan for what is missing, is
`docs/livelits.md`.

Tests that read these files: `Test_Unproject` (every slide with a projected
use, projected and not, plus the values of a few), `Test_TreeCare`,
`Test_Either`, `Test_Parameters`, `Test_ResultView`, `Test_ExpansionErrors`,
`Test_Quote` (Hygiene, Color, Dynamic Row or Column), and the corpus-wide
`MenhirCorpus` and `DocSlides.ReparseBackuptext`.

`graph-editor.hz` is not in the deck.
