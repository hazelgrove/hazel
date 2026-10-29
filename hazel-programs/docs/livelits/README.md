# User-defined livelit examples

Each `.hz` here defines a livelit and uses it several times, and ships as a
"Livelits /" documentation slide. The files run as ordinary programs
(`./hazel run hazel-programs/docs/livelits/defined-slider.hz`) — the
`^^livelit(...)` wrappers materialize the GUI in the editor and are inert on
the command line.

A livelit definition binds a livelit name to a module:

```
let ^name = {
  type Model = ...;
  type Action = ...;
  let init : Model = ...;
  let update = fun (m, a) : (Model, Action) -> ...;
  let view = fun m : Model -> ...;
  let expand = fun m : Model -> ...
} in ...
```

- `init`: the model a fresh use starts with (`^name` + space inserts it)
- `update: (Model, Action) => Model`
- `view: Model => HTML` — handlers emit Actions (same HTML API as the MVU
  apps, see ../mvu/README.md)
- `expand: Model => Expansion` — what a use means to the program
- optional member `shape`: `Inline(width)`, `Block(width, height)`, or
  `Tab(width, height)` — the widget's footprint in character cells

Type members are accepted but not yet load-bearing; helpers (like the color
picker's `css` and `pick`) are ordinary members. A positional
`(init, update, view, expand[, shape])` tuple is the desugared equivalent.
The definition must be closed — its functions evaluate in the builtin
environment — so helpers belong among the members. Each use's model is
stored in its own argument syntax, so state survives in the program text.

These files ARE the shipped `Livelits / ...` slides: they are embedded
at compile time (`src/livelitdemos/Slides.re`, ppx_blob) and parsed at
load, so an edit here ships on the next build — no encode step.
