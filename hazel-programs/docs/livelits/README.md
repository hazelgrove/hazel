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
  type Expansion = ...;
  let init : Model = ...;
  let update = fun (m, a) : (Model, Action) -> ...;
  let view = fun m : Model -> ...;
  let expand = fun m : Model -> ...
} in ...
```

All three type members are required — they are the livelit's interface:

- `type Model`: the state a use carries, in its own argument
- `type Action`: what the view's handlers emit
- `type Expansion`: what a use means to the program. This is the type
  clients see, so a use of `^name` has type `Expansion` however `expand`
  is written; statics checks each use's expansion against the declaration
  and marks the use when the two are inconsistent.

and the four value members:

- `init`: the model a fresh use starts with (`^name` + space inserts it)
- `update: (Model, Action) => Model`
- `view: Model => HTML` — handlers emit Actions (same HTML API as the MVU
  apps, see ../mvu/README.md)
- `expand: Model => Expansion`
- optional member `shape`: `Inline(width)`, `Block(width, height)`, or
  `Tab(width, height)` — the widget's footprint in character cells

Helpers (like the color picker's `css` and `pick`) are ordinary extra
members. There is no tuple form: a tuple has nowhere to declare the three
types. The definition must be closed — its functions evaluate in the builtin
environment — so helpers belong among the members. Each use's model is
stored in its own argument syntax, so state survives in the program text.

These files ARE the shipped `Livelits / ...` slides: they are embedded
at compile time (`src/livelitdemos/Slides.re`, ppx_blob) and parsed at
load, so an edit here ships on the next build — no encode step.
