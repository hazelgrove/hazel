# User-defined livelit examples

Each `.hz` here defines a livelit and uses it several times, and ships as a
"Livelits /" documentation slide. The files run as ordinary programs
(`./hazel run hazel-programs/docs/livelits/defined-slider.hz`) — the
`^^livelit(...)` wrappers materialize the GUI in the editor and are inert on
the command line.

Every definition is checked against one builtin module signature, `Livelit`,
which is the whole interface:

```
type Livelit = {
  type Model; type Action; type Expansion;
  let init   : Model;
  let update : (Model, Action) -> Model;
  let view   : Model -> Html.T;
  let expand : Model -> Expansion
}
```

Its three type members are abstract, which is what lets each livelit choose
its own; a definition's own `type Model = ...` realizes them, and the four
value members are then checked at those types. Nothing seals a livelit with
this signature — sealing would hide `Expansion`, which clients must see.

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
  is written. This is checked twice: at the DEFINITION, where `expand` must
  produce the declared `Expansion` (the `Livelit` signature check above),
  and at each USE, where the expansion is checked again and the use marked
  if the two are inconsistent. The use-site check is not redundant — an
  expansion whose type depends on the model VALUE is invisible at the
  definition.

and the four value members:

- `init`: the model a fresh use starts with (`^name` + space inserts it)
- `update: (Model, Action) => Model`
- `view: Model => Html.T` — handlers emit Actions (same Html.T API as the MVU
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

## Files that are not shipped as slides

`graph-editor.hz` and `scene-3d.hz` live here and are not in
`src/livelitdemos/Slides.re`. A registered slide is parsed and evaluated
every time its deck is opened, so a definition that is slow to evaluate
does not merely render slowly -- it wedges the editor for anyone who
selects it.

`scene-3d.hz` is a 3D renderer: it rotates each cube corner by the camera's
yaw and pitch, projects, and sorts the faces far-to-near. It is correct --
`hazel run` on a flattened copy returns the expected 24 faces for its four
cubes -- and far too slow to be a widget. Measured on a dev-profile
`.bc.js` build, four cubes take 3m43s and one cube 60s against a 38s
parse-and-elaborate baseline, so roughly 8 seconds of evaluation per
quadrilateral. Removing the `to_fixed` formatting changes nothing (3m57s),
which rules out string building and leaves the evaluator's own throughput
on a few thousand float operations. A release build is faster by some
unmeasured factor; nothing close to the factor this needs.

Keep it here, unregistered, until that changes.
