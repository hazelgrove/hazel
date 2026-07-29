# User-defined livelit examples

Each `.hz` here defines a livelit and uses it several times, and ships as a
"Livelits /" documentation slide. The files run as ordinary programs
(`./hazel run hazel-programs/livelits/defined-slider.hz`) — the
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
- optional member `size = (width, height)`: projector size in character cells

Type members are accepted but not yet load-bearing; helpers (like the color
picker's `css` and `pick`) are ordinary members. A positional
`(init, update, view, expand[, size])` tuple is the desugared equivalent.
The definition must be closed — its functions evaluate in the builtin
environment — so helpers belong among the members. Each use's model is
stored in its own argument syntax, so state survives in the program text.

After editing a source, regenerate the slide encodings:

```
./hazel-programs/livelits/regen-slides.sh          # all
./hazel-programs/livelits/regen-slides.sh slider   # by substring
```

Nothing checks encodings are current — an un-regenerated edit silently ships
the old program.
