# Livelits in Hazel

## Background

Hazel implements a version of the live literals (livelits) mechanism described in [our PLDI 2021 paper](https://hazel.org/papers/livelits-pldi2021.pdf), currently limited to:

- No parameters
- No splices

## Overview

A livelit is a live GUI widget which can be inserted into expressions and generates code by expansion to an expression of some given type. To invoke a livelit, insert the name of the livelit (always prefixed with ^) then press space.

Each livelit maintains an internal model, which we do not intend clients of the livelit to interact with. When testing a livelit you've created, you can unproject the livelit (by clicking the button on the bottom right of the Hazel UI) and edit this internal model directly.

Livelits live in the typing context, so they can be viewed using the context inspector. They can be defined either as OCaml builtins or in Hazel itself.

## User-Defined Livelits

A Hazel program can define a livelit by binding a livelit name to a module:

```
let ^pct = {
  type Model = Int;
  type Action = Int;
  let init : Model = 50;
  let update = fun (m, a) : (Model, Action) -> a;
  let view = fun m : Model -> ...;
  let expand = fun m : Model -> m
} in
^pct(25) + ^pct(75)
```

with `update: (Model, Action) => Model`, `view: Model => HTML` (handlers emit
Actions, as in the MVU apps — see mvu.md), and `expand: Model => Expansion`.
An optional member `shape = Inline(width) | Block(width, height) |
Tab(width, height)` (a `LivelitShape`) sets the projector's footprint in
character cells. Type members are accepted but not yet semantically
load-bearing. Helpers are ordinary additional members. Since modules are
sugar for labeled tuples, a positional `(init, update, view, expand[, shape])`
tuple is accepted as the equivalent form.

Each use elaborates to `^name.expand(model)` through the runtime `^name`
binding, so shadowing and scoping behave like ordinary lets, and each use's
model lives in its own argument syntax. `^name.member` is also surface
syntax: it accesses the definition record (e.g. `^pct.expand(25)`).

A projected use's `view` runs in the main evaluation (sampled at the
projector, which renders the live HTML), so probes inside `view` and
`expand` see samples per use. Interactions commit the transition itself as
the new argument — `^name(^name.update(prev, action))` — normalized by the
next evaluation, so the last interaction stays in the program where
`update`'s probes and the stepper can reach it; each commit collapses the
previous transition to its value first. Actions must therefore be
first-order data. `update` alone still evaluates in the builtin environment
(at event time, as a fallback), so helpers belong among the members.

The view is clipped to the footprint `shape` gives it, which is fixed per
livelit. A view can let one element out: an element with the class
`livelit-popover` (`Class("livelit-popover")`) may extend past the
footprint, and while one is shown the projector rises above the code, so the
element floats over the lines below like a menu instead of taking rows from
them. The `^color` lesson (Tutorial, Views / Color) opens its picker this
way: a swatch whose model carries an open flag that a click toggles.

Example programs: `hazel-programs/docs/livelits/` (shipped as the "Livelits"
doc slides, embedded at compile time — an edit there ships on the next
build). The adapter is `src/language/statics/UserLivelit.re`; rendering is
the `user_def` branch of `LivelitProj.re`.

### The View Context

A view may take a second argument. A view of type
`(Model, ViewContext) -> HTML` is told whether it is editable, that is,
whether its actions rewrite the program, and how much room it has:

```
type Room = + Free + Lines(Int, Int)             # built in
type ViewContext = (editable=Bool, room=Room)    # built in

let view(m: Model, ctx: ViewContext): HTML =
  case ctx.room
  | Lines(_, cols) => spark(m, cols)   # on the line: the readings that fit
  | Free => chart(m)                   # axes, labels, every reading
  end
```

- `editable`: true only at the livelit's literal, the projector at its use,
  where its actions update the model stored in the literal's syntax. A
  probe sample's value has no literal for an action to rewrite, so a view
  drawn for a probe is read-only. Editability is its own field, so a
  read-only literal (a locked slide, a past version) can be told apart
  later.
- `Free`: the view sizes itself, by its `shape`: at its literal, and in a
  probe's drawer, which reserves the shape's lines.
- `Lines(lines, columns)`: a probe sample on the line, which has two
  lines (`Lines(2, columns)`) and the sample's width: the budget its text is
  abbreviated to (12 columns a sample in Many mode, 150 in One), so
  resizing the sample (Shift+drag, Shift+arrows) resizes the room. The
  view is clipped to this room, fading out at an edge it overflows. A view
  that takes the context is drawn on the line whatever its `shape`, since
  it can fit itself into the room it is told; a one-argument view taller
  than the line's lines waits for the drawer.

Hazel tells the two forms apart by the view's type, not its arity (a
one-argument view's `Model` may itself be a pair): the context's type must
be written, as `ViewContext` or as `(editable=Bool, room=Room)`. A
one-argument view keeps working everywhere and is drawn the same in every
room. `Room` and `ViewContext` are ordinary built-in types (like
`LivelitShape`), and a program's own `Free` or `Lines` constructors shadow
theirs as usual.

### Livelits as Rich Probes

A user-defined livelit also renders probe samples of the type it expands
to, with no livelit syntax at the probed site. An optional member
`wrap : Expansion -> Model` rebuilds a display model from a sampled value,
which is then shown as `view(wrap(value))`. Without `wrap`, the value
itself is the model, which only works when `Model` and the expansion type
are the same type.

```
type Trace = + Trace([Int], Int, Int) in
let ^trace = {
  type Model = Trace;
  type Action = + Nothing;
  let init : Model = Trace([1, 2, 3], 0, 5);
  let update(m: Model, _: Action): Model = m;
  let view(tr: Model): HTML = Text("trace");
  let expand(m: Model): Trace = m;
  let wrap(v: Trace): Model = v;
  let shape : LivelitShape = Inline(6)
} in
^^probe(Trace([4, 5, 9], 2, 8))
```

- **By type name.** A livelit that expands to `Point` takes sites typed
  `Point`, not every `(Int, Int)`, so an alias opts values in. Two
  structural exceptions: an expansion type written structurally (`[Int]`)
  takes sites of that structure, and an alias whose body itself names a
  type or constructor (`Plan = (Mode, Temp)`) takes sites typed by that
  body. Pattern probes match through the pattern's type.
- **Innermost first.** The livelits in the probed site's context are tried
  innermost binding first, and the first whose `view` renders the value
  is the automatic view, so a nearer definition shadows an outer one. The
  others whose views render it are offered in the sample menu's "View as"
  list. `wrap` is not type-checked; if it (or `view`) fails on a value,
  that livelit passes.
- **On request.** "View as" also offers every livelit whose expansion type
  fits the site's once aliases are unfolded (is consistent with it), though
  an automatic pick never shows it: with `type Trace = [Int]`, a plain
  `[Int]` can be shown as a `^trace` on request. At a site of unknown type
  every livelit fits, so the list offers the ones whose view renders the
  sample. A livelit is offered only if it declares its expansion type.
- **Lists.** A list of a viewed type (`[Point]`) renders as a row of
  element views.
- **Display.** Views are read-only: their handlers dispatch nothing, and a
  view that takes a `ViewContext` is told `editable=false`, with room
  `Lines(2, columns)` in a sample chip on the line and `Free` in the
  drawer. With Rich Probes on (the probe sidebar toggle, on by default), a
  view is embedded in each sample on the line when it fits the line's room
  (it takes the context, or its `shape` is at most 2 lines tall; `Inline`
  is 1; a hand of cards takes 2), and is clipped to that room there;
  otherwise it is drawn in each sample the probe's drawer shows when it is
  open (the drawer keeps its layout: the count badge, then one sample or
  all of them, by the samples toggle).
- **View as.** The sample menu's action bar names the view the sample is
  drawn with (its badge and name; a livelit as written, `^name`) and opens
  a list of the views that apply: Text, then the views in the order Hazel
  ranks them for its automatic pick (livelits by type name, innermost
  first, then HTML, Cards), then the livelits offered on request, then
  Table, which is never picked automatically. Choosing one
  sets it for every sample of the probe, in the drawer as on the line,
  overriding the Rich Probes setting for that probe only; it is probe
  state, so it survives edits. A view too tall for the chip opens the
  drawer. In the keyboard menu (`/`), V opens the list, the arrows move,
  Enter chooses and Esc closes just the list. Double-clicking a sample
  toggles it between text and the chosen view.
- **On the chip.** An embedded view draws directly on the sample chip, with
  no backdrop, in a chip a text sample's height (24.4px at the default
  size), so a view drawn 24px tall fills it. Its `currentColor` is the
  sample's ink in every focus state, so marks drawn in it recede as
  unfocused text does; colors the view picks itself (a swatch, a red mark
  for an exception) are left as they are, unless the view applies the
  fade itself from `--sample-fade` (1, or 0.7 where text fades), e.g.
  `("opacity", "var(--sample-fade, 1)")`. A view taller than one line hangs
  over the lines below, as card fans do, as far as its room. The Views tutorial lessons
  (`hazel-programs/tutorial/views-*.hzt`) each define one such view.
- **In the drawer.** A view in a drawer chip sits in a card like the
  drawer's table (cream fill, a 1px border, rounded corners), in the code's
  ink rather than the sample's. It draws to the card's edges, and a view
  2px less than its lines' height (25.13px a line: 124px for five) fills the
  rows the drawer reserves for it. Views of `Tune` in Views / Piano Roll
  are drawn for it.

The renderer is `LivelitRenderer.re`, registered in
`RichProbeRegistry.re` ahead of the generic HTML and card renderers; the
"View as" control is `view_picker` in `ProbeProj.re`.

## Creating a Built-in Livelit

A built-in livelit is created by implementing the `BuiltinLivelit` module type. The current structure uses OCaml modules to define livelits, which are converted into raw livelits (which use Hazel language encodings in preparation for future work on user-defined livelits) at compile time.

### Module Type for Built-in Livelits

```reasonml
type model_exp = TermBase.Exp.t;
type expansion_exp = TermBase.Exp.t;
type action_exp = TermBase.Exp.t;

module type BuiltinLivelit = {
  // Livelit name (used with ^ prefix to invoke)
  let name: livelit_name;

  // Model type and related conversions
  type model_t;
  let hazel_model_t: TermBase.Typ.t; // defines the type of model_exp
  let model_to_hazel: model_t => model_exp;
  let model_from_hazel: model_exp => option(model_t);
  let model_default: model_t;

  // Expansion type and related conversions
  type expansion_t;
  let hazel_expansion_t: TermBase.Typ.t; // defines the type of expansion_exp
  let expansion_f: model_t => expansion_t;
  let expansion_to_hazel: expansion_t => expansion_exp;

  // Actions that update the model
  type action_t;
  let hazel_action_t: TermBase.Typ.t; // defines the type of action_exp
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => option(action_t);
  let update: (action_t, model_t) => model_t;

  // View/rendering function
  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list;

  // Shape (footprint) specification
  let shape: ProjectorShape.t;
};
```

## Registering a New Livelit

After creating a module that implements the `BuiltinLivelit` interface, add it to the `livelits` list at the end of the file:

```reasonml
let livelits: list(raw_livelit) =
  [(module Slider), (module Emotion), (module YourNewLivelit)]
  |> List.map(raw_of_builtin);
```

## Styling Livelits

To add CSS to style your livelit, modify the `src/web/www/style/projectors/proj-livelit.css` file.
