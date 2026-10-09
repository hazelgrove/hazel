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
`(Model, ViewContext) -> HTML` is told where it is drawn (at its literal,
offside as a probe sample, or in a probe's drawer) and whether it is
editable, that is, whether its actions rewrite the program:

```
type Place = + Literal + Offside + Drawer        # built in
type ViewContext = (at=Place, editable=Bool)     # built in

let view(m: Model, ctx: ViewContext): HTML =
  case ctx.at
  | Drawer => chart(m)                 # room for axes, labels, every reading
  | _ => spark(m, ctx.editable)        # drag handles only where edits land
  end
```

- `Literal`: the livelit used as an expression in the program, the
  projector at its use. Editable: its actions update the model, which is
  stored in the literal's syntax.
- `Offside`: a probe sample at the end of a line. Not editable.
- `Drawer`: a probe's drawer, below the line. Not editable.

Only literals are editable today; a sample's value has no literal for an
action to rewrite. Editability is still its own field, so a read-only
literal (a locked slide, a past version) can be told apart later.

Hazel tells the two forms apart by the view's type, not its arity (a
one-argument view's `Model` may itself be a pair): the context's type must
be written, as `ViewContext` or as `(at=Place, editable=Bool)`. A
one-argument view keeps working everywhere and is drawn the same in every
place. `Place` and `ViewContext` are ordinary built-in types (like
`LivelitShape`), and a program's own `Literal`, `Offside` or `Drawer`
constructors shadow theirs as usual. The place does not change the room a
view gets: the literal and a drawer's sample chip are as tall as `shape`
says, and a sample chip on the line is one line tall.

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
- **Display.** Views are inert: their handlers dispatch nothing, and a view
  that takes a `ViewContext` is told `at=Offside` in a sample chip on the
  line and `at=Drawer` in the drawer, both with `editable=false`. With Rich
  Views on (the probe sidebar toggle, on by default), a view whose `shape`
  is at most 4 lines tall (`Inline` is 1) is embedded in each sample, and
  a taller one in each sample the probe's drawer shows when it is open
  (the drawer keeps its layout: the count badge, then one sample or all of
  them, by the samples toggle).
- **View as.** The sample menu's action bar names the view the sample is
  drawn with (its badge and name; a livelit as written, `^name`) and opens
  a list of the views that apply: Text, then the views in the order Hazel
  ranks them for its automatic pick (livelits by type name, innermost
  first, then HTML, Cards), then the livelits offered on request, then
  Table, which is never picked automatically. Choosing one
  sets it for every sample of the probe, in the drawer as on the line,
  overriding the Rich Views setting for that probe only; it is probe
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
  over the lines below, as card fans do. The Views tutorial lessons
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
