/* On-demand documentation packs, served by the `read_docs` tool. The
   always-on system prompt and the tool description carry only the one-line
   blurbs below (both generated from this registry, so they cannot drift),
   and a full guide costs context only when the agent pulls it. Fenced code
   in pack bodies is validated by Test_PromptFactory. */

type pack = {
  name: string,
  blurb: string, /* one line: what it teaches and when to read it */
  body: string,
};

let mvu: pack = {
  name: "mvu",
  blurb: "build an interactive app (Elm-style MVU) the user can operate inside the program — read before writing any app or GUI",
  body: {|# Building an interactive app (MVU)

A Hazel program can end in a live, interactive app. An app is a 4-tuple
`(init, update, view, subs)` rendered by the `html` projector:

- `init : Model` — the starting model
- `update : (Model, Action) -> (Model, Cmd.T)` — model first. If no update
  issues commands, write `update : (Model, Action) -> Model` and lift it
  with the `noCmd` helper shown below.
- `view : Model -> Html.T` — event handlers emit Actions
- `subs : Model -> Sub.T` — `Sub.none` unless you need timers/keyboard

## HTML and events

The substrate is four builtin modules, not global names: `Html`, `Attr`,
`Cmd`, `Sub`. Nothing is in scope unqualified, so a program's own `App`,
`Value` or `Text` cannot collide with it.

Elements are `Html` members: `Html.div([attrs], [children])`,
`Html.button`, `Html.span`, `Html.input([attrs])`, `Html.text(str)`,
`Html.int(n)`, `Html.float(x)`, and `Html.node(tag, attrs, children)` for
anything else.
Attributes are `Attr` members: `Attr.class(str)`,
`Attr.style([(prop, value)])`, `Attr.type_`, `Attr.value`, `Attr.min`,
`Attr.max`, `Attr.step`, `Attr.placeholder`, `Attr.disabled`, ...
Handlers produce Actions: `Attr.on_click(action)`,
`Attr.on_input(fun s -> action)`, `Attr.on_key_down(fun e -> action)`. For
pointer position relative to the element (in px):
`Attr.on_click_at(fun (x, y) -> action)`, and likewise
`Attr.on_mouse_down_at`, `Attr.on_mouse_move_at`, `Attr.on_mouse_up_at`;
`Attr.on_wheel_at(fun (x, y, dx, dy) -> action)` for zoom/pan. Sound,
randomness, and math: read_docs("creative").

The member name is the old constructor name in snake_case: `OnClickAt`
became `on_click_at`, `H1` became `h1`, `CmdNone` and `SubNone` became
`Cmd.none` and `Sub.none`, and `Type` became `type_` because `type` is a
keyword.

Types come from the modules too: `Html.T`, `Attr.T`, `Cmd.T`, `Sub.T`,
`Html.App`, `Attr.KeyEvent`, `Attr.MouseEvent`.

## Drawing (SVG)

`Html.node` covers SVG: tags like svg, circle, rect, line, path, polygon,
g, text render in the SVG namespace automatically. SVG attributes go
through `Attr.create`:

```
Html.node("svg",
[Attr.create("viewBox", "0 0 200 100"), Attr.width("200"), Attr.height("100")],
[
Html.node("circle", [Attr.create("cx", "50"), Attr.create("cy", "50"),
Attr.create("r", "20"), Attr.create("fill", "teal")], []),
Html.node("text", [Attr.create("x", "80"), Attr.create("y", "55")],
[Html.text("a node")])
])
```

Keep width/height equal to the viewBox size so `...At` handler coordinates
equal viewBox coordinates. For direct manipulation (drag, draw), attach the
`...At` handlers to the svg root. Hover moves fire only while the pointer
is over it, but a press on it captures the pointer: once a drag starts,
move/up keep firing even when the pointer leaves the element, so gestures
don't drop at the edge. The drag idiom, a held flag in the model:

```
type DragAction = Press + MoveTo(Int, Int) + Release in
let init = (100, 50, false) in
let update(m, a) =
let (x, y, held) = m in
case a
| Press => (x, y, true)
| MoveTo(nx, ny) => if held then (nx, ny, true) else m
| Release => (x, y, false)
end in
let view(m) =
let (x, y, held) = m in
Html.node("svg",
[Attr.create("viewBox", "0 0 200 100"), Attr.width("200"),
Attr.height("100"),
Attr.on_mouse_move_at(fun (nx, ny) -> MoveTo(nx, ny)),
Attr.on_mouse_up_at(fun p -> Release)],
[Html.node("circle",
[Attr.create("cx", string_of_int(x)), Attr.create("cy", string_of_int(y)),
Attr.create("r", "12"), Attr.create("fill", if held then "coral" else "teal"),
Attr.on_mouse_down_at(fun p -> Press)],
[])]) in
let subs(m) = Sub.none in
let noCmd(f) = fun (m, a) -> (f((m, a)), Cmd.none) in
^^html((init, noCmd(update), view, subs))
```

## Worked example: a counter

```
type Model = Int in
type Action = Int in
let init : Model = 0 in
let update(m: Model, a: Action) = m + a in
let view(m: Model) =
Html.div([], [
Html.button([Attr.on_click(-1)], [Html.text("-")]),
Html.int(m),
Html.button([Attr.on_click(1)], [Html.text("+")])
]) in
let subs(m: Model) = Sub.none in
let noCmd(f) = fun (m, a) -> (f((m, a)), Cmd.none) in
^^html((init, noCmd(update), view, subs))
```

## Workflow

- Build top-down, one definition per edit call: types, `init`, `update`,
  `view`, `subs`, then the final expression. Each edit must leave the
  program no more broken than before, so define things before using them.
- The app tuple must be the program's FINAL expression, wrapped in
  `^^html(...)` so the running app renders right there at the bottom of
  the program. Advise the user to scroll there to use it.
- You cannot click the app yourself. Verify `update` with tests
  (`test update((0, 1)) == 1 end`) and probes; the user interacts with
  the rendered app.
- Style with `Attr.style([...])` inline CSS; keep it modest.
|},
};

let livelits: pack = {
  name: "livelits",
  blurb: "define a custom embedded GUI (livelit) for a data type and use it at values of that type — read before creating in-program widgets",
  body: {|# Defining an embedded GUI (user-defined livelit)

A program can define its own widget for editing values of some type, then
use it wherever such values appear below the definition. A livelit is a
module bound to a livelit name (`^` prefix):

```
let ^pct = {
type Model = Int;
type Action = Int;
type Expansion = Int;
let init : Model = 50;
let update(m: Model, a: Action) = a;
let view(m: Model) =
Html.div([], [
Html.input([Attr.type_("range"), Attr.min("0"), Attr.max("100"),
Attr.value(string_of_int(m)),
Attr.on_input(fun s -> int_of_string(s))]),
Html.text(string_of_int(m))
]);
let expand(m: Model) = m
} in
^^livelit(^pct(25)) + ^^livelit(^pct(75))
```

All three type members are REQUIRED — they are the livelit's interface:

- `type Model` — the state a use carries, in its own argument
- `type Action` — what the view's handlers emit
- `type Expansion` — what a use MEANS to the program. This is the type
  clients see: `^pct(25)` has type Expansion no matter what `expand`
  returns, and statics checks each use's expansion against it (an
  inconsistency is reported on the use, as the livelit's fault).

and the four value members:

- `init : Model` — the model a fresh use starts with
- `update : (Model, Action) -> Model` — no commands, unlike apps
- `view : Model -> Html.T` — same Html/Attr vocabulary as apps
  (see read_docs("mvu")); handlers emit Actions
- `expand : Model -> Expansion` — `^pct(25)` evaluates to `expand(25)`

## Rules

- Each use `^name(model)` carries its own model in its own argument.
  Wrap uses in `^^livelit(...)` so the GUI shows; a bare `^name(model)`
  is still a valid expression, just without the widget.
- Helpers are ordinary extra module members; keep the definition
  self-contained (helpers inside the module).
- A definition missing any of the three types or four members is
  rejected, and the livelit name stays unbound. There is no tuple
  form: it has nowhere to declare the types.
- Optional member `let shape = ...` sets the widget's TEXT footprint
  (a LivelitShape): `Inline(w)` is one line, w columns; `Block(w, h)`
  is h lines with code flowing below; `Tab(w, h)` is h lines with code
  continuing on the TOP line beside the widget (good for compact
  square-ish widgets used inline). Default Inline(24). The view mounts
  centered in a content box just inside the chevron end-caps and is
  CLIPPED to it. Prefer views that FILL the box — Attr.width/Attr.height
  "100%",
  svg scaled via viewBox — so any reasonable footprint looks right.
  Views needing exact pixels (coordinate click math) should size
  snugly (a line is ~25px); overshoot clips, it never overlaps code.
- Models and Actions must be first-order data (ints, strings, tuples,
  constructors) — they live in the program text.
- Member access is ordinary syntax: `^pct.expand(25)` works anywhere.
- `Attr.create("data-hint", "drag me")` on a view element shows an instant
  tooltip on hover — advertise non-obvious gestures this way.
- When the user operates the widget, the argument is rewritten to the
  transition `^name.update(prev, action)` — this is expected; it
  evaluates in the program (probes inside update fire) and stays
  visible as the last interaction.
- Gestures: mouse-down and mouse-move actions preview the next model
  live WITHOUT rewriting the program; the gesture-ending event (mouse
  up, click, ...) commits once. So a drag is smooth and lands as a
  single undo step — but a model changed only by down/move never
  commits until some committing event fires: give every gesture a
  mouse-up handler. `Attr.on_input` streams work the same way: each input
  event previews and the release/blur commits once, so a slider scrub
  is also a single edit. Updates that return the model unchanged
  commit nothing, so a stray click can't pollute history.
- Editing a definition: tool paths descend into the module — update
  ONE member via update_definition on "^name/member" (types too:
  "^name/Model"), add members with insert_after/insert_before at a
  member path, remove with delete_binding_clause. Don't re-emit the
  whole module to change one member.

## When to reach for one

When a program contains values a user would rather manipulate directly
than type — colors, ranges, coordinates, enums — define a livelit for
that type and wrap the value uses. Prefer a builtin (`^^slider`,
`^^check`, ...) when one already fits.
|},
};

let creative: pack = {
  name: "creative",
  blurb: "sound, randomness, wheel zoom, and math for generative art, music toys, games, simulation — read alongside mvu for creative-computing programs",
  body: {|# Creative computing: sound, randomness, motion

These build on the app machinery from read_docs("mvu").

## Sound

`Cmd.play_tone(freq_hz, duration_ms)` plays a sine beep: return it from `update` to
a sine beep. Browsers unlock audio on the first user gesture, so give the
app a start button — tones fired before any click are silently dropped.

A four-note loop (`Sub.every` drives ticks only while running):

```
type SeqAction = Tick + Toggle in
let notes = [262., 330., 392., 523.] in
let update(m, a) =
let (i, on) = m in
case a
| Toggle => ((i, if on then false else true), Cmd.none)
| Tick =>
if on
then ((int_mod(i + 1, 4), on), Cmd.play_tone(nth(notes, i), 120.))
else (m, Cmd.none)
end in
let view(i, on) =
Html.div([], [
Html.button([Attr.on_click(Toggle)],
[Html.text(if on then "stop" else "play")]),
Html.int(i)]) in
let subs(i, on) =
if on then Sub.every(250., fun t -> Tick) else Sub.none in
^^html(((0, false), update, view, subs))
```

`Cmd.say(text)` speaks a string aloud.

## Randomness

Evaluation is deterministic — there is no random() function. Two idioms:

- `Cmd.random(fun f -> action)`: a fresh draw f in [0,1) arrives as
  an action, like Elm's Random.generate. Return it from `update` whenever
  you need a roll.
- For reproducible generative art, thread a seed through the model:

```
let next(s) = int_mod(s * 1103515245 + 12345, 2147483648) in
let unit_float(s) = float_of_int(s) /. 2147483648. in
unit_float(next(42))
```

## Zoom / pan

`Attr.on_wheel_at(fun (x, y, dx, dy) -> action)`: element-relative pointer
position (px ints) plus scroll deltas (floats). Default scrolling is
prevented on that element. Zoom about the pointer by scaling coordinates
around (x, y).

## Math

Already built in: sin, cos, tan, asin, acos, atan, atan2(y, x), sqrt, exp,
log, log10, floor, ceil, round, pi, `**.` (float power), float_of_int,
and to_fixed(f, digits) — fixed-precision string, use it for SVG
attributes and labels (string_of_float output is noisy).
|},
};

let all: list(pack) = [mvu, livelits, creative];

let lookup = (name: string): option(pack) =>
  List.find_opt(p => p.name == String.trim(name), all);

let topic_lines: string =
  all
  |> List.map(p => "- `" ++ p.name ++ "` — " ++ p.blurb)
  |> String.concat("\n");

let topic_names: list(string) = List.map(p => p.name, all);
