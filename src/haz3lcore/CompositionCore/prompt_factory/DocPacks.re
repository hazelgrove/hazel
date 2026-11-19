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
- `update : (Model, Action) -> (Model, Cmd)` — model first. If no update
  issues commands, write `update : (Model, Action) -> Model` and lift it
  with the `noCmd` helper shown below.
- `view : Model -> HTML` — event handlers emit Actions
- `subs : Model -> Sub` — `SubNone` unless you need timers/keyboard

## HTML and events

Elements are constructors: `Div([attrs], [children])`, `Button`, `Span`,
`Input([attrs])`, `Text(str)`, `Int(n)`, `Float(x)`, and
`Node(tag, attrs, children)` for anything else.
Attributes: `Class(str)`, `Style([(prop, value)])`, `Type`, `Value`,
`Min`, `Max`, `Step`, `Placeholder`, `Disabled`, ...
Handlers produce Actions: `OnClick(action)`, `OnInput(fun s -> action)`,
`OnKeyDown(fun e -> action)`. For pointer position relative to the element
(in px): `OnClickAt(fun (x, y) -> action)`, and likewise `OnMouseDownAt`,
`OnMouseMoveAt`, `OnMouseUpAt`; `OnWheelAt(fun (x, y, dx, dy) -> action)`
for zoom/pan. Sound, randomness, and math: read_docs("creative").

## Drawing (SVG)

`Node` covers SVG: tags like svg, circle, rect, line, path, polygon, g,
text render in the SVG namespace automatically. SVG attributes go through
`Create`:

```
Node("svg", [Create("viewBox", "0 0 200 100"), Width("200"), Height("100")], [
Node("circle", [Create("cx", "50"), Create("cy", "50"), Create("r", "20"),
Create("fill", "teal")], []),
Node("text", [Create("x", "80"), Create("y", "55")], [Text("a node")])
])
```

Keep width/height equal to the viewBox size so `...At` handler coordinates
equal viewBox coordinates. For direct manipulation (drag, draw), attach the
`...At` handlers to the svg root — they only fire while the pointer is over
it. The drag idiom, a held flag in the model:

```
type DragAction = Press + MoveTo(Int, Int) + Release in
let init = (100, 50, false) in
let update = fun (m, a) ->
let (x, y, held) = m in
case a
| Press => (x, y, true)
| MoveTo(nx, ny) => if held then (nx, ny, true) else m
| Release => (x, y, false)
end in
let view = fun m ->
let (x, y, held) = m in
Node("svg",
[Create("viewBox", "0 0 200 100"), Width("200"), Height("100"),
OnMouseMoveAt(fun (nx, ny) -> MoveTo(nx, ny)),
OnMouseUpAt(fun p -> Release)],
[Node("circle",
[Create("cx", string_of_int(x)), Create("cy", string_of_int(y)),
Create("r", "12"), Create("fill", if held then "coral" else "teal"),
OnMouseDownAt(fun p -> Press)],
[])]) in
let subs = fun m -> SubNone in
let noCmd = fun f -> fun (m, a) -> (f((m, a)), CmdNone) in
^^html((init, noCmd(update), view, subs))
```

## Worked example: a counter

```
type Model = Int in
type Action = Int in
let init : Model = 0 in
let update = fun (m, a) : (Model, Action) -> m + a in
let view = fun m : Model ->
Div([], [
Button([OnClick(-1)], [Text("-")]),
Int(m),
Button([OnClick(1)], [Text("+")])
]) in
let subs = fun m : Model -> SubNone in
let noCmd = fun f -> fun (m, a) -> (f((m, a)), CmdNone) in
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
- Style with `Style([...])` inline CSS; keep it modest.
|},
};

let creative: pack = {
  name: "creative",
  blurb: "sound, randomness, wheel zoom, and math for generative art, music toys, games, simulation — read alongside mvu for creative-computing programs",
  body: {|# Creative computing: sound, randomness, motion

These build on the app machinery from read_docs("mvu").

## Sound

`PlayTone(freq_hz, duration_ms)` is a Cmd: return it from `update` to play
a sine beep. Browsers unlock audio on the first user gesture, so give the
app a start button — tones fired before any click are silently dropped.

A four-note loop (`Every` drives ticks only while running):

```
type SeqAction = Tick + Toggle in
let notes = [262., 330., 392., 523.] in
let update = fun (m, a) ->
let (i, on) = m in
case a
| Toggle => ((i, if on then false else true), CmdNone)
| Tick =>
if on
then ((int_mod(i + 1, 4), on), PlayTone(nth(notes, i), 120.))
else (m, CmdNone)
end in
let view = fun (i, on) ->
Div([], [
Button([OnClick(Toggle)], [Text(if on then "stop" else "play")]),
Int(i)]) in
let subs = fun (i, on) ->
if on then Every(250., fun t -> Tick) else SubNone in
^^html(((0, false), update, view, subs))
```

`Say(text)` is a Cmd that speaks a string aloud.

## Randomness

Evaluation is deterministic — there is no random() function. Two idioms:

- `Random(fun f -> action)` is a Cmd: a fresh draw f in [0,1) arrives as
  an action, like Elm's Random.generate. Return it from `update` whenever
  you need a roll.
- For reproducible generative art, thread a seed through the model:

```
let next = fun s -> int_mod(s * 1103515245 + 12345, 2147483648) in
let unit_float = fun s -> float_of_int(s) /. 2147483648. in
unit_float(next(42))
```

## Zoom / pan

`OnWheelAt(fun (x, y, dx, dy) -> action)`: element-relative pointer
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

let all: list(pack) = [mvu, creative];

let lookup = (name: string): option(pack) =>
  List.find_opt(p => p.name == String.trim(name), all);

let topic_lines: string =
  all
  |> List.map(p => "- `" ++ p.name ++ "` — " ++ p.blurb)
  |> String.concat("\n");

let topic_names: list(string) = List.map(p => p.name, all);
