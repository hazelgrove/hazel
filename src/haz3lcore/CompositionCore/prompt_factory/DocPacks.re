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
`OnKeyDown(fun e -> action)`, `OnClickAt(fun (x, y) -> action)` (click
position relative to the element, in px).

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
let init : Model = 50;
let update = fun (m, a) : (Model, Action) -> a;
let view = fun m : Model ->
Div([], [
Input([Type("range"), Min("0"), Max("100"), Value(string_of_int(m)),
OnInput(fun s -> int_of_string(s))]),
Text(string_of_int(m))
]);
let expand = fun m : Model -> m
} in
^^livelit(^pct(25)) + ^^livelit(^pct(75))
```

- `init : Model` — the model a fresh use starts with
- `update : (Model, Action) -> Model` — no commands, unlike apps
- `view : Model -> HTML` — same HTML/handler vocabulary as apps
  (see read_docs("mvu")); handlers emit Actions
- `expand : Model -> T` — what a use MEANS to the program: `^pct(25)`
  evaluates to `expand(25)`

## Rules

- Each use `^name(model)` carries its own model in its own argument.
  Wrap uses in `^^livelit(...)` so the GUI shows; a bare `^name(model)`
  is still a valid expression, just without the widget.
- Helpers and type members are ordinary module members; keep the
  definition self-contained (helpers inside the module).
- Models and Actions must be first-order data (ints, strings, tuples,
  constructors) — they live in the program text.
- Member access is ordinary syntax: `^pct.expand(25)` works anywhere.
- When the user operates the widget, the argument is rewritten to the
  transition `^name.update(prev, action)` — this is expected; it
  normalizes on the next evaluation and keeps the last interaction
  visible to probes and the stepper.

## When to reach for one

When a program contains values a user would rather manipulate directly
than type — colors, ranges, coordinates, enums — define a livelit for
that type and wrap the value uses. Prefer a builtin (`^^slider`,
`^^check`, ...) when one already fits.
|},
};

let all: list(pack) = [mvu, livelits];

let lookup = (name: string): option(pack) =>
  List.find_opt(p => p.name == String.trim(name), all);

let topic_lines: string =
  all
  |> List.map(p => "- `" ++ p.name ++ "` — " ++ p.blurb)
  |> String.concat("\n");

let topic_names: list(string) = List.map(p => p.name, all);
