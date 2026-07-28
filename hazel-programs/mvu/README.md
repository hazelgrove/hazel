# MVU Example Programs

Ten complete MVU apps with inline tests, sharing a night-garden setting.
These are the source for the `MVU / ...` documentation slides in `src/mvu`.

## How to Use

Each of these ships as a documentation slide under **MVU / …**, with the app
already running and docked in the projector sidebar. That is the easiest way
to try one.

To work from the file instead:

1. Copy the code from any `.hz` file
2. Paste it into a Hazel scratchpad
3. For sidebar apps (4-tuple): open the App View sidebar panel
4. For inline HTML: right-click on the expression, select "Add HTML"

From the command line: `./hazel run <file>.hz` evaluates the app, and
`./hazel test <file>.hz` runs its inline test suite.

## Regenerating the slides

The slides in `src/mvu` are generated from these files. **Nothing detects a
stale encoding** — edit a `.hz` without regenerating and the slide keeps
shipping the old program. After any edit here:

```
./hazel-programs/mvu/regen-slides.sh
```

That strips indentation (Hazel computes it at layout time, so baked-in
indentation renders doubled), wraps the trailing tuple in `^^html(...)`, and
docks the projector. See the comment at the top of the script.

## Architecture

### Elm-style MVU (sidebar App View)

Apps are 4-tuples: `(init, update, view, subs)`

- `init` - initial state (any type)
- `update: (Model, Action) -> (Model, Cmd)` - apply an action
- `view: Model -> HTML` - render the model
- `subs: Model -> Sub` - subscriptions, given the model

All ten follow the same skeleton: a named `Model`, a named `Action`, and
`init`/`update`/`view`/`subs` annotated as above.

The model comes **first** in `update`. That matches `fold_left`'s `(acc, elem)`
and Hazel's own subject-first convention (`mapi(xs, f)`), so `update` can be
handed straight to `fold_left` — which is exactly what the tests here do.

Handlers produce **actions**, not new models:
- `OnClick(action)` - a bare action value
- `OnInput(fun str -> action)` - event data to action
- `OnKeyDown(fun e -> action)` - key event to action (labeled tuple: use `e.key`, `e.code`, `e.ctrl`, ...)

### Apps with no commands

Nine of these ten never issue a command. Rather than thread `CmdNone` through
every branch, their `update` returns a bare `Model` and is lifted at the tuple:

```
let noCmd(f: (Model, Action) -> Model): (Model, Action) -> (Model, Cmd) =
  fun (model, action) -> (f(model, action), CmdNone)
in

(init, noCmd(update), view, subs)
```

The runtime does tolerate an `update` that returns a bare model, but only via a
fallback that logs a warning — so don't rely on it. Keeping the tuple's shape
uniform is what lets the 4-tuple be type-checked later; `seed-catalog.hz` is the
one example that issues real commands and returns the pair directly.

Note the naming is deliberately asymmetric: `Action` is the name in Hazel
programs, while the OCaml implementation says `msg`, because `action` is already
taken there by both `HTMLProj`'s own action type and `Haz3lcore.Action`.

### Legacy self-modifying (inline HTML projector)

The inline HTML projector still supports the old self-modifying pattern where
handlers are `model -> model` functions. These run in "legacy mode" automatically.

## Examples

Roughly in order of size.

### mvu-counter.hz
The smallest possible app: `(init, update, view, subs)` with integer actions.

### timer.hz
Watering timer. Subscriptions: `Every` for periodic ticks, an `Action` sum type.

### keyboard-game.hz
Steer a firefly with the arrow keys. Global keyboard input via
`OnDocumentKeyDown` and `e.key`.

### todo-list.hz
Tonight's planting list. Text entry through a real `Form` with `OnSubmit`,
`Required`, and a `Disabled` submit button; list add/remove with `mapi`.

### crop-plotter.hz
Plant a 3x3 grove cell by cell, row, or column. Nested `mapi` over a list of
lists; `OnClick` to plant and `OnDoubleClick` to uproot.

### tictactoe.hz
Sprouts and shrooms race for three in a row. Sum types for cells, players, and
game status; win detection over a flat 9-element board.

### gameoflife.hz
A garden that spreads by Conway's rules. Clickable `Table` grid, plus `Every`
driving generations while running.

### nutrient-rotation.hz
Soil lab: plant, harvest, compost, and advance seasons while N/P/K move.
An optional field (`+None +Some(Crop)`) and CSS-width nutrient bars.

### harvest-streak.hz
Harvest ledger with quality tiers and streak bonuses. Record-list model
rendered as a `Table`.

### seed-catalog.hz
The widest API surface: `Select`/`Option`/`OnChange`, `OnMouseEnter`/`OnMouseLeave`,
`OnDoubleClick`, `Delay`, `CopyToClipboard`, `SubBatch`, `OnResize`, `Ol`, `A`,
`Classes`, `Checked`. The only example that issues real commands, so the only
one whose `update` returns `(Model, Cmd)` directly. Also the only one with a
day/night palette, via `Checked`.

## Types Reference

```
HTML = Div([attrs], [children]) | Button([attrs], [children]) | Text(str) | ...
Attr = Class(str) | Style([(key, value)]) | OnClick(action) | OnInput(String -> action)
     | OnSubmit(action) | Required(bool) | Disabled(bool) | ...
Cmd  = CmdNone | Focus(id) | Log(str) | Delay(ms, action) | CopyToClipboard(str) | ...
Sub  = SubNone | Every(ms, Float -> action) | OnDocumentKeyDown(KeyEvent -> action) | ...
KeyEvent   = (key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
MouseEvent = (x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
App  = (Model, (Model, Action) -> (Model, Cmd), Model -> HTML, Model -> Sub)
```
