# HazelHtml Example Programs

These example programs demonstrate the HazelHtml web app library features.
They share a night-garden setting, and every one is a complete MVU app with
inline tests.

## How to Use

1. Copy the code from any `.hz` file
2. Paste it into a Hazel scratchpad
3. For sidebar apps (4-tuple): open the App View sidebar panel
4. For inline HTML: right-click on the expression, select "Add HTML"

From the command line: `./hazel run <file>.hz` evaluates the app, and
`./hazel test <file>.hz` runs its inline test suite.

## Architecture

### Elm-style MVU (sidebar App View)

Apps are 4-tuples: `(init_model, update, view, subs)`

- `init_model` - initial state (any type)
- `update: (msg, model) -> model` - handle messages (can also return `(model, Cmd)`)
- `view: model -> HTML` - render model as HTML
- `subs: model -> Sub` - subscriptions based on model

Handlers produce **messages**, not new models:
- `OnClick(msg_value)` - bare message value
- `OnInput(fun str -> msg)` - event data to message
- `OnKeyDown(fun e -> msg)` - key event to message (labeled tuple: use `e.key`, `e.code`, `e.ctrl`, ...)

### Legacy self-modifying (inline HTML projector)

The inline HTML projector still supports the old self-modifying pattern where
handlers are `model -> model` functions. These run in "legacy mode" automatically.

## Examples

Roughly in order of size.

### mvu-counter.hz
The smallest possible app: `(0, update, view, subs)` with integer messages.

### timer.hz
Watering timer. Subscriptions: `Every` for periodic ticks, a `Msg` sum type.

### keyboard-game.hz
Steer a firefly with the arrow keys. Global keyboard input via
`OnDocumentKeyDown` and `e.key`.

### todo-list.hz
Tonight's planting list. Text entry through a real `Form` with `OnSubmit`,
`Required`, and a `Disabled` submit button; list add/remove with `mapi`.

### full-app.hz
Garden almanac with tabs and a notes form. `OnInput`, `TextArea`, and
`CmdBatch([Log(...), Focus(...)])`.

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
`Classes`, `Checked`.

## Types Reference

```
HTML = Div([attrs], [children]) | Button([attrs], [children]) | Text(str) | ...
Attr = Class(str) | Style([(key, value)]) | OnClick(msg) | OnInput(String -> msg)
     | OnSubmit(msg) | Required(bool) | Disabled(bool) | ...
Cmd  = CmdNone | Focus(id) | Log(msg) | Delay(ms, msg) | CopyToClipboard(str) | ...
Sub  = SubNone | Every(ms, Float -> msg) | OnDocumentKeyDown(KeyEvent -> msg) | ...
KeyEvent   = (key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
MouseEvent = (x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
App  = (init_model, (msg, model) -> model, model -> HTML, model -> Sub)
```
