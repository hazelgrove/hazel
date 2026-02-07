# HazelHtml Example Programs

These example programs demonstrate the HazelHtml web app library features.

## How to Use

1. Copy the code from any `.hz` file
2. Paste it into a Hazel scratchpad
3. For sidebar apps (4-tuple): open the App View sidebar panel
4. For inline HTML: right-click on the expression, select "Add HTML"

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
- `OnKeyDown(fun key_event -> msg)` - key event to message

### Legacy self-modifying (inline HTML projector)

The inline HTML projector still supports the old self-modifying pattern where
handlers are `model -> model` functions. These run in "legacy mode" automatically.

## Examples

### mvu-counter.hz
Elm-style counter: `(0, update, view, subs)` with integer messages.

### timer.hz
Timer with subscriptions: uses `Every` for periodic ticks, tuple messages.

### full-app.hz
Tabs, forms, commands: demonstrates `OnInput`, `Cmd`, tuple message dispatch.

### counter.hz
Legacy self-modifying counter for the inline HTML projector.

### todo-list.hz
Legacy self-modifying todo list for the inline HTML projector.

## Types Reference

```
HTML = Div([attrs], [children]) | Button([attrs], [children]) | Text(str) | ...
Attr = Class(str) | Style([(key, value)]) | OnClick(msg) | OnInput(String -> msg) | ...
Cmd  = CmdNone | Focus(id) | Log(msg) | Delay(ms, msg) | ...
Sub  = SubNone | Every(ms, Float -> msg) | OnDocumentKeyDown(KeyEvent -> msg) | ...
App  = (init_model, (msg, model) -> model, model -> HTML, model -> Sub)
```
