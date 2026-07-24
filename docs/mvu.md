# HazelHtml MVU Architecture

This document describes the Model-View-Update (MVU) architecture for HazelHtml apps on the `hazel-html` branch.

For type definitions (HTML, Attr, Cmd, Sub constructors), see `hazel-html-implementation.md`.

## Program Structure

An MVU program is a **4-tuple**:

```
(init, update, view, subs)
```

- `init`: The initial model (any Hazel value)
- `update`: `(Msg, Model) -> (Model, Cmd)` — processes a message, returns new model and command
- `view`: `Model -> Html` — renders model to HTML
- `subs`: `Model -> Sub` — returns active subscriptions based on current model

### Example: Timer

```
type Msg = + Toggle + Reset + Tick in

let init = (seconds=0, running=false) in

let update(msg, model) =
  case msg
  | Toggle =>
      ((seconds=model.seconds, running=if model.running then false else true), CmdNone)
  | Reset =>
      ((seconds=0, running=false), CmdNone)
  | Tick =>
      if model.running then
        ((seconds=model.seconds + 1, running=model.running), CmdNone)
      else (model, CmdNone)
  end
in

let view(model) =
  Div([], [
    Text(string_of_int(model.seconds)),
    Button([OnClick(Toggle)], [Text(if model.running then "Pause" else "Start")]),
    Button([OnClick(Reset)], [Text("Reset")])
  ])
in

let subs(model) =
  if model.running then Every(1000.0, fun _timestamp -> Tick)
  else SubNone
in

(init, update, view, subs)
```

## Event Handlers

Event handlers produce **messages** that get routed through `update`:

| Event | Handler type | Example |
|-------|-------------|---------|
| OnClick | `Msg` (value) | `OnClick(Toggle)` |
| OnInput | `String -> Msg` | `OnInput(fun s -> UpdateInput(s))` |
| OnChange | `String -> Msg` | `OnChange(fun s -> SetValue(s))` |
| OnKeyDown | `KeyEvent -> Msg` | `OnKeyDown(fun ke -> HandleKey(ke))` |
| OnKeyUp | `KeyEvent -> Msg` | |
| OnMouseDown | `MouseEvent -> Msg` | `OnMouseDown(fun me -> Click(me))` |
| OnMouseUp | `MouseEvent -> Msg` | |
| OnMouseMove | `MouseEvent -> Msg` | |
| OnFocus | `Msg` (value) | `OnFocus(Focused)` |
| OnBlur | `Msg` (value) | `OnBlur(Blurred)` |
| OnSubmit | `Msg` (value) | `OnSubmit(Submit)` |

Event data types (labeled tuples — access fields with dot projection, e.g. `fun e -> if e.key == "ArrowUp" then MoveUp else NoOp`):
- `KeyEvent = (key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`
- `MouseEvent = (x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`

## Commands

Commands are side effects returned from `update` alongside the new model:

| Command | Description |
|---------|-------------|
| `CmdNone` | No effect |
| `CmdBatch([cmd1, cmd2, ...])` | Run multiple commands |
| `Focus("element-id")` | Focus a DOM element by ID |
| `Blur("element-id")` | Blur a DOM element |
| `ScrollIntoView("element-id")` | Scroll element into view |
| `ScrollTo("element-id", x, y)` | Scroll to position |
| `CopyToClipboard("text")` | Copy text to clipboard |
| `Delay(500.0, MyMsg)` | Dispatch a message after a delay (ms) |
| `Log("debug info")` | Print to browser console |

## Subscriptions

Subscriptions connect external event sources to the message loop. The `subs` function is re-evaluated whenever the model changes; returning `SubNone` for a previously active subscription tears it down.

| Subscription | Handler type | Description |
|-------------|-------------|-------------|
| `SubNone` | | No subscription |
| `SubBatch([sub1, sub2])` | | Multiple subscriptions |
| `Every(1000.0, handler)` | `Float -> Msg` | Interval timer (ms). Handler receives timestamp. |
| `AnimationFrame(handler)` | `Float -> Msg` | Called every animation frame. Handler receives timestamp. |
| `OnResize(handler)` | `(Int, Int) -> Msg` | Window resize. Handler receives (width, height). |
| `OnVisibilityChange(handler)` | `Bool -> Msg` | Tab visibility. Handler receives `true` when visible. |
| `OnDocumentKeyDown(handler)` | `KeyEvent -> Msg` | Global keydown (capture phase). |
| `OnDocumentKeyUp(handler)` | `KeyEvent -> Msg` | Global keyup (capture phase). |

## Inline Tests

Programs can include tests alongside the 4-tuple. Tests run against the pure `update` function without needing the runtime:

```
hint "toggle starts timer"
test
  let (m, _cmd) = update(Toggle, init) in
  m.running == true
end;

hint "reset stops and zeroes"
test
  let (m, _cmd) = update(Reset, (seconds=42, running=true)) in
  m.seconds == 0 && m.running == false
end;

(init, update, view, subs)
```

Run from the CLI: `node _build/default/src/CLI/cli.bc.js test myprogram.hz`

## Runtime Architecture

```
                    Hazel Editor
                         |
                    [evaluation]
                         |
                         v
              (init, update, view, subs)
                         |
              AppViewPanel.detect_app_kind
                         |
                         v
                    InitAppView         <-- Page.re action
                         |
          +--------------+--------------+
          |              |              |
          v              v              v
    view(model)    subs(model)       model
          |              |              |
          v              v              |
     HazelDOM       SubManager         |
    render_elem      subscribe         |
     -> DOM          -> listeners      |
          |              |             |
          |  [event fires]             |
          v              |             |
     inject(msg)  -------+             |
          |                            |
          v                            |
     AppViewMsg(msg)                   |
          |                            |
    update(msg, model) -> (new_model, cmd)
          |                            |
    +-----+-----+                     |
    |           |                      |
    v           v                      |
  CmdRunner  new model ---------------+
  run(cmd)   -> re-render loop
```

### Dispatch Cycle

1. An event fires (user click, subscription timer, etc.)
2. The event handler produces a `Msg` value
3. `inject(msg)` dispatches `AppViewMsg(msg)` to Page.re
4. Page.re calls `update(msg, current_model)` via `evaluate_direct`
5. The result `(new_model, cmd)` is extracted
6. If `cmd` is not `CmdNone`, CmdRunner executes it
7. `view(new_model)` and `subs(new_model)` are re-evaluated
8. HazelDOM re-renders; SubManager tears down old subscriptions and creates new ones

### Key Implementation Details

**evaluate_direct**: All runtime function applications (update, view, subs, event handlers, subscription callbacks) use direct evaluation — just `Evaluator.evaluate`, skipping `Statics.mk` and `Elaborator.elaborate`. This is necessary because handler functions are `Closure` nodes from a previous evaluation, and the elaborator doesn't understand runtime-only constructs like `Closure(env, body)`.

**Wrapper stripping**: The evaluator wraps values in `Asc(term, type)`, `Closure(env, body)`, and `Parens(inner)`. These must be stripped before pattern-matching on constructors. `strip_wrappers` and `of_constructor` handle this throughout HazelDOM and SubManager.

**Subscription lifecycle**: Subscriptions are fully torn down and recreated on every model change. This is simple but means timers restart their countdown each tick (still fire at approximately the right interval) and AnimationFrame loops are re-requested each frame.

**Cmd extraction**: The runtime always expects `update` to return a `(Model, Cmd)` tuple. CmdRunner handles `CmdNone` as a no-op. If the result is not a tuple (e.g. during mid-edit), the runtime falls back to treating the whole result as the model with `CmdNone`.

## Keyboard Focus

Hazel's editor captures all keyboard events on the `#page` element with `Stop_propagation` and `Prevent_default`. This conflicts with form inputs in MVU apps.

**Form inputs**: Page.re's key handler checks if the event target is an `INPUT`, `TEXTAREA`, or `SELECT` element. If so, it yields, letting the browser handle the event normally.

**Global keyboard subscriptions**: `OnDocumentKeyDown` uses capture-phase listeners on `document`, so they fire before Hazel's page-level handlers. However, they don't call `stopPropagation`, so events also reach the editor. Keyboard-game-style apps will trigger both game logic and editor navigation — this needs a more sophisticated focus management solution.

## Divergences from Elm

Our MVU is inspired by [The Elm Architecture](https://guide.elm-lang.org/architecture/) but diverges in several ways:

### Update return type

In Elm, there are two entry points:
- `Browser.sandbox`: `update : Msg -> Model -> Model` (no commands possible)
- `Browser.element`: `update : Msg -> Model -> (Model, Cmd Msg)` (always returns tuple)

Our `update` always returns `(Model, Cmd)`, matching Elm's `Browser.element`. Use `CmdNone` when no command is needed. The runtime always extracts a 2-tuple from the update result.

### No type parameters

Elm's types are parameterized: `Html Msg`, `Cmd Msg`, `Sub Msg`. This provides compile-time guarantees that event handlers and subscriptions produce the right message type.

Hazel doesn't have type parameters yet, so our HTML, Cmd, and Sub types are unparameterized. The runtime dispatches messages by matching constructor names as strings. This means type errors in message routing are caught at runtime, not compile time.

### Subscription handler signatures

In Elm, subscription handlers match the update function's `Msg` type. In our system, subscription handlers are functions that take event data and return a `Msg`:

- `Every(1000.0, fun timestamp -> Tick)` — the handler receives a Float, returns a Msg
- `OnDocumentKeyDown(fun ke -> HandleKey(ke))` — receives KeyEvent, returns Msg

This matches Elm's pattern.

### No Cmd.map / Sub.map

Elm provides `Cmd.map` and `Sub.map` for composing nested applications (child components with their own message types). We don't have this, which limits component composition.

### No ports or HTTP

Elm uses `Cmd` for HTTP requests and `port` for JavaScript interop. Our command system is limited to DOM operations (focus, scroll, clipboard) and scheduling (`Delay`). There's no built-in way to make HTTP requests or call arbitrary JavaScript.

## Known Limitations

1. **Subscription teardown on every render**: All subscriptions are torn down and recreated whenever the model changes. Timers and animation frames still work but with slightly imprecise timing.

2. **No focus isolation**: MVU apps share keyboard event space with the Hazel editor. Form inputs work via the INPUT/TEXTAREA check, but global keyboard subscriptions conflict with editor navigation.

3. **Stale state on scratchpad switch**: Switching scratchpads can show broken HTML in the App View panel until Reset is pressed. The sidebar caches state across editor switches.

4. **No structural hot-reload**: Code changes attempt to preserve the current model, but fall back to re-initialization if the model structure changed.

5. **Performance**: Each event handler and subscription callback runs a full evaluation pass. Complex models may cause lag.

6. **Constructor name matching**: The runtime identifies HTML elements, attributes, commands, and subscriptions by matching constructor name strings. Typos in constructor names fail silently (rendered as fallback text).

## File Map

```
src/web/app/
  Page.re                    # InitAppView, AppViewMsg, keyboard fix
  globals/Globals.re         # App-view actions
  globals/AppStore.re        # Id-keyed MVU state store (the state-commit target)
  sidebar/AppViewPanel.re    # App detection, rendering entry point

src/haz3lcore/projectors/
  MvuShape.re                # Shared shape detection (wrappers, constructors, app kind)
  HazelDOM.re                # Html -> Virtual_dom renderer + unified msg dispatch
  SubManager.re              # Subscription setup/cleanup, handler dispatch
  CmdRunner.re               # Command execution (Focus, Delay, Log, etc.)
  implementations/HTMLProj.re # Inline projector (the syntax-commit target)

src/language/builtins/
  BuiltinsADT.re            # Type definitions: Html, Attr, Cmd, Sub, KeyEvent, MouseEvent

src/CLI/
  Cli.re                    # CLI: analyze, test commands
  Run.re                    # evaluate_with_tests

hazel-programs/html-examples/
  mvu-counter.hz            # Minimal counter (Int model, no subs)
  timer.hz                  # Timer with Every subscription
  animation.hz              # Bouncing ball with AnimationFrame
  keyboard-game.hz          # Arrow key movement with OnDocumentKeyDown
  todo-list.hz              # Todo list with form input
  full-app.hz               # Multi-tab app with commands
  emojipaint.hz             # Emoji grid painting
  tictactoe.hz              # Tic-tac-toe game
```

## One Dispatch Model, Two Commit Targets

Handlers always produce a **msg**; dispatch is uniform (HazelDOM's handler builders hand the msg to `inject` and stop propagation). What differs per surface is the commit strategy — `HazelDOM.t.commit`:

- **State commit** (sidebar App View, `commit: State`): the web-side AppStore evaluates `update(msg, model)` and stores the new model. This is the Elm-style path described throughout this document.

- **Syntax commit** (inline HTML projector, `commit: Syntax`): the projected expression IS the model, and a msg is an `Html -> Html` transform. Committing evaluates `msg(model)` and splices the result back into the document via `SetSyntax`. In other words: self-modifying = Elm with `update = apply` and a different commit target.

Details of the syntax-commit surface (HTMLProj.re):

- Simple-event handlers (`OnClick`, ...) ARE the msg — an `Html -> Html` function.
- Payload-event handlers (`OnInput`, `OnKeyDown`, `OnMouseDown`, ...) keep the shape `(Html, payload) -> Html`; at dispatch time HazelDOM wraps them into the transform msg `fun m -> handler((m, payload))` (`HazelDOM.payload_transform`).
- A msg may also produce `(Html, Cmd)`: the Html is spliced and the Cmd runs afterward.
- `Delay(ms, msg)` works in both modes uniformly, since its payload is always a msg (in syntax-commit mode: a transform that re-enters the same inject).

(Formerly the self-modifying pattern was a separate "legacy mode" selected by `update_fn: option(...)` branches throughout HazelDOM/CmdRunner/SubManager, with 2- and 3-tuple app shapes and a `SetAppViewModel` action; that split is gone.)

Sources:
- [The Elm Architecture](https://guide.elm-lang.org/architecture/)
- [Beginning Elm - Commands](https://elmprogramming.com/commands.html)
