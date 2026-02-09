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

let update = fun (msg, model) ->
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

let view = fun model ->
  Div([], [
    Text(string_of_int(model.seconds)),
    Button([OnClick(Toggle)], [Text(if model.running then "Pause" else "Start")]),
    Button([OnClick(Reset)], [Text("Reset")])
  ])
in

let subs = fun model ->
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

Event data types:
- `KeyEvent = (key: String, code: String, ctrl: Bool, shift: Bool, alt: Bool, meta: Bool)`
- `MouseEvent = (clientX: Float, clientY: Float, button: Int, ctrl: Bool, shift: Bool, alt: Bool, meta: Bool)`

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
  Page.re                    # InitAppView, AppViewMsg, evaluate_direct, keyboard fix
  globals/Globals.re         # AppViewState type definition
  sidebar/AppViewPanel.re    # App detection, rendering entry point

src/haz3lcore/projectors/
  HazelDOM.re                # Html -> Virtual_dom renderer, subscription management
  SubManager.re              # Subscription setup/cleanup, handler dispatch
  CmdRunner.re               # Command execution (Focus, Delay, Log, etc.)

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

## Legacy: Self-Modifying Pattern

There is an older "self-modifying" pattern (documented in `hazel-html-implementation.md`) where the HTML tree IS the model — event handlers receive the current HTML and return new HTML directly, with no separate model or message type.

This pattern is still supported in the runtime but is not recommended for new programs. It exists as a separate code path, not as a variant of MVU.

### Where legacy code is entangled with MVU

The branching point is `update_fn: option(DHExp.t)` in `AppViewState` and `HazelDOM.t` — when `Some`, the runtime uses Elm-style dispatch; when `None`, it falls back to legacy behavior. This option is set cleanly at initialization time (`AppViewPanel.detect_app_kind`), not via runtime type inspection.

The branching happens in:

- **HazelDOM.re**: All 4 event handler functions (`on_`, `on_input`, `on_mouse`, `on_key`) check `mvu.update_fn` to decide whether handlers produce messages (Elm) or return new HTML (legacy).

- **SubManager.re**: `apply_handler` checks `ctx.update_fn`. In Elm mode, the handler takes just event data and produces a message. In legacy mode, the handler takes `(html, event_data)` and returns new HTML.

- **CmdRunner.re**: The `Delay` command checks `ctx.update_fn`. In Elm mode, the delayed value IS the message. In legacy mode, it's a `model -> model` function.

- **AppViewPanel.re**: `detect_app_kind` checks for 4-tuple (Elm) vs 3-tuple (legacy MVU) vs 2-tuple (legacy self-modifying). `render_legacy_app` handles the self-modifying path separately.

- **Page.re**: `AppViewMsg` only fires for Elm apps (guarded by `Option.is_some(state.update_fn)`). `SetAppViewModel` handles legacy direct model replacement.

- **Sidebar.re**: `app_inject` checks `state.update_fn` option to route to `AppViewMsg` (Elm) or `SetAppViewModel` (legacy).

- **HTMLProj.re**: The inline HTML projector always uses `update_fn: None` (legacy mode only).

### Path to further separation

To fully divorce the systems:
1. Move all legacy branching into a separate module (e.g., `LegacyHazelDOM`)
2. Have AppViewPanel route to completely different render paths based on app kind

Sources:
- [The Elm Architecture](https://guide.elm-lang.org/architecture/)
- [Beginning Elm - Commands](https://elmprogramming.com/commands.html)
