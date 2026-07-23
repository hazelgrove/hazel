# HazelHtml Implementation Guide

This document describes the HazelHtml web app library as implemented on the `hazel-html` branch. It covers the architecture, types, and runtime components that enable building interactive web applications in Hazel.

The canonical program shape is the Elm-style MVU 4-tuple `(init, update, view, subs)`; see `docs/mvu.md` for the authoritative architecture description. An older "self-modifying" mode (the HTML tree is the model, handlers are `HTML -> HTML`) is still supported as a legacy alternative and is marked as such below.

## Overview

HazelHtml provides:
- A comprehensive set of HTML element and attribute types for Hazel
- A command system for side effects (focus, scroll, clipboard, delays)
- A subscription system for event sources (resize, keyboard, timers, animation frames)
- An Elm-style App shape: `(init, update, view, subs)`
- Integration with the HTML projector for live rendering

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Hazel Program                            │
│                                                             │
│  Plain Html:  Div([Class("app")], [Text("Hello")])         │
│                                                             │
│  Or MVU App:  (init, update, view, subs)                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   HTMLProj.re (Projector)                   │
│  - Detects App vs plain Html                                │
│  - Evaluates subscriptions function for App type            │
│  - Creates HazelDOM.t context                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      HazelDOM.re                            │
│  - Renders Html ADT to Virtual_dom nodes                    │
│  - Manages subscription lifecycle (setup/cleanup)           │
│  - Event handlers execute Cmd when result is (Html, Cmd)    │
└─────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
       CmdRunner.re    SubManager.re    Virtual_dom
       (executes Cmd)  (manages Sub)    (renders DOM)
```

## Types

All types are defined in `src/language/builtins/BuiltinsADT.re`.

### HTML Type

Recursive sum type with ~40 element constructors:

```
HTML =
  // Text content
  | Text(String)
  | Bool(Bool) | Int(Int) | Float(Float)  // Convenience renderers

  // Structural
  | Div(attrs, children) | Span(...) | P(...) | Pre(...) | Code(...) | Blockquote(...)

  // Headings
  | H1(...) | H2(...) | H3(...) | H4(...) | H5(...) | H6(...)

  // Lists
  | Ul(...) | Ol(...) | Li(...)

  // Forms
  | Form(...) | Label(...) | Input(attrs) | TextArea(attrs, content)
  | Button(...) | Select(...) | Option(attrs, label)
  | Checkbox(attrs) | Radio(attrs) | Range(attrs)  // Legacy variants

  // Links/Media
  | A(...) | Img(attrs)

  // Tables
  | Table(...) | Thead(...) | Tbody(...) | Tr(...) | Th(...) | Td(...)

  // Semantic
  | Header(...) | Footer(...) | Nav(...) | Main(...) | Section(...) | Article(...) | Aside(...)

  // Utility
  | Br | Hr(attrs)

  // Escape hatch
  | Node(tagName, attrs, children)  // For any HTML element
```

Where `attrs = List(Attr)` and `children = List(HTML)`.

### Attr Type

Sum type with ~45 attribute/event constructors:

```
Attr =
  // Identity
  | Id(String) | Class(String) | Classes(List(String))

  // Properties
  | Disabled(Bool) | Placeholder(String) | Value(String) | Checked(Bool)
  | Selected(Bool) | ReadOnly(Bool) | Required(Bool) | AutoFocus(Bool)

  // Links/Media
  | Href(String) | Src(String) | Alt(String) | Title(String) | Target(String)

  // Input specifics
  | Type(String) | Name(String) | Min(String) | Max(String) | Step(String)
  | MaxLength(Int) | Pattern(String)

  // Layout
  | Width(String) | Height(String) | ColSpan(Int) | RowSpan(Int)

  // Styling
  | Style(List((String, String)))  // CSS as key-value pairs

  // Data attributes
  | Data(String, String)  // data-{name}={value}

  // Event handlers (Elm mode: handlers produce messages)
  | OnClick(Msg) | OnDoubleClick(Msg)
  | OnMouseEnter(Msg) | OnMouseLeave(Msg)
  | OnFocus(Msg) | OnBlur(Msg)
  | OnSubmit(Msg)                        // prevents default form submission
  | OnMouseDown(MouseEvent -> Msg)
  | OnMouseUp(MouseEvent -> Msg)
  | OnMouseMove(MouseEvent -> Msg)
  | OnKeyDown(KeyEvent -> Msg)
  | OnKeyUp(KeyEvent -> Msg)
  | OnKeyPress(KeyEvent -> Msg)
  | OnInput(String -> Msg)
  | OnChange(String -> Msg)

  // Generic fallbacks
  | Create(String, String)  // attr(name, value)
  | BoolAttr(String, Bool)
```

In legacy self-modifying mode the same handlers instead transform the HTML
tree directly: simple events are `HTML -> HTML`, data-carrying events are
`(HTML, data) -> HTML`, and handlers may return `(HTML, Cmd)` to also run a
command.

### Event Types

Labeled product types for event data (access fields with dot projection,
e.g. `fun e -> if e.key == "ArrowUp" then MoveUp else NoOp`):

```
KeyEvent = (key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)

MouseEvent = (x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)
```

### Cmd Type

Recursive sum type for side effects:

```
Cmd =
  | CmdNone                           // No effect
  | CmdBatch(List(Cmd))               // Multiple effects
  | Focus(String)                     // Focus element by ID
  | Blur(String)                      // Blur element by ID
  | ScrollIntoView(String)            // Scroll element into view
  | ScrollTo(String, Float, Float)    // Scroll element to (x, y)
  | CopyToClipboard(String)           // Copy text to clipboard
  | Delay(Float, Msg)                 // Dispatch msg after delay (ms)
                                      // (legacy mode: HTML -> HTML transform)
  | Log(String)                       // Console log
```

### Sub Type

Recursive sum type for event sources:

```
Sub =
  | SubNone                                 // No subscription
  | SubBatch(List(Sub))                     // Multiple subscriptions
  | OnResize((Int, Int) -> Msg)             // Window resize
  | OnVisibilityChange(Bool -> Msg)         // Tab visibility
  | OnDocumentKeyDown(KeyEvent -> Msg)      // Global keydown
  | OnDocumentKeyUp(KeyEvent -> Msg)        // Global keyup
  | Every(Float, Float -> Msg)              // Interval timer
  | AnimationFrame(Float -> Msg)            // Animation frame
```

In legacy mode, subscription handlers take the current model as an extra
first argument and return new HTML, e.g. `Every(Float, (HTML, Float) -> HTML)`.

### App Type

An MVU app is a 4-tuple (see `docs/mvu.md` for the full architecture):

```
App = (init, update, view, subs)

where:
  init   = initial model (any Hazel value)
  update = (Msg, Model) -> (Model, Cmd)
  view   = Model -> HTML
  subs   = Model -> Sub
```

## Legacy Self-Modifying Mode

Superseded by the MVU 4-tuple above, but still supported. The app shape is
`((HTML, Cmd), HTML -> Sub)`: the model IS the HTML tree, event handlers
receive the current HTML and return new HTML (optionally `(HTML, Cmd)` to
also trigger effects).

Example:
```
let counter = Div([
  OnClick(fun html ->
    // html is the current state, return new state
    Div([...], [Int(get_count(html) + 1)])
  )
], [Int(0)])
```

The runtime selects the mode via `HazelDOM.update_fn`: `Some(update)` means
Elm mode (handlers produce messages), `None` means legacy mode (handlers
transform the HTML tree).

## Runtime Components

### HazelDOM.re

The core renderer that:
1. Converts Hazel HTML ADT to Virtual_dom nodes
2. Sets up event handlers that evaluate Hazel functions
3. Detects `(HTML, Cmd)` returns and executes commands via CmdRunner
4. Manages subscription lifecycle via global registry

Key type:
```reason
type t = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
  view_term: DHExp.t => Node.t,
  projector_id: option(Id.t),
  subscriptions: option(DHExp.t),
  // Some(update) = Elm mode (inject receives msgs);
  // None = legacy mode (inject receives new HTML models)
  update_fn: option(DHExp.t),
};
```

Shared shape-detection helpers (wrapper stripping, constructor extraction,
app detection, the HTML constructor name set) live in `MvuShape.re`.

### CmdRunner.re

Interprets Cmd values as UI effects:
- `Focus`/`Blur` - calls DOM element methods
- `ScrollIntoView`/`ScrollTo` - manipulates scroll position
- `CopyToClipboard` - uses clipboard API
- `Delay` - schedules callback with setTimeout
- `Log` - writes to console

### SubManager.re

Manages subscription lifecycle:
- `subscribe()` - sets up event listeners, returns cleanup handles
- `cleanup()` - removes all listeners for a subscription set
- Handles window events, document keyboard, intervals, animation frames

### HTMLProj.re

The projector wrapper that:
1. Detects if model is App type vs plain HTML
2. For App type, extracts HTML and evaluates subscriptions function
3. Creates HazelDOM.t context with subscription info
4. Passes projector ID for subscription tracking

## File Structure

```
src/language/builtins/
  BuiltinsADT.re          # All types: HTML, Attr, Cmd, Sub, App, KeyEvent, MouseEvent

src/haz3lcore/projectors/
  MvuShape.re             # Shared shape detection (wrappers, constructors, app kinds)
  HazelDOM.re             # HTML -> Virtual_dom renderer with Cmd/Sub integration
  CmdRunner.re            # Cmd interpreter
  SubManager.re           # Subscription manager
  implementations/
    HTMLProj.re           # Projector with App detection
```

## Usage Examples

### Plain HTML
```
Div([Class("greeting")], [
  H1([], [Text("Hello World")]),
  Button([OnClick(fun _ -> Text("Clicked!"))], [Text("Click me")])
])
```

### MVU App (canonical)
```
type Msg = + Inc in
let update(msg, model) =
  case msg | Inc => (model + 1, CmdNone) end in
let view(model) =
  Div([], [Int(model), Button([OnClick(Inc)], [Text("+")])]) in
let subs(model) = SubNone in
(0, update, view, subs)
```

See `docs/mvu.md` for a fuller example with commands and subscriptions.

### Legacy Self-Modifying App
```
let app = (
  // init: (HTML, Cmd)
  (Div([Id("counter")], [Int(0)]), CmdNone),

  // subscriptions: HTML -> Sub
  fun html -> Every(1000.0, fun (h, timestamp) ->
    Div([Id("counter")], [Int(get_count(h) + 1)])
  )
)
```

## Known Limitations

1. **Subscription cleanup on removal**: When a projector is removed entirely (not just re-rendered), subscriptions may leak. Proper cleanup would require projector lifecycle hooks.

2. **Error boundaries**: Handler evaluation errors are caught. In Elm mode no message is dispatched and the error is logged to the console; in legacy mode an inline error UI (red-bordered div) replaces the model. Subscription callback errors are caught and logged to console. Infinite loops or stack overflows may still crash.

## Future Enhancements

When Hazel gains type parameters, the types could be parameterized over a message type (`Html(msg)`, `Attr(msg)`, `Cmd(msg)`, `Sub(msg)`) for statically-typed message dispatch.
