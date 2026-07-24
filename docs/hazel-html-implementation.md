# HazelHtml Implementation Guide

This document describes the HazelHtml web app library as implemented on the `hazel-html` branch. It covers the architecture, types, and runtime components that enable building interactive web applications in Hazel.

The canonical program shape is the Elm-style MVU 4-tuple `(init, update, view, subs)`; see `docs/mvu.md` for the authoritative architecture description. The inline HTML projector runs the same dispatch model with a different commit target ("syntax-commit mode": update = apply, msgs are `Html -> Html` transforms spliced back into the document); see below.

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
│   Commit targets                                            │
│   - AppStore.re (sidebar): update(msg, model) -> new state  │
│   - HTMLProj.re (inline):  msg(model) -> SetSyntax splice   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      HazelDOM.re                            │
│  - Renders Html ADT to Virtual_dom nodes                    │
│  - One dispatch path: handlers produce msgs -> inject(msg)  │
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

In syntax-commit mode (inline projector) the same handlers describe HTML
transforms: simple-event handlers are `HTML -> HTML` (the handler IS the
msg), payload handlers are `(HTML, data) -> HTML` and are wrapped into the
msg `fun m -> handler((m, data))` at dispatch time. A msg may return
`(HTML, Cmd)` to also run a command.

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

## Syntax-Commit Mode (inline HTML projector)

The inline projector renders a bare HTML value; there is no separate model —
the projected expression IS the model. Dispatch is the same as MVU
(handlers produce msgs), with `update = apply`: a msg is an `Html -> Html`
transform, and committing evaluates `msg(model)` and splices the result
back into the document via `SetSyntax`.

Example:
```
Div([
  OnClick(fun html ->
    // html is the current tree, return the new tree
    Div([...], [Int(get_count(html) + 1)])
  )
], [Int(0)])
```

(Formerly a separate "legacy mode" with its own `((HTML, Cmd), HTML -> Sub)`
app shape and per-handler branching on `update_fn: option(...)`; that split
is gone — the commit target is the only difference, selected by
`HazelDOM.t.commit`.)

## Runtime Components

### HazelDOM.re

The core renderer that:
1. Converts Hazel HTML ADT to Virtual_dom nodes
2. Sets up event handlers: each produces a msg and hands it to `inject`
   (State commit: payload handlers are evaluated `handler(payload)`;
   Syntax commit: payload handlers are wrapped into
   `fun m -> handler((m, payload))`)

Key type:
```reason
type commit = State | Syntax; // where inject commits the msg

type t = {
  inject: DHExp.t => Ui_effect.t(unit),
  view_term: DHExp.t => Node.t,
  commit,
};
```

HazelDOM is render + dispatch only: subscriptions and state live in the
web-side AppStore (State) or the document itself (Syntax).

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

The inline projector (syntax-commit target):
1. Accepts bare HTML values (recognized head constructor, or `Br`)
2. On inject(msg), evaluates `msg(model)` and splices the result via
   `SetSyntax`; an `(Html, Cmd)` result also runs the Cmd via CmdRunner

## File Structure

```
src/language/builtins/
  BuiltinsADT.re          # All types: HTML, Attr, Cmd, Sub, App, KeyEvent, MouseEvent

src/haz3lcore/projectors/
  MvuShape.re             # Shared shape detection (wrappers, constructors, app kind)
  HazelDOM.re             # HTML -> Virtual_dom renderer + unified msg dispatch
  CmdRunner.re            # Cmd interpreter
  SubManager.re           # Subscription manager
  implementations/
    HTMLProj.re           # Inline projector (syntax-commit target)
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

## Known Limitations

1. **Subscription cleanup on removal**: When a projector is removed entirely (not just re-rendered), subscriptions may leak. Proper cleanup would require projector lifecycle hooks.

2. **Error boundaries**: Handler and msg evaluation errors are caught and logged to the console; nothing is dispatched or committed. Subscription callback errors are caught and logged. Infinite loops or stack overflows may still crash.

## Future Enhancements

When Hazel gains type parameters, the types could be parameterized over a message type (`Html(msg)`, `Attr(msg)`, `Cmd(msg)`, `Sub(msg)`) for statically-typed message dispatch.
