# HazelHtml Implementation Guide

This document describes the HazelHtml web app library as implemented on the `hazel-html` branch. It covers the architecture, types, and runtime components that enable building interactive web applications in Hazel.

## Overview

HazelHtml provides:
- A comprehensive set of HTML element and attribute types for Hazel
- A command system for side effects (focus, scroll, clipboard, delays)
- A subscription system for event sources (resize, keyboard, timers, animation frames)
- An App type for full application structure with init and subscriptions
- Integration with the HTML projector for live rendering

The library follows an Elm-inspired architecture adapted for Hazel's constraints (no type parameters yet).

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Hazel Program                            │
│                                                             │
│  Plain Html:  Div([Class("app")], [Text("Hello")])         │
│                                                             │
│  Or App:      ((init_html, init_cmd), subscriptions_fn)    │
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

  // Event handlers (self-modifying pattern)
  | OnClick(HTML -> HTML)
  | OnDoubleClick(HTML -> HTML)
  | OnMouseEnter(HTML -> HTML) | OnMouseLeave(HTML -> HTML)
  | OnMouseDown((HTML, MouseEvent) -> HTML)
  | OnMouseUp((HTML, MouseEvent) -> HTML)
  | OnMouseMove((HTML, MouseEvent) -> HTML)
  | OnKeyDown((HTML, KeyEvent) -> HTML)
  | OnKeyUp((HTML, KeyEvent) -> HTML)
  | OnKeyPress((HTML, KeyEvent) -> HTML)
  | OnInput((HTML, String) -> HTML)
  | OnChange((HTML, String) -> HTML)
  | OnFocus(HTML -> HTML) | OnBlur(HTML -> HTML)
  | OnSubmit(HTML -> HTML)

  // Generic fallbacks
  | Create(String, String)  // attr(name, value)
  | BoolAttr(String, Bool)
```

### Event Types

Product types for event data:

```
KeyEvent = (key, code, ctrl, shift, alt, meta)
         = (String, String, Bool, Bool, Bool, Bool)

MouseEvent = (clientX, clientY, button, ctrl, shift, alt, meta)
           = (Float, Float, Int, Bool, Bool, Bool, Bool)
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
  | Delay(Float, HTML -> HTML)        // Run transform after delay (ms)
  | Log(String)                       // Console log
```

### Sub Type

Recursive sum type for event sources:

```
Sub =
  | SubNone                                           // No subscription
  | SubBatch(List(Sub))                               // Multiple subscriptions
  | OnResize((HTML, Int, Int) -> HTML)                // Window resize
  | OnVisibilityChange((HTML, Bool) -> HTML)          // Tab visibility
  | OnDocumentKeyDown((HTML, KeyEvent) -> HTML)       // Global keydown
  | OnDocumentKeyUp((HTML, KeyEvent) -> HTML)         // Global keyup
  | Every(Float, (HTML, Float) -> HTML)               // Interval timer
  | AnimationFrame((HTML, Float) -> HTML)             // Animation frame
```

### App Type

Product type for full applications:

```
App = ((HTML, Cmd), HTML -> Sub)
    = (init, subscriptions)

where:
  init = (initial_html, startup_cmd)
  subscriptions = function from current HTML to Sub
```

## Self-Modifying Pattern

Since Hazel doesn't have type parameters yet, we use a "self-modifying" pattern where:
- The model IS the HTML tree
- Event handlers receive the current HTML and return new HTML
- Optionally, handlers can return `(HTML, Cmd)` to also trigger effects

Example:
```
let counter = Div([
  OnClick(fun html ->
    // html is the current state, return new state
    Div([...], [Int(get_count(html) + 1)])
  )
], [Int(0)])
```

When handlers return a tuple `(new_html, cmd)`, the command is executed after updating the model.

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
};
```

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

### With Commands
```
Button([
  OnClick(fun html ->
    (Div([], [Text("Focused!")]), Focus("my-input"))
  )
], [Text("Focus input")])
```

### Full App
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

2. **AnimationFrame cleanup**: AnimationFrame subscriptions recursively request frames and currently can't be stopped.

3. **No error boundaries**: Runtime errors in Hazel expression evaluation (e.g., type mismatches, failed pattern matches, infinite loops) are not caught. If an event handler or subscription callback fails to evaluate, it may crash or silently fail. Error boundaries would catch these exceptions and display a fallback UI like "Error: [message]" instead of breaking the entire projector.

## Future Enhancements

When Hazel gains type parameters, the types could be parameterized over a message type for full Elm-style architecture:
```
Html(msg)
Attr(msg)
Cmd(msg)
Sub(msg)
```

This would enable separate model/view/update rather than the self-modifying pattern.
