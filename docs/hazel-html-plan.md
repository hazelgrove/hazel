# HazelHtml: Web App Library Plan

> **Note:** This was the original planning document. For the final implementation details, see **[hazel-html-implementation.md](hazel-html-implementation.md)**.

## Overview

This document outlines a plan to build a comprehensive web app library for Hazel, enabling:
1. Full web apps as Hazel programs (rendered in a sidebar/panel)
2. User-defined LiveLits (once modules exist)
3. An improved HTML projector with self-modifying behavior

The library builds on Jane Street's Virtual_dom (already used by Hazel) and follows Elm's architecture patterns where practical.

## Implementation Status (hazel-html branch)

**✅ Completed:**
- All types defined in `BuiltinsADT.re`: `HTML`, `Attr`, `Cmd`, `Sub`, `App`, `KeyEvent`, `MouseEvent`
- `HazelDOM.re` renders all HTML elements (~40) and attributes (~45) to Virtual_dom
- `CmdRunner.re` executes commands (Focus, Blur, ScrollTo, Delay, Log, etc.)
- `SubManager.re` manages subscriptions (OnResize, OnKeyDown, Every, AnimationFrame, etc.)
- Event handlers support both `Html -> Html` and `Html -> (Html, Cmd)` return types
- `HTMLProj.re` detects App type and evaluates subscriptions
- Subscription lifecycle management with global registry and cleanup on re-render

**⏳ Remaining (minor):**
- Execute init_cmd when App first renders (detected but not run)
- Error boundaries for runtime issues
- Testing with real Hazel programs

**📝 Deviations from original plan:**
- Types kept in `BuiltinsADT.re` rather than separate `src/language/html/` directory
- Subscription lifecycle uses global registry rather than projector lifecycle hooks
- AnimationFrame subscriptions can't be cleanly stopped (recursive request pattern)
- Testing and documentation

## Original State (projector-html branch, for reference)

- Limited elements: Div, Span, Button, Checkbox, Radio, Range, Text, Bool, Int, Float
- Limited attributes: Create, Style, OnClick, OnMousedown, OnInput
- No command/effect system

## Design Constraints

1. **Build on Virtual_dom** - Don't replace Jane Street's vdom, build on top
2. **No type parameters yet** - Hazel doesn't have them; design for unparameterized now
3. **CSS as strings** - `Style([("color", "red")])` not typed CSS
4. **Typed common attributes** - Id, Class, etc. with string fallback
5. **Reasonable type sizes** - Avoid hundreds of constructors

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Hazel Program                             │
│  ┌─────────────┐  ┌─────────────────────────────────────┐   │
│  │   Model     │  │         View / Update                │   │
│  │  (= Html)   │  │  Html -> Html (self-modifying)       │   │
│  └─────────────┘  └─────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   HazelHtml Library                          │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Html        │  Attr           │  Cmd       │  Sub   │   │
│  │  - Elements  │  - Properties   │  - Focus   │  - ... │   │
│  │  - Nesting   │  - Events       │  - Scroll  │        │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              Runtime (OCaml/ReasonML)                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  HazelDOM.re: Html -> Virtual_dom.Node.t              │   │
│  │  CmdRunner.re: Cmd -> Ui_effect.t                     │   │
│  │  SubManager.re: Sub -> event listeners                │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Polymorphism Constraint

**Problem:** Elm's types are parameterized over message type: `Html msg`, `Attribute msg`, `Cmd msg`. Without type parameters in Hazel, we can't do this directly.

**Solution for now:** Use the "self-modifying" pattern where event handlers transform state directly:
- `OnClick(Html -> Html)` - handler receives current state, returns new state
- `OnInput((Html, String) -> Html)` - handler receives state + input value

This works well for the HTML projector use case and simple apps.

**TODO (when type parameters exist):** Parameterize `Html`, `Attr`, `Cmd`, `Sub` over a message type to enable full Elm-style architecture with separate model/message types.

## File Organization

Create new directory: `src/language/html/`

```
src/language/html/
├── Html.re          # Html element type
├── Attr.re          # Attribute type
├── Event.re         # Event data types (KeyEvent, MouseEvent)
├── Cmd.re           # Command type for effects
├── Sub.re           # Subscription type for event sources
└── HazelHtml.re     # Re-exports all types, convenience constructors
```

Runtime interpreters stay in haz3lcore:
```
src/haz3lcore/projectors/
├── HazelDOM.re      # Existing: Html -> Node.t (expand this)
├── CmdRunner.re     # New: Cmd interpreter
└── SubManager.re    # New: Subscription manager
```

---

## Phase 1: Expanded Html Type

### 1.1 Element Types

```reason
// src/language/html/Html.re

type t =
  // Text content
  | Text(String)

  // Structural
  | Div(List(Attr.t), List(t))
  | Span(List(Attr.t), List(t))
  | P(List(Attr.t), List(t))
  | Pre(List(Attr.t), List(t))
  | Code(List(Attr.t), List(t))

  // Headings
  | H1(List(Attr.t), List(t))
  | H2(List(Attr.t), List(t))
  | H3(List(Attr.t), List(t))
  | H4(List(Attr.t), List(t))

  // Lists
  | Ul(List(Attr.t), List(t))
  | Ol(List(Attr.t), List(t))
  | Li(List(Attr.t), List(t))

  // Forms
  | Form(List(Attr.t), List(t))
  | Label(List(Attr.t), List(t))
  | Input(List(Attr.t))
  | TextArea(List(Attr.t), String)  // attrs, content
  | Button(List(Attr.t), List(t))
  | Select(List(Attr.t), List(t))
  | Option(List(Attr.t), String)    // attrs, label text

  // Links and media
  | A(List(Attr.t), List(t))
  | Img(List(Attr.t))

  // Tables
  | Table(List(Attr.t), List(t))
  | Thead(List(Attr.t), List(t))
  | Tbody(List(Attr.t), List(t))
  | Tr(List(Attr.t), List(t))
  | Th(List(Attr.t), List(t))
  | Td(List(Attr.t), List(t))

  // Semantic
  | Header(List(Attr.t), List(t))
  | Footer(List(Attr.t), List(t))
  | Nav(List(Attr.t), List(t))
  | Section(List(Attr.t), List(t))
  | Article(List(Attr.t), List(t))

  // Utility
  | Br
  | Hr(List(Attr.t))

  // Escape hatch for custom elements
  | Node(String, List(Attr.t), List(t))  // tagName, attrs, children
```

**Note:** Keep primitive renderers (`Bool`, `Int`, `Float` from current impl) as separate helper functions rather than Html constructors - they're conveniences, not HTML elements.

### 1.2 Attribute Types

```reason
// src/language/html/Attr.re

type t =
  // Identity
  | Id(String)
  | Class(String)
  | Classes(List(String))

  // Common properties
  | Disabled(Bool)
  | Placeholder(String)
  | Value(String)
  | Checked(Bool)
  | Selected(Bool)
  | ReadOnly(Bool)
  | Required(Bool)
  | AutoFocus(Bool)

  // Links/media
  | Href(String)
  | Src(String)
  | Alt(String)
  | Title(String)
  | Target(String)

  // Input specifics
  | Type(String)       // "text", "password", "checkbox", "radio", "range", "number", etc.
  | Name(String)
  | Min(String)
  | Max(String)
  | Step(String)
  | MaxLength(Int)
  | Pattern(String)

  // Layout
  | Width(String)
  | Height(String)
  | ColSpan(Int)
  | RowSpan(Int)

  // Styling
  | Style(List((String, String)))  // CSS as key-value pairs

  // Data attributes
  | Data(String, String)           // data-{name}={value}

  // Event handlers (self-modifying pattern)
  // Each takes a function: (currentHtml, eventData) -> newHtml
  | OnClick(Html.t -> Html.t)
  | OnDoubleClick(Html.t -> Html.t)
  | OnMouseDown((Html.t, Event.Mouse.t) -> Html.t)
  | OnMouseUp((Html.t, Event.Mouse.t) -> Html.t)
  | OnMouseEnter(Html.t -> Html.t)
  | OnMouseLeave(Html.t -> Html.t)
  | OnInput((Html.t, String) -> Html.t)        // state, input value
  | OnChange((Html.t, String) -> Html.t)       // state, new value
  | OnSubmit(Html.t -> Html.t)
  | OnKeyDown((Html.t, Event.Key.t) -> Html.t)
  | OnKeyUp((Html.t, Event.Key.t) -> Html.t)
  | OnFocus(Html.t -> Html.t)
  | OnBlur(Html.t -> Html.t)

  // Fallback for any attribute
  | Attr(String, String)           // name, value
  | BoolAttr(String, Bool)         // name, value
```

### 1.3 Event Data Types

```reason
// src/language/html/Event.re

module Key = {
  type t = {
    key: String,       // "Enter", "Escape", "a", "ArrowUp", etc.
    code: String,      // "KeyA", "Enter", etc.
    ctrl: Bool,
    shift: Bool,
    alt: Bool,
    meta: Bool,        // Cmd on Mac, Win on Windows
  }
};

module Mouse = {
  type t = {
    clientX: Float,
    clientY: Float,
    offsetX: Float,
    offsetY: Float,
    button: Int,       // 0=left, 1=middle, 2=right
    ctrl: Bool,
    shift: Bool,
    alt: Bool,
    meta: Bool,
  }
};
```

### 1.4 Deliverables

- [x] Define types in `BuiltinsADT.re` (HTML, Attr, KeyEvent, MouseEvent)
- [x] Register types with Hazel's builtin type system
- [x] Update `HazelDOM.re` to render all new elements/attributes
- [x] Add event data extraction for keyboard/mouse events
- [ ] Test with HTML projector

---

## Phase 2: Command System

Commands represent effects that the runtime should perform. Without type parameters, commands that need to "call back" with a result use the self-modifying pattern.

### 2.1 Command Types

```reason
// src/language/html/Cmd.re

type t =
  | None                                      // No effect
  | Batch(List(t))                            // Multiple effects

  // DOM manipulation (fire-and-forget)
  | Focus(String)                             // element id
  | Blur(String)                              // element id
  | ScrollIntoView(String)                    // element id
  | ScrollTo(String, Float, Float)            // id, x, y

  // Clipboard (fire-and-forget)
  | CopyToClipboard(String)

  // Time-delayed state update
  | Delay(Float, Html.t -> Html.t)            // ms, then apply transform

  // Console (for debugging)
  | Log(String)
```

**Note:** Commands that need to return data (like `GetElement`, `ReadClipboard`) are deferred until we have type parameters. For now, focus on fire-and-forget effects.

### 2.2 Extended Event Handlers

To support commands, event handlers can return `(Html.t, Cmd.t)` tuples:

```reason
// Alternative handler signatures that return commands
| OnClickCmd(Html.t -> (Html.t, Cmd.t))
| OnInputCmd((Html.t, String) -> (Html.t, Cmd.t))
// etc.
```

Or we could change all handlers to return `(Html.t, Cmd.t)` and have `Cmd.None` for pure updates. TBD based on ergonomics.

### 2.3 Runtime Interpreter

```reason
// src/haz3lcore/projectors/CmdRunner.re

let run: Cmd.t -> Ui_effect.t(unit) = cmd =>
  switch (cmd) {
  | None => Effect.Ignore
  | Batch(cmds) => Effect.Many(List.map(run, cmds))
  | Focus(id) => Effect.of_sync_fun(() => JsUtil.get_elem_by_id(id)##focus)
  | Blur(id) => Effect.of_sync_fun(() => JsUtil.get_elem_by_id(id)##blur)
  | ScrollIntoView(id) => Effect.of_sync_fun(() => /* ... */)
  | CopyToClipboard(text) => Effect.of_sync_fun(() => JsUtil.copy(text))
  | Delay(ms, transform) => /* schedule callback */
  | Log(msg) => Effect.of_sync_fun(() => Js.log(msg))
  };
```

### 2.4 Deliverables

- [x] Define `Cmd.t` type in `BuiltinsADT.re`
- [x] Implement `CmdRunner.re`
- [x] Integrate with HTML projector lifecycle (handlers can return `(Html, Cmd)`)
- [x] Handler signature: both plain `Html` and `(Html, Cmd)` tuples supported

---

## Phase 3: Subscription System

Subscriptions are event sources that continuously produce updates.

### 3.1 Subscription Types

```reason
// src/language/html/Sub.re

type t =
  | None
  | Batch(List(t))

  // Window events
  | OnResize((Html.t, Int, Int) -> Html.t)           // state, width, height
  | OnVisibilityChange((Html.t, Bool) -> Html.t)     // state, visible?

  // Global keyboard (document level)
  | OnDocumentKeyDown((Html.t, Event.Key.t) -> Html.t)
  | OnDocumentKeyUp((Html.t, Event.Key.t) -> Html.t)

  // Time-based
  | Every(Float, (Html.t, Float) -> Html.t)          // interval ms, state, timestamp
  | AnimationFrame((Html.t, Float) -> Html.t)        // state, timestamp
```

### 3.2 Subscription Manager

```reason
// src/haz3lcore/projectors/SubManager.re

type active_subs = /* track active listeners */

let update: (Sub.t, Html.t, Html.t -> unit) -> active_subs
// Compare old/new subs, add/remove listeners as needed
```

### 3.3 Deliverables

- [x] Define `Sub.t` type in `BuiltinsADT.re`
- [x] Implement `SubManager.re` with proper cleanup
- [ ] Integration point in app/projector lifecycle (requires projector lifecycle hooks)

---

## Phase 4: App Viewer

### 4.1 App Definition

For full apps with explicit model/update cycle:

```reason
// For now, using self-modifying pattern:
type App = {
  init: (Html.t, Cmd.t),
  subscriptions: Html.t -> Sub.t,
}

// The "update" and "view" are implicit:
// - Event handlers in Html are the update functions
// - The Html IS the view (self-modifying pattern)
```

### 4.2 Integration Options

**Option A: Projector-based**
- User writes expression that evaluates to `Html.t` or `App`
- Attach HTML/App projector to visualize
- Projector runs the lifecycle

**Option B: Sidebar panel**
- Designated "app" expression in the editor
- Rendered in a sidebar panel
- Panel manages lifecycle

**Option C: Probe-based**
- Use dynamics/probe system to capture runtime Html value
- Render in probe visualization area

Recommend starting with **Option A** (projector-based) since the infrastructure exists.

### 4.3 Deliverables

- [x] `App` type definition in `BuiltinsADT.re`: `((HTML, Cmd), HTML -> Sub)`
- [ ] App runner that manages init, subs, cmd execution
- [ ] UI for displaying app output (projector or panel)

---

## Phase 5: HTML Projector Improvements

### 5.1 Current Behavior (preserve)

The HTML projector's self-modifying pattern:
- Model IS the rendered Html
- Event handlers return new Html, which becomes new model
- `SetSyntax` updates the underlying code

### 5.2 Enhancements

1. **Use new expanded Html type** - more elements, attributes, events
2. **Add Cmd support** - handlers can return `(Html, Cmd)`
3. **Add Sub support** - projector manages subscriptions
4. **Better error handling** - graceful fallback for invalid Html

### 5.3 Deliverables

- [x] Update `HazelDOM.re` to use new types (all elements/attributes supported)
- [x] Add Cmd execution in event handlers (handlers can return `(Html, Cmd)`)
- [ ] Add Sub management in projector lifecycle (requires projector lifecycle hooks)
- [ ] Error boundaries for runtime issues

---

## Implementation Order

| Priority | Task | Complexity | Dependencies |
|----------|------|------------|--------------|
| 1 | Create `src/language/html/` directory structure | Low | None |
| 2 | Define `Html.t` with expanded elements | Low | #1 |
| 3 | Define `Attr.t` with expanded attributes | Low | #1 |
| 4 | Define `Event.Key.t` and `Event.Mouse.t` | Low | #1 |
| 5 | Register types with Hazel builtins | Medium | #2-4 |
| 6 | Update `HazelDOM.re` renderer | Medium | #2-4 |
| 7 | Update `HTMLProj.re` to use new types | Medium | #6 |
| 8 | Define `Cmd.t` type | Low | #1 |
| 9 | Implement `CmdRunner.re` | Medium | #8 |
| 10 | Integrate Cmd with HTMLProj | Medium | #7, #9 |
| 11 | Define `Sub.t` type | Low | #1 |
| 12 | Implement `SubManager.re` | Medium | #11 |
| 13 | Integrate Sub with HTMLProj | Medium | #10, #12 |
| 14 | App viewer UI | High | #13 |

---

## Future Work (post-type-parameters)

Once Hazel has type parameters:

1. **Parameterize over message type:**
   ```reason
   type Html('msg) = Div(List(Attr('msg)), List(Html('msg))) | ...
   type Attr('msg) = OnClick('msg) | OnInput(String -> 'msg) | ...
   type Cmd('msg) = Delay(Float, 'msg) | ...
   ```

2. **Full Elm-style App:**
   ```reason
   type App('model, 'msg) = {
     init: ('model, Cmd('msg)),
     update: ('msg, 'model) -> ('model, Cmd('msg)),
     view: 'model -> Html('msg),
     subscriptions: 'model -> Sub('msg),
   }
   ```

3. **User-defined LiveLits:**
   - Livelit specifies `view: model -> Html(action)`
   - Actions route through livelit's update function
   - Full type safety between view events and update handling

---

## Open Questions

1. **Handler signature uniformity:** Should all event handlers return `(Html, Cmd)` or have separate `OnClick` vs `OnClickCmd` variants?

2. **Subscription lifecycle:** How do subscriptions interact with projector focus/unfocus?

3. **App viewer location:** Sidebar panel vs inline projector vs separate window?

4. **Error boundaries:** What happens when Html evaluation fails mid-render?

---

## References

- [Elm Html.Events documentation](https://package.elm-lang.org/packages/elm/html/latest/Html.Events) - Event handler patterns
- [Elm Architecture](https://guide.elm-lang.org/architecture/) - MVU pattern
- LiveLits paper: "Filling Typed Holes with Live GUIs" (PLDI 2021)
- Current implementation: `projector-html` branch
