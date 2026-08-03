# HazelHtml MVU

Write an Elm-style app in Hazel, put the HTML projector on it, and it runs — in
place in the code, or docked to a sidebar panel. This is the one doc for the
feature: program shape, types, runtime, and the design decisions that are worth
revisiting.

## Program structure

An MVU program is a **4-tuple** `(init, update, view, subs)`:

- `init` — the initial model (any Hazel value)
- `update` — `(Model, Action) -> (Model, Cmd)`
- `view` — `Model -> Html`
- `subs` — `Model -> Sub`

The labeled form `(init=..., update=..., view=..., subs=...)` is also accepted,
in any order.

**Naming.** Hazel programs call the message type `Action`, and the model comes
first in `update` — that matches `fold_left`'s `(acc, elem)` and Hazel's own
subject-first argument convention, so `update` folds directly over a list of
actions. The OCaml implementation still says `msg`, because `action` is taken
there twice over: `HTMLProj` has its own projector `action` type, and
`Haz3lcore.Action` is the editor-action module. The asymmetry is deliberate.

```
type Action = + Toggle + Tick in
let init = (seconds=0, running=false) in
let update(model, action) =
  case action
  | Toggle => ((seconds=model.seconds, running=!model.running), CmdNone)
  | Tick =>
      if model.running
      then ((seconds=model.seconds + 1, running=model.running), CmdNone)
      else (model, CmdNone)
  end
in
let view(model) =
  Div([], [
    Text(string_of_int(model.seconds)),
    Button([OnClick(Toggle)], [Text(if model.running then "Pause" else "Start")])
  ])
in
let subs(model) =
  if model.running then Every(1000.0, fun _timestamp -> Tick) else SubNone
in
(init, update, view, subs)
```

## Types

All defined in `src/language/builtins/BuiltinsADT.re`. They are unparameterized
— Hazel has no type parameters, so unlike Elm's `Html msg` there is no static
guarantee that a handler produces the message type `update` expects; message
routing is by constructor name at runtime.

- **`Html`** (~47 constructors) — `Text`/`Bool`/`Int`/`Float` renderers; the usual
  structural, heading, list, form, link, table and semantic elements, each taking
  `(List(Attr), List(Html))`; `Br`/`Hr`; plus the escape hatch
  `Node(tag, attrs, children)`. `Node` also covers SVG: tags in
  `HazelDOM.svg_tags` (svg, circle, rect, line, path, g, text, ...) are created
  in the SVG namespace, with SVG attributes via `Create` (`createElement` on
  these would yield an inert `HTMLUnknownElement`).
- **`Attr`** (~45) — identity (`Id`/`Class`/`Classes`), properties
  (`Disabled`/`Value`/`Checked`/`Placeholder`/...), link/media, input specifics,
  layout, `Style(List((String, String)))`, `Data(name, value)`, the event handlers
  below, plus escape hatches `Create(name, value)` and `BoolAttr(name, bool)`.
- **`KeyEvent`** = `(key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`
- **`MouseEvent`** = `(x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`

Event data are labeled tuples, so fields come out by dot projection:
`fun e -> if e.key == "ArrowUp" then MoveUp else NoOp`.

### Event handlers

Handlers produce **actions**, which route through `update`.

| Event | Handler type |
|-------|-------------|
| `OnClick`, `OnDoubleClick`, `OnMouseEnter`, `OnMouseLeave`, `OnFocus`, `OnBlur`, `OnSubmit` | `Action` (a value, e.g. `OnClick(Toggle)`) |
| `OnInput`, `OnChange` | `String -> Action` |
| `OnKeyDown`, `OnKeyUp`, `OnKeyPress` | `KeyEvent -> Action` |
| `OnMouseDown`, `OnMouseUp`, `OnMouseMove` | `MouseEvent -> Action` |
| `OnClickAt`, `OnMouseDownAt`, `OnMouseMoveAt`, `OnMouseUpAt` | `(Int, Int) -> Action` — pointer position relative to the element, in px |
| `OnWheelAt` | `(Int, Int, Float, Float) -> Action` — element-relative position plus wheel deltas; prevents default scrolling |

`OnSubmit` also prevents the browser's default form submission.
The `...At` handlers are what a widget needs to interpret pointer events on
its own surface; the `MouseEvent` coordinates are viewport-relative, which
Hazel code cannot convert (it has no access to the element's position). They
measure against the handler's own element (`currentTarget`), not the event
target, so coordinates stay stable when the pointer is over a child —
attach them to an svg root and drag its shapes.

### Commands

Side effects returned from `update` alongside the new model.

| Command | Description |
|---------|-------------|
| `CmdNone` | No effect |
| `CmdBatch([cmd1, cmd2, ...])` | Run multiple commands |
| `Focus("element-id")` / `Blur("element-id")` | Focus/blur a DOM element by ID |
| `ScrollIntoView("element-id")` | Scroll element into view |
| `ScrollTo("element-id", x, y)` | Scroll to position |
| `CopyToClipboard("text")` | Copy text to clipboard |
| `Delay(500.0, MyAction)` | Dispatch an action after a delay (ms) |
| `PlayTone(440.0, 200.0)` | Play a sine beep (freq Hz, duration ms) via Web Audio; the shared AudioContext stays suspended until the first user gesture, so tones fired before any click are dropped |
| `Say("text")` | Speak a string aloud (speech synthesis) |
| `Random(handler)` | `Float -> Action` — dispatch the handler applied to a uniform draw in [0,1). Randomness lives at the command boundary (Elm's `Random.generate`): evaluation itself stays deterministic, which probes/stepper re-runs rely on |
| `Log("debug info")` | Print to the browser console |

### Subscriptions

External event sources. `subs` is re-evaluated whenever the model changes;
returning `SubNone` for a previously active subscription tears it down.

| Subscription | Handler type |
|-------------|-------------|
| `SubNone`, `SubBatch([sub1, sub2])` | — |
| `Every(1000.0, handler)` | `Float -> Action` (interval ms; handler gets a timestamp) |
| `AnimationFrame(handler)` | `Float -> Action` |
| `OnResize(handler)` | `(Int, Int) -> Action` (width, height) |
| `OnVisibilityChange(handler)` | `Bool -> Action` (`true` when visible) |
| `OnDocumentKeyDown(handler)`, `OnDocumentKeyUp(handler)` | `KeyEvent -> Action` (capture phase) |

### Inline tests

Tests can sit alongside the 4-tuple and run against the pure `update` without
the runtime:

```
hint "toggle starts timer"
test
  let (m, _) = update(init, Toggle) in
  m.running == true
end;

(init, update, view, subs)
```

An `update` that never issues a command can return a bare `Model` and be lifted
at the tuple, which keeps the tests free of `CmdNone` noise:

```
let noCmd(f: (Model, Action) -> Model): (Model, Action) -> (Model, Cmd) =
  fun (model, action) -> (f(model, action), CmdNone)
in

(init, noCmd(update), view, subs)
```

From the CLI: `node _build/default/src/CLI/cli.bc.js test myprogram.hz`

## Where an app runs

An app does not have to be the last expression of the program, and does not have
to be viewed in a sidebar. Put the HTML projector (right-click → Add HTML) on
*any* expression that evaluates to a 4-tuple and it runs there, in place. `Alt+S`
docks any projector to the Projectors sidebar panel, so an inline app can also be
viewed alongside the code instead of inside it.

`HTMLProj` is one projector with two commit modes, chosen by what the projected
expression's live value turns out to be:

- **State commit** (`commit: State`) — the value is an `(init, update, view, subs)`
  4-tuple. The model lives in the web-side AppStore; an action goes to
  `update(model, action)`. This is the Elm path.
- **Syntax commit** (`commit: Syntax`) — the value is bare HTML. The projected
  expression IS the model, and an action is an `Html -> Html` transform.
  Committing evaluates `action(model)` and splices the result back into the
  document via `SetSyntax`.

Handlers always produce an **action** and dispatch is uniform; only the commit
strategy differs.

### Dispatch cycle

An event fires; the handler produces an `Action`; `HazelDOM` hands it to
`inject` and stops propagation. Then, for **state commit**: the AppStore entry
evaluates `update(model, action)`, runs the returned `cmd` through `CmdRunner`,
re-derives `view` and `subs`, and reconciles subscription handles. For **syntax
commit**: `HTMLProj` evaluates `action(model)` and splices the resulting Html
back in.

Three things to know if you work on this path:

- **Everything runs through plain `Evaluator.evaluate`** — no statics, no
  elaboration. Handlers are `Closure` nodes left from a previous evaluation, and
  the elaborator doesn't understand runtime-only constructs.
- **Values arrive wrapped** in `Asc`/`Closure`/`Parens`; strip them before
  matching on constructors (`MvuShape.strip_wrappers`).
- **A non-tuple `update` result** is treated as the model with `CmdNone`, and
  warns. That is for transient edit states only — don't rely on it, since it
  puts two `update` types through one slot and defeats a future type check on
  the 4-tuple. Use the `noCmd` lift shown above.

## Design decisions

The load-bearing ones — where a different choice was available and might still
be worth taking.

**State lives in page state, not the syntax.** The model is an entry in
`AppStore.t = Id.Map.t(Entry.t)`, keyed by the projector's term id. It can't
live in the syntax: models hold evaluated closures, which don't serialize, and
a projector `SetModel` is an *edit*, so a write per message would be an
O(document) rewrite with statics and re-evaluation behind it. Only `model` is
state; `update_fn`/`view_fn`/`subs_fn`/`html` are memos, re-derived on
re-evaluation. On re-eval the model survives if `view(old_model)` still
evaluates, so editing your update function live keeps app state. Store updates
ride non-historic `is_edit=false` actions, so app interaction stays out of undo.

**`Project(SetModel)` recomputes layout only.** Model strings are opaque to
statics — every kind was audited, and those that do affect semantics (sliders,
checkbox, textarea, livelits) commit via `SetSyntax`, which stays Full. So
`SetModel` rebuilds `CachedSyntax` and the measured maps, reuses
`CachedStatics`, and schedules no re-evaluation. `SetModelQuiet` is the
non-historic twin: a drag pushes one history entry on the first tick and
streams quiet updates after.

**Self-modifying HTML is Elm with `update = apply`.** There is no separate
legacy mode; a self-modifying handler is just an action interpreted by
application, and payload events build it as `fun m -> handler((m, payload))`.
*Limitation worth revisiting*: syntax commit splices an **evaluated** value
back, and evaluation erases variables, so it only really works on closed HTML.
A syntactic transformation would lift that.

**Placement is a framework feature, not an HTML one.** `placement: Inline |
Sidebar` lives on `ProjectorCore.t`, defaulted on deserialization for
back-compat, so any projector kind docks with zero per-kind changes.
`ProjectorChip` owns both the chip segment and its `Shape`, so the drawn and
reserved widths can't drift.

**App detection is two-phase.** `init` is permissive and syntactic (anything
that could evaluate to HTML or an app); `view` is authoritative and reads the
live value from this projector's probe. The cost is a wider context menu — see
limitations.

**AppBridge.** Core can't depend on web, so the web layer installs function
refs at startup and core-only builds (CLI, tests) get inert defaults. Precedent:
`Ascriptions.ctx_ref`.

**Checkpoints.** After 2s idle, a closure-free model is written quietly into the
projector model so an app survives reload. Restoring requires `view(model)` to
both evaluate and yield HTML; a wrong shape is discarded silently rather than
erroring.

## Keyboard focus

Hazel's editor captures keyboard events on `#page` with `Stop_propagation` and
`Prevent_default`, which conflicts with app inputs.

- **Form inputs**: the page key handler yields when the event target is an
  `INPUT`, `TEXTAREA` or `SELECT`, letting the browser handle it.
- **Global keyboard subscriptions**: `OnDocumentKeyDown` uses capture-phase
  listeners on `document`, so they fire before the page handler — but they don't
  `stopPropagation`, so events still reach the editor. A keyboard-driven app will
  trigger both game logic and editor navigation. Real focus isolation is unbuilt.

## Divergences from Elm

No type parameters, so `Html`/`Cmd`/`Sub` are unparameterized and message-routing
errors surface at runtime. `update` always returns `(Model, Cmd)` (Elm's
`Browser.element` shape; there is no `Browser.sandbox` — use `CmdNone`). No
`Cmd.map`/`Sub.map`, so nested components with their own message types don't
compose. No ports and no HTTP: commands cover DOM operations and scheduling only.

## Known limitations and revisit candidates

1. **Permissive `init`** widens the context menu: the Add-HTML item offers itself
   on any `Var` or `Ap`, since `init` can't tell what will evaluate to an app.
2. **`AppStore.gc` is implemented but not wired** for inline entries, so entries
   for deleted projectors linger.
3. **Checkpoint timer captures a render-time projector index** (the general
   `Action.project` idx hazard), so adding or removing a projector within the 2s
   window can misroute one quiet write.
4. **The Projectors panel shows only the reported editor's projectors.**
5. **Syntax commit only really works on closed HTML** (see self-modifying above).
6. **Subscriptions are torn down and recreated on every model change**, so timers
   restart their countdown each tick and AnimationFrame loops are re-requested
   each frame.
7. **Constructor names are matched as strings**, so a typo in an element,
   attribute, command or subscription fails silently and renders as fallback text.
8. **Every handler and subscription callback is a full evaluation pass.** Handler
   and msg errors are caught and logged (nothing dispatched or committed), but
   infinite loops or stack overflows can still take the page down.

## File map

| Path | Role |
|---|---|
| `src/web/app/globals/AppStore.re` | id-keyed MVU state store (the state-commit target) |
| `src/web/app/globals/Globals.re` | `Model.apps` + the id-keyed `AppView*` actions |
| `src/web/app/globals/AppBridgeInstall.re` | installs AppBridge's refs from the store + inject |
| `src/web/app/Page.re` | app-view actions, bridge install, keyboard fix |
| `src/web/app/sidebar/ProjectorPanel.re` | Projectors sidebar tab: cards for docked projectors |
| `src/haz3lcore/projectors/MvuShape.re` | shape + app-kind detection, checkpoint (de)serialization |
| `src/haz3lcore/projectors/HazelDOM.re` | `Html` → Virtual_dom renderer + unified msg dispatch |
| `src/haz3lcore/projectors/SubManager.re` | subscription setup/cleanup, handler dispatch |
| `src/haz3lcore/projectors/CmdRunner.re` | command execution (Focus, Delay, Log, ...) |
| `src/haz3lcore/projectors/AppBridge.re` | core → web indirection for the AppStore |
| `src/haz3lcore/projectors/implementations/HTMLProj.re` | the projector: state or syntax commit, resize |
| `src/language/builtins/BuiltinsADT.re` | `Html`, `Attr`, `Cmd`, `Sub`, `KeyEvent`, `MouseEvent` |
| `src/CLI/Cli.re`, `src/CLI/Run.re` | the `test` command, `evaluate_with_tests` |
| `hazel-programs/mvu/` | the ten example apps; `regen-slides.sh` re-encodes them into `src/mvu` |
| `src/mvu/` | those apps as documentation slides, grouped under `MVU / ...` |

Sources: [The Elm Architecture](https://guide.elm-lang.org/architecture/),
[Beginning Elm — Commands](https://elmprogramming.com/commands.html).
