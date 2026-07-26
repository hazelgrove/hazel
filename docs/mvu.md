# HazelHtml MVU

Write an Elm-style app in Hazel, put the HTML projector on it, and it runs — in
place in the code, or docked to a sidebar panel. This is the one doc for the
feature: program shape, types, runtime, and the design decisions that are worth
revisiting.

## Program structure

An MVU program is a **4-tuple** `(init, update, view, subs)`:

- `init` — the initial model (any Hazel value)
- `update` — `(Msg, Model) -> (Model, Cmd)`
- `view` — `Model -> Html`
- `subs` — `Model -> Sub`

The labeled form `(init=..., update=..., view=..., subs=...)` is also accepted,
in any order.

```
type Msg = + Toggle + Tick in
let init = (seconds=0, running=false) in
let update(msg, model) =
  case msg
  | Toggle =>
      ((seconds=model.seconds, running=if model.running then false else true), CmdNone)
  | Tick =>
      if model.running then
        ((seconds=model.seconds + 1, running=model.running), CmdNone)
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
  `Node(tag, attrs, children)`.
- **`Attr`** (~45) — identity (`Id`/`Class`/`Classes`), properties
  (`Disabled`/`Value`/`Checked`/`Placeholder`/...), link/media, input specifics,
  layout, `Style(List((String, String)))`, `Data(name, value)`, the event handlers
  below, plus escape hatches `Create(name, value)` and `BoolAttr(name, bool)`.
- **`KeyEvent`** = `(key=String, code=String, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`
- **`MouseEvent`** = `(x=Float, y=Float, button=Int, ctrl=Bool, shift=Bool, alt=Bool, meta=Bool)`

Event data are labeled tuples, so fields come out by dot projection:
`fun e -> if e.key == "ArrowUp" then MoveUp else NoOp`.

### Event handlers

Handlers produce **messages**, which route through `update`.

| Event | Handler type |
|-------|-------------|
| `OnClick`, `OnDoubleClick`, `OnMouseEnter`, `OnMouseLeave`, `OnFocus`, `OnBlur`, `OnSubmit` | `Msg` (a value, e.g. `OnClick(Toggle)`) |
| `OnInput`, `OnChange` | `String -> Msg` |
| `OnKeyDown`, `OnKeyUp`, `OnKeyPress` | `KeyEvent -> Msg` |
| `OnMouseDown`, `OnMouseUp`, `OnMouseMove` | `MouseEvent -> Msg` |

`OnSubmit` also prevents the browser's default form submission.

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
| `Delay(500.0, MyMsg)` | Dispatch a message after a delay (ms) |
| `Log("debug info")` | Print to the browser console |

### Subscriptions

External event sources. `subs` is re-evaluated whenever the model changes;
returning `SubNone` for a previously active subscription tears it down.

| Subscription | Handler type |
|-------------|-------------|
| `SubNone`, `SubBatch([sub1, sub2])` | — |
| `Every(1000.0, handler)` | `Float -> Msg` (interval ms; handler gets a timestamp) |
| `AnimationFrame(handler)` | `Float -> Msg` |
| `OnResize(handler)` | `(Int, Int) -> Msg` (width, height) |
| `OnVisibilityChange(handler)` | `Bool -> Msg` (`true` when visible) |
| `OnDocumentKeyDown(handler)`, `OnDocumentKeyUp(handler)` | `KeyEvent -> Msg` (capture phase) |

### Inline tests

Tests can sit alongside the 4-tuple and run against the pure `update` without
the runtime:

```
hint "toggle starts timer"
test
  let (m, _cmd) = update(Toggle, init) in
  m.running == true
end;

(init, update, view, subs)
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
  4-tuple. The model lives in the web-side AppStore; a msg goes to
  `update(msg, model)`. This is the Elm path.
- **Syntax commit** (`commit: Syntax`) — the value is bare HTML. The projected
  expression IS the model, and a msg is an `Html -> Html` transform. Committing
  evaluates `msg(model)` and splices the result back into the document via
  `SetSyntax`.

Handlers always produce a **msg** and dispatch is uniform; only the commit
strategy differs.

### Dispatch cycle

1. An event fires (click, subscription timer, delayed command, ...).
2. The handler produces a `Msg` value; `HazelDOM` hands it to `inject` and stops propagation.
3. **State commit**: the AppStore entry evaluates `update(msg, model)`, extracts
   `(new_model, cmd)`, runs `cmd` through `CmdRunner`, re-derives `view(new_model)`
   and `subs(new_model)`, and reconciles its subscription handles.
   **Syntax commit**: `HTMLProj` evaluates `msg(model)` and splices the resulting
   Html back into the document (an `(Html, Cmd)` result also runs the Cmd).

**`evaluate_direct`**: every runtime application (update, view, subs, handlers,
subscription callbacks) is plain `Evaluator.evaluate` — no `Statics.mk`, no
elaboration. Necessary because handlers are `Closure` nodes left over from a
previous evaluation, and the elaborator does not understand runtime-only
constructs like `Closure(env, body)`.

**Wrapper stripping**: the evaluator wraps values in `Asc`, `Closure` and
`Parens`; these must be stripped before matching on constructors.
`MvuShape.strip_wrappers` / `of_constructor` do this throughout.

**Cmd extraction**: `update` is expected to return `(Model, Cmd)`. If the result
is not a tuple (e.g. mid-edit), the whole result is treated as the model with
`CmdNone`.

## Design decisions

These are the load-bearing ones — the places where a different choice was
available and might still be worth taking.

### MVU state lives in page state, not in the syntax

The app model is an entry in an id-keyed store (`Globals.Model.apps`,
`AppStore.t = Id.Map.t(Entry.t)`), not in the projector's model string or the
document.

Why not in the syntax: the model holds closures — `update`/`view`/`subs` are
evaluated function values, which don't serialize — and a projector `SetModel` is
an *edit* action, so a write per message would be an O(document) rewrite with
statics, elaboration and re-evaluation behind it. (The Layout classification
below cuts that cost for the model writes that do happen — checkpoints,
drag-resize — but it doesn't make a per-message document write a good idea.)
Keeping state in page state makes dispatch O(handler eval).

Contracts the store keeps:

- **State vs memo split.** Only `model` is state. `update_fn`/`view_fn`/`subs_fn`/
  `html` are memos derived by evaluating the program: never persisted, always
  rebuildable. The type documents which is which.
- **Keyed by syntax identity.** `Id.Map` keyed by the projector's term id, so
  multiple inline apps in one document each get their own entry for free.
- **Edit-independent.** All store updates ride actions with `is_edit=false` and
  non-historic, so app interaction never enters the undo history.
- **Entries own their subscriptions.** `sub_handles` live in the entry, and every
  operation that changes `model` reconciles them. This retired a module-global
  `HazelDOM.active_subscriptions` table and moved subscription reconciliation out
  of the render path into the update path — so unrelated renders no longer reset
  timer phase.
- **Rebind on re-eval.** When the program re-evaluates, the memos are re-derived
  and the model *survives* if it is still compatible with the new program
  (`view(old_model)` still evaluates), else it resets to the new `init`. Editing
  your update function live keeps your app state.

### `Project(SetModel)` recomputes layout only

Projector model strings are opaque to statics — every projector kind was audited,
and the ones that do affect semantics (sliders, checkbox, textarea, livelits)
commit through `SetSyntax`, which stays a Full recompute. So `Project(SetModel)`
is classified as a **Layout** edit: `CachedSyntax` and the measured/shape maps
rebuild, `CachedStatics` is reused, and no worker re-evaluation is scheduled.
This is what makes app dispatch and drag-resize cheap, and it also stopped
sliders from re-running statics per tick.

`SetModelQuiet` is the non-historic twin: a drag gesture pushes one history entry
on the first tick and streams quiet updates after, so undo restores the pre-drag
state in one step. Autosave still fires, via a separate `Updated.save` field.

### Self-modifying HTML is Elm with `update = apply`

The old inline projector had its own "legacy mode" — separate app shapes, a
`SetAppViewModel` action, and `update_fn: option(...)` branching through
HazelDOM/CmdRunner/SubManager. That is gone. A self-modifying handler is just a
msg whose interpretation is `apply`: `HazelDOM.t = {inject, view_term, commit}`,
and payload events build the msg as `fun m -> handler((m, payload))`
(`payload_transform`), so `Delay(ms, msg)` and `(Html, Cmd)` results work
identically in both modes.

**Known limitation, worth revisiting**: syntax-commit splices an *evaluated*
value back into the document. Evaluation erases variables — `let x = "ON" in
Div(..., [Text(x)])` commits with `x` already replaced by its value, and any
surrounding binding structure is lost. So syntax commit really only works on
closed HTML. A syntactic transformation (rewriting the projected expression
rather than replacing it with its value) would lift that, and is the natural next
step if self-modifying HTML is pursued further.

### Projector placement is a framework feature, not an HTML feature

`placement: Inline | Sidebar` is a field on the projector instance
(`ProjectorCore.t`), marked `[@sexp.default]` so documents serialized before the
field still load. `Alt+S` or the context menu toggles it; the code site keeps a
chip and the projector's primary view renders as a card in the Projectors sidebar
panel, with jump-to-source and undock.

Routing is generic — `mk_view` is unchanged and placement-agnostic, and
`split_views` picks chip-vs-inline the same way refractors already pick
`skip_inline` — so this works for every projector kind with zero per-projector
code. Overlay and offside decorations keep rendering at the code site in both
placements. `TogglePlacement` is a Layout edit (footprint changes, semantics
cannot), historic, one undo step.

Two details that cost a round of fixes. **`ProjectorChip` owns both the chip's
segment and its `Shape`**, so the reserved footprint and the drawn width cannot
drift apart. **Panel order comes from the projector's measured origin (row, col)**,
not from `projector_list` — that list follows MakeTerm's skel-driven traversal,
which is neither source order nor its reverse, whereas the measured origin is the
same measurement that positions the projector at the code site, so it is
on-screen order by construction.

Placement also pushed two generally-useful things into the projector framework:
`View.args` carries `col_width`/`row_height` (font metrics live web-side and
projectors had no access), and `View.status` carries `placement` (projectors could
not tell whether they were docked). The HTML projector's resize uses the former:
it anchors cursor and cols/rows at pointerdown and sizes from the delta, never
re-reading geometry mid-drag, which is what makes it immune to the reflow its own
resizing causes.

### App detection is two-phase

`Projector.init` only ever sees pre-evaluation syntax, so it cannot know what an
expression will evaluate to.

- `init` is **permissive and syntactic**: bare HTML (an HTML constructor
  application, or `Br`), plus anything that could evaluate to an app — a literal
  or labeled 4-tuple, a variable, an application.
- `view` is **authoritative**: `HTMLProj` sets `dynamics = true`, so the
  projected expression is probed and `info.dynamics` carries its live value (the
  latest sample by `seq`). `MvuShape.detect_app_kind` picks the commit mode from
  that value. A value that is neither an app nor HTML says so rather than dumping
  a term; no value at all (not yet evaluated) falls back to rendering the syntax.

`Projector.dynamics` had been a **dead flag** since its consumer was removed with
the legacy maketerm probe code. It was rewired additively in `CachedStatics`:
a projector that asks for dynamics gets its term id added to the probe targets,
exactly like a manual probe, so nothing changes for projectors that don't ask.

### AppBridge: core reaches the web-side store through refs

`HTMLProj` is in `haz3lcore`; the AppStore is in `web`, and core cannot depend on
web. `AppBridge` (core) holds a set of function refs that `AppBridgeInstall`
populates from `Page.main_view`, where both the store and `inject_global` are in
scope — the same indirection `Ascriptions.ctx_ref` uses. Defaults are inert, so
core-only builds behave as if no store existed.

| Ref | Meaning |
|-----|---------|
| `ensure_app(id, app, checkpoint)` | Create or rebind the store entry for `id`; no-op when already bound to this value |
| `current_html(id)` | The entry's current `view_fn(model)`, `None` until built |
| `dispatch(id, msg)` | Route a msg to the entry's `update_fn` |
| `checkpoint(id)` | The entry's model, serialized, if closure-free |
| `version` | Bumped on every store change, so `ProjectorView.ViewCache` can see it |

`ensure_app` schedules the existing init action rather than mutating, so render
stays effect-light.

### Checkpoints

A projected app's model is persisted into the projector's own model string, so an
app keeps its state across a document reload.

- **What persists**: the model, sexp-serialized, **iff it is closure-free** — no
  `Fun`/`TypFun`/`FixF`/`Closure`/`BuiltinFun` anywhere inside. A model holding a
  function simply isn't checkpointed; no error, the app just starts from `init`
  next time.
- **When**: debounced, never per message. Each dispatch re-arms a 2s idle timer;
  the write goes through the quiet non-historic path, so it is neither an edit nor
  an undo entry.
- **Restore**: a checkpoint is *validated against the current view* before being
  adopted — it must deserialize, and `view` must accept it. How strict that second
  test is (evaluates, vs. evaluates and yields HTML) is a tuning knob: Hazel is
  gradual, so a wrong-shaped model usually yields an indeterminate value rather
  than an error, and looser trades "stale state survives an edit" against "state
  is dropped too eagerly". Either way rejection is silent and `init` is used.

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
| `hazel-programs/html-examples/` | mvu-counter, timer, animation, keyboard-game, todo-list, full-app, emojipaint, tictactoe |

Sources: [The Elm Architecture](https://guide.elm-lang.org/architecture/),
[Beginning Elm — Commands](https://elmprogramming.com/commands.html).
