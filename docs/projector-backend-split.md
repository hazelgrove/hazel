# Plan: splitting projectors into shared logic + per-backend views

Status: **planned** (not started). Prerequisite context: the TUI
(`src/tui/`) renders projectors via its own registry of terminal views
(`src/tui/TermProjector.re`) — "option A". This document describes
"option B": restructuring the core projector interface so each
projector's semantics exist once, with web (Vdom) and terminal (cell
grid) views as separate backends. A is a strict prefix of B: the
TermProjector signature, the TUI registry, and the terminal view
implementations all carry over unchanged; B relocates the web halves.

## Why B

- Each projector's semantics (init/update/get/put/placeholder/error)
  should exist once. Today they're fused with the Vdom view inside
  `module M: Projector`; option A worked around this by lifting the
  pure helpers to file level in `CheckboxProj`/`SliderProj`/`TypeProj`
  (the lift itself is a B step — those helpers become the logic
  modules' contents).
- Moving Vdom views out of haz3lcore removes its `Virtual_dom`
  dependency path and (with the three `[%js]` projector files:
  ProbeProj, CardProj, CSVProjector) its `js_of_ocaml-ppx` preprocess
  — a prerequisite for ever compiling the core natively (see also the
  broader de-webbing scoping: `util` must also lose `bonsai.web`,
  `Unicode`/`StringUtil` need native fallbacks).

## Target shape

In haz3lcore (per projector, e.g. `projectors/logic/CheckboxLogic.re`):

```reason
module type ProjectorLogic = {
  type model;     /* + sexp/yojson deriving, Cook-style serialization */
  type action;
  let init: Any.t => option(model);
  let update: (model, info, action) => model;
  let placeholder: (model, info) => Shape.t;   /* stays char-cell based */
  let error: (model, info) => option(error);
  let dynamics: bool;
  let elaborate_syntax: bool;
  /* NO view, NO Focusable callbacks (see Focus below) */
};
```

In src/web (`projectors/views/CheckboxView.re`):

```reason
module type WebView = (L: ProjectorLogic) => {
  let view: View.args(L.model, L.action) => View.t;  /* Vdom, as today */
  let focusable: Focusable.t;                        /* DOM callbacks */
};
```

In src/tui (`TermProjector.re`, already exists from option A):

```reason
/* view fills the Shape-reserved cell region; reactions are data */
module type TermView = { inline_view / offside_view / on_click / ... };
```

Registries: web keeps `ProjectorInit.to_module`-equivalent in
`src/web`; the TUI keeps its `Kind => option((module TermView))`.
Core keeps a logic-only registry `Kind => (module CookedLogic)` for
Perform/Measured/statics needs (`placeholder`, `init`, `update`,
`dynamics`, `elaborate_syntax` are all consumed from core code —
grep `ProjectorInit.to_module` callers).

## Steps

1. **Logic extraction** (mechanical, one projector at a time):
   move model/action/init/update/placeholder/error +
   dynamics/elaborate_syntax flags into `projectors/logic/*Logic.re`;
   the existing implementation files keep their views and `include`
   the logic. No registry changes yet; web unaffected. The file-level
   helpers lifted during option A move into the logic modules.
2. **Core registry split**: `ProjectorInit` exposes a `CookedLogic`
   registry (no view, no Focusable). Audit core callers
   (ProjectorPerform, ProjectorInfo.ShapeMapSemantics, Editor) — they
   only need logic; switch them to it.
3. **View relocation**: move the Vdom view halves + `Focusable`
   implementations to `src/web/projectors/`; web's ProjectorView gets
   its own Kind→WebView registry. Delete `View`/`Focusable`/Vdom from
   ProjectorBase (core keeps `info`, `utility`, `external_action`,
   `Shape`). Remove `js_of_ocaml-ppx` from haz3lcore's pps once
   ProbeProj/CardProj/CSVProjector views (the `[%js]` users) have
   moved.
4. **Focus redesign** (the only non-mechanical step): replace the
   web's DOM-focus callbacks (`Focusable.focus_keyboard` does
   `JsUtil.get_elem_by_id(..)##focus`) with focus as *data*. Proposal:
   `Action.Project(Focus(idx, kind, dir))` records focus in editor
   state (e.g. on Editor.Model or zipper-adjacent state) instead of
   firing a callback; the web view layer reflects it into DOM focus,
   the TUI reads it to route keys (the TUI already needs a
   `focused_projector` model field for TextArea-style projectors).
5. **TUI catches up for free**: TermView modules call the logic
   modules directly (no more reaching into web-fused files); add
   focusable terminal projectors (TextArea) using step 4's focus
   state.

## Hazards learned during option A

- `Cook` serializes models to sexp strings; keep serialization at the
  logic layer so web/TUI views never see unparsed models drift apart.
- The `Shape` contract is the cross-backend invariant: terminal views
  must fill exactly the reserved cells (Hazel's `Unicode.Width` counts
  Extended_Pictographic clusters as 2 columns — avoid glyphs like ✔
  U+2714 where Hazel's count disagrees with terminal wcwidth; ✓ ✗ ⇒ ⋱
  are safe width-1).
- TypeProj is offside-only (`Shape.default`, zero inline width) — any
  backend view interface needs the inline/offside/overlay triple, not
  just inline.
- `ProjectorInfo.mk_info`/`utility` are already pure core functions —
  both backends construct `info` identically through them.

## Non-goals (for B as scoped here)

- Porting HTTP/JS-dependent projectors (Card, CSV, Probe rich views)
  to the terminal; their *logic* still gets extracted, but terminal
  views remain unregistered (TUI falls back to blank-space rendering).
- Native compilation itself — B removes haz3lcore's Vdom/js-ppx
  obstacles but `util`'s web deps are a separate workstream.
