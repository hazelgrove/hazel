# Rich Probes

Rich probes let a probe sample render its value as a domain-specific
interactive view (table, card hand, calculator, …) instead of the default
abbreviated syntax. The system is a thin plug-in layer on top of
`ProbeProj`: each renderer is a module that knows how to recognize a
shape of value, hold per-instance UI state, and render itself; the
projector handles modal stacking, menu integration, focus, and the
elaboration ↔ syntax bridge.

This doc covers what the moving parts are, how to add a new renderer,
and where to look when something doesn't render.

## Where the code lives

| File | Purpose |
| ---- | ------- |
| `src/haz3lcore/projectors/implementations/RichProbe.re` | Module signature `RichProbe`, existential pack types, `pack_renderer` |
| `src/haz3lcore/projectors/implementations/RichProbeRegistry.re` | Registry list + sexp/yojson dispatch for `packed_model` / `packed_action` |
| `src/haz3lcore/projectors/implementations/TableRenderer.re` | Reference implementation (the gradebook-style table view) |
| `src/haz3lcore/projectors/implementations/TableRenderer.rei` | Mandatory interface file (see "Why a `.rei`" below) |
| `src/haz3lcore/projectors/implementations/HtmlRenderer.re` | Renders an HTML-valued sample as the DOM it describes (see `docs/charts.md`) |
| `src/haz3lcore/projectors/implementations/ProbeProj.re` | Probe projector — owns `active_renderer`, drives `ToggleModal` / `RendererAction`, renders the modal overlay and the sample-context-menu entries |

## The `RichProbe` module signature

The full, authoritative signature is in `RichProbe.re` — read it
there. The fields and what each is for:

- **`type value`** — your parsed representation of the probed value
  (e.g., `(headers, rows)` for TableRenderer). The result of `parse`.
- **`type model`** — your per-instance UI state. Should be small and
  serializable; don't store derived data you can recompute from `value`
  at render time.
- **`type action`** — events that mutate `model` (and optionally
  rewrite the surrounding syntax).
- **`parse: (Sort.t, Exp.t) => option(value)`** — the only place that
  defines what shapes your renderer handles. If it returns `Some(_)`,
  the renderer's "View as …" entry appears in the sample context menu
  and `init` / `render` get called. Returning `None` is the standard
  way to opt out for unsupported expressions.
- **`init: value => model`** — fresh state for a newly opened view.
- **`update: (model, action) => model`** — pure state transition. If
  an action wants to *also* rewrite the surrounding syntax, dispatch
  `parent(SetSyntax(seg))` from a UI handler; don't try to thread that
  through `update`.
- **`badge`** — a small icon `Node.t` (typically an inline SVG) shown
  next to the "View as <id>" entry in the sample context menu.
- **`render`** — your view. The named parameters give you everything
  you need: `info` (carries `info.elaborated` and a `utility` record
  with `term_to_seg` / `seg_to_term` / `lift_syntax` / `seg_to_string`),
  `view_seg` (renders embedded segments without recursive projector
  handling — use for cell contents, sub-expressions, etc.), `local`
  (dispatches your `action`), `parent` (dispatches into the surrounding
  editor — most commonly `SetSyntax`).

`TableRenderer.re` is the reference implementation; copy its skeleton
when starting a new renderer.

## How a renderer plugs in

`ProbeProj` doesn't import concrete renderers — it goes through the
registry. The flow:

1. **Discovery.** When the sample context menu opens, `ProbeProj`
   iterates `RichProbeRegistry.renderers`, calls `can_handle` on each
   (which under the hood calls `parse`), and renders a "View as <id>"
   entry for every match.
2. **Open.** Clicking the entry dispatches `ToggleModal` with a fresh
   packed model from `init_model`. `ProbeProj` stores it in
   `probe_model.active_renderer`.
3. **Render.** Each draw, `ProbeProj.modal_overlay` finds the renderer
   by id, calls `render_model`, and wraps the result in a modal
   (`.modal-backdrop` → `.modal` with a close-X).
4. **Inner actions.** Your `~local` callback delivers your renderer's
   `action`; `ProbeProj` wraps it in `RendererAction` and dispatches
   `update_model`, which casts back to your concrete types via the
   registry's `Type.Id.t` witness.
5. **Close.** Clicking the close-X or pressing Escape dispatches
   `ToggleModal(None)`.

## Existential packs and `Type.Id.t`

`packed_model` / `packed_action` are existential records carrying a
string renderer id, a `Type.Id.t` witness allocated by `pack_renderer`,
and the value itself. `pack_renderer` captures the witness in closures
and exposes `update_model` / `render_model` that recover the concrete
types via `Type.Id.provably_equal` — no `Obj.magic`. Mismatches
(e.g., an action from a different renderer reaching your
`update_model`) become no-ops rather than crashes.

Type.Id witnesses aren't serializable, so after a reload the registry
substitutes the renderer's currently-registered witness when decoding.
That means **the string id is what's durable** — don't rename a
renderer id in `RichProbeRegistry.renderers` without thinking about
existing saved state.

## Adding a new renderer

1. **Implement the module.** Create `MyRenderer.re` + `MyRenderer.rei`
   alongside `TableRenderer.re/.rei` and follow that file's skeleton:
   define `value` / `model` / `action` types with `[@deriving (sexp,
   yojson, show)]`, then `parse` / `init` / `update` / `badge` /
   `render`. The `.rei` exposes the abstract types and `include
   RichProbe.RichProbe with type model = ... and type action = ... and
   type value = ...;`.
2. **Register it.** Add one line to `RichProbeRegistry.renderers`:
   `pack_renderer((module MyRenderer), "<id>")`. The string id is what
   gets serialized — pick something stable.
3. **Styles (optional).** Put renderer-specific CSS in
   `src/web/www/style/projectors/proj-<name>.css` and `@import` it from
   the main projector stylesheet.

That's it. The new renderer appears as "View as <id>" in the sample
context menu of any probe whose value `parse` accepts.

## Why the `.rei` is required

`pack_renderer` takes a first-class module constrained on `type
model` / `type action` / `type value`. To produce a module value with
that constrained type, the implementation has to satisfy a module type
whose abstract types are pinned to your concrete ones — and the
cleanest way is via an `.rei` that exposes those types and `include
RichProbe.RichProbe with type ...`. `TableRenderer.rei` is the
canonical example.

If you skip the `.rei`, the deriving-generated sexp/yojson functions
still exist, but the `pack_renderer` call won't typecheck.

## Interacting with the surrounding syntax

If your renderer modifies the probed expression (e.g., TableRenderer's
"drop column" rewrites the underlying `gradebook |> map (...)`),
dispatch `parent(SetSyntax(seg))` from a click/key handler. Build the
segment via `info.utility.lift_syntax` — see
`TableTransforms.to_segment` for the canonical pattern.

## What `parse` actually sees

Rich-probe renderers run against a probe **sample value** — a runtime
`Exp.t` produced by the evaluator. Auto-labeling and type-driven
rearrangement have already happened upstream, so tuples in the value
carry whatever labels the elaborator could infer.

When the probed value changes shape such that `parse` returns `None`,
`ProbeProj` leaves the existing `model` in place; `render_model`
returns `None` and the modal effectively disappears. If your
renderer should *survive* shape changes, keep enough state in `model`
to render without re-parsing — otherwise the default behavior
(gracefully disappearing) is usually right.

## Quick troubleshooting

- **"View as …" entry doesn't appear.** `parse` is returning `None`.
  Add a `print_endline` in `parse` to see what `Exp.t` it's getting
  and check whether the shape matches what you're pattern-matching.
- **Modal opens empty.** `render_model` returned `None`. Either the
  type-id cast failed (renderer id mismatch?) or `parse` failed at
  render time because the value shape changed. Same fix as above.
- **Persisted state didn't restore.** Check the renderer's string id
  matches what's in `RichProbeRegistry.renderers`. If you renamed it,
  add a migration or accept that old saves lose their state.
- **Actions don't update the model.** Verify that `local` is being
  called with your renderer's `action` type. If it's wrapped in
  something else, the registry's cast returns `None` and `update_model`
  becomes a no-op.

## ProbeProj integration points

`ProbeProj.re` owns the small surface that knows about rich probes at
all — search for `rich_probe_items` (the sample-context-menu entries),
`modal_overlay` (the modal chrome around the renderer's view), and
`active_renderer` (the field in `probe_model` holding the open
renderer's packed model). The registry pattern is intentionally kept
thin so `ProbeProj` doesn't depend on any specific renderer.
