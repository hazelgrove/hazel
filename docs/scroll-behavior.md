# Scroll Behavior

Last updated 2026-02-14

## The Problem

When an editor action moves the caret, the viewport may need to scroll to
keep the caret visible. But "keep the caret visible" means different things
depending on how the user is interacting:

- **Keyboard actions** move the caret to a place the user can't see yet.
  The viewport should follow the caret, because the caret *is* the user's
  point of attention. This applies equally to plain movement (arrow keys),
  selection (Shift+arrow), and jump commands (go-to-definition, next
  hole) — the caret is moving somewhere and the user needs to see where.

- **Mouse clicks** place the caret where the user is already looking. The
  viewport should stay put. If it jumped to the caret's previous position,
  or to wherever the caret logically ends up after the action, the thing
  the user just clicked on would vanish.

- **Mouse drag selection** is a third case. The user is extending a
  selection by dragging, and may drag past the visible area. The viewport
  needs to scroll to follow, but smoothly and continuously — not by
  jumping to the caret position after each selection-resize action.

Conflating these cases causes scroll jumps: you scroll away from the
caret, click somewhere visible, and the viewport snaps back. Or you start
a drag selection and the viewport lurches. The solution is to handle
keyboard-driven and mouse-driven scrolling as separate systems.

## Scroll-to-Caret (Keyboard, Editing, and Jump Actions)

When you press an arrow key, type a character, extend a selection with
Shift+Down, or jump to a definition with F12, the caret moves to a
position that may be off-screen. The scroll-to-caret system ensures you
can always see where the caret ended up.

### Actions that trigger scroll-to-caret

The `scroll_active` flag (set per-action in `CodeEditable.Update.update`)
controls this. The key rule is: **`Point(_)` actions — those originating
from mouse position — do not scroll.** Everything else does:

- **Local movement:** Arrow keys, Home/End (`Move(Local/Vertical/...)`).
  The caret moves by one character, token, or line — usually staying
  on-screen, but scroll-to-caret catches it when it crosses a viewport
  boundary.

- **Selection resize:** Shift+arrow, Shift+Home/End
  (`Select(Resize(Local/Vertical/...))`). The leading edge of the
  selection extends, and the viewport follows it. Since keyboard actions
  are discrete (one keypress = one movement), the one-shot scroll after
  each action produces smooth line-at-a-time following.

- **Jump commands:** These can move the caret arbitrarily far:
  - Go-to-definition (`Move(Goal(BindingSiteOfIndicatedVar))`) — F12,
    Cmd+click, or context menu. Jumps to wherever the indicated variable
    is bound, which may be hundreds of lines away.
  - Jump to tile (`Move(Goal(TileId(id)))`) — clicking an error in the
    sidebar, a probe link, or an exercise test result. Jumps to a specific
    syntax node by ID.
  - Next/previous hole (`Move(Goal(Hole(Right/Left)))`) — Tab key or
    keyboard shortcuts. Jumps to the nearest hole in the given direction.

  Jump commands are the most important case for scroll-to-caret because
  the destination is often completely off-screen. (Note: Cmd+click first
  dispatches `Move(Point(_))` to place the caret at the click site, then
  `Move(Goal(BindingSiteOfIndicatedVar))` to jump. The first action
  doesn't scroll; the second does — so the viewport follows the jump,
  not the click.)

- **Editing actions:** Insert, delete, paste, etc. These move the caret
  as a side effect of editing.

- **Actions that do NOT scroll:** `Move(Point(_))` (mouse click),
  `Select(Resize(Point(_)))` (mouse drag), `Select(All)`, projector
  actions, and probe actions other than step-into.

### How scroll-to-caret works

After rendering, the `after_display` hook in `Main.re` calls
`JsUtil.scroll_cursor_into_view_if_needed`, which:

1. Finds the `#caret` SVG element in the DOM
2. Finds the nearest scrollable ancestor (walks up the DOM looking for an
   element with `scrollHeight > clientHeight`)
3. Computes a **margin** of 10% of the container height
4. If the caret is above `container.top + margin`, scrolls up just enough
   to place it at that boundary. If below `container.bottom - margin`,
   scrolls down. Otherwise does nothing.

The margin means the caret never ends up flush against the viewport edge —
there's always context visible above or below. This is a one-shot
adjustment: it fires once per action, scrolls the minimum amount needed,
and doesn't continuously track.

**Key files:**
- `CodeEditable.re` — `scroll_active` in `Updated.return`
- `Main.re` — `scroll_to_caret` ref, `after_display` hook
- `JsUtil.re` — `scroll_cursor_into_view_if_needed`,
  `find_scroll_container`, `adjust_scroll`

**Tuning:** The margin is `margin_ratio = 0.10` (10%) in
`scroll_cursor_into_view_if_needed`. Increase for more context around the
caret after scroll; decrease to allow the caret closer to the viewport
edge before triggering.

## Edge-Scrolling (Mouse Drag)

During drag-select, we need a different kind of viewport following:
continuous, speed-proportional scrolling driven by pointer position. The
`EdgeScroll` module handles this.

### Why not scroll-to-caret for drag?

Scroll-to-caret fires once per action and scrolls the minimum amount to
show the caret. During a drag, `Select(Resize(Point(...)))` actions fire
on every mousemove — dozens per second — each jumping the viewport to the
caret. The result is jerky and fights with the user's mouse movement.
Worse, when the user holds the mouse stationary at the screen edge, no
mousemove fires, so nothing scrolls at all.

Edge-scrolling solves both problems: a steady timer scrolls at a
predictable speed, and it keeps running even when the mouse isn't moving.

### How it works

When the pointer is near the top or bottom edge of the `#main` container
(or past it entirely), `EdgeScroll` starts a `setInterval` timer. Each
tick:

1. Scrolls the container by a delta proportional to how close the pointer
   is to the edge (zero at the inner boundary of the edge zone, maximum
   at the container edge, maximum when past it entirely)
2. Invokes an `on_scroll` callback that recomputes the grid position for
   the pointer's pixel coordinates — the new scroll offset changes the
   result of `getBoundingClientRect`, so the same pixel position maps to
   a different line after scrolling — and dispatches a selection update
   via `Bonsai.Effect.Expert.handle`

### Lifecycle

- **Starts** when `drag_select` detects the pointer in the edge zone
  during an active drag
- **Stops** on pointer-up or stuck-state recovery
- **Pauses** when the pointer moves back to the center (timer cleared;
  restarts if the pointer re-enters the edge zone)

**Key files:**
- `EdgeScroll.re` — timer, delta computation, tick, parameters
- `CodeEditable.re` — `drag_select` (calls `EdgeScroll.update`),
  `toggle_button` (calls `EdgeScroll.stop`)

**Tuning** (top of `EdgeScroll.re`):
- `margin_px` — width of the activation zone inside the container edge
- `max_speed_px` — pixels scrolled per tick at maximum
- `interval_ms` — tick interval (lower = smoother but more CPU per drag)

## Pointer Capture and Button Tracking

Drag-select needs to work even when the pointer leaves the editor element
or browser window — for example, flinging the mouse to the screen edge to
trigger edge-scrolling. `setPointerCapture` on the `.code-container`
element ensures pointer events keep flowing to the editor during a drag.

The `buttons` bitmask on `Pointer.Event.t` provides real-time button state
from the DOM, used to guard `drag_select` and to detect stuck state: if
`buttons` says no left button but `MouseState` thinks the button is down
(e.g., a missed pointerup from alt-tab), both `MouseState` and
`EdgeScroll` are reset.

**Key files:**
- `Pointer.re` — `Event.t`, `MkState` (multi-click detection)
- `CodeEditable.re` — `PointerCapture`, `move_or_select`, `drag_select`

## Caret Animation

`Action.should_animate` controls whether the caret animates (smooth
transition) or teleports on each action. Most actions animate. This is
independent of scrolling — animation moves the caret SVG element within
the code layout, while scrolling moves the viewport container.

**Key files:**
- `Action.re` — `should_animate`
- `CodeEditable.re` — `Animation.request` in `Update.update`
