# Scroll Behavior

Last updated 2026-02-14

This document describes how the Hazel editor manages viewport scrolling
during editing, selection, and navigation. The scroll container is the
`#main` element (CSS `overflow-y: auto`).

## Two Scroll Systems

Hazel uses two independent scroll mechanisms for different interaction
modes. This separation follows the pattern used by CodeMirror, ProseMirror,
and VS Code.

### 1. Scroll-to-caret (keyboard/programmatic)

After certain actions, the viewport scrolls to bring the `#caret` SVG
element into view. This is controlled by the `scroll_active` flag returned
from `Updated.return` in `CodeEditable.Update.update`.

**When it fires:** Keyboard movement (`Move(Local/Vertical/...)`), keyboard
selection (`Select(Resize(Local/Vertical/...))`), typing (`Insert`,
`Destruct`), paste, buffer accept, and other editing actions.

**When it does NOT fire:** Mouse clicks (`Move(Point(...))`), mouse drag
selection (`Select(Resize(Point(...)))`), `Select(All)`, probe navigation,
projector actions, and unselect.

**How it works:** After an action with `scroll_active=true`, the framework
calls `JsUtil.scroll_cursor_into_view_if_needed`, which finds the `#caret`
element and scrolls the `#main` container just enough to make it visible.
This is a one-shot adjustment per action -- it does not continuously track
the caret.

**Key files:**
- `src/web/app/editors/code/CodeEditable.re` -- `scroll_active` flag in
  `Updated.return`
- `src/util/JsUtil.re` -- `scroll_cursor_into_view_if_needed`

### 2. Edge-scrolling (mouse drag)

During drag-select, when the pointer is near or past the top/bottom edge
of the scroll container, the viewport scrolls continuously in that
direction. The selection updates in sync with the scroll.

**When it fires:** During an active drag-select (left button held, pointer
has moved from the click origin), when the pointer enters the edge zone
or goes past the container boundary.

**How it works:** `EdgeScroll` maintains a `setInterval` timer. On each
tick:

1. Compute a scroll delta based on pointer proximity to the container edge
2. Scroll the `#main` container by that delta
3. Invoke the `on_scroll` callback, which recomputes the grid position for
   the current pixel coordinates (accounting for the new scroll offset via
   a fresh `getBoundingClientRect`) and dispatches a selection update
   through `Bonsai.Effect.Expert.handle`

The scroll speed is proportional to how deep into the edge zone the pointer
is (zero at the inner boundary, maximum at the container edge). When the
pointer is *past* the container edge entirely, scrolling runs at maximum
speed.

**Parameters** (top of `EdgeScroll.re`):
- `margin_px` -- width of the edge zone inside the container (pixels)
- `max_speed_px` -- maximum scroll per tick (pixels)
- `interval_ms` -- tick rate (milliseconds between ticks)

**Lifecycle:**
- **Start:** `EdgeScroll.update` is called from `drag_select` on each
  `mousemove` during an active drag. It starts the timer if the pointer
  is in the edge zone.
- **Stop:** `EdgeScroll.stop` is called from `toggle_button` (pointer-up)
  and from stuck-state recovery.
- **Pause/resume:** If the pointer moves out of the edge zone back to the
  center, the timer is cleared. It restarts if the pointer re-enters the
  edge zone.

**Key files:**
- `src/web/EdgeScroll.re` -- timer management, delta computation, tick logic
- `src/web/app/editors/code/CodeEditable.re` -- `drag_select` (calls
  `EdgeScroll.update`), `toggle_button` (calls `EdgeScroll.stop`)

## Pointer Event Handling

Mouse interactions use the Pointer Events API with manual state tracking
for multi-click detection.

**Pointer capture:** On single-click pointerdown, `setPointerCapture` is
called on the `.code-container` element. This ensures mousemove/pointerup
events continue flowing to the editor even when the pointer exits the
editor element or browser window -- necessary for drag-select to work when
flinging the mouse to the screen edge.

**Button state:** `Pointer.Event.t` carries both `button` (which button
changed) and `buttons` (bitmask of currently held buttons, read in
real-time from the DOM event). The `drag_select` handler uses
`buttons land 1 != 0` to check if the left button is held, and includes
stuck-state recovery: if `buttons` says no left button but `MouseState`
thinks the button is down (missed pointerup), it resets both `MouseState`
and `EdgeScroll`.

**Key files:**
- `src/web/Pointer.re` -- `Event.t` record, `MkState` for click tracking
- `src/web/app/editors/code/CodeEditable.re` -- `PointerCapture` module,
  `move_or_select`, `drag_select`, `toggle_button`

## Caret Animation

Caret movement can be animated (smooth transition rather than instant
teleport). The `Action.should_animate` function determines which actions
trigger animation. Most actions animate, including `Select(Resize(_))`.
The animation is requested via `Animation.request` in
`CodeEditable.Update.update` when `settings.core.flip_animations` is on.

**Key files:**
- `src/haz3lcore/zipper/action/Action.re` -- `should_animate`
- `src/web/app/editors/code/CodeEditable.re` -- animation request in
  `Update.update`
