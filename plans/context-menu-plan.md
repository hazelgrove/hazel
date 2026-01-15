# Context Menu Improvements Plan

## Status Summary

### ✅ Completed
- **Phase 1: Viewport-Based Directional Opening** - Menu now opens up/down/left/right based on available viewport space
- **Phase 2.1: Click-Outside Close** - Document-level capture-phase listener for single-click UX (closes menu AND activates clicked element)
- **Phase 2.2: Scroll Close** - Menu closes on wheel events (via backdrop)
- **Phase 2.3: Window Blur Close** - Menu closes when switching to another application/window
- **Phase 3: Precise Caret Alignment** - Menu positions based on caret bottom edge with shadow offset
- **Phase 4.3: Animation** - Subtle 0.12s fade+scale animation on open, direction-aware

### ⏳ Not Started
- **Phase 4.1: Keyboard Navigation** - Arrow keys, Enter, type-ahead
- **Phase 4.2: Accessibility** - ARIA roles and focus management

---

## Current State Analysis

### Implementation Overview
- **Location**: [ContextMenu.re](src/web/app/editors/code/ContextMenu.re)
- **Styling**: [editor.css:359-416](src/web/www/style/editor.css#L359-L416)
- **Open/Close Logic**: [CodeEditable.re:98-101, 152-166, 338-344](src/web/app/editors/code/CodeEditable.re)

The context menu currently:
- Uses `position: absolute` within the code-container
- Positions at `caret_point.row + 1` (one row below caret)
- Opens on right-click via `ToggleContextMenu` action
- Closes on Escape or when performing any action
- Has CSS prepared for directional variants (`open-down-right`, etc.) but they're not being applied

### Key Dependencies
- **FloatingElement system**: [FloatingElement.re](src/util/FloatingElement.re) - handles viewport-relative positioning for elements that need to escape `overflow: auto` clipping (used by Backpack)
- **Backpack alignment**: [Backpack.re](src/web/app/editors/decoration/Backpack.re) - reference for precise caret alignment using `complete_bullshit` function which accounts for caret shape (concave/convex directions)
- **Caret metrics**: [CaretDec.re](src/web/app/editors/decoration/CaretDec.re), [ShardDec.re](src/web/app/editors/decoration/ShardDec.re)
  - `caret_width = 0.2`
  - `tip_width = 0.32`
  - `tip_height = 0.5`
  - `shadow_dy = 0.04`
  - Shape adjustments via `ShardDec.shape_adjust()`

---

## Phase 1: Viewport-Based Directional Opening

### Problem
Menu opens downward and rightward regardless of available space, causing scroll when near editor bottom/right edges.

### Solution: Use Viewport Coordinates
The `#main` div (id="main" in [Page.re:740](src/web/app/Page.re#L740)) is the scrollable viewport. Position decisions should be made relative to this viewport, not the code-container.

### Implementation

#### 1.1 Add Viewport Measurement Utility
Create a function to get available space in each direction from a point:

```reason
/* In ContextMenu.re or a new ViewportUtil module */
type available_space = {
  above: float,  /* pixels from point to top of #main */
  below: float,  /* pixels from point to bottom of #main */
  left: float,   /* pixels from point to left of #main */
  right: float,  /* pixels from point to right of #main */
};

let get_available_space = (point: Point.t, font_metrics: FontMetrics.t): available_space => {
  /* Get #main viewport rect via JsUtil.get_elem_by_id_opt("main") */
  /* Get code-container rect for coordinate conversion */
  /* Calculate pixel position of caret point */
  /* Return distances to each edge */
};
```

#### 1.2 Determine Opening Direction
```reason
type open_direction = {
  vertical: [ `Up | `Down ],
  horizontal: [ `Left | `Right ],
};

let menu_height_estimate = 200.0;  /* px - could measure dynamically */
let menu_width_estimate = 180.0;   /* px - based on min-width: 160px + padding */

let determine_direction = (space: available_space): open_direction => {
  vertical: space.below >= menu_height_estimate ? `Down : `Up,
  horizontal: space.right >= menu_width_estimate ? `Right : `Left,
};
```

#### 1.3 Apply Position and Direction Class
Update `pos_attr` to:
1. Calculate position based on direction (flip anchor point for up/left)
2. Add appropriate direction class for CSS border-radius

```reason
let pos_attr = (point: Point.t, font_metrics: FontMetrics.t, direction: open_direction) => {
  let base_left = Float.of_int(point.col) *. font_metrics.col_width;
  let base_top = Float.of_int(point.row + 1) *. font_metrics.row_height;

  let (left, top) = switch (direction) {
    | {vertical: `Down, horizontal: `Right} => (base_left, base_top)
    | {vertical: `Down, horizontal: `Left} => (base_left, base_top)  /* CSS handles offset */
    | {vertical: `Up, horizontal: `Right} => (base_left, base_top -. menu_height)
    | {vertical: `Up, horizontal: `Left} => (base_left, base_top -. menu_height)
  };

  let dir_class = switch (direction) {
    | {vertical: `Down, horizontal: `Right} => "open-down-right"
    | {vertical: `Down, horizontal: `Left} => "open-down-left"
    | {vertical: `Up, horizontal: `Right} => "open-up-right"
    | {vertical: `Up, horizontal: `Left} => "open-up-left"
  };

  /* Return both style and class attrs */
};
```

#### 1.4 CSS Updates for Left-Opening
Add CSS for horizontal positioning when opening left:
```css
.context-menu.open-down-left,
.context-menu.open-up-left {
  transform: translateX(-100%);  /* Anchor right edge to caret instead of left */
}
```

### Why Viewport-Based (Validation)
This is the correct approach because:
1. The `#main` div has `overflow: auto` and defines the visible scrollable area
2. Menu appearing outside visible viewport causes forced scroll (the current problem)
3. Viewport coordinates are stable regardless of scroll position
4. This matches how production context menus work (VS Code, browsers, etc.)

---

## Phase 2: Closing Behavior Improvements

### Current Behavior
- Closes on Escape key
- Closes when any menu item is clicked (via action dispatch)
- Does NOT close on click outside

### Best Practices (from UX research)
Per [NN/g guidelines](https://www.nngroup.com/articles/contextual-menus-guidelines/) and [WCAG 1.4.13](https://www.w3.org/WAI/WCAG21/Understanding/content-on-hover-or-focus.html):
- **Dismissible**: Must close on Escape (already implemented)
- **Click-outside**: Should close when clicking anywhere outside the menu
- **Focus loss**: Should close when focus moves elsewhere
- **Scroll**: Should close when user scrolls (menu position becomes stale)
- **Re-trigger**: Opening a new context menu should close the old one (already handled - single menu state)

### Implementation

#### 2.1 Click-Outside Detection

##### Current Implementation (Backdrop Approach)
We currently use an invisible backdrop element:
```reason
/* In CodeEditable.re - renders when context_menu is true */
Node.div(
  ~attrs=[
    Attr.classes(["context-menu-backdrop"]),
    Attr.on_pointerdown(_ => inject(ToggleContextMenu)),
    Attr.on_wheel(_ => inject(ToggleContextMenu)),
  ],
  [],
)
```

```css
.context-menu-backdrop {
  position: fixed;
  inset: 0;
  z-index: calc(var(--context-menu-z) - 1);
}
```

**Problem**: The backdrop sits below the top/bottom bars (which have higher z-index), so clicking those areas doesn't close the menu.

##### Best Practice: Document-Level Event Listener (Recommended)

Per [Angular Material Issue #9320](https://github.com/angular/components/issues/9320) and standard UX research:
> "When a menu is open, any click event outside of the menu... is expected to work from the first time. Menu should be closed along with the action handled by the clicked item."

The backdrop approach with high z-index creates a "two click" problem (click to close, click to activate), which is considered bad UX.

**Recommended approach**: Use a **capture-phase document event listener**:

```javascript
// Conceptual - needs adaptation for Incr_dom/virtual-dom
document.addEventListener('pointerdown', (e) => {
  if (!menuElement.contains(e.target)) {
    closeMenu();
  }
  // Event continues to propagate - target element still receives it
}, { capture: true });
```

Key points:
- **Capture phase** (`{ capture: true }`) runs before the event reaches target elements
- The event is NOT stopped, so the clicked element still receives and handles it
- Menu closes AND the clicked action fires - single click behavior

##### Implementation Options for Incr_dom

**Option A: Effect-based listener management**
Add/remove a document listener when menu state changes:

```reason
/* In CodeEditable.re or via a hook system */
let menu_close_effect = (context_menu: bool, inject) => {
  if (context_menu) {
    /* Add capture-phase listener */
    let handler = (evt) => {
      let target = evt##.target;
      if (!JsUtil.element_matches(target, ".context-menu, .context-menu *")) {
        inject(ToggleContextMenu);
      }
    };
    Dom_html.document##addEventListener(
      Js.string("pointerdown"),
      Js.wrap_callback(handler),
      Js.Unsafe.obj([|("capture", Js.Unsafe.inject(Js._true))|])
    );
    /* Return cleanup function */
    () => Dom_html.document##removeEventListener(...);
  }
};
```

**Option B: Global hook in Main.re or Page.re**
Register a global pointerdown handler that checks for open context menus and closes them.

**Option C: Keep backdrop but raise z-index**
Simpler but has the two-click problem for toolbar elements:
```css
.context-menu-backdrop {
  z-index: 9999; /* Above everything except context menu */
}
```

##### Recommendation
Option A (effect-based listener) is the most correct UX but requires more implementation work. Option C is a quick fix if the two-click behavior is acceptable for toolbar elements.

#### 2.2 Close on Scroll
Add scroll listener when menu is open:

```reason
/* In CodeEditable.re view, when context_menu is true */
Attr.on_scroll(_ =>
  if (model.context_menu) { inject(ToggleContextMenu) } else { Effect.Ignore }
)
```

Or attach to `#main` scroll event (preferred - catches all scroll):
```reason
/* Set up effect to close menu on #main scroll */
/* This could be a lifecycle effect that adds/removes listener based on context_menu state */
```

#### 2.3 Close on Window Blur
Close menu when browser window loses focus:
```reason
/* Global effect or hook to listen for window blur */
Dom_html.window##.onblur := Js.wrap_callback(_ => {
  if (model.context_menu) { inject(ToggleContextMenu) }
});
```

---

## Phase 3: Precise Caret Alignment

### Current Problem
The menu overlaps the bottom of the caret by a few pixels rather than sitting flush below it.

### Root Cause Analysis
1. Current positioning: `top = (point.row + 1) * row_height`
2. This doesn't account for:
   - Caret shape extensions (concave/convex tips extend beyond grid cell)
   - The caret's actual bottom edge position
   - Border/outline of menu

### Reference: Backpack Alignment
The [Backpack.re](src/web/app/editors/decoration/Backpack.re) uses `complete_bullshit` (lines 105-124) to calculate precise horizontal alignment based on caret direction. Key insight: different caret shapes require different offsets.

### Caret Geometry (from CaretDec.re, ShardDec.re)
- Base caret occupies one row height
- `shadow_dy = 0.04` - shadow extends below caret
- `tip_height = 0.5` - but this shapes the chevron WITHIN the row, doesn't extend beyond
- `caret_width = 0.2`

**Key insight discovered during implementation**: The chevron tips don't extend beyond the row boundary. Tracing through `ShardDec.chonky_path_base` with `height=0` (used for carets), the path draws a diamond shape that spans exactly y=0 to y=1.0 (one row). The `tip_height` controls the chevron shape, not an extension.

### Implementation (Completed)

#### 3.1 Calculate Caret Bottom Edge
```reason
/* Simplified - no tip_extension needed since chevrons stay within row */
let caret_bottom_offset = (font_metrics: FontMetrics.t): float => {
  let row_height = font_metrics.row_height;
  let shadow = ShardDec.shadow_dy *. row_height;
  row_height +. shadow;
};
```

#### 3.2 Update Positioning
```reason
let pos_attr = (point: Point.t, font_metrics: FontMetrics.t, caret_shape: option(Direction.t)) => {
  let left = Float.of_int(point.col) *. font_metrics.col_width;

  /* Position menu starting exactly at caret's bottom edge */
  let caret_top = Float.of_int(point.row) *. font_metrics.row_height;
  let top = caret_top +. caret_bottom_offset(caret_shape, font_metrics);

  /* Add small gap (1-2px) to avoid visual collision */
  let gap = 1.0;

  Printf.sprintf(
    "position: absolute; left: %fpx; top: %fpx;",
    left,
    top +. gap
  )
};
```

#### 3.3 Horizontal Alignment for Shaped Carets
For concave/convex carets, the menu's corner should align with where the caret point actually is (accounting for the chevron shape):

```reason
let caret_horizontal_offset = (caret_shape: option(Direction.t)): float => {
  switch (caret_shape) {
    | None => 0.0
    | Some(Left) => -. ShardDec.tip_width *. font_metrics.col_width  /* Concave left */
    | Some(Right) => ShardDec.tip_width *. font_metrics.col_width   /* Convex right */
  }
};
```

#### 3.4 CSS Adjustment
Remove any border/outline from the calculation side:
```css
.context-menu {
  /* Use box-sizing to include border in dimensions */
  box-sizing: border-box;

  /* Or use outline instead of border (doesn't affect layout) */
  border: none;
  outline: 0.6px solid var(--menu-outline);
}
```

---

## Phase 4: Additional UX Improvements

### 4.1 Keyboard Navigation
Currently missing. Best practice is to support:
- Arrow keys to move selection within menu
- Enter to activate selected item
- Type-ahead to jump to items starting with typed letter

```reason
/* Add to key handler when context_menu is open */
| D("ArrowDown") => Some(ContextMenuNav(Down))
| D("ArrowUp") => Some(ContextMenuNav(Up))
| D("Enter") => Some(ContextMenuActivate)
```

This requires adding a `selected_index` to the context menu state.

### 4.2 Accessibility
- Add `role="menu"` and `role="menuitem"` attributes
- Add `aria-label` for the menu
- Ensure focus management (focus should move to menu when opened)

```reason
div(
  ~attrs=[
    Attr.create("role", "menu"),
    Attr.create("aria-label", "Context menu"),
    /* ... */
  ],
  /* ... */
)
```

### 4.3 Animation
Add subtle open animation for polish:
```css
.context-menu {
  animation: context-menu-open 0.1s ease-out;
}

@keyframes context-menu-open {
  from {
    opacity: 0;
    transform: scale(0.95);
  }
  to {
    opacity: 1;
    transform: scale(1);
  }
}
```

### 4.4 Visual Polish
- Ensure consistent shadows with other menus (nut-menu uses `box-shadow: 0px 10px 20px var(--menu-shadow)`)
- Match border-radius treatment with direction (already in CSS, just need to apply classes)

---

## Implementation Order

### Priority 1: Directional Opening (Phase 1)
Most impactful fix - prevents scroll issues that disrupt workflow.

**Files to modify:**
- [ContextMenu.re](src/web/app/editors/code/ContextMenu.re) - positioning logic
- [editor.css](src/web/www/style/editor.css) - add translateX for left-opening

### Priority 2: Click-Outside Close (Phase 2.1)
Common user expectation that's currently missing.

**Files to modify:**
- [CodeEditable.re](src/web/app/editors/code/CodeEditable.re) - event handling

### Priority 3: Precise Alignment (Phase 3)
Visual polish that makes the menu feel more professional.

**Files to modify:**
- [ContextMenu.re](src/web/app/editors/code/ContextMenu.re) - positioning with caret metrics

### Priority 4: Scroll Close (Phase 2.2)
Prevents stale menu position.

### Priority 5: Keyboard Navigation (Phase 4.1)
Nice-to-have for power users and accessibility.

### Deferred: Full Accessibility (Phase 4.2)
Important but can be addressed in a follow-up pass.

---

## Testing Checklist

### Directional Opening
- [x] Menu opens downward when plenty of space below
- [x] Menu opens upward when near bottom edge of #main viewport
- [x] Menu opens rightward when plenty of space to right
- [x] Menu opens leftward when near right edge
- [x] Corner border-radius matches direction (sharp corner at anchor point)

### Closing Behavior
- [x] Escape closes menu
- [x] Clicking menu item closes menu (and performs action)
- [x] Clicking outside menu closes menu (document-level capture-phase listener)
- [x] Scrolling closes menu (wheel events)
- [x] Window blur closes menu

### Caret Alignment
- [x] Menu sits flush below straight caret (no overlap)
- [x] Menu sits flush below concave-left caret
- [x] Menu sits flush below convex-right caret
- [x] Small consistent gap between caret and menu (set to 0 per user preference)

### Edge Cases
- [x] Menu near top-left corner (should open down-right)
- [x] Menu near bottom-right corner (should open up-left)
- [ ] Menu when code-container is scrolled (needs verification)
- [x] Menu on first/last line of editor
- [x] Menu on very short/long lines

---

## Notes

### On Viewport vs Container Positioning
The context menu should use viewport-relative calculations (checking `#main` bounds) but remain positioned with `position: absolute` within the code-container. This is because:
1. The menu should scroll with the code (unlike the backpack which needs to escape clipping)
2. Absolute positioning within container keeps coordinates simple
3. We just need viewport awareness for direction decisions, not actual fixed positioning

If we find overflow clipping becomes an issue (menu gets cut off), we can adopt the FloatingElement pattern used by Backpack.

### On Menu Height Measurement
Rather than estimating menu height, we could:
1. Render menu hidden, measure, then show
2. Use a ref to measure after first render and reposition
3. Keep a reasonable estimate and accept minor imperfection

For initial implementation, estimation is simpler and sufficient.

---

## Implementation Details (Added During Development)

### Document-Level Click-Outside Listener

Implemented in `JsUtil.ContextMenuListener` module:

**Key design decisions:**
1. Uses **capture phase** (`{ capture: true }`) so the listener runs before target element handlers
2. This enables **single-click UX**: clicking outside closes the menu AND activates the clicked element
3. Uses `Bonsai.Effect.Expert.handle` to dispatch virtual-dom effects from outside the event loop
4. Includes ancestor check to avoid closing when clicking inside the menu itself

**Files:**
- [JsUtil.re:295-395](src/util/JsUtil.re#L295-L395) - `ContextMenuListener` module
- [CodeEditable.re:241-245](src/web/app/editors/code/CodeEditable.re#L241-L245) - `sync` call in view

**Usage pattern:**
```reason
/* Called on every render with current menu state */
JsUtil.ContextMenuListener.sync(
  selected && model.context_menu,
  inject(ToggleContextMenu),
);
```

### Direction-Aware Animation

Each opening direction has its own keyframe animation to maintain the correct transform while animating:

```css
/* Example for open-down-left direction */
@keyframes context-menu-open-down-left {
  from { opacity: 0; transform: translateX(-100%) scale(0.95); }
  to { opacity: 1; transform: translateX(-100%) scale(1); }
}
```

The `transform-origin` is set per-direction so the scale animation appears to grow from the anchor point (caret position).
