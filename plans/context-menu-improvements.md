# Context Menu Improvements Plan

**Status:** In progress
**Last Updated:** 2026-01-15
**Estimated Effort:** 2-4 hours for high-priority items (Steps 1-4)

## Implementation Progress

### ✅ Completed

**Step 2: Escape Key Handler** - [CodeEditable.re:150-167](src/web/app/editors/code/CodeEditable.re#L150-L167)
- Added Escape key support to close the context menu
- Implementation: Check for Escape in `handle_key_event` when `model.context_menu == true`
- Returns `Some(Update.ToggleContextMenu)` to close the menu

### ❌ Attempted but Reverted

**Steps 3-4: Viewport-Aware Positioning**
- First attempt used `visible_rows` (viewport culling data) and estimated column widths
- **Problem:** Mixed up coordinate systems - `point.row` is document position, not viewport position
- When editor is scrolled, menu appeared in wrong location (many rows away from caret)
- Horizontal positioning used arbitrary estimate (100 columns) which ignored actual window size
- **Lesson learned:** Smart positioning needs actual DOM/screen-space measurements, not document coordinates

### 📋 Still TODO

1. **Smart positioning** - Needs proper DOM-based approach:
   - Query actual `getBoundingClientRect()` of editor container (`#main` or `.code-container`)
   - Calculate caret position in screen space (accounting for scroll)
   - Determine available space from real DOM bounds, not measured code content
   - CSS classes for border-radius are already in place in editor.css

2. **Pixel-perfect alignment** - Fix ~1px overlap with straight cursor

3. **Shaped caret alignment** - Align menu corner with caret tip using `ShardDec.shape_adjust`

4. **Click-outside detection** - Close menu when clicking outside editor area

## Overview

This plan addresses UX improvements for Hazel's context menu, focusing on:
1. **Smart positioning** - Auto-detect viewport boundaries and open up/left as needed
2. **Better close behavior** - Add Escape key, improve click-outside detection
3. **Pixel-perfect alignment** - Fix overlap issue and align precisely with caret shapes

The plan includes detailed implementation guidance, reference code locations, and a step-by-step approach prioritized by impact.

## Current Implementation Analysis

### Files Involved
- [src/web/app/editors/code/ContextMenu.re](src/web/app/editors/code/ContextMenu.re) - Main menu logic and rendering
- [src/web/www/style/editor.css](src/web/www/style/editor.css#L359-L402) - Context menu styles
- [src/web/app/editors/code/CodeEditable.re](src/web/app/editors/code/CodeEditable.re) - Menu lifecycle (open/close)
- [src/web/app/editors/decoration/CaretDec.re](src/web/app/editors/decoration/CaretDec.re) - Caret rendering and metrics
- [src/web/app/editors/decoration/ShardDec.re](src/web/app/editors/decoration/ShardDec.re) - Caret shape calculations

### Current Behavior
1. **Positioning**: Menu always appears one row below caret (`point.row + 1`) using absolute positioning
2. **No viewport detection**: Menu always opens downward and rightward regardless of available space
3. **Close behavior**: Menu closes when any editor action is performed (clicking anywhere in editor)
4. **Caret alignment**: Menu positioned at caret point but has slight overlap/gap issues
5. **Caret shapes**: Hazel's structure editor has three caret types:
   - Straight (shape = None, offset = 0)
   - Concave/left-pointing (shape = Some(Left), concave_offset = 0.256)
   - Convex/right-pointing (shape = Some(Right), convex_offset = 0.192)
   - Caret tip_height = 0.5

### Reference Implementation: Backpack Caret Alignment

The **backpack** ([src/web/app/editors/decoration/Backpack.re](src/web/app/editors/decoration/Backpack.re)) is an excellent reference for precise caret alignment. It already "jumps through hoops" to align perfectly with the caret, particularly the pole element.

**Key insights from backpack implementation:**

1. **Uses `ShardDec.shape_adjust` for shape offsets** (lines 113-120)
   - The backpack reuses the same shape adjustment logic as the caret drawing
   - This ensures consistency with how the caret is visually rendered

2. **Complex positioning logic** (lines 105-124: `complete_bullshit` function)
   ```reason
   let complete_bullshit = (~caret_d: option(Direction.t), ~ind_d: option(Direction.t)) =>
     (-1.) +. (
       switch (caret_d) {
       | None => 0.
       | Some(Left) =>
         switch (ind_d) {
         | Some(Left) => ShardDec.shape_adjust(Left, Some(Left)) +. 3.0
         | Some(Right) => ShardDec.shape_adjust(Right, Some(Left)) +. 2.0
         | _ => 2.5
         }
       | Some(Right) =>
         switch (ind_d) {
         | Some(Left) => ShardDec.shape_adjust(Left, Some(Right)) -. 2.0
         | Some(Right) => ShardDec.shape_adjust(Right, Some(Right)) -. 3.0
         | _ => (-2.0)
         }
       }
     );
   ```

3. **Considerations for context menu:**
   - The backpack accounts for both `caret_d` (caret direction/shape) AND `ind_d` (indicated piece direction)
   - The context menu likely only needs `caret_d` since it's purely positioning relative to the caret
   - The base offset of `-1.0` and additional fudge factors (+3.0, +2.0, etc.) may be backpack-specific
   - However, the pattern of using `ShardDec.shape_adjust` is directly applicable

4. **What to borrow:**
   - Use `Zipper.Caret.direction(z)` to get the current caret shape (already used in ContextMenu.re via pos_attr)
   - Use `ShardDec.shape_adjust(side, shape)` to calculate horizontal offset
   - For context menu opening right: `ShardDec.shape_adjust(Left, caret_shape)` (adjusting from left side)
   - For context menu opening left: `ShardDec.shape_adjust(Right, caret_shape)` (adjusting from right side)
   - May need minimal additional fudging, but start with just the shape_adjust logic

**Implementation note:** The context menu positioning can be simpler than the backpack since it doesn't need to coordinate multiple elements (pole, genie, flag) with complex vertical displacement.

## Issues to Address

### High Priority

1. **Smart directional positioning**
   - Menu should open upward when near bottom of viewport
   - Menu should open leftward when near right edge of viewport
   - Should account for menu dimensions and available space in all directions

2. **Close behavior gaps**
   - Currently only closes on editor actions or toggling
   - Missing: Escape key support
   - Missing: Click outside detection (clicks on non-editor areas)
   - Missing: Close when selecting a menu item (currently relies on action triggering)

3. **Pixel-perfect caret alignment**
   - Menu currently overlaps cursor by ~1px
   - Should be flush with bottom edge of cursor for straight cursor
   - Menu outline/border may be contributing to misalignment

### Medium Priority

4. **Adaptive positioning for shaped carets**
   - Account for concave (left-pointing) caret offset
   - Account for convex (right-pointing) caret offset
   - Menu corner should align with caret tip, not just the base position

### Nice to Have

5. **Visual polish**
   - Smooth open/close transitions
   - Subtle animation when repositioning
   - Visual indicator of which direction menu opened

## Best Practices Research

### From NN/g and Industry Standards

**Positioning:**
- Auto-position based on available viewport space
- Flip vertically/horizontally when approaching edges
- Use dynamic positioning (Popper.js style logic)
- Position with slight offset from trigger element

**Close Behavior:**
- Close on: clicking outside, selecting option, Escape key
- Don't auto-close on mouse leave (can frustrate users mid-selection)
- Support keyboard navigation (arrow keys, Escape)

**Interaction:**
- Keep 5-10 items to avoid scrolling (✓ already doing well)
- Only show relevant actions (✓ already doing well)
- Ensure keyboard accessibility
- Maintain consistent behavior

**Content:**
- Position near related content for context (✓ doing well - caret-aligned)
- Use clear, descriptive labels (✓ doing well)
- Group related actions with dividers (✓ doing well)

## Implementation Plan

### Phase 1: Smart Positioning (High Impact)

#### 1.1 Add viewport detection
- Measure editor viewport dimensions
- Calculate menu dimensions (width, height)
- Determine available space in all four directions from caret
- Choose optimal opening direction (prefer down+right, fallback to up/left as needed)

#### 1.2 Implement multi-directional positioning
- Update `pos_attr` in ContextMenu.re to support four modes:
  - DownRight (default): top-left corner at caret bottom
  - DownLeft: top-right corner at caret bottom
  - UpRight: bottom-left corner at caret top
  - UpLeft: bottom-right corner at caret top
- Add CSS classes for each positioning mode to handle border-radius appropriately
- Calculate offsets accounting for caret width and menu dimensions

#### 1.3 Refine base positioning
- Fix the ~1px overlap issue with straight cursor
- Ensure menu top edge aligns exactly with caret bottom edge
- Check if menu outline/border needs adjustment in positioning calculation
- Position should be: `caret_bottom_pixel_position` not `caret_bottom_position - 1px`

### Phase 2: Enhanced Close Behavior

#### 2.1 Add Escape key handler
- In CodeEditable.re, handle Escape key when context_menu is true
- Close menu and return focus to editor

#### 2.2 Improve click-outside detection
- Current behavior closes on editor clicks (✓)
- Add handler for clicks on other UI elements (backpack, sidebar, etc.)
- Consider using a capture-phase event listener pattern

#### 2.3 Verify menu item selection
- Ensure selecting a menu item always closes the menu
- Currently relies on action dispatch setting context_menu: false
- May need explicit close in menu item handlers

### Phase 3: Shaped Caret Alignment

#### 3.1 Extract caret metrics
- Create helper to get current caret shape (None, Some(Left), Some(Right))
- Get the shape offset values from ShardDec constants:
  - concave_offset = 0.256
  - convex_offset = 0.192

#### 3.2 Adjust menu position for caret shape
- Use the same pattern as Backpack.re (see reference implementation above)
- Get caret shape via `Zipper.Caret.direction(z)` (returns `option(Direction.t)`)
- Apply `ShardDec.shape_adjust` to calculate horizontal offset:
  - For right-opening menu: `ShardDec.shape_adjust(Left, caret_shape)`
  - For left-opening menu: `ShardDec.shape_adjust(Right, caret_shape)`
- Calculate final position: `base_x + shape_adjustment * col_width`
- When caret is straight (None): offset = 0.0 (no adjustment needed)
- When caret points left (concave): applies concave_offset = 0.256
- When caret points right (convex): applies convex_offset = 0.192

#### 3.3 Handle corner alignment
- For UpRight/UpLeft modes, align to caret top instead of bottom
- Account for tip_height when positioning at top of caret

### Phase 4: Visual Polish

#### 4.1 Add positioning indicator
- Subtle visual cue showing menu direction (small notch/arrow pointing to caret)
- Update border-radius based on opening direction for more natural appearance

#### 4.2 Smooth transitions
- Add CSS transitions for position changes (if menu stays open during resize)
- Fade-in animation on open
- Consider using `transform` for performance

#### 4.3 Accessibility improvements
- Ensure menu has proper ARIA attributes
- Keyboard navigation with arrow keys (may already work via browser defaults)
- Focus management (trap focus in menu when open)

## Technical Implementation Details

### Viewport Detection Logic (Pseudocode)

```reason
let calculate_menu_position =
  (~caret_point: Point.t,
   ~caret_shape: option(Direction.t),
   ~menu_dims: {width: float, height: float},
   ~viewport_dims: {width: float, height: float},
   ~font_metrics: FontMetrics.t) => {

  /* Convert caret point to pixels */
  let caret_px = {
    x: caret_point.col * font_metrics.col_width,
    y: caret_point.row * font_metrics.row_height
  };

  /* Calculate available space in each direction */
  let space_below = viewport_dims.height - caret_px.y - font_metrics.row_height;
  let space_above = caret_px.y;
  let space_right = viewport_dims.width - caret_px.x;
  let space_left = caret_px.x;

  /* Determine vertical direction */
  let open_upward = space_below < menu_dims.height && space_above > space_below;

  /* Determine horizontal direction */
  let open_leftward = space_right < menu_dims.width && space_left > space_right;

  /* Calculate position with shape adjustment (using backpack pattern) */
  let shape_offset_px =
    ShardDec.shape_adjust(
      open_leftward ? Right : Left,  /* which side of menu aligns with caret */
      caret_shape
    ) *. font_metrics.col_width;

  let base_x = caret_px.x + shape_offset_px;
  let base_y = open_upward
    ? caret_px.y - menu_dims.height  /* align bottom to caret top */
    : caret_px.y + font_metrics.row_height; /* align top to caret bottom */

  let final_x = open_leftward ? base_x - menu_dims.width : base_x;

  {x: final_x, y: base_y, mode: (open_upward, open_leftward)}
};
```

**Getting viewport dimensions:**
- For editor viewport: Query the `.code-container` element dimensions
- Use JavaScript DOM APIs: `getBoundingClientRect()` or `clientWidth/clientHeight`
- Menu dimensions can be queried from the rendered menu element before positioning
- Consider caching viewport dimensions and updating on resize events

### Measuring Menu and Viewport Dimensions

**Challenge:** Need to know menu dimensions before positioning, but menu isn't rendered yet.

**Solution approaches:**

1. **Two-pass rendering** (recommended):
   - First render: Position menu off-screen or hidden (`visibility: hidden`)
   - Measure menu dimensions using `getBoundingClientRect()`
   - Second render: Re-position with correct calculated position
   - Use ReasonReact/Virtual_dom hooks if available

2. **Fixed menu width** (simpler but less flexible):
   - Set `min-width: 160px` in CSS (already exists)
   - Calculate height based on number of items × item height
   - Less accurate but avoids two-pass rendering

3. **Estimate with fallback** (pragmatic):
   - Start with sensible estimates (width: 200px, height: item_count × 25px)
   - If menu would go off-screen, use fallback positioning
   - More accurate viewport detection can come in future iteration

**Viewport measurement:**
```reason
/* Get the editor container element */
let get_editor_viewport = () => {
  let container = Document.querySelector(".code-container");
  let rect = Element.getBoundingClientRect(container);
  {width: rect.width, height: rect.height}
};
```

**Menu measurement:**
```reason
/* After rendering menu with visibility:hidden */
let get_menu_dimensions = (menu_element) => {
  let rect = Element.getBoundingClientRect(menu_element);
  {width: rect.width, height: rect.height}
};
```

### Border Radius Adjustment

```css
/* Current: border-radius: 0 0 0.5em 0; (top-left sharp, others rounded) */

.context-menu.open-down-right {
  border-radius: 0 0.5em 0.5em 0.5em; /* top-left sharp (near caret) */
}

.context-menu.open-down-left {
  border-radius: 0.5em 0 0.5em 0.5em; /* top-right sharp (near caret) */
}

.context-menu.open-up-right {
  border-radius: 0.5em 0.5em 0.5em 0; /* bottom-left sharp (near caret) */
}

.context-menu.open-up-left {
  border-radius: 0.5em 0.5em 0 0.5em; /* bottom-right sharp (near caret) */
}
```

## Testing Checklist

### Positioning Tests
- [ ] Menu opens downward when plenty of space below
- [ ] Menu opens upward when near bottom of editor
- [ ] Menu opens rightward when plenty of space to right
- [ ] Menu opens leftward when near right edge of editor
- [ ] All four combinations work (up-left, up-right, down-left, down-right)
- [ ] Menu never extends beyond viewport boundaries

### Alignment Tests
- [ ] Straight cursor: menu flush with cursor bottom, no overlap
- [ ] Concave cursor (left-pointing): menu corner aligns with tip
- [ ] Convex cursor (right-pointing): menu corner aligns with tip
- [ ] No gaps between menu and cursor

### Close Behavior Tests
- [ ] Menu closes when clicking in editor
- [ ] Menu closes when pressing Escape key
- [ ] Menu closes when clicking outside editor (other UI areas)
- [ ] Menu closes when selecting a menu item
- [ ] Menu doesn't close when hovering/moving mouse over menu

### Visual Tests
- [ ] Appropriate border radius for opening direction
- [ ] Smooth open animation
- [ ] Menu is clearly visible against all backgrounds
- [ ] Keyboard focus visible and correct

## Priority Order for Implementation

1. **Fix pixel-perfect alignment for straight cursor** (Quick win, obvious improvement)
2. **Add viewport-aware vertical positioning** (High impact - fixes the scrolling issue)
3. **Add Escape key handler** (Easy and standard behavior)
4. **Add viewport-aware horizontal positioning** (Complete the smart positioning)
5. **Add shaped caret alignment** (Polish, but not critical)
6. **Visual polish and animations** (Nice to have)

## Questions/Decisions Needed

1. Should menu have a small gap (e.g., 2px) from caret for visual separation, or be perfectly flush?
2. Should menu opening direction be indicated visually (arrow/notch), or is positioning sufficient?
3. When menu must reposition (e.g., window resize), should it animate or snap?
4. Should there be a minimum padding from viewport edges (e.g., never closer than 10px)?

## Getting Started: Implementation Guide

When beginning implementation, follow this sequence:

### Step 1: Quick Win - Fix Straight Cursor Alignment
**Goal:** Fix the ~1px overlap issue with straight cursor
**Files:** [ContextMenu.re](src/web/app/editors/code/ContextMenu.re) lines 8-19
**Changes:**
- Modify `pos_attr` to position at `point.row + 1` with no additional offset
- Test that menu top edge is perfectly flush with cursor bottom edge
- May need to account for outline/border thickness

### Step 2: Add Escape Key Handler
**Goal:** Close menu when user presses Escape
**Files:** [CodeEditable.re](src/web/app/editors/code/CodeEditable.re) Selection module, lines 139-154
**Changes:**
- Add Escape key handling in `handle_key_event`
- When `model.context_menu == true` and Escape pressed, inject action to close menu
- Pattern: `| {key: D("Escape"), ...} when context_menu => Some(Update.ToggleContextMenu)`

### Step 3: Viewport-Aware Vertical Positioning
**Goal:** Make menu open upward when near bottom of editor
**Files:** [ContextMenu.re](src/web/app/editors/code/ContextMenu.re)
**Changes:**
- Add viewport dimension measurement
- Add menu dimension measurement (or estimation)
- Modify `pos_attr` to accept positioning mode parameter
- Implement upward positioning logic
- Add CSS class for upward-opening menus

### Step 4: Viewport-Aware Horizontal Positioning
**Goal:** Make menu open leftward when near right edge
**Files:** [ContextMenu.re](src/web/app/editors/code/ContextMenu.re)
**Changes:**
- Add horizontal space detection
- Implement leftward positioning logic
- Add CSS classes for left-opening menus
- Update border-radius for all four positioning modes

### Step 5: Shaped Caret Alignment
**Goal:** Align menu corner precisely with caret tip
**Files:** [ContextMenu.re](src/web/app/editors/code/ContextMenu.re)
**Changes:**
- Extract caret shape from zipper: `Zipper.Caret.direction(z)`
- Apply `ShardDec.shape_adjust` to calculate offset
- Integrate shape offset into positioning calculation
- Reference [Backpack.re](src/web/app/editors/decoration/Backpack.re) lines 105-150 for pattern

### Step 6: Visual Polish
**Goal:** Smooth animations and visual refinements
**Files:** [editor.css](src/web/www/style/editor.css) lines 359-402
**Changes:**
- Add CSS transitions for fade-in
- Add border-radius variations for positioning modes
- Consider adding subtle notch/arrow indicator

## Key Code Locations Summary

| Component | File | Lines | Purpose |
|-----------|------|-------|---------|
| Menu rendering | ContextMenu.re | 304-357 | Main view function and menu assembly |
| Positioning | ContextMenu.re | 8-19 | Position calculation (pos_str, pos_attr) |
| Lifecycle | CodeEditable.re | 98-103 | Toggle context menu open/close |
| Key handling | CodeEditable.re | 139-154 | Keyboard event handling |
| Caret metrics | CaretDec.re | 40-66 | Caret shape and positioning |
| Shape offsets | ShardDec.re | 14-36 | Caret shape adjustment calculations |
| Backpack reference | Backpack.re | 105-150 | Reference impl for caret alignment |
| Menu styles | editor.css | 359-402 | CSS for context menu appearance |

## References

- [NN/g: Designing Effective Contextual Menus (10 Guidelines)](https://www.nngroup.com/articles/contextual-menus-guidelines/)
- [Height: Building Context Menus Guide](https://height.app/blog/guide-to-build-context-menus)
- [Mobbin: Context Menu UI Design Best Practices](https://mobbin.com/glossary/context-menu)
- [UXPin: Dropdown Interaction Patterns](https://www.uxpin.com/studio/blog/dropdown-interaction-patterns-a-complete-guide/)
