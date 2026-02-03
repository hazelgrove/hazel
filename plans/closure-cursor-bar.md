# Closure Cursor Bar

## Status: Implemented, needs testing

## What was done

1. **Created `ClosureCursorBar.re`** - New component that displays call stack breadcrumbs
   - Shows function names from applications in the call stack
   - Uses arrow separators between entries
   - Highlights the "focused" entry (at sample_cursor.index)
   - Shows ghosted entries beyond current index
   - Only appears when probes exist
   - Shows "⌀" when at top level (empty call stack)
   - Click on entry jumps to that syntax location

2. **CSS in `style.css`** - Added closure cursor bar styling
   - Positioned in row 2 of a 4-row grid layout
   - Matches top bar aesthetics
   - Hidden when no probes exist (via `.hidden` class)

3. **Modified `Page.re`** - Integrated closure cursor bar into page layout
   - Bar is rendered between top bar and main content

4. **Modified `Transition.re`** - Added `RecordStackFrame` to built-in function applications
   - Both print and other built-in functions now record stack frames

5. **Fixed grid layout** - Added grid positioning to `#sidebars` in `style.css`

## What needs testing

- Closure cursor bar appearance when probes are active
- Click-to-jump functionality
- Breadcrumb display with nested function calls
- Ghost indicator for entries beyond current index

## Known limitations (documented in code)

- Function names show "?" for applications inside built-in function implementations
  (e.g., recursive calls within `map`) because those app_ids aren't in info_map
- Future enhancement: Augment RecordStackFrame to carry function name directly

## Files changed

- `src/web/app/probesystem/ClosureCursorBar.re` (new)
- `src/web/www/style.css`
- `src/web/www/style/sidebar.css`
- `src/web/app/Page.re`
- `src/language/dynamics/transition/Transition.re`
