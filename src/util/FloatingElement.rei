/* Lets an element render outside an ancestor's `overflow` clipping while
   staying positioned relative to content inside it -- Hazel's backpack
   floating above the caret, past the scroll container that would clip it.
   CSS clips descendants geometrically, so z-index cannot buy this; the
   element is `position: fixed` and its viewport coordinates are recomputed
   from its anchor's bounding box. FloatingElement.re explains why the
   simpler alternatives don't work.

   To float an element: give it class `floating-fixed`, the data attributes
   `data-float-anchor-class` (the ancestor to measure against),
   `data-float-local-top`/`-left` (offset from that ancestor, in pixels) and
   optionally `data-float-anchor-edge="bottom"` to hang off the anchor's
   bottom rather than its top, and inline `position: fixed; visibility:
   hidden`. It stays hidden until positioned, so nothing flashes at 0,0. */

/* Position every `.floating-fixed` in the document. Call after each render. */
let update_all: unit => unit;

/* Keep floating elements positioned while `#main` scrolls. Call once at
   startup; repeat calls are ignored. */
let setup_scroll_listener: unit => unit;
