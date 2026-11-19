/* Auto-scroll while a drag holds the pointer near an edge, faster the closer
   it is: of #main at its top or bottom edge, and of the code scroller the
   drag is in (JsUtil.CodeScroll) at its left or right. Scrolling continues
   while the pointer is still, so whatever tracks the drag is told after each
   step. */

let stop: unit => unit;

let is_active: unit => bool;

/** Call on every pointer move of the drag, with the code container the drag
    is in. `on_scroll` runs after each scroll step, until the pointer leaves
    the edge or `stop`. */
let update:
  (
    ~container: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element),
    ~client_x: float,
    ~client_y: float,
    ~on_scroll: unit => unit
  ) =>
  unit;
