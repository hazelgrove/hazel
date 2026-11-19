/* Auto-scroll of #main while a drag holds the pointer near its top or bottom
   edge, faster the closer it is. Scrolling continues while the pointer is
   still, so whatever tracks the drag is told after each step. */

let stop: unit => unit;

let is_active: unit => bool;

/** Call on every pointer move of the drag. `on_scroll` runs after each
    scroll step, until the pointer leaves the edge or `stop`. */
let update: (~client_y: float, ~on_scroll: unit => unit) => unit;
