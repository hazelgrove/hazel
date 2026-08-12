/* Auto-scroll while dragging near a viewport edge. The animation frame
   loop and its mutable state are private. */

let stop: unit => unit;

let update: (~client_y: float, ~on_scroll: unit => unit) => unit;
