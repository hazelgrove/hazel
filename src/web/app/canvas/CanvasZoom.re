/* Overview and direct manipulation share a zoom range. Automatic follow
   has its own, higher readability floor in CanvasCamera/CanvasScore. */
let min_zoom = 0.01;
let max_zoom = 2.5;
let clamp = (z: float): float => max(min_zoom, min(max_zoom, z));

/* Padding is in screen pixels, so an overview keeps a visible margin
   even when the graph needs a very small scale. */
let fit = (~width: float, ~height: float, ~aw: float, ~ah: float): float =>
  clamp(
    min(
      max(1., aw -. 32.) /. max(1., width),
      max(1., ah -. 32.) /. max(1., height),
    ),
  );
