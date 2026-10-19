/* Where a code editor's cells are on the screen.

   Measured lays code out on a grid of rows and columns, and the decorations,
   the caret and the mouse all work in those cells. This is the one place
   that turns a cell into pixels and back. Without padding a cell is at
   (col * col_width, row * row_height); block padding inserts gadgets, pixels
   placed in the gap between two fragments of text on a row or between two
   rows, which move everything after them. Two touching fragments share a
   boundary column, so a gadget there needs the caller to say which side it
   means: `x_start` is after it, `x_end` before. */

open Util;

type t;

/* No padding: the affine map on the font metrics. */
let flat: FontMetrics.t => t;

let font_metrics: t => FontMetrics.t;

/* The left edge of cell `col` on `row`, after any gadget at that boundary. */
let x_start: (t, ~row: int, int) => float;

/* The right edge of cell `col - 1` on `row`, before any gadget at that
   boundary. */
let x_end: (t, ~row: int, int) => float;

/* A fractional column on `row`. A gadget before the column counts; one on
   the column counts as `x_start` would. */
let x: (t, ~row: int, float) => float;

let y_top: (t, int) => float;

let y_bottom: (t, int) => float;

/* The cell under a pixel position relative to the editor's text box: the
   row whose box holds the y, and the nearest column boundary at the x. */
let point_of_px: (t, ~x: float, ~y: float) => Point.t;
