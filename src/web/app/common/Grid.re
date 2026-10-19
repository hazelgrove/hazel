open Util;

type t = {font_metrics: FontMetrics.t};

let flat = (font_metrics: FontMetrics.t): t => {font_metrics: font_metrics};

let font_metrics = ({font_metrics}: t) => font_metrics;

let x = ({font_metrics: {col_width, _}}: t, ~row as _: int, col: float) =>
  col *. col_width;

let x_start = (grid: t, ~row: int, col: int) =>
  x(grid, ~row, float_of_int(col));

let x_end = x_start;

let y_top = ({font_metrics: {row_height, _}}: t, row: int) =>
  float_of_int(row) *. row_height;

let y_bottom = ({font_metrics: {row_height, _}} as grid: t, row: int) =>
  y_top(grid, row) +. row_height;

let point_of_px =
    ({font_metrics: {col_width, row_height}}: t, ~x: float, ~y: float)
    : Point.t => {
  row: int_of_float(y /. row_height),
  col: int_of_float(Float.round(x /. col_width)),
};
