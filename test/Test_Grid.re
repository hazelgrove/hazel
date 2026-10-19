open Alcotest;
open Web;

let font_metrics: FontMetrics.t = {
  row_height: 20.0,
  col_width: 10.0,
};

let grid = Grid.flat(font_metrics);

let close = float(1e-9);

let point =
  testable(
    (fmt, p: Util.Point.t) =>
      Format.fprintf(fmt, "{row: %d, col: %d}", p.row, p.col),
    Util.Point.equals,
  );

let flat_edges =
  test_case(
    "flat: x_start, x_end and x agree on a boundary",
    `Quick,
    () => {
      check(close, "x_start", 30., Grid.x_start(grid, ~row=2, 3));
      check(close, "x_end", 30., Grid.x_end(grid, ~row=2, 3));
      check(close, "x", 30., Grid.x(grid, ~row=2, 3.));
      check(close, "fractional x", 35., Grid.x(grid, ~row=2, 3.5));
    },
  );

let flat_rows =
  test_case(
    "flat: rows stack by row_height",
    `Quick,
    () => {
      check(close, "y_top", 40., Grid.y_top(grid, 2));
      check(close, "y_bottom", 60., Grid.y_bottom(grid, 2));
      check(
        close,
        "next row starts where this ends",
        Grid.y_bottom(grid, 2),
        Grid.y_top(grid, 3),
      );
    },
  );

let point_of_px_inverts =
  test_case(
    "point_of_px inverts x_start and y_top",
    `Quick,
    () => {
      let p: Util.Point.t = {
        row: 2,
        col: 7,
      };
      check(
        point,
        "inside the cell",
        p,
        Grid.point_of_px(
          grid,
          ~x=Grid.x_start(grid, ~row=p.row, p.col) +. 4.,
          ~y=Grid.y_top(grid, p.row) +. 10.,
        ),
      );
      check(
        point,
        "past the cell's midpoint rounds to the next boundary",
        {
          ...p,
          col: 8,
        },
        Grid.point_of_px(
          grid,
          ~x=Grid.x_start(grid, ~row=p.row, p.col) +. 6.,
          ~y=Grid.y_top(grid, p.row) +. 10.,
        ),
      );
    },
  );

let tests = ("Grid", [flat_edges, flat_rows, point_of_px_inverts]);
