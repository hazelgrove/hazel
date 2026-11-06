open Alcotest;
open Web;

let font_metrics: FontMetrics.t = {
  row_height: 20.0,
  col_width: 10.0,
  text_height: 16.0,
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

/* Three pads per edge: a touching boundary at col 5 on row 0 with two
   edges needs 6px and has no whitespace; row 1's two-column indent holds
   its three edges; row 2's one-column gap holds its one; row 3 starts with
   two edges at col 0. */
let padded =
  Grid.of_padding(
    ~font_metrics,
    ~pad=3.,
    {
      blocks: [],
      gaps: [
        {
          row: 0,
          before: Some(5),
          hi: 5,
          demand: 2,
        },
        {
          row: 1,
          before: None,
          hi: 2,
          demand: 3,
        },
        {
          row: 2,
          before: Some(3),
          hi: 4,
          demand: 1,
        },
        {
          row: 3,
          before: None,
          hi: 0,
          demand: 2,
        },
      ],
      leading: [
        {
          above: 0,
          below: 1,
          demand: 2,
        },
      ],
    },
  );

let gadget_sides =
  test_case(
    "of_padding: a touching boundary's gadget has two sides",
    `Quick,
    () => {
      check(close, "x_end is before it", 50., Grid.x_end(padded, ~row=0, 5));
      check(
        close,
        "x_start is after it",
        56.,
        Grid.x_start(padded, ~row=0, 5),
      );
      check(
        close,
        "a point before rides left",
        49.,
        Grid.x(padded, ~row=0, 4.9),
      );
      check(
        close,
        "a point after rides right",
        59.,
        Grid.x(padded, ~row=0, 5.3),
      );
      check(
        option(close),
        "the text layer inserts it before col 5",
        Some(6.),
        Grid.gadget_before(padded, ~row=0, 5),
      );
    },
  );

let whitespace_absorbs =
  test_case(
    "of_padding: whitespace already there is used first",
    `Quick,
    () => {
      check(
        close,
        "an indent wide enough",
        20.,
        Grid.x_start(padded, ~row=1, 2),
      );
      check(
        close,
        "a gap wide enough",
        40.,
        Grid.x_start(padded, ~row=2, 4),
      );
      check(
        option(close),
        "no gadget",
        None,
        Grid.gadget_before(padded, ~row=1, 0),
      );
    },
  );

let row_start =
  test_case(
    "of_padding: a row-start gadget moves the whole row",
    `Quick,
    () => {
      check(close, "col 0 after it", 6., Grid.x_start(padded, ~row=3, 0));
      check(
        close,
        "an overhang left of col 0 too",
        3.,
        Grid.x(padded, ~row=3, -0.3),
      );
      check(
        point,
        "a click at the overhang lands on col 0",
        {
          row: 3,
          col: 0,
        },
        Grid.point_of_px(padded, ~x=3., ~y=70.),
      );
    },
  );

let point_of_px_snaps =
  test_case(
    "point_of_px: a pixel inside a gadget is its boundary",
    `Quick,
    () => {
      let at = (x: float) => Grid.point_of_px(padded, ~x, ~y=5.);
      let p = (col): Util.Point.t => {
        row: 0,
        col,
      };
      check(point, "inside", p(5), at(52.));
      check(point, "just after", p(5), at(58.));
      check(point, "past the next cell's midpoint", p(6), at(62.));
    },
  );

let leading_gaps =
  test_case(
    "of_padding: leading demand past the font's leading opens the row",
    `Quick,
    () => {
      check(close, "row 0 unmoved", 0., Grid.y_top(padded, 0));
      check(close, "row 1 opens by the excess", 22., Grid.y_top(padded, 1));
      check(close, "later rows follow", 42., Grid.y_top(padded, 2));
      check(close, "the gap below row 0", 2., Grid.row_gap(padded, 0));
      check(close, "none below row 1", 0., Grid.row_gap(padded, 1));
      check(close, "text box top", 2., Grid.text_top(padded, 0));
      check(close, "text box bottom", 18., Grid.text_bottom(padded, 0));
      let at = (y: float) => Grid.point_of_px(padded, ~x=0., ~y);
      let p = (row): Util.Point.t => {
        row,
        col: 0,
      };
      check(point, "a pixel in the gap is still row 0", p(0), at(21.));
      check(point, "the row after", p(1), at(22.));
      check(point, "well below the table", p(4), at(85.));
    },
  );

/* At pad 8: the first opens 4px below row 0; the second has the two rows'
   text boxes 24px apart plus those 4px, so it opens the 4px short below
   row 1. */
let leading_spans =
  test_case(
    "of_padding: leading demand across a row counts the row and the gaps already open",
    `Quick,
    () => {
      let grid =
        Grid.of_padding(
          ~font_metrics,
          ~pad=8.,
          {
            blocks: [],
            gaps: [],
            leading: [
              {
                above: 0,
                below: 1,
                demand: 1,
              },
              {
                above: 0,
                below: 2,
                demand: 4,
              },
            ],
          },
        );
      check(close, "below row 0", 4., Grid.row_gap(grid, 0));
      check(close, "below row 1", 4., Grid.row_gap(grid, 1));
      check(close, "row 2 top", 48., Grid.y_top(grid, 2));
    },
  );

let tests = (
  "Grid",
  [
    flat_edges,
    flat_rows,
    point_of_px_inverts,
    gadget_sides,
    whitespace_absorbs,
    row_start,
    point_of_px_snaps,
    leading_gaps,
    leading_spans,
  ],
);
