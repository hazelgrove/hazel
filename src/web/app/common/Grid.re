open Util;
open Haz3lcore;

/* Pixels inserted on a row: in the map at `at` (None: the row's start), in
   the text before the piece at column `col`. */
type gadget = {
  at: option(float),
  col: int,
  px: float,
};

type t = {
  font_metrics: FontMetrics.t,
  pad: float,
  gadgets: IntMap.t(list(gadget)) /* per row, ascending `at` */,
  /* the pixels inserted below a row, by row */
  gaps: IntMap.t(float),
  /* the top of every row up to the first one below the last gap; the rows
     after it follow at row_height */
  tops: array(float),
};

let flat = (font_metrics: FontMetrics.t): t => {
  font_metrics,
  pad: 0.,
  gadgets: IntMap.empty,
  gaps: IntMap.empty,
  tops: [||],
};

let font_metrics = ({font_metrics, _}: t) => font_metrics;

let pad = ({pad, _}: t) => pad;

let of_padding =
    (
      ~font_metrics: FontMetrics.t,
      ~pad: float,
      {gaps, leading, _}: Padding.t,
    )
    : t => {
  let {col_width, row_height, text_height}: FontMetrics.t = font_metrics;
  let gadgets =
    List.fold_left(
      (acc, {row, before, hi, demand}: Padding.gap) => {
        let lo = Option.value(before, ~default=0);
        let px =
          float_of_int(demand) *. pad -. float_of_int(hi - lo) *. col_width;
        if (px > 0.) {
          let gadget = {
            at:
              Option.map(
                lo => (float_of_int(lo) +. float_of_int(hi)) /. 2.,
                before,
              ),
            col: (lo + hi) / 2,
            px,
          };
          IntMap.update(
            row,
            fun
            | None => Some([gadget])
            | Some(l) => Some([gadget, ...l]),
            acc,
          );
        } else {
          acc;
        };
      },
      IntMap.empty,
      gaps,
    )
    |> IntMap.map(List.rev);
  /* the leading between two rows' text boxes is used first */
  let gaps =
    List.fold_left(
      (acc, {above, demand}: Padding.leading) => {
        let px = float_of_int(demand) *. pad -. (row_height -. text_height);
        px > 0. ? IntMap.add(above, px, acc) : acc;
      },
      IntMap.empty,
      leading,
    );
  let tops =
    switch (IntMap.max_binding_opt(gaps)) {
    | None => [||]
    | Some((last_gap, _)) =>
      let tops = Array.make(last_gap + 2, 0.);
      for (row in 0 to last_gap) {
        tops[row + 1] =
          tops[row]
          +. row_height
          +. Option.value(IntMap.find_opt(row, gaps), ~default=0.);
      };
      tops;
    };
  {
    font_metrics,
    pad,
    gadgets,
    gaps,
    tops,
  };
};

let gadgets = ({gadgets, _}: t, row: int): list(gadget) =>
  Option.value(IntMap.find_opt(row, gadgets), ~default=[]);

/* The pixels of the gadgets before `col`: those at the row's start, those
   strictly before it, and with `inclusive` one on it. */
let shift = (grid: t, ~row: int, ~inclusive: bool, col: float): float =>
  List.fold_left(
    (acc, {at, px, _}) =>
      switch (at) {
      | None => acc +. px
      | Some(at) => at < col || inclusive && at == col ? acc +. px : acc
      },
    0.,
    gadgets(grid, row),
  );

let x =
    ({font_metrics: {col_width, _}, _} as grid: t, ~row: int, col: float) =>
  col *. col_width +. shift(grid, ~row, ~inclusive=true, col);

let x_start = (grid: t, ~row: int, col: int) =>
  x(grid, ~row, float_of_int(col));

let x_end =
    ({font_metrics: {col_width, _}, _} as grid: t, ~row: int, col: int) =>
  float_of_int(col)
  *. col_width
  +. shift(grid, ~row, ~inclusive=false, float_of_int(col));

let gadget_before = (grid: t, ~row: int, col: int): option(float) =>
  switch (
    List.fold_left(
      (acc, g) => g.col == col ? acc +. g.px : acc,
      0.,
      gadgets(grid, row),
    )
  ) {
  | 0. => None
  | px => Some(px)
  };

let y_top = ({font_metrics: {row_height, _}, tops, _}: t, row: int) => {
  let n = Array.length(tops);
  if (row < n) {
    tops[row];
  } else if (n == 0) {
    float_of_int(row) *. row_height;
  } else {
    tops[n - 1] +. float_of_int(row - (n - 1)) *. row_height;
  };
};

let y_bottom = ({font_metrics: {row_height, _}, _} as grid: t, row: int) =>
  y_top(grid, row) +. row_height;

let row_gap = ({gaps, _}: t, row: int): float =>
  Option.value(IntMap.find_opt(row, gaps), ~default=0.);

let text_top =
    ({font_metrics: {row_height, text_height, _}, _} as grid: t, row: int) =>
  y_top(grid, row) +. (row_height -. text_height) /. 2.;

let text_bottom = ({font_metrics: {text_height, _}, _} as grid: t, row: int) =>
  text_top(grid, row) +. text_height;

/* The row whose box holds `y`: through the table of tops, then at
   row_height. */
let row_of_px = ({font_metrics: {row_height, _}, tops, _}: t, y: float): int => {
  let n = Array.length(tops);
  if (n == 0 || y < 0.) {
    int_of_float(y /. row_height);
  } else {
    let rec go = row => row + 1 < n && tops[row + 1] <= y ? go(row + 1) : row;
    let row = go(0);
    row == n - 1 ? row + int_of_float((y -. tops[row]) /. row_height) : row;
  };
};

let point_of_px =
    ({font_metrics: {col_width, _}, _} as grid: t, ~x, ~y): Point.t => {
  let row = row_of_px(grid, y);
  /* the cell-space x: a point in a gadget snaps to the gadget's column */
  let rec cell = (offset: float, gs: list(gadget)) =>
    switch (gs) {
    | [] => x -. offset
    | [{at, px, _}, ...rest] =>
      let left =
        switch (at) {
        | None => neg_infinity
        | Some(at) => at *. col_width +. offset
        };
      if (x < left) {
        x -. offset;
      } else if (x < left +. px) {
        left -. offset;
      } else {
        cell(offset +. px, rest);
      };
    };
  {
    row,
    col:
      int_of_float(Float.round(cell(0., gadgets(grid, row)) /. col_width)),
  };
};
