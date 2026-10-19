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
};

let flat = (font_metrics: FontMetrics.t): t => {
  font_metrics,
  pad: 0.,
  gadgets: IntMap.empty,
};

let font_metrics = ({font_metrics, _}: t) => font_metrics;

let pad = ({pad, _}: t) => pad;

let of_padding =
    (~font_metrics: FontMetrics.t, ~pad: float, {gaps, _}: Padding.t): t => {
  let {col_width, _}: FontMetrics.t = font_metrics;
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
  {
    font_metrics,
    pad,
    gadgets,
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

let y_top = ({font_metrics: {row_height, _}, _}: t, row: int) =>
  float_of_int(row) *. row_height;

let y_bottom = ({font_metrics: {row_height, _}, _} as grid: t, row: int) =>
  y_top(grid, row) +. row_height;

let point_of_px =
    ({font_metrics: {col_width, row_height}, _} as grid: t, ~x, ~y): Point.t => {
  let row = int_of_float(y /. row_height);
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
