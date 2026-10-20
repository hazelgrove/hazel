open Util;
open WebUtil;

module Profile = {
  type t = {
    side: Direction.t,
    origin: Point.t,
    shape: option(Direction.t),
  };
};

let caret_width = 0.2; /* Width of editor caret */

/* The straight top edge, in character units. Decorations joining the
   caret must use this edge, including its indication-side offset. */
let top_edge = (side, shape): (float, float) => (
  -. (ShardDec.shape_adjust(side, shape) +. 0.5 *. caret_width),
  caret_width,
);

let caret_base_path = (side, shape): list(SvgUtil.Path.cmd) => {
  let (left, width) = top_edge(side, shape);
  ShardDec.chonky_path_base((shape, shape), -. left, width, 0.);
};

/* Stretched to reach over the shards' drop shadow. */
let over_shadow = (p: SvgUtil.Point.t): SvgUtil.Point.t => {
  ...p,
  y: p.y *. (1. +. ShardDec.shadow_dy),
};

let main = (~grid: Grid.t, ~profile as {shape, side, origin}: Profile.t) =>
  DecUtil.svg(
    ~grid,
    ~lo=origin,
    ~hi=origin,
    ~id="caret",
    ~base_cls=["blink"],
    ~path_cls=["caret-path"],
    SvgUtil.Path.transform(
      ~pt=over_shadow,
      ~scale={
        dx: 1.,
        dy: 1.,
      },
      caret_base_path(side, shape),
    ),
  );

let side_of = (z: Haz3lcore.Zipper.t): Direction.t => {
  Haz3lcore.(
    switch (Indicated.for_decoration(z)) {
    | _
        when
          !Selection.is_empty(z.selection)
          && !Selection.is_buffer(z.selection) =>
      z.selection.focus
    | Some({side, _}) => Direction.toggle(side)
    | _ => Right
    }
  );
};

let view =
    (~measured: Haz3lcore.Measured.t, ~grid: Grid.t, z: Haz3lcore.Zipper.t)
    : Node.t => {
  open Haz3lcore;
  let side = side_of(z);
  main(
    ~grid,
    ~profile={
      side,
      origin: Zipper.Caret.point(measured, z),
      shape: Zipper.Caret.direction(z),
    },
  );
};
