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

let caret_base_path = (side, shape): list(SvgUtil.Path.cmd) =>
  ShardDec.chonky_path_base(
    (shape, shape),
    ShardDec.shape_adjust(side, shape) +. 0.5 *. caret_width,
    caret_width,
    float_of_int(0),
  );

let main =
    (
      ~font_metrics: FontMetrics.t,
      ~profile as {shape, side, origin}: Profile.t,
    ) =>
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~id="caret",
    ~base_cls=["blink"],
    ~path_cls=["caret-path"],
    /* A smaller scale causes scroll-to-caret issues */
    ~scale=1.0,
    /* Make caret as tall as shard + shard's shadow */
    ~height_fudge=ShardDec.shadow_dy *. font_metrics.row_height,
    caret_base_path(side, shape),
  );

let view =
    (
      ~measured: Haz3lcore.Measured.t,
      ~font_metrics: FontMetrics.t,
      z: Haz3lcore.Zipper.t,
    )
    : Node.t => {
  open Haz3lcore;
  let side =
    switch (Indicated.piece(z)) {
    | _
        when
          !Selection.is_empty(z.selection)
          && !Selection.is_buffer(z.selection) =>
      z.selection.focus
    | Some((_, side, _)) => Direction.toggle(side)
    | _ => Right
    };
  main(
    ~font_metrics,
    ~profile={
      side,
      origin: Zipper.Caret.point(measured, z),
      shape: Zipper.Caret.direction(z),
    },
  );
};
