open SvgUtil;
open Util;

let caret_width_straight = 0.1;

module Profile = {
  type t = {
    side: Direction.t,
    origin: Point.t,
    shape: option(Direction.t),
  };
};

let caret_base_path = (side, shape): list(Path.cmd) =>
  SvgUtil.Path.[m(~x=0, ~y=0), H({x: caret_width_straight})]
  @ DecUtil.chevron(side, shape, Right)
  @ SvgUtil.Path.[H({x: -. caret_width_straight})]
  @ DecUtil.chevron(side, shape, Left);

let caret_path = (side: Direction.t, shape: option(Direction.t)) => {
  caret_base_path(side, shape);
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~profile as {shape, side, origin}: Profile.t,
    ) => {
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~id="caret",
    ~base_cls=["blink"],
    ~path_cls=["caret-path"],
    /* Make caret as tall as shard + shard's shadow */
    ~height_fudge=DecUtil.shadow_dy *. font_metrics.row_height,
    caret_path(side, shape),
  );
};
