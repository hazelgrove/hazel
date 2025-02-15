open Util;

module Profile = {
  type t = {
    side: Direction.t,
    origin: Point.t,
    shape: option(Direction.t),
  };
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
    DecUtil.caret_base_path(side, shape),
  );
};
