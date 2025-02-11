open Virtual_dom.Vdom;
open Haz3lcore;
open SvgUtil.Path;

let s_y = 0.28;
let s_x = s_y *. 1.5;

let tip_width = ShardDec.tip_width;

let tr_bl_north = [H_({dx: 0.}), L_({dx: -. tip_width, dy: 0.5})];
let tl_br_north = [H_({dx: 0.}), L_({dx: tip_width, dy: 0.5})];
let tr_bl_south = [L_({dx: -. tip_width, dy: 0.5}), H_({dx: 0.})];

let tl_br_south = [L_({dx: tip_width, dy: 0.5}), H_({dx: 0.})];

let bl_tr_north = SvgUtil.Path.reverse(tr_bl_north);
let bl_tr_south = SvgUtil.Path.reverse(tr_bl_south);

let br_tl_north = SvgUtil.Path.reverse(tl_br_north);
let br_tl_south = SvgUtil.Path.reverse(tl_br_south);

let left_tip_path_convex = br_tl_south @ bl_tr_north;

let right_tip_path_convex = tl_br_north @ tr_bl_south;

let left_tip_path_concave =
  [H_({dx: Float.neg(tip_width)}), ...bl_tr_south]
  @ br_tl_north
  @ [H_({dx: tip_width})];

let right_tip_path_concave =
  [H_({dx: tip_width}), ...tr_bl_north]
  @ tl_br_south
  @ [H_({dx: Float.neg(tip_width)})];

let path = (tip_l, tip_r): list(SvgUtil.Path.cmd) =>
  List.concat([
    [M({x: 0.5, y: 0.5 -. s_y /. 2.}), H_({dx: s_x /. 2.})],
    scale_x(s_x, scale_y(s_y, tip_l)),
    [H_({dx: -. s_x})],
    scale_x(s_x, scale_y(s_y, tip_r)),
    [Z],
  ]);

let path_convex = path(right_tip_path_convex, left_tip_path_convex);
let path_concave = path(right_tip_path_concave, left_tip_path_concave);

let path_of_mold = (shape: Grout.shape): list(SvgUtil.Path.cmd) =>
  switch (shape) {
  | Convex => path_convex
  | Concave => path_concave
  };

let view =
  Core.Memo.general((font_metrics: FontMetrics.t, shape: Grout.shape) =>
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["empty-hole"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "width: %fpx; height: %fpx;",
            font_metrics.col_width,
            font_metrics.col_width //font_metrics.row_height,
          ),
        ),
        Attr.create(
          "viewBox",
          Printf.sprintf("%f %f %f %f", 0., 0.3, 1., 0.4),
        ),
        Attr.create("preserveAspectRatio", "none"),
      ],
      [SvgUtil.Path.view(~attrs=[], path_of_mold(shape))],
    )
  );
