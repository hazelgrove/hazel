open Virtual_dom.Vdom;
open Haz3lcore;
open SvgUtil.Path;

let s_y = 0.28;
let s_x = s_y *. 1.5;

let tip_width = ShardDec.tip_width;

let tr_bl_north = [
  H_({dx: 0.}),
  L_({
    dx: -. tip_width,
    dy: 0.5,
  }),
];
let tl_br_north = [
  H_({dx: 0.}),
  L_({
    dx: tip_width,
    dy: 0.5,
  }),
];
let tr_bl_south = [
  L_({
    dx: -. tip_width,
    dy: 0.5,
  }),
  H_({dx: 0.}),
];

let tl_br_south = [
  L_({
    dx: tip_width,
    dy: 0.5,
  }),
  H_({dx: 0.}),
];

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
    [
      M({
        x: 0.5,
        y: 0.5 -. s_y /. 2.,
      }),
      H_({dx: s_x /. 2.}),
    ],
    scale_x(s_x, scale_y(s_y, tip_l)),
    [H_({dx: -. s_x})],
    scale_x(s_x, scale_y(s_y, tip_r)),
    [Z],
  ]);

let path_convex = path(right_tip_path_convex, left_tip_path_convex);
let path_concave = path(right_tip_path_concave, left_tip_path_concave);

let path_of_mold = (shape: Nib.Shape.t): list(SvgUtil.Path.cmd) =>
  switch (shape) {
  | Convex => path_convex
  | Concave(_) => path_concave
  };

let view =
  Core.Memo.general((font_metrics: FontMetrics.t, shape: Nib.Shape.t) =>
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

/* Thin X decoration: two mirrored caret chevrons forming an X.
 * Used at conflict boundaries with no whitespace run.
 * Proportions match the caret: tip extends tip_width (0.32)
 * horizontally, spine half-width matches caret_width/2 (0.1). */
let thin_x_hw = CaretDec.caret_width /. 2.; /* 0.1 */
let thin_x_tip = ShardDec.tip_width; /* 0.32 */

/* Left-pointing chevron (<) centered at x=0.5 */
let thin_x_left: list(SvgUtil.Path.cmd) = [
  M({
    x: 0.5 +. thin_x_hw,
    y: 0.,
  }),
  L({
    x: 0.5 -. thin_x_tip,
    y: 0.5,
  }),
  L({
    x: 0.5 +. thin_x_hw,
    y: 1.0,
  }),
  L({
    x: 0.5 -. thin_x_hw,
    y: 1.0,
  }),
  L({
    x: 0.5 -. thin_x_tip,
    y: 0.5,
  }),
  L({
    x: 0.5 -. thin_x_hw,
    y: 0.,
  }),
  Z,
];

/* Right-pointing chevron (>) centered at x=0.5 */
let thin_x_right: list(SvgUtil.Path.cmd) = [
  M({
    x: 0.5 -. thin_x_hw,
    y: 0.,
  }),
  L({
    x: 0.5 +. thin_x_tip,
    y: 0.5,
  }),
  L({
    x: 0.5 -. thin_x_hw,
    y: 1.0,
  }),
  L({
    x: 0.5 +. thin_x_hw,
    y: 1.0,
  }),
  L({
    x: 0.5 +. thin_x_tip,
    y: 0.5,
  }),
  L({
    x: 0.5 +. thin_x_hw,
    y: 0.,
  }),
  Z,
];

let view_thin =
  Core.Memo.general((font_metrics: FontMetrics.t, _shape: Nib.Shape.t) => {
    /* The X extends from x=(0.5-tip) to x=(0.5+tip) = 0.18 to 0.82.
     * Use the caret width (0.2 * col_width) as the SVG element width
     * so it stays narrow, and let the viewBox map the full X into it. */
    let svg_width = font_metrics.col_width *. CaretDec.caret_width *. 2.;
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["empty-hole-thin"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "width: %fpx; height: %fpx;",
            svg_width,
            font_metrics.row_height,
          ),
        ),
        Attr.create(
          "viewBox",
          Printf.sprintf("%f 0 %f 1", 0.5 -. thin_x_tip, thin_x_tip *. 2.),
        ),
        Attr.create("preserveAspectRatio", "none"),
      ],
      [
        SvgUtil.Path.view(~attrs=[], thin_x_left),
        SvgUtil.Path.view(
          ~attrs=[Attr.create("style", "translate: -0.6px;")],
          thin_x_right,
        ),
      ],
    );
  });
