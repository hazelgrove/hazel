open Util;
open Virtual_dom.Vdom;
open Haz3lcore;

let path_of_mold = (shape: Nib.Shape.t): list(SvgUtil.Path.cmd) =>
  switch (shape) {
  | Convex => WebUtil.EmptyHole.path_convex
  | Concave(_) => WebUtil.EmptyHole.path_concave
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
