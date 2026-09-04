open Util_web;
open Virtual_dom.Vdom;
open Haz3lcore;

let path_of_mold = (shape: Grout.shape): list(SvgUtil.Path.cmd) =>
  switch (shape) {
  | Convex => WebUtil.EmptyHole.path_convex
  | Concave => WebUtil.EmptyHole.path_concave
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
