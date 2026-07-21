open Util;
open Virtual_dom.Vdom;
open Haz3lcore;

let path_of_mold = (shape: Grout.shape): list(SvgUtil.Path.cmd) =>
  switch (shape) {
  | Convex => WebUtil.EmptyHole.path_convex
  | Concave => WebUtil.EmptyHole.path_concave
  };

/* the cell a zero-width hole paints into: layout width is always 0
   (negative margins swallow the glyph's box); the glyph overlaps the
   following cell, the preceding cell, or straddles the boundary as a
   thin mark — the FeltPrint weave rules, in CSS */
[@deriving (show({with_path: false}), sexp)]
type cell =
  | NextCell /* following space / line-end free cell */
  | PrevCell /* preceding space holds the hole */
  | Thin /* pinched between tokens */
  | Boxed; /* standalone (chips): keep the glyph's own box */

let view =
  Core.Memo.general(
    ((font_metrics: FontMetrics.t, shape: Grout.shape, cell: cell)) => {
    let w = font_metrics.col_width;
    let margins =
      switch (cell) {
      | NextCell => Printf.sprintf("margin-right: -%fpx;", w)
      | PrevCell => Printf.sprintf("margin-left: -%fpx;", w)
      | Thin =>
        Printf.sprintf(
          "margin-left: -%fpx; margin-right: -%fpx; transform: scaleX(0.4);",
          w /. 2.,
          w /. 2.,
        )
      | Boxed => ""
      };
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["empty-hole"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "width: %fpx; height: %fpx; %s",
            font_metrics.col_width,
            font_metrics.col_width, //font_metrics.row_height,
            margins,
          ),
        ),
        Attr.create(
          "viewBox",
          Printf.sprintf("%f %f %f %f", 0., 0.3, 1., 0.4),
        ),
        Attr.create("preserveAspectRatio", "none"),
      ],
      [SvgUtil.Path.view(~attrs=[], path_of_mold(shape))],
    );
  });
