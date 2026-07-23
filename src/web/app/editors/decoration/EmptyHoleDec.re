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
      | Thin => "" /* dispatched to view_thin below */
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

/* Thin X decoration (ported from virtual-grout): two mirrored caret
 * chevrons forming an X, straddling the boundary between tokens.
 * Proportions match the caret: tip extends tip_width (0.32)
 * horizontally, spine half-width matches caret_width/2 (0.1). */
let thin_x_hw = CaretDec.caret_width /. 2.; /* 0.1 */
let thin_x_tip = ShardDec.tip_width; /* 0.32 */

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
  Core.Memo.general((font_metrics: FontMetrics.t) => {
    /* The X extends x=0.18..0.82; the SVG element stays caret-narrow
     * and out of flow so it never expands the line box. */
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
