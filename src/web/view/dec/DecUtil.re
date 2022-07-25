open Virtual_dom.Vdom;
open Node;
open Util;

let tip_width = 0.32;
let concave_adj = 0.25;
let convex_adj = (-0.13);
let shadow_adj = 0.015;

let caret_adjust = (side: Direction.t, shape: option(Direction.t)) =>
  switch (side, shape) {
  | (_, None) => 0.
  | (Left, Some(Left)) => concave_adj
  | (Right, Some(Right)) => -. concave_adj
  | (Left, Some(Right)) => convex_adj
  | (Right, Some(Left)) => -. convex_adj
  };

let child_border_thickness = 0.05;

let t = child_border_thickness /. 0.5;
let short_tip_height = (1. -. t) *. 0.5;

let stretch_dx = 0.15;

let raised_shadow_dx = "0.1";
let raised_shadow_dy = "0.037";
let shadow_dx = raised_shadow_dx;
let shadow_dy = raised_shadow_dy;

let extra_tail = 0.;
let jagged_edge_h = child_border_thickness /. 3.;
let jagged_edge_w = child_border_thickness /. 1.;

let short_tip_width = (1. -. t) *. tip_width;

let abs_position =
    (
      ~left_fudge=0.0,
      ~top_fudge=0.0,
      ~width_fudge=0.0,
      ~height_fudge=0.0,
      ~font_metrics: FontMetrics.t,
      origin: Core.Measured.point,
    ) => {
  Attr.create(
    "style",
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. left_fudge,
      Float.of_int(origin.row) *. font_metrics.row_height +. top_fudge,
      font_metrics.col_width +. width_fudge,
      font_metrics.row_height +. height_fudge,
    ),
  );
};

let code_svg =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: Core.Measured.point,
      ~base_cls=[],
      ~path_cls=[],
      ~left_fudge=0.0,
      ~top_fudge=0.0,
      ~width_fudge=0.0,
      ~height_fudge=0.0,
      ~id="",
      ~attrs=[],
      paths: list(SvgUtil.Path.cmd),
    ) =>
  create_svg(
    "svg",
    (id == "" ? [] : [Attr.id(id)])
    @ [
      Attr.classes(base_cls),
      abs_position(
        ~font_metrics,
        ~left_fudge,
        ~top_fudge,
        ~width_fudge,
        ~height_fudge,
        origin,
      ),
      Attr.create("viewBox", Printf.sprintf("0 0 1 1")),
      Attr.create("preserveAspectRatio", "none"),
    ]
    @ attrs,
    [SvgUtil.Path.view(~attrs=[Attr.classes(path_cls)], paths)],
  );

let raised_shadow_filter = (sort: Core.Sort.t) => {
  let s = Core.Sort.to_string(sort);
  create_svg(
    "filter",
    [Attr.id("raised-drop-shadow-" ++ s)],
    [
      create_svg(
        "feDropShadow",
        [
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", raised_shadow_dx),
          Attr.create("dy", raised_shadow_dy),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );
};

let shadow_filter = (sort: Core.Sort.t) => {
  let s = Core.Sort.to_string(sort);
  create_svg(
    "filter",
    [Attr.id("drop-shadow-" ++ s)],
    [
      create_svg(
        "feDropShadow",
        [
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", shadow_dx),
          Attr.create("dy", shadow_dy),
          Attr.create("stdDeviation", "0"),
        ],
        [],
      ),
    ],
  );
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    List.map(raised_shadow_filter, Core.Sort.all)
    @ List.map(shadow_filter, Core.Sort.all),
  );
