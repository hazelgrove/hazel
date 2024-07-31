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

type dims = {
  width: int,
  height: int,
  left: int,
  top: int,
};

type fdims = {
  width: float,
  height: float,
  left: float,
  top: float,
};

let fzero: fdims = {width: 0., height: 0., left: 0., top: 0.};

let pos_str = (~d: dims, ~fudge: fdims=fzero, font_metrics: FontMetrics.t) =>
  Printf.sprintf(
    "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
    Float.of_int(d.left) *. font_metrics.col_width +. fudge.left,
    Float.of_int(d.top) *. font_metrics.row_height +. fudge.top,
    Float.of_int(d.width) *. (font_metrics.col_width +. fudge.width),
    Float.of_int(d.height) *. (font_metrics.row_height +. fudge.height),
  );

let code_svg_sized =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement as {origin, last}: Haz3lcore.Measured.measurement,
      ~base_cls=[],
      ~path_cls=[],
      ~fudge: fdims=fzero,
      paths: list(SvgUtil.Path.cmd),
    ) => {
  let (left, top) = (origin.col, origin.row);
  let (width, height) = (
    abs(last.col - origin.col),
    abs(last.row - origin.row + 1),
  );
  let style = pos_str(~d={left, top, width, height}, ~fudge, font_metrics);
  create_svg(
    "svg",
    ~attrs=[
      Attr.classes(base_cls),
      Attr.create("style", style),
      Attr.create("viewBox", Printf.sprintf("0 0 %d %d", width, height)),
      Attr.create("preserveAspectRatio", "none"),
    ],
    [SvgUtil.Path.view(~attrs=[Attr.classes(path_cls)], paths)],
  );
};

let position =
    (
      ~style="",
      ~left_fudge=0.0,
      ~top_fudge=0.0,
      ~width_fudge=0.0,
      ~height_fudge=0.0,
      ~scale=1.,
      ~font_metrics: FontMetrics.t,
      origin: Haz3lcore.Measured.Point.t,
    ) =>
  Attr.create(
    "style",
    style
    ++ ";"
    ++ Printf.sprintf(
         "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
         Float.of_int(origin.col) *. font_metrics.col_width +. left_fudge,
         Float.of_int(origin.row) *. font_metrics.row_height +. top_fudge,
         scale *. (font_metrics.col_width +. width_fudge),
         scale *. (font_metrics.row_height +. height_fudge),
       ),
  );

let abs_position =
    (
      ~left_fudge=0.0,
      ~top_fudge=0.0,
      ~width_fudge=0.0,
      ~height_fudge=0.0,
      ~scale=1.,
      ~font_metrics: FontMetrics.t,
      origin: Haz3lcore.Measured.Point.t,
    ) => {
  position(
    ~style="position: absolute",
    ~left_fudge,
    ~top_fudge,
    ~width_fudge,
    ~height_fudge,
    ~scale,
    ~font_metrics,
    origin,
  );
};

let code_svg =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: Haz3lcore.Measured.Point.t,
      ~base_cls=[],
      ~path_cls=[],
      ~left_fudge=0.0,
      ~top_fudge=0.0,
      ~width_fudge=0.0,
      ~height_fudge=0.0,
      ~id="",
      ~attrs=[],
      ~abs_pos=true,
      paths: list(SvgUtil.Path.cmd),
    ) => {
  // Using a viewBox of 0 0 1 1 seems to trigger Chrome rounding bug
  // (https://bugs.chromium.org/p/chromium/issues/detail?id=424288) that
  // causes miaslignment between piece decorations and text.
  // Using a different viewBox size seems to fix this.
  let scale = 2.;
  create_svg(
    "svg",
    ~attrs=
      (id == "" ? [] : [Attr.id(id)])
      @ [
        Attr.classes(base_cls),
        abs_pos
          ? abs_position(
              ~font_metrics,
              ~left_fudge,
              ~top_fudge,
              ~width_fudge,
              ~height_fudge,
              ~scale,
              origin,
            )
          : position(
              ~font_metrics,
              ~left_fudge,
              ~top_fudge,
              ~width_fudge,
              ~height_fudge,
              ~scale,
              origin,
            ),
        Attr.create("viewBox", Printf.sprintf("0 0 %f %f", scale, scale)),
        Attr.create("preserveAspectRatio", "none"),
      ]
      @ attrs,
    [SvgUtil.Path.view(~attrs=[Attr.classes(path_cls)], paths)],
  );
};

let raised_shadow_filter = (sort: Haz3lcore.Sort.t) => {
  let s = Haz3lcore.Sort.to_string(sort);
  create_svg(
    "filter",
    ~attrs=[Attr.id("raised-drop-shadow-" ++ s)],
    [
      create_svg(
        "feDropShadow",
        ~attrs=[
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

let shadow_filter = (sort: Haz3lcore.Sort.t) => {
  let s = Haz3lcore.Sort.to_string(sort);
  create_svg(
    "filter",
    ~attrs=[Attr.id("drop-shadow-" ++ s)],
    [
      create_svg(
        "feDropShadow",
        ~attrs=[
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
    List.map(raised_shadow_filter, Haz3lcore.Sort.all)
    @ List.map(shadow_filter, Haz3lcore.Sort.all),
  );
