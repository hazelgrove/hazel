open Virtual_dom.Vdom;
open Node;
open Util;

let tip_width = 0.32;
let shadow_adj = 0.015;
let shadow_dy = 0.037;
let shadow_dx = 0.08;
let child_border_thickness = 0.05;
let t = child_border_thickness /. 0.5;
let short_tip_width = (1. -. t) *. tip_width;
let short_tip_height = (1. -. t) *. 0.5;

let d_comp = (d1: Direction.t, d2: Direction.t): float =>
  switch (d1, d2) {
  | (Left, Left)
  | (Right, Right) => 0.
  | (Left, Right) => 1.0
  | (Right, Left) => (-1.0)
  };

let shape_of_run = (shape: Direction.t): float =>
  switch (shape) {
  | Left => +. tip_width
  | Right => -. tip_width
  };

let caret_run = (shape: option(Direction.t)) =>
  switch (shape) {
  | None => 0.
  | Some(d2) => shape_of_run(d2)
  };

let caret_adjust = (side: Direction.t, shape: option(Direction.t)) =>
  switch (shape) {
  | None => 0.
  | Some(d2) => d_comp(side, d2) *. tip_width
  };

let chevron' = (a: float, b: float, c: float): list(SvgUtil.Path.cmd) =>
  SvgUtil.Path.[
    H_({dx: -. a}),
    L_({dx: -. b, dy: c}),
    L_({dx: +. b, dy: c}),
    H_({dx: +. a}),
  ];

let chevron =
    (side: Direction.t, shape: option(Direction.t), drawing: Direction.t) => {
  chevron'(
    caret_adjust(side, shape),
    caret_run(shape),
    drawing == Left ? (-0.5) : 0.5,
  );
};

let extra_tail = 0.;
let jagged_edge_h = child_border_thickness /. 3.;
let jagged_edge_w = child_border_thickness /. 1.;

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

let abs_dims = ({origin, last}: Haz3lcore.Measured.measurement): dims => {
  left: origin.col,
  top: origin.row,
  width: abs(last.col - origin.col),
  height: abs(last.row - origin.row + 1),
};

let abs_style = (~font_metrics, ~fudge: fdims=fzero, measurement): Attr.t =>
  Attr.create(
    "style",
    pos_str(~d=abs_dims(measurement), ~fudge, font_metrics),
  );

let code_svg_sized =
    (
      ~font_metrics: FontMetrics.t,
      ~absolute=true,
      ~measurement: Haz3lcore.Measured.measurement,
      ~base_cls=[],
      ~path_cls=[],
      ~fudge: fdims=fzero,
      paths: list(SvgUtil.Path.cmd),
    ) => {
  let d = abs_dims(measurement);
  let d = absolute ? d : {left: 0, top: 0, width: d.width, height: d.height};
  create_svg(
    "svg",
    ~attrs=[
      Attr.classes(base_cls),
      Attr.create("style", pos_str(~d, ~fudge, font_metrics)),
      Attr.create("viewBox", Printf.sprintf("0 0 %d %d", d.width, d.height)),
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
      origin: Point.t,
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
      origin: Point.t,
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
      ~origin: Point.t,
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
  let scale = 0.5;
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

let drop_shadow_filter = (sort: Haz3lcore.Sort.t) => {
  let s = Haz3lcore.Sort.to_string(sort);
  create_svg(
    "filter",
    ~attrs=[Attr.id("drop-shadow-" ++ s)],
    [
      create_svg(
        "feDropShadow",
        ~attrs=[
          Attr.classes(["tile-drop-shadow"]),
          Attr.create("dx", Printf.sprintf("%.3f", shadow_dx)),
          Attr.create("dy", Printf.sprintf("%.3f", shadow_dy)),
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
    List.map(drop_shadow_filter, Haz3lcore.Sort.all),
  );
