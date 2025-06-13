open Virtual_dom.Vdom;
open Node;
open Util;

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

let fzero: fdims = {
  width: 0.,
  height: 0.,
  left: 0.,
  top: 0.,
};

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
      ~attr=[],
      ~fudge: fdims=fzero,
      paths: list(SvgUtil.Path.cmd),
    ) => {
  let d = abs_dims(measurement);
  let d =
    absolute
      ? d
      : {
        left: 0,
        top: 0,
        width: d.width,
        height: d.height,
      };
  create_svg(
    "svg",
    ~attrs=
      [
        Attr.classes(base_cls),
        Attr.create("style", pos_str(~d, ~fudge, font_metrics)),
        Attr.create(
          "viewBox",
          Printf.sprintf("0 0 %d %d", d.width, d.height),
        ),
        Attr.create("preserveAspectRatio", "none"),
      ]
      @ attr,
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
      ~width=1,
      ~height=1,
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
         scale
         *. (font_metrics.col_width *. Float.of_int(width) +. width_fudge),
         scale
         *. (font_metrics.row_height *. Float.of_int(height) +. height_fudge),
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
      ~scale=0.5,
      paths: list(SvgUtil.Path.cmd),
    ) => {
  // re: scale
  // Using a viewBox of 0 0 1 1 seems to trigger Chrome rounding bug
  // (https://bugs.chromium.org/p/chromium/issues/detail?id=424288) that
  // causes miaslignment between piece decorations and text.
  // Using a different viewBox size seems to fix this.
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
