open Virtual_dom.Vdom;

module Profile = {
  type style = [ | `Bare | `Sibling | `Anchor | `Caret];
  type t = {
    style,
    measurement: Core.Measured.measurement,
    sort: Core.Sort.t,
  };
};

let caret_position_radii =
    (~font_metrics: FontMetrics.t, ~style: Profile.style) => {
  let r =
    switch (style) {
    | `Caret => 3.75
    | `Anchor
    | `Sibling => 2.75
    | `Bare => 2.0
    };
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let view = (~font_metrics, {style, sort, measurement}: Profile.t) => {
  let (r_x, r_y) = caret_position_radii(~font_metrics, ~style);
  let c_cls = Core.Sort.to_string(sort);
  let cls =
    switch (style) {
    | `Bare => "outer-cousin"
    | `Caret => "current-caret-pos"
    | `Anchor => "anchor"
    | `Sibling => "sibling"
    };
  Node.create_svg(
    "svg",
    [
      Attr.class_(cls),
      DecUtil.abs_position(~font_metrics, measurement.origin),
      Attr.create("viewBox", Printf.sprintf("0 0 1 1")),
      Attr.create("preserveAspectRatio", "none"),
    ],
    [
      Node.create_svg(
        "rect",
        Attr.[
          create("x", Printf.sprintf("%fpx", -. r_x)),
          create("y", Printf.sprintf("%fpx", 0.1 -. r_y)),
          create("width", Printf.sprintf("%fpx", 1. *. r_x)),
          create("height", Printf.sprintf("%fpx", 1. *. r_y)),
          Attr.classes(["caret-position-path", cls, c_cls]),
        ],
        [],
      ),
    ],
  );
};
