open Virtual_dom.Vdom;

module Profile = {
  type style = [ | `Sibling];
  type t = {
    style,
    measurement: Haz3lcore.Measured.measurement,
  };
};

let caret_position_radii =
    (~font_metrics: FontMetrics.t, ~style: Profile.style) => {
  let r =
    switch (style) {
    | `Sibling => 2.75
    };
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let view = (~font_metrics, {style, measurement}: Profile.t) => {
  let (r_x, r_y) = caret_position_radii(~font_metrics, ~style);
  Node.create_svg(
    "svg",
    ~attrs=[
      Attr.class_("backpack-target"),
      DecUtil.abs_position(~font_metrics, measurement.origin),
      Attr.create("viewBox", Printf.sprintf("0 0 1 1")),
      Attr.create("preserveAspectRatio", "none"),
    ],
    [
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", Printf.sprintf("%fpx", -. r_x)),
            create("y", Printf.sprintf("%fpx", 0.1 -. r_y)),
            create("width", Printf.sprintf("%fpx", 1. *. r_x)),
            create("height", Printf.sprintf("%fpx", 1. *. r_y)),
            Attr.classes(["caret-position-path"]),
          ],
        [],
      ),
    ],
  );
};
