open Virtual_dom.Vdom;
open Node;

let base = (~font_metrics, measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["fold"]),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱")],
  );

let indicated = (~font_metrics as _, _measurement) => [];
