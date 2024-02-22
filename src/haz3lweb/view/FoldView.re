open Virtual_dom.Vdom;

let base = (~font_metrics, measurement) => {
  let style =
    DecUtil.pos_str(~d=DecUtil.abs_style(measurement), font_metrics);
  Node.div(
    ~attr=Attr.many([Attr.classes(["fold"]), Attr.create("style", style)]),
    [Node.text("⋱")],
  );
};

let indicated = (~font_metrics as _, _measurement) => [];
