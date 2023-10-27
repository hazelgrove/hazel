open Virtual_dom.Vdom;
open Haz3lcore;

let stop_mousedown_propagation =
  Attr.on_mousedown(evt => {
    Js_of_ocaml.Dom_html.stopPropagation(evt);
    Virtual_dom.Vdom.Effect.Ignore;
  });

let fold_button_style = (font_metrics: FontMetrics.t) =>
  Attr.create(
    "style",
    Printf.sprintf(
      "width: %fpx; height: %fpx; margin-top: 0; margin-bottom: 0; margin-left: %fpx; margin-right: %fpx; vertical-align: bottom; display: inline-block",
      font_metrics.col_width,
      font_metrics.row_height,
      font_metrics.col_width *. 0.5,
      font_metrics.col_width *. 0.5,
    ),
  );

let button_view =
    (
      font_metrics: FontMetrics.t,
      inject,
      folded: list(Id.t),
      tile_id,
      origin: Measured.Point.t,
    ) => {
  let button_origin: Measured.Point.t = {...origin, col: (-2)};
  Node.span(
    ~attr=
      Attr.many([
        DecUtil.abs_position(~font_metrics, button_origin),
        Attr.classes(["token", "default"]),
        Attr.on_mousedown(_evt => {
          Js_of_ocaml.Dom_html.stopPropagation(_evt);
          let goal: Measured.Point.t = {...origin, col: origin.col + 1};
          let events = [
            inject(UpdateAction.PerformAction(Click(tile_id))),
            inject(UpdateAction.PerformAction(Move(Goal(Point(goal))))),
            inject(UpdateAction.PerformAction(Move(Goal(Point(origin))))),
          ];
          Virtual_dom.Vdom.Effect.Many(events);
        }),
      ]),
    [Node.text(List.mem(tile_id, folded) ? "> " : "∨")],
  );
};

let view = {
  let cls = "default";
  [Node.span(~attr=Attr.classes(["token", cls]), [Node.text(" <...> ")])];
};
