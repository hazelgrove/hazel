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
      "width: %fpx; height: %fpx; margin-top: 0; margin-bottom: 0; margin-left: 0; margin-right: 0; vertical-align: bottom",
      font_metrics.col_width *. 1.6,
      font_metrics.row_height *. 0.8,
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
  Node.div(
    ~attr=
      Attr.many([
        DecUtil.abs_position(
          ~font_metrics,
          ~width_fudge=font_metrics.col_width,
          ~top_fudge=font_metrics.row_height *. 0.1,
          ~left_fudge=font_metrics.col_width *. 0.1,
          ~scale=0.7,
          button_origin,
        ),
        Attr.classes(["fold-icon"]),
      ]),
    [
      Node.div(
        ~attr=
          Attr.many([
            fold_button_style(font_metrics),
            Attr.classes(["hide-icon"]),
            Attr.on_mousedown(_evt => {
              Js_of_ocaml.Dom_html.stopPropagation(_evt);
              inject(
                List.mem(tile_id, folded)
                  ? UpdateAction.PerformAction(Unfold(tile_id))
                  : UpdateAction.PerformAction(Fold(tile_id)),
              );
            }),
          ]),
        [Node.text(List.mem(tile_id, folded) ? " 🞂" : "🞃")],
      ),
    ],
  );
};

let view = {
  let cls = "default";
  [Node.span(~attr=Attr.classes(["token", cls]), [Node.text(" <...> ")])];
};
