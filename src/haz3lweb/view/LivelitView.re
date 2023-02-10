open Virtual_dom.Vdom;
open Haz3lcore;

let stop_mousedown_propagation =
  Attr.on_mousedown(evt => {
    Js_of_ocaml.Dom_html.stopPropagation(evt);
    Virtual_dom.Vdom.Effect.Ignore;
  });

let livelit_style = (font_metrics: FontMetrics.t, livelit_width: int) =>
  Attr.create(
    "style",
    Printf.sprintf(
      "width: %fpx; height: %fpx; margin: 0; vertical-align: bottom",
      float_of_int(livelit_width) *. font_metrics.col_width,
      font_metrics.row_height,
    ),
  );
let view =
    (
      font_metrics: FontMetrics.t,
      inject,
      name,
      livelit_state: Id.Map.t(DHExp.t),
      tile_id,
    ) => {
  switch (name) {
  | "^slider" => [
      Node.input(
        ~attr=
          Attr.many([
            Attr.create("type", "range"),
            livelit_style(font_metrics, 10),
            Attr.create(
              "value",
              string_of_int(
                switch (Id.Map.find_opt(tile_id, livelit_state)) {
                | Some(IntLit(i)) => i
                | _ => 50
                },
              ),
            ),
            Attr.on_input((_evt, str) =>
              (
                {
                  inject(
                    UpdateAction.LivelitStateChange(
                      tile_id,
                      IntLit(int_of_string(str)),
                    ),
                  );
                }:
                  Virtual_dom.Vdom.Effect.t(unit)
              )
            ),
            stop_mousedown_propagation,
          ]),
        (),
      ),
    ]
  | "^checkbox" =>
    let checkbox_state: bool =
      switch (Id.Map.find_opt(tile_id, livelit_state)) {
      | Some(BoolLit(b)) => b
      | _ => false
      };

    [
      Node.input(
        ~attr=
          Attr.many([
            Attr.create("type", "checkbox"),
            livelit_style(font_metrics, 1),
            checkbox_state ? Attr.checked : Attr.create("foo", "bar"),
            Attr.on_change((_evt, _str) => {
              inject(
                UpdateAction.LivelitStateChange(
                  tile_id,
                  BoolLit(!checkbox_state),
                ),
              )
            }),
            stop_mousedown_propagation,
          ]),
        (),
      ),
    ];
  | _ => []
  };
};
