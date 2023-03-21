open Virtual_dom.Vdom;
open Haz3lcore;

let stop_mousedown_propagation =
  Attr.on_mousedown(evt => {
    Js_of_ocaml.Dom_html.stopPropagation(evt);
    Virtual_dom.Vdom.Effect.Ignore;
  });

let livelit_style = (font_metrics: FontMetrics.t, livelit_width: float) =>
  Attr.create(
    "style",
    Printf.sprintf(
      "width: %fpx; height: %fpx; margin-top: 0; margin-bottom: 0; margin-left: %fpx; margin-right: %fpx; vertical-align: bottom; display: inline-block",
      livelit_width *. font_metrics.col_width,
      font_metrics.row_height,
      livelit_width *. 0.5,
      livelit_width *. 0.5,
    ),
  );
let view =
    (
      font_metrics: FontMetrics.t,
      inject,
      name,
      livelits: Livelit.state,
      tile_id,
    ) =>
  switch (name) {
  | "^slider" => [
      Node.input(
        ~attr=
          Attr.many([
            Attr.create("type", "range"),
            livelit_style(font_metrics, float_of_int(10)),
            Attr.create(
              "value",
              string_of_int(
                switch (Id.Map.find_opt(tile_id, livelits)) {
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
      switch (Id.Map.find_opt(tile_id, livelits)) {
      | Some(BoolLit(b)) => b
      | _ => false
      };

    [
      Node.input(
        ~attr=
          Attr.many(
            [
              Attr.create("type", "checkbox"),
              livelit_style(font_metrics, float_of_int(1)),
              Attr.on_change((_evt, _str) => {
                inject(
                  UpdateAction.LivelitStateChange(
                    tile_id,
                    BoolLit(!checkbox_state),
                  ),
                )
              }),
              stop_mousedown_propagation,
            ]
            @ (checkbox_state ? [Attr.checked] : []),
          ),
        (),
      ),
    ];
  | _ => []
  };
