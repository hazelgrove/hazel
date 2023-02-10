open Virtual_dom.Vdom;
open Haz3lcore;

let stop_mousedown_propagation =
  Attr.on_mousedown(evt => {
    Js_of_ocaml.Dom_html.stopPropagation(evt);
    Virtual_dom.Vdom.Effect.Ignore;
  });
let view =
    (
      font_metrics: FontMetrics.t,
      inject,
      name,
      livelit_state: Id.Map.t(DHExp.t),
      tile_id,
    ) =>
  switch (name) {
  | "^slider" => [
      Node.input(
        ~attr=
          Attr.many([
            Attr.create("type", "range"),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %fpx; height: %fpx; margin: 0; vertical-align: bottom",
                10.0 *. font_metrics.col_width,
                font_metrics.row_height,
              ),
            ),
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
    let checkbox_callback: Attr.t =
      Attr.on_change((_evt, _str) => {
        inject(
          UpdateAction.LivelitStateChange(tile_id, BoolLit(!checkbox_state)),
        )
      });

    [
      Node.input(
        ~attr=
          Attr.many([
            Attr.create("type", "checkbox"),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %fpx; height: %fpx; margin: 0; vertical-align: bottom",
                1.0 *. font_metrics.col_width,
                font_metrics.row_height,
              ),
            ),
            checkbox_state ? Attr.checked : Attr.create("foo", "bar"),
            checkbox_callback,
            stop_mousedown_propagation,
          ]),
        (),
      ),
    ];
  | _ => []
  };
