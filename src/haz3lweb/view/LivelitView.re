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
      font_metrics.col_width *. 0.5,
      font_metrics.col_width *. 0.5,
    ),
  );

let view_slider =
    (
      font_metrics: FontMetrics.t,
      inject,
      livelits: Livelit.state,
      tile_id,
      decode: int => DHExp.t,
      encode: DHExp.t => option(int),
    ) => {
  // let encode = t => {
  //   switch ((t: DHExp.t)) {
  //   | IntLit(i) => Some(i)
  //   | _ => None
  //   };
  // };
  // let decode = (i: int): DHExp.t => IntLit(i);
  Node.input(
    ~attr=
      Attr.many([
        Attr.create("type", "range"),
        livelit_style(font_metrics, float_of_int(10)),
        Attr.create(
          "value",
          string_of_int(
            Id.Map.find_opt(tile_id, livelits)
            |> Option.bind(_, encode)
            |> Option.value(~default=50),
          ),
        ),
        Attr.on_input((_evt, str) =>
          (
            {
              inject(
                UpdateAction.LivelitStateChange(
                  tile_id,
                  decode(int_of_string(str)),
                ),
              );
            }:
              Virtual_dom.Vdom.Effect.t(unit)
          )
        ),
        stop_mousedown_propagation,
      ]),
    (),
  );
};

let view =
    (
      font_metrics: FontMetrics.t,
      inject,
      name,
      livelits: Livelit.state,
      tile_id,
    ) => {
  switch (name) {
  | x when x == Livelit.slider.name => [
      view_slider(
        font_metrics,
        inject,
        livelits,
        tile_id,
        i => IntLit(i),
        t => {
          switch (t) {
          | IntLit(i) => Some(i)
          | _ => None
          }
        },
      ),
    ]
  | x when x == Livelit.fslider.name => [
      view_slider(
        font_metrics,
        inject,
        livelits,
        tile_id,
        i => FloatLit(float_of_int(i) /. 100.0),
        t => {
          switch (t) {
          | FloatLit(i) => Some(int_of_float(i *. 100.0))
          | _ => None
          }
        },
      ),
    ]
  | x when x == Livelit.checkbox.name =>
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
              Attr.on_input((_evt, _str) => {
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
};
