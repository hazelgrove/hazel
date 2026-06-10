open Util;
open Virtual_dom.Vdom;
open Language;

/* Views for the builtin livelits defined in src/language/Livelit.re,
   registered by livelit name. LivelitProjView looks views up here at
   render time. Keeping the views here (rather than in core Livelit.re)
   keeps src/language free of Virtual_dom/Js_of_ocaml dependencies. */

type view_fn =
  (LivelitCtx.model_exp, LivelitCtx.action_exp => Ui_effect.t(unit)) => Node.t;

/* Wrap a view over a builtin livelit's native model/action types into a
   view_fn over hazel expressions (this conversion used to live in
   LivelitCtx.raw_of_builtin). */
let of_typed =
    (
      model_from_hazel: LivelitCtx.model_exp => option('model),
      action_to_hazel: 'action => LivelitCtx.action_exp,
      view: ('model, 'action => Ui_effect.t(unit)) => Node.t,
    )
    : view_fn =>
  (model, send_action) =>
    switch (model_from_hazel(model)) {
    | Some(m) => view(m, action => send_action(action_to_hazel(action)))
    | None => Node.text("Error: invalid model")
    };

let slider_view = (model: Livelit.Slider.model_t, send_action) => {
  let n = model;

  WebUtil.range(
    ~attrs=[
      Attr.on_input((_, v: string) => {
        send_action(Livelit.Slider.SetModel(Bigint.of_string(v)))
      }),
    ],
    ~min="0",
    ~max="100",
    Bigint.to_string(n),
  );
};

let emotion_view = (model: Livelit.Emotion.model_t, send_action) => {
  let n = model;
  let n_int = int_of_string(Bigint.to_string(n));
  /* Calculate mouth curvature from the model value */
  let smile = (100.0 -. float_of_int(n_int)) /. 100.0 *. 50.0 -. 25.0;
  let pathData =
    "M60 130 Q100 " ++ Printf.sprintf("%.1f", 130.0 -. smile) ++ " 140 130";

  Node.div([
    Node.create_svg(
      "svg",
      ~attrs=[Attr.create("width", "200"), Attr.create("height", "200")],
      [
        Node.create_svg(
          "circle",
          ~attrs=[
            Attr.create("cx", "100"),
            Attr.create("cy", "100"),
            Attr.create("r", "90"),
            Attr.create("fill", "yellow"),
            Attr.create("stroke", "black"),
          ],
          [],
        ),
        Node.create_svg(
          "circle",
          ~attrs=[
            Attr.create("cx", "65"),
            Attr.create("cy", "80"),
            Attr.create("r", "10"),
            Attr.create("fill", "black"),
          ],
          [],
        ),
        Node.create_svg(
          "circle",
          ~attrs=[
            Attr.create("cx", "135"),
            Attr.create("cy", "80"),
            Attr.create("r", "10"),
            Attr.create("fill", "black"),
          ],
          [],
        ),
        Node.create_svg(
          "path",
          ~attrs=[
            Attr.create("d", pathData),
            Attr.create("stroke", "black"),
            Attr.create("fill", "transparent"),
            Attr.create("stroke-width", "5"),
          ],
          [],
        ),
      ],
    ),
    WebUtil.range(
      ~attrs=[
        Attr.on_input((_, v) => {
          send_action(Livelit.Emotion.SetModel(Bigint.of_string(v)))
        }),
      ],
      ~min="0",
      ~max="100",
      Bigint.to_string(n),
    ),
  ]);
};

/* Render: show code input, a compute button, and the result. */
let js_view = (model: Livelit.Js.model_t, send_action) => {
  let code = model.Livelit.Js.code;
  let result = model.Livelit.Js.result;

  Node.div([
    /* Code input field */
    Node.input(
      ~attrs=[
        Attr.type_("text"),
        Attr.value(code),
        Attr.on_input((_, v: string) => {
          /* Update the code, keep the same result */
          send_action(
            Livelit.Js.SetModel(
              Livelit.Js.{
                code: v,
                result,
              },
            ),
          )
        }),
      ],
      (),
    ),
    /* Compute button */
    Node.button(
      ~attrs=[
        Attr.on_click(_ => {
          /* Evaluate the code and set the result */
          let evaluated =
            Js_of_ocaml.Js.Unsafe.eval_string("String(" ++ code ++ ")");

          send_action(
            Livelit.Js.SetModel(
              Livelit.Js.{
                code,
                result: Js_of_ocaml.Js.to_string(evaluated),
              },
            ),
          );
        }),
      ],
      [Node.text("Compute")],
    ),
    /* Display the current result */
    Node.div([Node.text("Result: " ++ result)]),
  ]);
};

let views: list((string, view_fn)) = [
  (
    Livelit.Slider.name,
    of_typed(
      Livelit.Slider.model_from_hazel,
      Livelit.Slider.action_to_hazel,
      slider_view,
    ),
  ),
  (
    Livelit.Emotion.name,
    of_typed(
      Livelit.Emotion.model_from_hazel,
      Livelit.Emotion.action_to_hazel,
      emotion_view,
    ),
  ),
  (
    Livelit.Js.name,
    of_typed(
      Livelit.Js.model_from_hazel,
      Livelit.Js.action_to_hazel,
      js_view,
    ),
  ),
];

let find = (name: string): option(view_fn) => List.assoc_opt(name, views);
