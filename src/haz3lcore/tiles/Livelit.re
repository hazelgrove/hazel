open Virtual_dom.Vdom;
open ProjectorCore;
open LivelitCtx;
open Grammar;

type livelit_name = string;

module Slider: BuiltinLivelit = {
  let name = "slider";

  type model_t = Bigint.t;
  type expansion_t = Bigint.t;
  type action_t =
    | SetModel(model_t);

  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));
  let model_from_hazel: model_exp => model_t =
    (x: model_exp) =>
      switch (x.term) {
      | Atom(Int(n)) => n
      | _ => Bigint.of_int(-1)
      };
  let model_default: model_t = Bigint.of_int(50);

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let expansion_f: model_t => expansion_t =
    (x: model_t) =>
      switch (x) {
      | n => n
      };
  let expansion_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | n => DHExp.fresh(Atom(Int(n)))
      };
  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      /* No action needed for this livelit */
      switch (action) {
      | SetModel(n) => n
      };
    };

  /* You could also make this just an Int */
  let hazel_action_t: TermBase.Typ.t =
    Sum([Variant("SetModel", [], Some(Atom(Int) |> Typ.fresh))])
    |> Typ.fresh;
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(n) =>
        Ap(
          Forward,
          Constructor("SetModel", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(n)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      };
  let action_from_hazel: action_exp => action_t =
    (action: action_exp) => {
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        SetModel(n)
      | _ => SetModel(Bigint.of_int(-1))
      };
    };

  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list =
    (model: model_t, send_action) => {
      let n = model;

      List([
        Node.div([Node.text("Slider value: " ++ Bigint.to_string(n))]),
        Util.Web.range(
          ~attrs=[
            Attr.value(Bigint.to_string(n)),
            Attr.on_input((_, v: string) => {
              send_action(SetModel(Bigint.of_string(v)))
            }),
          ],
          ~min="0",
          ~max="100",
          Bigint.to_string(n),
        ),
      ]);
    };

  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 20,
    };
};

let livelits: list(raw_livelit) = [raw_of_builtin((module Slider))];

// // /* Type for a livelit */
// // [@deriving (show({with_path: false}), sexp, yojson)]
// // type t = LivelitCtx.BuiltinLivelit;
// type Livelit = LivelitCtx.BuiltinLivelit;

// /* Slider livelit */
// let slider: t = {
//   explain_this: ["A slider livelit -- a simple integer input from 0 to 100."],
//   name: "slider",
//   expansion_t: Typ.temp(Int),
//   expansion_f: (model: Exp.t) =>
//     switch (model.term) {
//     | Int(n) => DHExp.fresh(Int(n))
//     | _ => DHExp.fresh(Undefined)
//     },
//   model_t: Typ.temp(Int),
//   model_default: "50",
//   projector: (model: list(Ctx.model_piece), update, _id: Id.t) => {
//     let Ctx.{model, piece} = List.nth(model, 0);
//     let n =
//       switch (model.term) {
//       | Int(n) => n
//       | _ => failwith("Slider livelit: not given int")
//       };

//     Node(
//       Util.Web.range(
//         ~attrs=[Attr.on_input((_, v) => update(put(v, Piece.id(piece))))],
//         string_of_int(n),
//       ),
//     );
//   },
//   size:
//     ProjectorCore.Shape.{
//       vertical: Inline,
//       horizontal: 20,
//     },
// };

// /* JS livelit */
// let js: t = {
//   explain_this: ["JavaScript execution livelit"],
//   name: "js",
//   expansion_t: Typ.temp(String),
//   expansion_f: (model: Exp.t) =>
//     switch (model.term) {
//     | Tuple([_code, result]) => result
//     | _ => DHExp.fresh(Undefined)
//     },
//   model_t: Typ.temp(Prod([Typ.temp(String), Typ.temp(String)])),
//   model_default: "\"1 + 1\", \"\"",
//   projector: (models: list(model_piece), update, _id: Id.t) => {
//     /* We expect exactly two model pieces: (code, result). */
//     let ((code_model, _code_piece), (_result_model, result_piece)) =
//       switch (models) {
//       | [{model: m_code, piece: p_code}, {model: m_result, piece: p_result}] => (
//           (m_code, p_code),
//           (m_result, p_result),
//         )
//       | _ => failwith("JS livelit: expected two model pieces (code, result)")
//       };

//     /* Extract the user-supplied code from the first piece. */
//     let code =
//       switch (code_model.term) {
//       | String(s) => s
//       | _ => failwith("JS livelit: 'code' is not a string")
//       };

//     let result = Js_of_ocaml.Js.Unsafe.eval_string("String(" ++ code ++ ")");

//     List([
//       Node.div([Node.text("Code: " ++ code)]),
//       /* compute button */
//       Node.button(
//         ~attrs=[
//           Attr.on_click(_ => {
//             update(put("\"" ++ result ++ "\"", Piece.id(result_piece)))
//           }),
//         ],
//         [Node.text("Compute")],
//       ),
//     ]);
//   },
//   size:
//     ProjectorCore.Shape.{
//       vertical: Inline,
//       horizontal: 20,
//     },
// };

// /* Inline Emotion livelit
//           - Draws a face with eyes and a mouth
//           - Shows a slider below the face
//           - The mouth shape changes based on the slider’s value.
//    */
// let emotion: t = {
//   explain_this: ["An emotion livelit"],
//   name: "emotion",
//   expansion_t: Typ.temp(String),
//   expansion_f: (model: Exp.t) =>
//     switch (model.term) {
//     | Int(n) =>
//       DHExp.fresh(
//         String(
//           if (n < 40) {
//             "sad";
//           } else if (n > 70) {
//             "happy";
//           } else {
//             "neutral";
//           },
//         ),
//       )
//     | _ => DHExp.fresh(Undefined)
//     },
//   model_default: "50",
//   model_t: Typ.temp(Int),
//   projector: (model: list(model_piece), update, _id: Id.t) => {
//     let {model, piece} = List.nth(model, 0);
//     let n =
//       switch (model.term) {
//       | Int(n) => n
//       | _ => failwith("Emotion livelit: not given int")
//       };

//     /* Calculate mouth curvature based on n */
//     let smile = (100.0 -. float_of_int(n)) /. 100.0 *. 50.0 -. 25.0;
//     let pathData =
//       "M60 130 Q100 " ++ Printf.sprintf("%.1f", 130.0 -. smile) ++ " 140 130";

//     List([
//       Node.create_svg(
//         "svg",
//         ~attrs=[Attr.create("width", "200"), Attr.create("height", "200")],
//         [
//           Node.create_svg(
//             "circle",
//             ~attrs=[
//               Attr.create("cx", "100"),
//               Attr.create("cy", "100"),
//               Attr.create("r", "90"),
//               Attr.create("fill", "yellow"),
//               Attr.create("stroke", "black"),
//             ],
//             [],
//           ),
//           Node.create_svg(
//             "circle",
//             ~attrs=[
//               Attr.create("cx", "65"),
//               Attr.create("cy", "80"),
//               Attr.create("r", "10"),
//               Attr.create("fill", "black"),
//             ],
//             [],
//           ),
//           Node.create_svg(
//             "circle",
//             ~attrs=[
//               Attr.create("cx", "135"),
//               Attr.create("cy", "80"),
//               Attr.create("r", "10"),
//               Attr.create("fill", "black"),
//             ],
//             [],
//           ),
//           Node.create_svg(
//             "path",
//             ~attrs=[
//               Attr.create("d", pathData),
//               Attr.create("stroke", "black"),
//               Attr.create("fill", "transparent"),
//               Attr.create("stroke-width", "5"),
//             ],
//             [],
//           ),
//         ],
//       ),
//       Util.Web.range(
//         ~attrs=[
//           Attr.value(string_of_int(n)),
//           Attr.on_input((_, v) => update(put(v, Piece.id(piece)))),
//         ],
//         ~min="0",
//         ~max="100",
//         string_of_int(n),
//       ),
//     ]);
//   },
//   size:
//     ProjectorCore.Shape.{
//       vertical: Block(10),
//       horizontal: 20,
//     },
// };

// let livelits: list(t) = [slider, emotion, js];
