open Util;
open Virtual_dom.Vdom;
open ProjectorCore;
open Ctx;

let put: (string, Uuidm.t) => Piece.t =
  (s, id) => {
    Piece.replace_id(id, Piece.mk_mono(Exp, s));
  };

/* Type for a livelit */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Ctx.livelit_entry;

/* Slider livelit */
let slider: t = {
  explain_this: [
    "A slider livelit -- a simple integer input from 0 to 100.",
    "Usage: ^slider(n: Int) -> Int",
  ],
  name: "slider",
  expansion_t: Typ.temp(Int),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) => DHExp.fresh(Int(n))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Int),
  model_default: "50",
  projector: (model: list(Ctx.model_piece), update) => {
    let Ctx.{model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Slider livelit: not given int")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Util.Web.range(
          ~attrs=[
            Attr.on_input((_, v) => update(put(v, Piece.id(piece)))),
          ],
          string_of_int(n),
        ),
      ],
    );
  },
  size: Inline(20),
  id: Id.invalid,
};

/* JS livelit */
let js: t = {
  explain_this: [
    "JavaScript execution livelit",
    "Usage: ^js(code: String, result: String) -> String",
  ],
  name: "js",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([_code, result]) => DHExp.fresh(Tuple([result]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Prod([Typ.temp(String), Typ.temp(String)])),
  model_default: "\"1 + 1\", \"\"",
  projector: (models: list(model_piece), update) => {
    /* We expect exactly two model pieces: (code, result). */
    let ((code_model, _code_piece), (_result_model, result_piece)) =
      switch (models) {
      | [{model: m_code, piece: p_code}, {model: m_result, piece: p_result}] => (
          (m_code, p_code),
          (m_result, p_result),
        )
      | _ => failwith("JS livelit: expected two model pieces (code, result)")
      };

    /* Extract the user-supplied code from the first piece. */
    let code =
      switch (code_model.term) {
      | String(s) => s
      | _ => failwith("JS livelit: 'code' is not a string")
      };

    /* We'll store the updated result here when code is run. */
    let hidden_result_id = Uuidm.to_string(Piece.id(result_piece));

    /* This script:
          - Reads `code`
          - Executes it via `eval` (or your function of choice)
          - Stores the result into the second hidden input
          - Dispatches an `input` event so we can update the tile’s model
       */
    let result = Js_of_ocaml.Js.Unsafe.eval_string(code);

    let hidden_input =
      Node.input(
        ~attrs=[
          Attr.id(hidden_result_id),
          Attr.value(""),
          Attr.on_input((_, new_text) => {
            print_endline("Updating result: " ++ new_text);
            update(put(new_text, Piece.id(result_piece)));
          }),
        ],
        (),
      );

    let out =
      Node.div(
        ~attrs=[Attr.class_("livelit")],
        [
          /* Hidden input to store the current result. */
          hidden_input,
          /* compute button */
          Node.button(
            ~attrs=[
              Attr.on_click(_ => {
                let unsafe_code =
                  "document.getElementById('"
                  ++ hidden_result_id
                  ++ "').value = String("
                  ++ result
                  ++ ")";
                print_endline("Running code: " ++ unsafe_code);
                Js_of_ocaml.Js.Unsafe.eval_string(unsafe_code);
              }),
            ],
            [Node.text("Compute")],
          ),
          /* A bit of UI showing code and/or result. */
          Node.div([Node.text("Code: " ++ code)]),
          Node.div([Node.text("Result: " ++ result)]),
        ],
      );

    out;
  },
  size: Inline(20),
  id: Id.invalid,
};

/* Timestamp livelit */
let timestamp: t = {
  explain_this: [
    "Timestamp livelit",
    "Usage: ^timestamp(time: Int, update_freq: Int) -> Int",
  ],
  name: "timestamp",
  expansion_t: Typ.temp(Int),
  expansion_f: (model: UExp.t) =>
    DHExp.fresh(Int(Float.to_int(JsUtil.timestamp()))),
  model_default: "0, 1000",
  model_t: Typ.temp(Prod([Typ.temp(Int), Typ.temp(Int)])),
  projector: (model: list(model_piece), _parent) => {
    let ((m_time, time_piece), (m_update_freq, update_freq_piece)) =
      switch (model) {
      | [{model: m_time, piece: p_time}, {model: m_freq, piece: p_freq}] => (
          (m_time, p_time),
          (m_freq, p_freq),
        )
      | _ =>
        failwith(
          "Timestamp livelit: expected two model pieces (time, update_freq)",
        )
      };

    let (time, update_freq) =
      switch (m_time.term, m_update_freq.term) {
      | (Int(t), Int(f)) => (t, f)
      | _ => failwith("Timestamp livelit: not given two ints")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Node.text(
          "Timestamp livelit -- time: "
          ++ string_of_int(time)
          ++ ", update_freq: "
          ++ string_of_int(update_freq),
        ),
      ],
    );
  },
  size: Inline(20),
  id: Id.invalid,
};

/* Syntax error livelit */
let syntax_error: t = {
  explain_this: ["A syntax error livelit"],
  name: "syntax_error",
  expansion_t: Typ.temp(Unknown(Internal)),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(String("Syntax error -- are statics enabled?")),
  model_t: Typ.temp(Unknown(Internal)),
  model_default: "I SHOULD NEVER APPEAR",
  projector: (_model: list(model_piece), _) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Syntax error -- are statics enabled?")],
    ),
  size: Inline(20),
  id: Id.invalid,
};

/* Inline Emotion livelit
       - Draws a face with eyes and a mouth
       - Shows a slider below the face
       - The mouth shape changes based on the slider’s value.
   */
let emotion: t = {
  explain_this: ["An emotion livelit"],
  name: "emotion",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) =>
      DHExp.fresh(
        String(
          if (n < 40) {
            "sad";
          } else if (n > 70) {
            "happy";
          } else {
            "neutral";
          },
        ),
      )
    | _ => DHExp.fresh(Undefined)
    },
  model_default: "50",
  model_t: Typ.temp(Int),
  projector: (model: list(model_piece), update) => {
    let {model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Emotion livelit: not given int")
      };

    /* Calculate mouth curvature based on n */
    let smile = (100.0 -. float_of_int(n)) /. 100.0 *. 50.0 -. 25.0;
    let pathData =
      "M60 130 Q100 " ++ Printf.sprintf("%.1f", 130.0 -. smile) ++ " 140 130";

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Node.create_svg(
          "svg",
          ~attrs=[
            Attr.create("width", "200"),
            Attr.create("height", "200"),
          ],
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
        Util.Web.range(
          ~attrs=[
            Attr.value(string_of_int(n)),
            Attr.on_input((_, v) => update(put(v, Piece.id(piece)))),
          ],
          ~min="0",
          ~max="100",
          string_of_int(n),
        ),
      ],
    );
  },
  size: Block({row: 10, col: 20}),
  id: Id.invalid,
};

/* Export the final set of livelits we want to keep. */
let livelits: list(t) = [slider, js, timestamp, emotion, syntax_error];

/* A helper to find a livelit by name. Returns syntax_error if not found, or if ctx not given. */
let find_livelit = (name: string, ctx: Ctx.t) => {
  let entry =
    List.find_opt(
      entry =>
        switch (entry) {
        | LivelitEntry({name: n, _}) when n == name => true
        | _ => false
        },
      ctx,
    );

  switch (entry) {
  | Some(LivelitEntry(l)) => l
  | _ => syntax_error
  };
};
