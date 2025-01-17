open Util;
open Virtual_dom.Vdom;

/* A utility function for putting a string into a tile. */
type model_piece = {
  model: UExp.t,
  piece: Piece.tile,
};

let put: (string, Uuidm.t) => Piece.tile =
  (s, id) => {
    let piece = Piece.replace_id(id, Piece.mk_mono(Exp, s));
    switch (piece) {
    | Tile(t) => t
    | _ => failwith("put: not a tile")
    };
  };

/* Type for a livelit */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  model_t: Typ.t,
  expansion_t: Typ.t,
  expansion_f: UExp.t => UExp.t,
  projector:
    (list(model_piece), Piece.tile => Ui_effect.t(unit)) =>
    Virtual_dom.Vdom.Node.t,
  size: ProjectorBase.shape,
};

/* Slider livelit */
let slider: t = {
  name: "slider",
  expansion_t: Typ.temp(Int),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) => DHExp.fresh(Int(n))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Int),
  projector: (model: list(model_piece), update) => {
    let {model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Slider livelit: not given int")
      };

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        Util.Web.range(
          ~attrs=[Attr.on_input((_, v) => update(put(v, piece.id)))],
          string_of_int(n),
        ),
      ],
    );
  },
  size: Inline(20),
};

/* JS livelit */
let js: t = {
  name: "js",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Tuple([_code, result]) => DHExp.fresh(Tuple([result]))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Prod([Typ.temp(String), Typ.temp(String)])),
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
    let hidden_result_id = "hidden-result-test"; // ++ Uuidm.to_string(result_piece.id);

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
          Attr.type_("hidden"),
          Attr.value(""),
          /* When the script is done, it sets this to the new result. */
          Attr.on_input((_, newVal) => {
            /* Update the second piece’s model with the new result. */
            update(
              put(newVal, result_piece.id),
            )
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
              Attr.on_click(_ =>
                Js_of_ocaml.Js.Unsafe.eval_string(
                  "document.getElementById('"
                  ++ hidden_result_id
                  ++ "').value = String("
                  ++ result
                  ++ ")",
                )
              ),
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
};

/* Timestamp livelit */
let timestamp: t = {
  name: "timestamp",
  expansion_t: Typ.temp(Int),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(Int(Float.to_int(JsUtil.timestamp()))),
  model_t: Typ.temp(Prod([])),
  projector: (_model: list(model_piece), _parent) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Timestamp livelit")],
    ),
  size: Inline(20),
};

/* Syntax error livelit */
let syntax_error: t = {
  name: "syntax_error",
  expansion_t: Typ.temp(Unknown(Internal)),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(String("Syntax error -- are statics enabled?")),
  model_t: Typ.temp(Unknown(Internal)),
  projector: (_model: list(model_piece), _) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Syntax error -- are statics enabled?")],
    ),
  size: Inline(20),
};

/* Inline Emotion livelit
           - Draws a face with eyes and a mouth
           - Shows a slider below the face
           - The mouth shape changes based on the slider’s value.
   */
let emotion: t = {
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
      "M60 130 Q100 " ++ string_of_float(130.0 -. smile) ++ " 140 130";

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
            Attr.on_input((_, v) => update(put(v, piece.id))),
          ],
          ~min="0",
          ~max="100",
          string_of_int(n),
        ),
      ],
    );
  },
  size: Block({row: 10, col: 20}),
};

let fetch_url: t = {
  name: "fetch_url",
  expansion_t: Typ.temp(String),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | String(s) => DHExp.fresh(String(s))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(String),
  projector: (model: list(model_piece), update) => {
    let {model, piece} = List.nth(model, 0);
    let url =
      switch (model.term) {
      | String(s) => s
      | _ => failwith("fetch_url: not given a string URL")
      };

    /* We'll stash fetched text into this hidden input when it arrives. */
    let hidden_input_id = "hidden-input-" ++ Uuidm.to_string(piece.id);

    let script_code =
      {|
    (function() {
      fetch("|}
      ++ url
      ++ {|")
        .then(resp => resp.text())
        .then(text => {
          const input = document.getElementById("|}
      ++ hidden_input_id
      ++ {|\");
          if (input) {
            input.value = text;
            input.dispatchEvent(new Event('input', { bubbles: true }));
          }
        })
        .catch(err => console.error("Fetch error: ", err));
    })();
    |};
    // let script_code = "console.log('fetching from: " ++ url ++ "');";

    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [
        /* Hidden input to store the fetched text once available. */
        Node.input(
          ~attrs=[
            Attr.id(hidden_input_id),
            Attr.type_("hidden"),
            Attr.value(url),
            Attr.on_input((_, v) => {
              /* Once the fetch completes, update the tile with the new text */
              update(
                put("\"" ++ v ++ "\"", piece.id),
              )
            }),
          ],
          (),
        ),
        /* The script that performs the fetch and populates the hidden input. */
        Node.create("script", [Node.text(script_code)]),
        /* Visible text for debugging or user feedback. */
        Node.text("Fetching from: " ++ url),
      ],
    );
  },
  size: Inline(20),
};

/* Export the final set of livelits we want to keep. */
let livelits: list(t) = [
  slider,
  js,
  timestamp,
  emotion,
  syntax_error,
  fetch_url,
];

/* A helper to find a livelit by name. Returns syntax_error if not found. */
let find_livelit = (name: string): t =>
  switch (List.find_opt(l => l.name == name, livelits)) {
  | Some(l) => l
  | None => syntax_error
  };
