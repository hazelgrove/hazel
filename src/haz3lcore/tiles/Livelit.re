open Virtual_dom.Vdom;
open ProjectorCore;
open LivelitCtx;
open Grammar;

type livelit_name = string;

// referenced in docs/livelits.md
module Slider: BuiltinLivelit = {
  let name = "slider";

  type model_t = Bigint.t;
  type expansion_t = Bigint.t;
  type action_t =
    | SetModel(model_t);

  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));
  let model_from_hazel: model_exp => option(model_t) =
    (x: model_exp) => {
      switch (x.term) {
      | Atom(Int(n)) => Some(n)
      | _ => None
      };
    };
  let model_default: model_t = Bigint.of_int(50);

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let expand: model_t => expansion_t =
    (x: model_t) =>
      switch (x) {
      | n => n
      };
  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | n => DHExp.fresh(Atom(Int(n)))
      };
  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      switch (action) {
      | SetModel(n) => n
      };
    };

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
  let action_from_hazel: action_exp => option(action_t) =
    (action: action_exp) => {
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        Some(SetModel(n))
      | _ => None
      };
    };

  let view = (model: model_t, send_action) => {
    let n = model;

    Util.Web.range(
      ~attrs=[
        Attr.on_input((_, v: string) => {
          send_action(SetModel(Bigint.of_string(v)))
        }),
      ],
      ~min="0",
      ~max="100",
      Bigint.to_string(n),
    );
  };

  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 20,
    };
};

module Emotion: BuiltinLivelit = {
  let name = "emotion";

  /* The model is an integer represented as Bigint.t */
  type model_t = Bigint.t;
  /* The expansion is a string representing the emotion */
  type expansion_t = string;
  type action_t =
    | SetModel(model_t);

  /* Hazel model type is an integer */
  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));

  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));

  let model_from_hazel: model_exp => option(model_t) =
    (x: model_exp) =>
      switch (x.term) {
      | Atom(Int(n)) => Some(n)
      | _ => None
      };

  /* Default model value is 50 */
  let model_default: model_t = Bigint.of_int(50);

  /* Hazel expansion type is a String */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  /* Compute the emotion based on the slider value:
     - less than 40: "sad"
     - greater than 70: "happy"
     - otherwise: "neutral" */
  let expand: model_t => expansion_t =
    (x: model_t) => {
      let n = int_of_string(Bigint.to_string(x));
      if (n < 40) {
        "sad";
      } else if (n > 70) {
        "happy";
      } else {
        "neutral";
      };
    };

  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) => DHExp.fresh(Atom(String(x)));

  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      /* Update the model based on the action */
      switch (action) {
      | SetModel(n) => n
      };
    };

  /* Define the action type for Hazel */
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

  let action_from_hazel: action_exp => option(action_t) =
    (action: action_exp) =>
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        Some(SetModel(n))
      | _ => None
      };

  let size =
    ProjectorCore.Shape.{
      vertical: Block(10),
      horizontal: 20,
    };

  let view = (model: model_t, send_action) => {
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
      Util.Web.range(
        ~attrs=[
          Attr.on_input((_, v) => {
            send_action(SetModel(Bigint.of_string(v)))
          }),
        ],
        ~min="0",
        ~max="100",
        Bigint.to_string(n),
      ),
    ]);
  };
};

module Js: BuiltinLivelit = {
  let name = "js";

  /* The model holds (code, result) both as strings. */
  type model_t = {
    code: string,
    result: string,
  };

  /* The expansion is just the result string. */
  type expansion_t = string;

  /* We update the entire model at once. */
  type action_t =
    | SetModel(model_t);

  /* Model type in Hazel: a 2-tuple of strings. */
  let hazel_model_t: TermBase.Typ.t =
    Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))]) |> Typ.fresh;

  /* Convert model to a Hazel expression. */
  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let code_expr = DHExp.fresh(Atom(String(m.code)));
      let result_expr = DHExp.fresh(Atom(String(m.result)));
      DHExp.fresh(Tuple([code_expr, result_expr]));
    };

  /* Convert a Hazel expression back to the model. */
  let model_from_hazel: model_exp => option(model_t) =
    (expr: model_exp) => {
      switch (expr.term) {
      | Tuple([
          {term: Atom(String(code)), _},
          {term: Atom(String(result)), _},
        ]) =>
        Some({
          code,
          result,
        })
      | _ => None
      };
    };

  /* Default model: "1 + 1" with empty result. */
  let model_default: model_t = {
    code: "1 + 1",
    result: "",
  };

  /* Expansion type in Hazel: a string. */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(String));

  /* The expansion is just the current `result`. */
  let expand: model_t => expansion_t = (m: model_t) => m.result;

  let expand_to_hazel: expansion_t => expansion_exp =
    (res: expansion_t) => DHExp.fresh(Atom(String(res)));

  /* Updating the model means storing the new model. */
  let update: (action_t, model_t) => model_t =
    (action: action_t, _oldModel: model_t) =>
      switch (action) {
      | SetModel(m) => m
      };

  /* Hazel action type: single variant with our product type. */
  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant(
        "SetModel",
        [],
        Some(
          Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))])
          |> Typ.fresh,
        ),
      ),
    ])
    |> Typ.fresh;

  /* Convert action -> Hazel expression. */
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(m) =>
        let code_expr = DHExp.fresh(Atom(String(m.code)));
        let result_expr = DHExp.fresh(Atom(String(m.result)));
        let tuple_expr = DHExp.fresh(Tuple([code_expr, result_expr]));

        Ap(
          Forward,
          Constructor(
            "SetModel",
            Some(
              Some(
                Prod([Typ.temp(Atom(String)), Typ.temp(Atom(String))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      };

  /* Convert Hazel expression -> action. */
  let action_from_hazel: action_exp => option(action_t) =
    (expr: action_exp) =>
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {
            term:
              Tuple([
                {term: Atom(String(code)), _},
                {term: Atom(String(result)), _},
              ]),
            _,
          },
        ) =>
        Some(
          SetModel({
            code,
            result,
          }),
        )
      | _ => None
      };

  /* Render: show code input, a compute button, and the result. */
  let view = (model: model_t, send_action) => {
    let {code, result} = model;

    Node.div([
      /* Code input field */
      Node.input(
        ~attrs=[
          Attr.type_("text"),
          Attr.value(code),
          Attr.on_input((_, v: string) => {
            /* Update the code, keep the same result */
            send_action(
              SetModel({
                code: v,
                result: model.result,
              }),
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
              SetModel({
                code,
                result: Js_of_ocaml.Js.to_string(evaluated),
              }),
            );
          }),
        ],
        [Node.text("Compute")],
      ),
      /* Display the current result */
      Node.div([Node.text("Result: " ++ result)]),
    ]);
  };

  /* Reasonable default shape. */
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 40,
    };
};

module BoundedSlider: BuiltinLivelit = {
  let name = "boundedslider";

  type model_t = {
    value: Bigint.t,
    min: Bigint.t,
    max: Bigint.t,
  };

  type expansion_t = Bigint.t;

  type action_t =
    | SetValue(Bigint.t)
    | SetBounds(Bigint.t, Bigint.t);

  // Default model with value=50, min=0, max=100
  let model_default: model_t = {
    value: Bigint.of_int(50),
    min: Bigint.of_int(0),
    max: Bigint.of_int(100),
  };

  // Hazel type for model: tuple of three integers
  let hazel_model_t: TermBase.Typ.t =
    Prod([
      Typ.temp(Atom(Int)), // value
      Typ.temp(Atom(Int)), // min
      Typ.temp(Atom(Int)) // max
    ])
    |> Typ.fresh;

  // Conversion functions between model and Hazel expressions
  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let value_expr = DHExp.fresh(Atom(Int(m.value)));
      let min_expr = DHExp.fresh(Atom(Int(m.min)));
      let max_expr = DHExp.fresh(Atom(Int(m.max)));
      DHExp.fresh(Tuple([value_expr, min_expr, max_expr]));
    };

  let model_from_hazel: model_exp => option(model_t) =
    (expr: model_exp) => {
      switch (expr.term) {
      | Tuple([
          {term: Atom(Int(value)), _},
          {term: Atom(Int(min)), _},
          {term: Atom(Int(max)), _},
        ]) =>
        Some({
          value,
          min,
          max,
        })
      | _ => None
      };
    };

  // Expansion type is just an integer
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));

  // Expansion function (just returns the current value)
  let expand: model_t => expansion_t = model => model.value;

  // Convert expansion to Hazel expression
  let expand_to_hazel: expansion_t => expansion_exp =
    (value: expansion_t) => DHExp.fresh(Atom(Int(value)));

  // Update function to handle actions
  let update: (action_t, model_t) => model_t =
    (action: action_t, model: model_t) => {
      switch (action) {
      | SetValue(value) => {
          ...model,
          value,
        }
      | SetBounds(min, max) => {
          ...model,
          min,
          max,
        }
      };
    };

  // Hazel action type
  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant("SetValue", [], Some(Atom(Int) |> Typ.fresh)),
      Variant(
        "SetBounds",
        [],
        Some(
          Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))]) |> Typ.fresh,
        ),
      ),
    ])
    |> Typ.fresh;

  // Convert action to Hazel expression
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetValue(value) =>
        Ap(
          Forward,
          Constructor("SetValue", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(value)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      | SetBounds(min, max) =>
        let min_expr = DHExp.fresh(Atom(Int(min)));
        let max_expr = DHExp.fresh(Atom(Int(max)));
        let tuple_expr = DHExp.fresh(Tuple([min_expr, max_expr]));

        Ap(
          Forward,
          Constructor(
            "SetBounds",
            Some(
              Some(
                Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(Int))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          tuple_expr,
        )
        |> DHExp.fresh;
      };

  // Convert Hazel expression to action
  let action_from_hazel: action_exp => option(action_t) =
    (expr: action_exp) =>
      switch (expr.term) {
      | Ap(
          Forward,
          {term: Constructor("SetValue", _), _},
          {term: Atom(Int(value)), _},
        ) =>
        Some(SetValue(value))
      | Ap(
          Forward,
          {term: Constructor("SetBounds", _), _},
          {
            term:
              Tuple([
                {term: Atom(Int(min)), _},
                {term: Atom(Int(max)), _},
              ]),
            _,
          },
        ) =>
        Some(SetBounds(min, max))
      | _ => None
      };

  // View function to render the slider with bounds
  let view = (model: model_t, send_action) => {
    let {value, min, max} = model;

    Node.div([
      // Inputs to adjust bounds
      Node.div([
        Node.text("Min: "),
        Node.input(
          ~attrs=[
            Attr.type_("number"),
            Attr.value(Bigint.to_string(min)),
            Attr.on_input((_, v: string) => {
              let new_min = Bigint.of_string(v);
              send_action(SetBounds(new_min, max));
            }),
          ],
          (),
        ),
        Node.text(" Max: "),
        Node.input(
          ~attrs=[
            Attr.type_("number"),
            Attr.value(Bigint.to_string(max)),
            Attr.on_input((_, v: string) => {
              let new_max = Bigint.of_string(v);
              send_action(SetBounds(min, new_max));
            }),
          ],
          (),
        ),
      ]),
      // The slider itself
      Util.Web.range(
        ~attrs=[
          Attr.on_input((_, v: string) => {
            send_action(SetValue(Bigint.of_string(v)))
          }),
        ],
        ~min=Bigint.to_string(min),
        ~max=Bigint.to_string(max),
        Bigint.to_string(value),
      ),
    ]);
  };

  // Size specification
  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Block(5),
      horizontal: 30,
    };
};

let livelits: list(raw_livelit) =
  [(module Slider), (module Emotion), (module Js), (module BoundedSlider)]
  |> List.map(raw_of_builtin);
