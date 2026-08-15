open Virtual_dom.Vdom;
open LivelitCtx;
open Grammar;

type livelit_name = string;

// referenced in docs/livelits.md
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
        ConstructorMap.mk_variant_ann(~ids=[], ()),
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

    Node.div(
      ~attrs=[
        Attr.style(
          Css_gen.concat([
            Css_gen.create(~field="display", ~value="flex"),
            Css_gen.create(~field="flex-direction", ~value="column"),
            Css_gen.create(~field="gap", ~value="3px"),
            Css_gen.create(~field="width", ~value="100%"),
          ]),
        ),
      ],
      [
        /* Code input field. Keystrokes stay here: without the stop, the
           keydown bubbles into the editor and edits the program. */
        Node.input(
          ~attrs=[
            Attr.type_("text"),
            Attr.value(code),
            Attr.on_keydown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
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
      ],
    );
  };

  /* Input row + button + result row. */
  let shape: Util.ProjectorShape.t = {
    vertical: Block(2),
    horizontal: 40,
  };
};

let livelits: list(raw_livelit) = [(module Js)] |> List.map(raw_of_builtin);
