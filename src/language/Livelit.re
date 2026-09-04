open Virtual_dom.Vdom;
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

  /* The result's shape depends on the program text -- 1 + 2 is an Int,
     (get(1), get(2)) is a pair -- so no single static type is right for every
     program. Unknown lets a livelit be used wherever its actual result fits,
     and mismatches surface as ordinary Hazel type errors. */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Unknown(Internal));
  let requires_annotation = false;
  let expand: (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model_t) => expansion_t =
    (~ana as _, ~tools as _, x: model_t) =>
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
    Sum([
      Variant(
        "SetModel",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        Some(Atom(Int) |> Typ.fresh),
      ),
    ])
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

  let view = (~id as _: Id.t, model: model_t, send_action) => {
    let n = model;

    Util.WebUtil.range(
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

  let size: Util.ProjectorShape.t = {
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
  let requires_annotation = false;
  let expand: (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model_t) => expansion_t =
    (~ana as _, ~tools as _, x: model_t) => {
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
    Sum([
      Variant(
        "SetModel",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        Some(Atom(Int) |> Typ.fresh),
      ),
    ])
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
    Util.ProjectorShape.{
      vertical: Block(10),
      horizontal: 20,
    };

  let view = (~id as _: Id.t, model: model_t, send_action) => {
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
      Util.WebUtil.range(
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
  let requires_annotation = false;
  let expand: (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model_t) => expansion_t =
    (~ana as _, ~tools as _, m: model_t) => m.result;

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
  let view = (~id as _: Id.t, model: model_t, send_action) => {
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
  let size: Util.ProjectorShape.t = {
    vertical: Inline,
    horizontal: 40,
  };
};

/* The Fumola livelit.

   Its Hazel-visible model is a pair `(instance_id, program_text)`. The
   runtime that `instance_id` names does not live in Hazel's value domain at
   all: it lives in a store held by the Fumola wasm module,

       sigma : FumolaInstanceId -> FumolaRuntimeState

   reached here through the `window.fumola` shim. Editing the livelit keeps
   the same instance id and re-evaluates the new text against the same
   persistent Fumola runtime, so that runtime's adapton store is carried
   across the edit rather than being rebuilt. Expansion is an *observation*
   of that external state, translated back into a Hazel value; the result is
   deliberately not a second source of truth in the model.

   See the design notes for the open questions this MVP does not settle. */
module Fumola: BuiltinLivelit = {
  let name = "fumola";

  type model_t = {
    /* Opaque name for an entry of sigma. Represented as an integer for now;
       it is deliberately never used as an integer by the Hazel program. */
    instance_id: int,
    program: string,
  };

  /* A Fumola result becomes a Hazel value of whatever shape it has: an
     integer, a tuple, a record, a variant. An untranslatable result (a syntax
     error mid-edit, a Fumola value with no Hazel counterpart, or a runtime
     that has not finished loading) expands to a hole rather than to something
     misleading. The string carries the reason, for the widget to show. */
  type expansion_t = result(expansion_exp, string);

  type action_t =
    | SetModel(model_t);

  /* ---- the shim boundary -------------------------------------------- */

  /* The shim is absent outside the browser (notably under the test runner),
     and absent in the browser until the wasm artifacts have been built. Both
     are reported rather than raised: a livelit whose runtime is missing should
     degrade to a message, not take down evaluation. */
  exception No_runtime;

  /* Looked up as a property of the global object rather than with
     [js_expr]: js_of_ocaml cannot compile a [js_expr] string ahead of time
     and falls back to runtime evaluation, which it reports as an error on
     every call. */
  let runtime = () =>
    switch (
      Js_of_ocaml.Js.Optdef.to_option(
        Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "fumola"),
      )
    ) {
    | exception _ => None
    | shim => shim
    };

  let shim = (method_name: string, args): Js_of_ocaml.Js.Unsafe.any =>
    switch (runtime()) {
    | Some(shim) => Js_of_ocaml.Js.Unsafe.meth_call(shim, method_name, args)
    | None => raise(No_runtime)
    };

  let js_string = (s: string) =>
    Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(s));
  let js_int = (n: int) => Js_of_ocaml.Js.Unsafe.inject(n);

  /* Ask the shim which instance this projector should be using. The shim
     hands back the same id when this projector already owns it, and a fresh
     one when the id is already owned by a different live projector -- which
     is what makes duplicating a livelit generative rather than aliasing one
     runtime between two copies. */
  let claim = (~owner: string, instance_id: int): int =>
    switch (shim("claim", [|js_int(instance_id), js_string(owner)|])) {
    | exception _ => instance_id
    | claimed =>
      claimed
      |> Js_of_ocaml.Js.Unsafe.coerce
      |> Js_of_ocaml.Js.float_of_number
      |> int_of_float
    };

  /* Evaluate against sigma(instance_id), realizing the runtime first if this
     session has no entry for that id (the reload path). The shim answers with
     the runtime's JSON verbatim:

       {"ok": true,  "tag": <tag>, "value": <json>}
       {"ok": false, "error": <message>}

     Structure is preserved on the way across, so that a Fumola tuple can be
     rebuilt here as a Hazel tuple rather than as a wrapper Hazel has to take
     apart. */

  /* The rendering and the expansion, from one evaluation. */
  let observe_described =
      (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model: model_t)
      : (expansion_t, string) => {
    let response =
      switch (
        shim(
          "evalSync",
          [|js_int(model.instance_id), js_string(model.program)|],
        )
      ) {
      | exception _ => None
      | r =>
        Some(r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string)
      };
    switch (response) {
    | None =>
      let message = "no Fumola runtime available";
      (Error(message), message);
    | Some(response) =>
      switch (Yojson.Safe.from_string(response)) {
      | exception _ =>
        let message = "could not read the Fumola runtime's response";
        (Error(message), message);
      | `Assoc(obj) as json =>
        switch (List.assoc_opt("ok", obj)) {
        | Some(`Bool(true)) =>
          switch (FumolaValue.exp_of_json(~ana, ~tools, json)) {
          | Ok(exp) => (Ok(exp), FumolaValue.describe(json))
          | Error(message) => (Error(message), message)
          }
        | _ =>
          let message =
            switch (List.assoc_opt("error", obj)) {
            | Some(`String(message)) => message
            | _ => "the Fumola program did not produce a value"
            };
          (Error(message), message);
        }
      | _ =>
        let message = "could not read the Fumola runtime's response";
        (Error(message), message);
      }
    };
  };

  /* ---- Hazel encodings ---------------------------------------------- */

  let hazel_model_t: TermBase.Typ.t =
    Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(String))]) |> Typ.fresh;

  let model_to_hazel: model_t => model_exp =
    (m: model_t) =>
      DHExp.fresh(
        Tuple([
          DHExp.fresh(Atom(Int(Bigint.of_int(m.instance_id)))),
          DHExp.fresh(Atom(String(m.program))),
        ]),
      );

  let model_from_hazel: model_exp => option(model_t) =
    (e: model_exp) =>
      switch (e.term) {
      | Tuple([
          {term: Atom(Int(id)), _},
          {term: Atom(String(program)), _},
        ]) =>
        switch (int_of_string_opt(Bigint.to_string(id))) {
        | Some(instance_id) =>
          Some({
            instance_id,
            program,
          })
        | None => None
        }
      | _ => None
      };

  /* Instance id 0 is never handed out by the shim; it means "this livelit has
     not claimed a runtime yet", and the first view claims a real one. */
  let model_default: model_t = {
    instance_id: 0,
    program: "1 + 2",
  };

  /* The result's shape depends on the program text -- 1 + 2 is an Int,
     (get(1), get(2)) is a pair -- so no single static type is right for every
     program. Unknown lets a livelit be used wherever its actual result fits,
     and mismatches surface as ordinary Hazel type errors. */
  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Unknown(Internal));

  /* The result's shape depends on both the program and the type asked of it,
     so this livelit only expands in checking mode. */
  let requires_annotation = true;

  /* The widget renders outside of any typing context, so it resolves nothing
     and unfolds nothing. Names it cannot resolve simply render as themselves,
     which is all the widget needs -- it shows what the program produced, not
     how it will be typed. */
  let view_tools: LivelitCtx.type_tools = {
    resolve_ctr: (~ana as _, _) => None,
    normalize: ty => ty,
  };

  let expand: (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model_t) => expansion_t =
    (~ana, ~tools, m: model_t) => fst(observe_described(~ana, ~tools, m));

  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | Ok(exp) => exp
      | Error(_) => DHExp.fresh(EmptyHole)
      };

  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) =>
      switch (action) {
      | SetModel(m) => m
      };

  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant(
        "SetModel",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        Some(
          Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(String))]) |> Typ.fresh,
        ),
      ),
    ])
    |> Typ.fresh;

  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(m) =>
        Ap(
          Forward,
          Constructor(
            "SetModel",
            Some(
              Some(
                Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(String))])
                |> Typ.fresh,
              ),
            ),
          )
          |> DHExp.fresh,
          model_to_hazel(m),
        )
        |> DHExp.fresh
      };

  let action_from_hazel: action_exp => option(action_t) =
    (e: action_exp) =>
      switch (e.term) {
      | Ap(Forward, {term: Constructor("SetModel", _), _}, model) =>
        switch (model_from_hazel(model)) {
        | Some(m) => Some(SetModel(m))
        | None => None
        }
      | _ => None
      };

  let view = (~id: Id.t, model: model_t, send_action) => {
    /* A model whose instance id is still 0 has never named a runtime, so it
       claims one here and writes the id back.

       This fires only in that one case, deliberately. An earlier version
       re-claimed on every render so that a duplicated livelit could be given
       a fresh runtime, but that makes rendering rewrite the very syntax being
       rendered: the projector's id is the id of its syntax root, so the
       rewrite can change the owner, which invalidates the next claim, which
       rewrites again. Claiming once, and only from 0, cannot loop. */
    let claimed =
      if (model.instance_id == 0) {
        let claimed = claim(~owner=Id.to_string(id), 0);
        if (claimed != 0) {
          /* Deferred rather than run here: applying an action in the middle
             of rendering would mutate the state being rendered. */
          let effect =
            send_action(
              SetModel({
                ...model,
                instance_id: claimed,
              }),
            );
          let _ =
            Js_of_ocaml.Js.Unsafe.fun_call(
              Js_of_ocaml.Js.Unsafe.js_expr("window.setTimeout"),
              [|
                Js_of_ocaml.Js.Unsafe.inject(
                  Js_of_ocaml.Js.wrap_callback(() =>
                    Ui_effect.Expert.handle(effect)
                  ),
                ),
                Js_of_ocaml.Js.Unsafe.inject(0),
              |],
            );
          ();
        };
        claimed;
      } else {
        model.instance_id;
      };

    /* Observe the instance the *model* names, which is the one `expand` will
       use. Observing the newly claimed instance instead would let the widget
       display a result computed in a different runtime from the one the
       program evaluates in. A claim made during this render takes effect from
       the next one, once the model actually names it. */
    /* The widget has no expected type of its own -- it is rendering what the
       program produced, not what some enclosing annotation asked for -- so it
       observes against Unknown. The expansion, which does have an expected
       type, is computed separately by expand. */
    let result =
      snd(
        observe_described(
          ~ana=Typ.fresh(Unknown(Internal)),
          ~tools=view_tools,
          model,
        ),
      );

    Node.div(
      ~attrs=[Attr.class_("fumola-livelit")],
      [
        Node.input(
          ~attrs=[
            Attr.type_("text"),
            Attr.value(model.program),
            Attr.on_input((_, program: string)
              /* The instance id is preserved across the edit: this is what
                 makes the edit incremental rather than a fresh run. */
              =>
                send_action(
                  SetModel({
                    instance_id: claimed,
                    program,
                  }),
                )
              ),
          ],
          (),
        ),
        Node.div(
          ~attrs=[Attr.class_("fumola-result")],
          [Node.text(result)],
        ),
        Node.div(
          ~attrs=[Attr.class_("fumola-id")],
          [Node.text("#" ++ string_of_int(model.instance_id))],
        ),
      ],
    );
  };

  let size: Util.ProjectorShape.t = {
    vertical: Inline,
    horizontal: 40,
  };
};

let livelits: list(raw_livelit) =
  [(module Slider), (module Emotion), (module Js), (module Fumola)]
  |> List.map(raw_of_builtin);
