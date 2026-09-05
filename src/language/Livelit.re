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
  let expand:
    (
      ~id: Id.t,
      ~ana: TermBase.Typ.t,
      ~tools: LivelitCtx.type_tools,
      model_t
    ) =>
    expansion_t =
    (~id as _: Id.t, ~ana as _, ~tools as _, x: model_t) =>
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
  let expand:
    (
      ~id: Id.t,
      ~ana: TermBase.Typ.t,
      ~tools: LivelitCtx.type_tools,
      model_t
    ) =>
    expansion_t =
    (~id as _: Id.t, ~ana as _, ~tools as _, x: model_t) => {
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
  let expand:
    (
      ~id: Id.t,
      ~ana: TermBase.Typ.t,
      ~tools: LivelitCtx.type_tools,
      model_t
    ) =>
    expansion_t =
    (~id as _: Id.t, ~ana as _, ~tools as _, m: model_t) => m.result;

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
/* The two Fumola livelits differ only in how they run their program, so they
   share one implementation.

   A thunk livelit wraps its program as `force(<name> := thunk { ... })`,
   which is what gives an edit its incremental meaning. An editor livelit
   evaluates at the top level instead: the wrapper puts a program inside a
   force, and some things cannot run there -- Adapton.reset clears the store
   the enclosing force is still inside, peekForce asserts, and a binding made
   inside a thunk does not outlive it.

   Both name an instance, so two livelits carrying the same id share one
   runtime and can see each other's state and bindings. */
/* ---- the shim boundary, shared by every fumola livelit ------------- */

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

module type FumolaConfig = {
  let name: string;
  /* Evaluate at the top level rather than inside a thunk. */
  let top_level: bool;
  /* A thunk livelit's model carries the name of its thunk; the editor has no
     thunk and carries none. */
  let default_thunk_name: option(string);
  let default_program: string;
};

module MakeFumola = (C: FumolaConfig) : BuiltinLivelit => {
  let name = C.name;

  type model_t = {
    /* Opaque name for an entry of sigma. Represented as an integer for now;
       it is deliberately never used as an integer by the Hazel program. */
    instance_id: int,
    /* Fumola source for the symbol naming this livelit's thunk, for a thunk
       livelit; absent for the editor, which has no thunk.

       Source text rather than an encoded symbol, so Fumola's own parser
       decides what a symbol is: every form the language spells as one works
       and Hazel needs to know about none of them. Written by the programmer
       rather than derived, so it is stable across edits -- a name taken from
       a Hazel id would start a new thunk whenever that id changed, losing the
       history the thunk exists to keep. */
    thunk_name: option(string),
    program: string,
  };

  /* A Fumola result becomes a Hazel value of whatever shape it has: an
     integer, a tuple, a record, a variant. An untranslatable result (a syntax
     error mid-edit, a Fumola value with no Hazel counterpart, or a runtime
     that has not finished loading) expands to a hole rather than to something
     misleading. The string carries the reason, for the widget to show. */
  /* A failure carries whether the program merely failed to parse. A
     half-written program is a syntax error on nearly every keystroke, and
     saying so in the expansion would be noise; a program that parsed and then
     went wrong is worth surfacing. */
  type failure = {
    syntax: bool,
    message: string,
  };

  type expansion_t = result(expansion_exp, failure);

  type action_t =
    | SetModel(model_t);

  /* ---- the shim boundary -------------------------------------------- */

  /* Evaluate against sigma(instance_id), realizing the runtime first if this
     session has no entry for that id (the reload path). The shim answers with
     the runtime's JSON verbatim:

       {"ok": true,  "tag": <tag>, "value": <json>}
       {"ok": false, "error": <message>}

     Structure is preserved on the way across, so that a Fumola tuple can be
     rebuilt here as a Hazel tuple rather than as a wrapper Hazel has to take
     apart. */

  /* Run a program in this instance and hand back its JSON, for translation
     to dereference pointers with. Uncached in both directions: a later edit
     can change what a cell holds, and these calls must not evict the cached
     main program either.

     evalFresh runs at the top level, which is where this belongs: peek
     is how the editor reads a cell, and the editor mode is the one on a
     force-free stack. A peek does also work inside a force -- untracked
     but meaningful, which is what makes it useful for looking at a
     running computation -- so this is a matter of asking in the right
     mode rather than of avoiding a failure. What genuinely refuses from
     inside a force is a reset; that is where
     AdaptonError(UnreachableForceEnd) comes from. */
  let eval_in = (instance_id: int, program: string): Yojson.Safe.t => {
    let response =
      switch (
        shim("evalFresh", [|js_int(instance_id), js_string(program)|])
      ) {
      | exception _ => None
      | r =>
        Some(r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string)
      };
    switch (response) {
    | None => `Null
    | Some(response) =>
      switch (Yojson.Safe.from_string(response)) {
      | exception _ => `Null
      | json => json
      }
    };
  };

  /* The rendering and the expansion, from one evaluation. */
  let observe_described =
      (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model: model_t)
      : (expansion_t, string) => {
    let response =
      switch (
        C.top_level
          ? shim(
              "evalTop",
              [|js_int(model.instance_id), js_string(model.program)|],
            )
          : shim(
              "evalSync",
              [|
                js_int(model.instance_id),
                js_string(
                  switch (model.thunk_name) {
                  | Some(name) => name
                  | None => "`topLevel"
                  },
                ),
                js_string(model.program),
              |],
            )
      ) {
      | exception _ => None
      | r =>
        Some(r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string)
      };
    switch (response) {
    | None =>
      let message = "no Fumola runtime available";
      (
        Error({
          syntax: false,
          message,
        }),
        message,
      );
    | Some(response) =>
      switch (Yojson.Safe.from_string(response)) {
      | exception _ =>
        let message = "could not read the Fumola runtime's response";
        (
          Error({
            syntax: false,
            message,
          }),
          message,
        );
      | `Assoc(obj) as json =>
        switch (List.assoc_opt("ok", obj)) {
        | Some(`Bool(true)) =>
          switch (
            FumolaValue.exp_of_json(
              ~instance_id=model.instance_id,
              ~eval=eval_in(model.instance_id),
              ~ana,
              ~tools,
              json,
            )
          ) {
          | Ok(exp) => (Ok(exp), FumolaValue.describe(json))
          | Error(message) => (
              Error({
                syntax: false,
                message,
              }),
              message,
            )
          }
        | _ =>
          let message =
            switch (List.assoc_opt("error", obj)) {
            | Some(`String(message)) => message
            | _ => "the Fumola program did not produce a value"
            };
          let syntax =
            List.assoc_opt("kind", obj) == Some(`String("syntax"));
          (
            Error({
              syntax,
              message,
            }),
            message,
          );
        }
      | _ =>
        let message = "could not read the Fumola runtime's response";
        (
          Error({
            syntax: false,
            message,
          }),
          message,
        );
      }
    };
  };

  /* ---- Hazel encodings ---------------------------------------------- */

  /* A thunk livelit's model is (instance, thunk name, program); the editor
     has no thunk, so its model is (instance, program). Both name an instance,
     so two livelits carrying the same id share one runtime. */
  let hazel_model_t: TermBase.Typ.t =
    (
      switch (C.default_thunk_name) {
      | Some(_) =>
        Prod([
          Typ.temp(Atom(Int)),
          Typ.temp(Atom(String)),
          Typ.temp(Atom(String)),
        ])
      | None => Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(String))])
      }
    )
    |> Typ.fresh;

  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let instance = DHExp.fresh(Atom(Int(Bigint.of_int(m.instance_id))));
      let program = DHExp.fresh(Atom(String(m.program)));
      DHExp.fresh(
        Tuple(
          switch (m.thunk_name) {
          | Some(thunk_name) => [
              instance,
              DHExp.fresh(Atom(String(thunk_name))),
              program,
            ]
          | None => [instance, program]
          },
        ),
      );
    };

  let model_from_hazel: model_exp => option(model_t) =
    (e: model_exp) => {
      let instance = (id, rest) =>
        switch (int_of_string_opt(Bigint.to_string(id))) {
        | Some(instance_id) => Some(rest(instance_id))
        | None => None
        };
      switch (e.term, C.default_thunk_name) {
      | (
          Tuple([
            {term: Atom(Int(id)), _},
            {term: Atom(String(thunk_name)), _},
            {term: Atom(String(program)), _},
          ]),
          Some(_),
        ) =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: Some(thunk_name),
            program,
          }
        )
      | (
          Tuple([
            {term: Atom(Int(id)), _},
            {term: Atom(String(program)), _},
          ]),
          None,
        ) =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: None,
            program,
          }
        )
      | _ => None
      };
    };

  /* Instance id 0 is never handed out by the shim; it means "this livelit has
     not claimed a runtime yet", and the first view claims a real one. */
  let model_default: model_t = {
    instance_id: 0,
    thunk_name: C.default_thunk_name,
    program: C.default_program,
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

  let expand:
    (
      ~id: Id.t,
      ~ana: TermBase.Typ.t,
      ~tools: LivelitCtx.type_tools,
      model_t
    ) =>
    expansion_t =
    (~id as _, ~ana, ~tools, m: model_t) =>
      fst(observe_described(~ana, ~tools, m));

  let expand_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | Ok(exp) => exp
      /* A half-written program is a syntax error on nearly every keystroke,
         so that expands to a hole and says nothing. A program that parsed and
         then went wrong expands to a description of what went wrong, which is
         the only place the reader would otherwise see nothing at all. */
      | Error({syntax: true, _}) => DHExp.fresh(EmptyHole)
      | Error({syntax: false, message}) => DHExp.fresh(Invalid(message))
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
                    ...model,
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

/* Runs its program inside a named thunk, so editing it reuses that thunk's
   execution history. The default is arithmetic, which needs nothing else. */
module FumolaPutForce =
  MakeFumola({
    let name = "fumola_put_force";
    let top_level = false;
    let default_thunk_name = Some("`thunk");
    let default_program = "1 + 2";
  });

/* Runs its program at the top level of the same kind of runtime: no thunk, so
   no incremental reuse, but bindings outlive the program and the adapton
   operations that cannot run inside a force will work. */
module FumolaEval =
  MakeFumola({
    let name = "fumola_eval";
    let top_level = true;
    let default_thunk_name = None;
    let default_program = "1 := 2";
  });

/* Declares a runtime and the Adapton semantics it runs, and expands to the
     id naming it.
   *
   * The other two livelits attach to whatever runtime their id names, creating
   * one with the default semantics if none exists. This one is how a program
   * says which semantics it wants, once, where the runtime is made -- so that
   * every livelit sharing that id is talking to a runtime whose mode was
   * declared rather than inherited by accident.
   *
   * The id would ideally be abstract, hiding the number. Hazel's abstract types
   * come only from polymorphic binders today -- there is no signature sealing --
   * so it expands to an Int, and the docs call it a handle. */
module FumolaNew: BuiltinLivelit = {
  let name = "fumola_new";

  /* Fumola spells this {#simple; #graphical}. Hazel spells the same structure
     + Simple + Graphical, and the livelit API uses Hazel's spelling: a mode
     is a choice between two named alternatives, so it belongs in a sum rather
     than in a string that happens to hold one of two words. */
  let mode_t: TermBase.Typ.t =
    BuiltinsADT.sum_type([("Simple", None), ("Graphical", None)]);

  type mode =
    | Simple
    | Graphical;

  /* The constructor, as Hazel writes it. */
  let mode_name = (m: mode): string =>
    switch (m) {
    | Simple => "Simple"
    | Graphical => "Graphical"
    };

  /* What the runtime is asked for, which is Fumola's own tag. */
  let mode_source = (m: mode): string =>
    switch (m) {
    | Simple => "simple"
    | Graphical => "graphical"
    };

  let mode_of_name = (name: string): option(mode) =>
    switch (name) {
    | "Simple" => Some(Simple)
    | "Graphical" => Some(Graphical)
    | _ => None
    };

  type model_t = {
    instance_id: int,
    mode,
  };

  type expansion_t = result(expansion_exp, string);

  type action_t =
    | SetModel(model_t);

  let hazel_model_t: TermBase.Typ.t =
    Prod([Typ.temp(Atom(Int)), mode_t]) |> Typ.fresh;

  let model_to_hazel: model_t => model_exp =
    (m: model_t) =>
      DHExp.fresh(
        Tuple([
          DHExp.fresh(Atom(Int(Bigint.of_int(m.instance_id)))),
          DHExp.fresh(Constructor(mode_name(m.mode), Some(Some(mode_t)))),
        ]),
      );

  let model_from_hazel: model_exp => option(model_t) =
    (e: model_exp) =>
      switch (e.term) {
      | Tuple([{term: Atom(Int(id)), _}, {term: Constructor(name, _), _}]) =>
        switch (
          int_of_string_opt(Bigint.to_string(id)),
          mode_of_name(name),
        ) {
        | (Some(instance_id), Some(mode)) =>
          Some({
            instance_id,
            mode,
          })
        | _ => None
        }
      | _ => None
      };

  /* Graphical by default: a program reaches for this livelit precisely when
     it wants the graph, since the runtime it would get otherwise is already
     the simple one. */
  let model_default: model_t = {
    instance_id: 0,
    mode: Graphical,
  };

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));

  /* The expansion is an Int whatever the program says, so no annotation is
     needed to decide it. */
  let requires_annotation = false;

  let expand =
      (~id as _: Id.t, ~ana as _: TermBase.Typ.t, ~tools as _, model: model_t)
      : expansion_t =>
    if (model.instance_id == 0) {
      /* Not claimed yet; the first view claims and writes the id back, and
         this runs again with a real one. Nothing to declare until then, and
         0 is not an id the shim ever hands out. */
      Error(
        "waiting for a runtime",
      );
    } else {
      switch (
        shim(
          "ensureMode",
          [|
            js_int(model.instance_id),
            js_string(mode_source(model.mode)),
          |],
        )
      ) {
      | exception No_runtime => Error("the Fumola runtime is not loaded")
      | exception _ => Error("the Fumola runtime could not be reached")
      | response =>
        let text =
          response |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string;
        switch (Yojson.Safe.from_string(text)) {
        | exception _ => Error("unreadable answer from the Fumola runtime")
        | `Assoc(fields) =>
          switch (List.assoc_opt("ok", fields)) {
          | Some(`Bool(true)) =>
            Ok(DHExp.fresh(Atom(Int(Bigint.of_int(model.instance_id)))))
          | _ =>
            switch (List.assoc_opt("error", fields)) {
            | Some(`String(message)) => Error(message)
            | _ => Error("the Fumola runtime refused the mode")
            }
          }
        | _ => Error("unreadable answer from the Fumola runtime")
        };
      };
    };

  let expand_to_hazel: expansion_t => expansion_exp =
    fun
    | Ok(e) => e
    | Error(message) => DHExp.fresh(Invalid(message));

  let update: (action_t, model_t) => model_t = (SetModel(m), _) => m;

  let view = (~id: Id.t, model: model_t, send_action) => {
    /* Claims once, and only from 0, for the reason the other fumola livelits
       do: re-claiming on every render rewrites the syntax being rendered. */
    let claimed =
      if (model.instance_id == 0) {
        let claimed = claim(~owner=Id.to_string(id), 0);
        if (claimed != 0) {
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
    Virtual_dom.Vdom.Node.(
      div(
        ~attrs=[Virtual_dom.Vdom.Attr.classes(["fumola-new"])],
        [
          span(
            ~attrs=[Virtual_dom.Vdom.Attr.classes(["fumola-new-mode"])],
            [text(mode_name(model.mode))],
          ),
          span(
            ~attrs=[Virtual_dom.Vdom.Attr.classes(["fumola-new-id"])],
            [text(claimed == 0 ? "?" : string_of_int(claimed))],
          ),
        ],
      )
    );
  };

  let hazel_action_t: TermBase.Typ.t = hazel_model_t;
  let action_to_hazel: action_t => action_exp =
    (SetModel(m)) => model_to_hazel(m);
  let action_from_hazel: action_exp => option(action_t) =
    (e: action_exp) => Option.map(m => SetModel(m), model_from_hazel(e));

  let size: Util.ProjectorShape.t = {
    vertical: Inline,
    horizontal: 18,
  };
};

let livelits: list(raw_livelit) =
  [
    (module Slider),
    (module Emotion),
    (module Js),
    (module FumolaNew),
    (module FumolaPutForce),
    (module FumolaEval),
  ]
  |> List.map(raw_of_builtin);
