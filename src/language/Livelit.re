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
  let view_below = (~id as _, ~splice as _, _model, _send_action) => None;

  let shape: Util.ProjectorShape.t = {
    vertical: Block(2),
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
  /* Carry a Hazel value in the model and bind it, as `input`, in the Fumola
     program's scope. The way in: everything else here reports outward. */
  let takes_input: bool;
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
    /* The Hazel value this program runs on, rendered into Fumola source and
       bound as `input`. Absent for the livelits that take no input. */
    input: option(TermBase.Exp.t),
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
  /* The program as the runtime sees it: a livelit that takes input binds it
     first, so the program can name it. Rendering can fail -- a hole, or
     anything with no written Fumola form -- and that is worth saying plainly
     rather than running something that does not mean what was written. */
  let effective_program = (model: model_t): result(string, string) =>
    switch (model.input) {
    | None => Ok(model.program)
    | Some(input) =>
      switch (FumolaSource.of_exp(input)) {
      | Error(message) => Error(message)
      | Ok(source) => Ok("let input = " ++ source ++ "; " ++ model.program)
      }
    };

  let observe_described =
      (~ana: TermBase.Typ.t, ~tools: LivelitCtx.type_tools, model: model_t)
      : (expansion_t, string) => {
    switch (effective_program(model)) {
    | Error(message) => (
        Error({
          syntax: false,
          message,
        }),
        message,
      )
    | Ok(program) =>
      let response =
        switch (
          C.top_level
            ? shim(
                "evalTop",
                [|js_int(model.instance_id), js_string(program)|],
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
                  js_string(program),
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
  };

  /* ---- Hazel encodings ---------------------------------------------- */

  /* A thunk livelit's model is (instance, thunk name, program); the editor
     has no thunk, so its model is (instance, program). Both name an instance,
     so two livelits carrying the same id share one runtime. */
  /* Unknown for the input: a model type is fixed, and the value a program
     runs on is whatever its author passes. */
  let hazel_model_t: TermBase.Typ.t =
    (
      switch (C.default_thunk_name, C.takes_input) {
      | (Some(_), false) =>
        Prod([
          Typ.temp(Atom(Int)),
          Typ.temp(Atom(String)),
          Typ.temp(Atom(String)),
        ])
      | (Some(_), true) =>
        Prod([
          Typ.temp(Atom(Int)),
          Typ.temp(Atom(String)),
          Typ.temp(Atom(String)),
          Typ.temp(Unknown(Internal)),
        ])
      | (None, false) =>
        Prod([Typ.temp(Atom(Int)), Typ.temp(Atom(String))])
      | (None, true) =>
        Prod([
          Typ.temp(Atom(Int)),
          Typ.temp(Atom(String)),
          Typ.temp(Unknown(Internal)),
        ])
      }
    )
    |> Typ.fresh;

  let model_to_hazel: model_t => model_exp =
    (m: model_t) => {
      let instance = DHExp.fresh(Atom(Int(Bigint.of_int(m.instance_id))));
      let program = DHExp.fresh(Atom(String(m.program)));
      let head =
        switch (m.thunk_name) {
        | Some(thunk_name) => [
            instance,
            DHExp.fresh(Atom(String(thunk_name))),
            program,
          ]
        | None => [instance, program]
        };
      DHExp.fresh(
        Tuple(
          head
          @ (
            switch (m.input) {
            | Some(input) => [input]
            | None => []
            }
          ),
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
        )
          when !C.takes_input =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: Some(thunk_name),
            program,
            input: None,
          }
        )
      | (
          Tuple([
            {term: Atom(Int(id)), _},
            {term: Atom(String(thunk_name)), _},
            {term: Atom(String(program)), _},
            input,
          ]),
          Some(_),
        )
          when C.takes_input =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: Some(thunk_name),
            program,
            input: Some(input),
          }
        )
      | (
          Tuple([
            {term: Atom(Int(id)), _},
            {term: Atom(String(program)), _},
          ]),
          None,
        )
          when !C.takes_input =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: None,
            program,
            input: None,
          }
        )
      | (
          Tuple([
            {term: Atom(Int(id)), _},
            {term: Atom(String(program)), _},
            input,
          ]),
          None,
        )
          when C.takes_input =>
        instance(id, instance_id =>
          {
            instance_id,
            thunk_name: None,
            program,
            input: Some(input),
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
    /* A value to start from, so the livelit says what it is for the moment
       it appears. */
    input:
      C.takes_input
        ? Some(DHExp.fresh(Atom(Int(Bigint.of_int(1))))) : None,
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

  let view_below = (~id as _, ~splice as _, _model, _send_action) => None;

  let shape: Util.ProjectorShape.t = {
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
    let takes_input = false;
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
    let takes_input = false;
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

  let view_below = (~id as _, ~splice as _, _model, _send_action) => None;

  let shape: Util.ProjectorShape.t = {
    vertical: Inline,
    horizontal: 18,
  };
};

/* Runs a Fumola program on a Hazel value.
 *
 * The other livelits report outward: a Fumola program runs and its result
 * becomes a Hazel value. This one goes the other way as well. The Hazel value
 * in its model is rendered into Fumola source and bound as `input`, so the
 * program can name it -- and the result comes back translated as usual.
 *
 * So a sort written in Fumola can be run on a list Hazel holds, and both the
 * input and the output can be read in Hazel's own terms. */
module FumolaWith =
  MakeFumola({
    let name = "fumola_with";
    let top_level = false;
    let takes_input = true;
    let default_thunk_name = Some("`with");
    let default_program = "input";
  });

/* ^fumola_wip: a Fumola computation between two remote refs, edited as tiles.

   A prototype of a DSL as a tile-based livelit, and a work in progress: it is
   named so, because today's livelit view/update is slow enough that editing
   tiles inside one is felt. Its model names an instance and a livelit name
   and holds two splices, the input (any Hazel value) and the code (a
   `fumola … end` tile, of which only the body is used, for now).

   The livelit owns three cells in that instance, all under its name:

     `name(`input)    the input, written from Hazel  -- the In wire
     `name(`compute)  the thunk the code runs in, so an edit reuses its history
     `name(`output)   the code's result, read back    -- the Out wire

   (`in and `thunk would read better, but both are Fumola keywords.)

   It expands to one Fumola quote that writes the input, forces the
   computation, and reads the output. The input crosses as a `hazel … end`
   escape, evaluated when the quote runs and so in scope at the use: it can
   name the use's variables, which a model rendered at expansion time cannot. */
module FumolaWip: BuiltinLivelit = {
  let name = "fumola_wip";

  type model_t = {
    instance: string,
    name: string,
    /* The code as tiles -- a `fumola … end` splice -- or as a string
       literal holding the body's Fumola text. */
    tiles: bool,
    /* Held verbatim, so that a commit keeps their Splice nodes. */
    input: TermBase.Exp.t,
    code: TermBase.Exp.t,
  };

  type expansion_t = expansion_exp;

  type action_t =
    | SetModel(model_t);

  let field_t = (label, ty): TermBase.Typ.t =>
    Typ.fresh(TupLabel(Typ.fresh(Label(label)), ty));

  let hazel_model_t: TermBase.Typ.t =
    Typ.fresh(
      Prod([
        field_t("instance", Typ.temp(Atom(String))),
        field_t("name", Typ.temp(Atom(String))),
        field_t("tiles", Typ.temp(Atom(Bool))),
        field_t("input", Typ.temp(Unknown(Internal))),
        field_t("code", Typ.temp(Unknown(Internal))),
      ]),
    );

  let field = (label, e) =>
    DHExp.fresh(TupLabel(DHExp.fresh(Label(label)), e));

  let model_to_hazel = (m: model_t): model_exp =>
    DHExp.fresh(
      Tuple([
        field("instance", DHExp.fresh(Atom(String(m.instance)))),
        field("name", DHExp.fresh(Atom(String(m.name)))),
        field("tiles", DHExp.fresh(Atom(Bool(m.tiles)))),
        field("input", m.input),
        field("code", m.code),
      ]),
    );

  let rec unparen = (e: TermBase.Exp.t) =>
    switch (e.term) {
    | Parens(e) => unparen(e)
    | _ => e
    };

  let model_from_hazel = (e: model_exp): option(model_t) =>
    switch (unparen(e).term) {
    | Tuple(items) =>
      let fields =
        List.filter_map(
          (item: TermBase.Exp.t) =>
            switch (item.term) {
            | TupLabel({term: Label(label), _}, v) => Some((label, v))
            | _ => None
            },
          items,
        );
      let get = label => List.assoc_opt(label, fields);
      switch (
        Option.map(unparen, get("instance")),
        Option.map(unparen, get("name")),
        Option.map(unparen, get("tiles")),
        get("input"),
        get("code"),
      ) {
      | (
          Some({term: Atom(String(instance)), _}),
          Some({term: Atom(String(name)), _}),
          Some({term: Atom(Bool(tiles)), _}),
          Some(input),
          Some(code),
        ) =>
        Some({
          instance,
          name,
          tiles,
          input,
          code,
        })
      | _ => None
      };
    | _ => None
    };

  /* Fumola terms carry ids like Hazel's; fresh ones, since none of these is
     anything the user wrote. */
  let f = IdTagged.fresh;

  /* `name(`cell) */
  let cell = (m: model_t, cell): FumolaTermBase.t =>
    f(
      FumolaGrammar.Ap(
        f(FumolaGrammar.QuotedId(m.name)),
        f(FumolaGrammar.Paren(f(FumolaGrammar.QuotedId(cell)))),
      ),
    );

  /* @(`name(`cell)): parenthesized, since `@ `a (`b)` reads as (@`a)(`b). */
  let read = (m: model_t, c): FumolaTermBase.t =>
    f(FumolaGrammar.Get(f(FumolaGrammar.Paren(cell(m, c)))));

  let model_default: model_t = {
    instance: "myInstance",
    name: "myLivelit",
    tiles: true,
    input: DHExp.fresh(Parens(DHExp.fresh(Atom(Int(Bigint.of_int(3)))))),
    code:
      DHExp.fresh(
        Parens(
          DHExp.fresh(
            FumolaQuote(
              f(FumolaGrammar.Var("myInstance")),
              f(FumolaGrammar.Hole(EmptyHole)),
              f(
                FumolaGrammar.Bin(
                  f(FumolaGrammar.Var("input")),
                  FumolaGrammar.Add,
                  f(FumolaGrammar.Lit(FumolaGrammar.Nat("1"))),
                ),
              ),
            ),
          ),
        ),
      ),
  };

  /* ---- the toggle ------------------------------------------------- */

  /* String mode keeps only the body, so the mode slot does not survive a
     round trip; code that comes back as tiles is `$graphical`, Fumola's
     default and the mode that records what the watch pane shows. */
  let graphical = () => f(FumolaGrammar.Variant("graphical", None));

  let body_of_decs = (ds: list(FumolaTermBase.dec)): FumolaTermBase.t =>
    switch (ds) {
    | [{term: FumolaGrammar.DExp(e), _}] => e
    | ds => f(FumolaGrammar.Block(ds))
    };

  let decs_of_body = (body: FumolaTermBase.t): list(FumolaTermBase.dec) =>
    switch (Annotated.term_of(body)) {
    | FumolaGrammar.Block(ds) => ds
    | _ => [f(FumolaGrammar.DExp(body))]
    };

  /* The text a body's tiles print as. A body with a hole in it has no text,
     and says why, rather than printing something that means less. */
  let text_of_body = (body: FumolaTermBase.t): result(string, string) =>
    Fumola.has_hole(body)
      ? Error(
          Option.value(
            Fumola.why_unprintable(body),
            ~default="the code is not finished",
          ),
        )
      : Ok(
          switch (Annotated.term_of(body)) {
          | FumolaGrammar.Block(ds) => Fumola.program(ds)
          | _ => Fumola.of_exp(body)
          },
        );

  /* A Hazel string literal cannot hold a double quote -- it has no escapes
     -- so Fumola text with a text literal in it has no string form. Taking
     it anyway would end the literal early and lose the code. */
  let fits_a_string = (text: string): bool => !String.contains(text, '"');
  let no_string_form = "a Hazel string cannot hold a double quote, so code with a Fumola text literal stays as tiles";

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Unknown(Internal));
  let requires_annotation = false;

  /* The mode and body of the code splice's `fumola … end` tile. Its mode
     is passed on, so `$graphical` there is what makes the instance record
     what the watch pane shows; its instance name is ignored for the
     model's. */
  let rec code_body =
          (e: TermBase.Exp.t): option((FumolaTermBase.t, FumolaTermBase.t)) =>
    switch (e.term) {
    | Parens(e)
    | Splice(e) => code_body(e)
    | FumolaQuote(_, mode, body) => Some((mode, body))
    | _ => None
    };

  /* The mode and declarations the code stands for, in either form. A
     string that does not parse is a program being typed, and expands to a
     hole rather than to an error on every keystroke. */
  let code_decs =
      (m: model_t)
      : result(
          (FumolaTermBase.t, list(FumolaTermBase.dec)),
          option(string),
        ) =>
    switch (m.tiles, unparen(m.code).term) {
    | (false, Atom(String(text))) =>
      switch (FumolaParse.program(text)) {
      | Ok(ds) => Ok((graphical(), ds))
      | Error(_) => Error(None)
      }
    | _ =>
      switch (code_body(m.code)) {
      | Some((mode, body)) => Ok((mode, decs_of_body(body)))
      | None => Error(Some("the code field needs a fumola … end tile"))
      }
    };

  let expand = (~id as _, ~ana as _, ~tools as _, m: model_t): expansion_t =>
    switch (code_decs(m)) {
    | Error(None) => DHExp.fresh(EmptyHole)
    | Error(Some(message)) => DHExp.fresh(Invalid(message))
    | Ok((mode, decs)) =>
      /* thunk { let input = @(`name(`input)); <code> }: reading the cell,
         rather than taking the value, is what gives the computation an edge
         to the input the watch pane can show. */
      let thunk =
        f(
          FumolaGrammar.Thunk([
            f(
              FumolaGrammar.DLet(
                f(FumolaGrammar.PVar("input")),
                read(m, "input"),
              ),
            ),
            ...decs,
          ]),
        );
      let program =
        f(
          FumolaGrammar.Block([
            f(
              FumolaGrammar.DExp(
                f(
                  FumolaGrammar.Put(
                    cell(m, "input"),
                    f(FumolaGrammar.Hazel(m.input)),
                  ),
                ),
              ),
            ),
            f(
              FumolaGrammar.DExp(
                f(
                  FumolaGrammar.Put(
                    cell(m, "output"),
                    f(
                      FumolaGrammar.Force(
                        f(
                          FumolaGrammar.Paren(
                            f(FumolaGrammar.Put(cell(m, "compute"), thunk)),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
            f(FumolaGrammar.DExp(read(m, "output"))),
          ]),
        );
      DHExp.fresh(
        FumolaQuote(f(FumolaGrammar.Var(m.instance)), mode, program),
      );
    };

  let expand_to_hazel = (e: expansion_t): expansion_exp => e;

  let update = (action: action_t, _m: model_t): model_t =>
    switch (action) {
    | SetModel(m) => m
    };

  let hazel_action_t: TermBase.Typ.t =
    Sum([
      Variant(
        "SetModel",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        Some(hazel_model_t),
      ),
    ])
    |> Typ.fresh;

  let action_to_hazel = (SetModel(m): action_t): action_exp =>
    DHExp.fresh(
      Ap(
        Forward,
        DHExp.fresh(Constructor("SetModel", Some(Some(hazel_model_t)))),
        model_to_hazel(m),
      ),
    );

  let action_from_hazel = (e: action_exp): option(action_t) =>
    switch (e.term) {
    | Ap(Forward, {term: Constructor("SetModel", _), _}, model) =>
      Option.map(m => SetModel(m), model_from_hazel(model))
    | _ => None
    };

  /* A text field whose keystrokes stay in it rather than editing the
     program around the livelit. */
  let text_field = (~label, value, set) =>
    Node.label(
      ~attrs=[Attr.class_("fumola-wip-field")],
      [
        Node.span([Node.text(label)]),
        Node.input(
          ~attrs=[
            Attr.type_("text"),
            Attr.value(value),
            Attr.on_keydown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_input((_, v) => set(v)),
          ],
          (),
        ),
      ],
    );

  /* Flip the code between tiles and text. Each way converts what is there;
     code that cannot be converted -- tiles with a hole, text that does not
     parse -- stays as it is, and the toggle says why in its title. */
  let flip = (m: model_t): result(model_t, string) =>
    switch (m.tiles, unparen(m.code).term) {
    | (true, _) =>
      switch (code_body(m.code)) {
      | None => Error("the code field holds no fumola … end tile")
      | Some((_, body)) =>
        switch (text_of_body(body)) {
        | Error(why) => Error(why)
        | Ok(text) when !fits_a_string(text) => Error(no_string_form)
        | Ok(text) =>
          Ok({
            ...m,
            tiles: false,
            code: DHExp.fresh(Atom(String(text))),
          })
        }
      }
    | (false, Atom(String(text))) =>
      switch (FumolaParse.program(text)) {
      | Error({message, _}) => Error(message)
      | Ok(ds) =>
        Ok({
          ...m,
          tiles: true,
          /* A Splice node, so the tiles come back as a splice with an
             editor of their own. */
          code:
            DHExp.fresh(
              Parens(
                DHExp.fresh(
                  Splice(
                    DHExp.fresh(
                      FumolaQuote(
                        f(FumolaGrammar.Var(m.instance)),
                        graphical(),
                        body_of_decs(ds),
                      ),
                    ),
                  ),
                ),
              ),
            ),
        })
      }
    | (false, _) => Error("the code field holds no string")
    };

  let toggle = (m: model_t, send_action) => {
    let flipped = flip(m);
    Node.label(
      ~attrs=[
        Attr.class_("fumola-wip-field"),
        Attr.title(
          switch (flipped) {
          | Ok(_) =>
            m.tiles ? "edit the code as text" : "edit the code as tiles"
          | Error(why) => "cannot convert: " ++ why
          },
        ),
      ],
      [
        Node.input(
          ~attrs=
            [
              Attr.type_("checkbox"),
              Attr.bool_property("checked", m.tiles),
              Attr.on_keydown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
              Attr.on_change((_, _) =>
                switch (flipped) {
                | Ok(m) => send_action(SetModel(m))
                | Error(_) => Virtual_dom.Vdom.Effect.Ignore
                }
              ),
            ]
            @ (Result.is_ok(flipped) ? [] : [Attr.disabled]),
          (),
        ),
        Node.span([Node.text("tiles")]),
      ],
    );
  };

  let view = (~id as _, m: model_t, send_action) =>
    Node.div(
      ~attrs=[Attr.class_("fumola-wip-head")],
      [
        Node.span(
          ~attrs=[
            Attr.class_("fumola-wip-badge"),
            Attr.title("a work in progress, and slow to edit"),
          ],
          [Node.text("wip")],
        ),
        text_field(~label="instance", m.instance, instance =>
          send_action(
            SetModel({
              ...m,
              instance,
            }),
          )
        ),
        text_field(~label="livelit", m.name, name =>
          send_action(
            SetModel({
              ...m,
              name,
            }),
          )
        ),
        toggle(m, send_action),
        {
          /* Debug: "runs" moves as runs happen, straight from FumolaRun;
             "drawn" is the count when this row was last rendered. Runs moving
             while drawn and the watch pane stay put is a redraw problem; runs
             not moving is a run that did not happen. */

          let drawn = string_of_int(FumolaRun.runs_of(m.instance));
          Node.span(
            ~attrs=[
              Attr.class_("fumola-wip-debug"),
              Attr.title(
                "runs in "
                ++ m.instance
                ++ ", counted as they happen / the count when this row was drawn",
              ),
            ],
            [
              Node.text("runs "),
              Node.span(
                ~attrs=[
                  Attr.class_("fumola-wip-runs"),
                  Attr.create("data-fumola-runs", m.instance),
                  Attr.create("data-live-runs", drawn),
                ],
                [],
              ),
              Node.text(" \u{b7} drawn " ++ drawn),
            ],
          );
        },
      ],
    );

  let rec splice_id = (e: TermBase.Exp.t): option(Id.t) =>
    switch (e.term) {
    | Splice(_) => Some(IdTagged.rep_id(e))
    | Parens(e) => splice_id(e)
    | _ => None
    };

  /* One of the livelit's panes, shown as a tab of the watch panel; the tab
     names it, so the title is only for the element. */
  let sub_panel = (title, body) =>
    Node.div(
      ~attrs=[
        Attr.class_("fumola-wip-sub"),
        Attr.create("data-pane", String.lowercase_ascii(title)),
      ],
      body,
    );

  /* Where each use's divider sits, as the left column's share of the width.
     Kept in the page, not the model: dragging it is a view of the program,
     not an edit, so it never writes to the syntax. */
  let splits: Hashtbl.t(string, float) = Hashtbl.create(4);

  let columns = (split: float): string =>
    Printf.sprintf(
      "minmax(0, %gfr) 7px minmax(0, %gfr)",
      split,
      1.0 -. split,
    );

  /* Dragging moves the columns directly on the DOM, and records the share
     for the next render; nothing is dispatched, so nothing re-renders. */
  let drag_divider =
      (key: string, evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
    module U = Js_of_ocaml.Js.Unsafe;
    let str = s => U.inject(Js_of_ocaml.Js.string(s));
    let num = v => Js_of_ocaml.Js.float_of_number(U.coerce(v));
    let handle = U.get(evt, "target");
    let body = U.meth_call(handle, "closest", [|str(".fumola-wip-body")|]);
    /* Capture keeps the drag going when the pointer leaves the handle; a
       pointer that is not active cannot be captured, and the drag still
       works without it. */
    switch (
      U.meth_call(handle, "setPointerCapture", [|U.get(evt, "pointerId")|])
    ) {
    | exception _ => ()
    | () => ()
    };
    let move =
      Js_of_ocaml.Js.wrap_callback(e => {
        let rect = U.meth_call(body, "getBoundingClientRect", [||]);
        let left = num(U.get(rect, "left"));
        let width = num(U.get(rect, "right")) -. left;
        let x = num(U.get(e, "clientX")) -. left;
        let split = Float.min(0.85, Float.max(0.15, x /. width));
        Hashtbl.replace(splits, key, split);
        U.set(
          U.get(body, "style"),
          "gridTemplateColumns",
          Js_of_ocaml.Js.string(columns(split)),
        );
      });
    let _ =
      U.meth_call(
        handle,
        "addEventListener",
        [|str("pointermove"), U.inject(move)|],
      );
    let rec up =
      lazy(
        Js_of_ocaml.Js.wrap_callback(_ => {
          let _ =
            U.meth_call(
              handle,
              "removeEventListener",
              [|str("pointermove"), U.inject(move)|],
            );
          let _ =
            U.meth_call(
              handle,
              "removeEventListener",
              [|str("pointerup"), U.inject(Lazy.force(up))|],
            );
          ();
        })
      );
    let _ =
      U.meth_call(
        handle,
        "addEventListener",
        [|str("pointerup"), U.inject(Lazy.force(up))|],
      );
    ();
  };

  let view_below = (~id, ~splice, m: model_t, send_action) => {
    let key = Id.to_string(id);
    let split = Option.value(Hashtbl.find_opt(splits, key), ~default=0.5);
    let editor = (e: TermBase.Exp.t) =>
      switch (Option.bind(splice_id(e), splice)) {
      | Some(node) => node
      | None => Node.span(~attrs=[Attr.class_("fumola-wip-missing")], [])
      };
    let symbol = c =>
      Node.code(
        ~attrs=[Attr.class_("fumola-wip-cell")],
        [Node.text("`" ++ m.name ++ "(`" ++ c ++ ")")],
      );
    /* A wire crosses the language boundary: Hazel on its left, the Fumola
       cell on its right, and the arrow says which way the value goes. */
    let wire = (~dir, hazel, fumola) =>
      Node.div(
        ~attrs=[Attr.classes(["fumola-wip-wire", dir])],
        [
          Node.div(~attrs=[Attr.class_("fumola-wip-hazel")], hazel),
          Node.span(
            ~attrs=[Attr.class_("fumola-wip-arrow")],
            [Node.text(dir == "in" ? "In →" : "← Out")],
          ),
          Node.div(~attrs=[Attr.class_("fumola-wip-fumola")], fumola),
        ],
      );
    Some(
      Node.div(
        ~attrs=[
          Attr.class_("fumola-wip-body"),
          Attr.create(
            "style",
            "grid-template-columns: " ++ columns(split) ++ ";",
          ),
        ],
        [
          Node.div(
            ~attrs=[Attr.class_("fumola-wip-left")],
            [
              wire(~dir="in", [editor(m.input)], [symbol("input")]),
              Node.div(
                ~attrs=[Attr.class_("fumola-wip-code")],
                [
                  switch (m.tiles, unparen(m.code).term) {
                  | (false, Atom(String(text))) =>
                    Node.div([
                      Node.textarea(
                        ~attrs=[
                          Attr.class_("fumola-wip-text"),
                          Attr.string_property("value", text),
                          Attr.create("spellcheck", "false"),
                          Attr.on_keydown(_ =>
                            Virtual_dom.Vdom.Effect.Stop_propagation
                          ),
                          Attr.on_input((_, text) =>
                            fits_a_string(text)
                              ? send_action(
                                  SetModel({
                                    ...m,
                                    code: DHExp.fresh(Atom(String(text))),
                                  }),
                                )
                              : Virtual_dom.Vdom.Effect.Ignore
                          ),
                        ],
                        [],
                      ),
                      /* A string that does not parse expands to a hole, which
                         says nothing; this says where and why. */
                      switch (FumolaParse.program(text)) {
                      | Error({at, message}) =>
                        Node.div(
                          ~attrs=[Attr.class_("fumola-wip-error")],
                          [
                            Node.text(
                              Printf.sprintf(
                                "does not parse, at character %d: %s",
                                at,
                                message,
                              ),
                            ),
                          ],
                        )
                      | Ok(_) => Node.none
                      },
                      Node.div(
                        ~attrs=[Attr.class_("fumola-wip-note")],
                        [
                          Node.text(
                            "no double quotes here: a Hazel string cannot hold one. For a Fumola text literal, switch to tiles.",
                          ),
                        ],
                      ),
                    ])
                  | _ => editor(m.code)
                  },
                ],
              ),
              wire(
                ~dir="out",
                [
                  /* The last run's value, or the runtime's error: a run
                     that fails part way changes nothing else on screen.
                     Code that makes no program has not run, so what is
                     here is its own error, not the run before it. */
                  switch (
                    code_decs(m),
                    FumolaRun.last_run_of(~instance=m.instance, ~name=m.name),
                  ) {
                  | (Error(None), _) =>
                    Node.span(
                      ~attrs=[Attr.class_("fumola-wip-note")],
                      [Node.text("not run: the code does not parse")],
                    )
                  | (Error(Some(message)), _) =>
                    Node.span(
                      ~attrs=[Attr.class_("fumola-wip-error")],
                      [Node.text(message)],
                    )
                  | (Ok(_), Some({outcome: Ok(value), _})) =>
                    Node.span(
                      ~attrs=[Attr.class_("fumola-wip-value")],
                      [Node.text(value)],
                    )
                  | (Ok(_), Some({outcome: Error(message), _})) =>
                    Node.span(
                      ~attrs=[Attr.class_("fumola-wip-error")],
                      [Node.text(message)],
                    )
                  | (Ok(_), None) => Node.text("the expansion")
                  },
                ],
                [symbol("output")],
              ),
            ],
          ),
          Node.div(
            ~attrs=[
              Attr.class_("fumola-wip-divider"),
              Attr.title("drag to move the divider"),
              Attr.on_pointerdown(evt => {
                drag_divider(key, evt);
                Virtual_dom.Vdom.Effect.Many([
                  Virtual_dom.Vdom.Effect.Prevent_default,
                  Virtual_dom.Vdom.Effect.Stop_propagation,
                ]);
              }),
            ],
            [],
          ),
          Node.div(
            ~attrs=[Attr.class_("fumola-wip-right")],
            [
              {
                /* The livelit's own panes go to the watch panel as tabs
                   before Events, Nodes and Edges: the pane's rows are
                   fixed, and stacked above the panel they pushed it out of
                   view. */

                let panes: FumolaWatch.panes = {
                  /* The program the last run sent, as printed from the code:
                     where the tiles grouped differently from how they read,
                     its parentheses show it. */
                  program:
                    sub_panel(
                      "Program",
                      [
                        switch (
                          FumolaRun.last_run_of(
                            ~instance=m.instance,
                            ~name=m.name,
                          )
                        ) {
                        | Some({program: "", _}) =>
                          Node.div(
                            ~attrs=[Attr.class_("fumola-wip-note")],
                            [
                              Node.text(
                                "not sent: the code could not be printed",
                              ),
                            ],
                          )
                        | Some({program, _}) =>
                          Node.pre(
                            ~attrs=[Attr.class_("fumola-wip-program")],
                            [Node.text(program)],
                          )
                        | None =>
                          Node.div(
                            ~attrs=[Attr.class_("fumola-wip-note")],
                            [Node.text("this livelit has not run yet")],
                          )
                        },
                      ],
                    ),
                  /* The outline of the runs, drawn as Fumola's web player
                     draws it (FumolaOutline). */
                  outline:
                    sub_panel(
                      "Outline",
                      [
                        switch (FumolaRun.outlines(m.instance)) {
                        | Ok([]) =>
                          Node.div(
                            ~attrs=[Attr.class_("fumola-wip-note")],
                            [
                              Node.text(
                                "no forces yet; a $simple instance never has any",
                              ),
                            ],
                          )
                        | Ok(trees) => FumolaOutline.forest(trees)
                        | Error(message) =>
                          Node.div(
                            ~attrs=[Attr.class_("fumola-wip-note")],
                            [Node.text(message)],
                          )
                        },
                      ],
                    ),
                  /* What the last run printed. */
                  printed:
                    sub_panel(
                      "Printed",
                      switch (FumolaRun.printed_of(m.instance)) {
                      | [] => [
                          Node.div(
                            ~attrs=[Attr.class_("fumola-wip-note")],
                            [Node.text("the last run printed nothing")],
                          ),
                        ]
                      | lines => [
                          Node.ol(
                            ~attrs=[Attr.class_("fumola-wip-printed")],
                            List.map(
                              line => Node.li([Node.text(line)]),
                              lines,
                            ),
                          ),
                        ]
                      },
                    ),
                };
                switch (FumolaWatch.instance_view^(m.instance, Some(panes))) {
                | Some(node) => node
                | None => Node.text("the watch pane draws in the browser")
                };
              },
            ],
          ),
        ],
      ),
    );
  };

  /* The 14 rows are the height proj-livelit.css gives the full-width
     rows; change both together. */
  let shape: Util.ProjectorShape.t = {
    vertical: Tab(14),
    /* Room for the whole head row: badge, both fields, the toggle and the
       run readout. At 44 the last two were clipped off. */
    horizontal: 84,
  };
};

let livelits: list(raw_livelit) =
  [
    (module Js),
    (module FumolaNew),
    (module FumolaPutForce),
    (module FumolaEval),
    (module FumolaWith),
    (module FumolaWip),
  ]
  |> List.map(raw_of_builtin);
