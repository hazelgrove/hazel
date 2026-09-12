/* Running a `fumola <mode> as <instance> in … end` against its instance.

   The shape is the one fumola-livelit-mvp established: print the program,
   hand it to the Fumola wasm module through the `window.fumola` shim, and
   read the result back as a Hazel value. What changes is where the instance
   comes from. The livelit kept an `instance_id` in its model, and the shim
   handed out ids per live projector. Here the program names its instance in
   its own text, and that name is what claims the runtime -- so the adapton
   store survives every edit that leaves the name alone, which is the whole
   reason the name is written rather than derived.

   Running happens during elaboration, as livelit expansion did, which means
   on every edit. See the note on eager evaluation in
   docs/fumola-tiles-design.md. */

/* The shim is absent outside the browser (notably under the test runner), and
   absent in the browser until the wasm artifacts have been built. Both are
   reported rather than raised: a Fumola expression whose runtime is missing
   should degrade to a message, not take down evaluation. */
exception No_runtime;

/* Looked up as a property of the global object rather than with [js_expr]:
   js_of_ocaml cannot compile a [js_expr] string ahead of time and falls back
   to runtime evaluation, which it reports as an error on every call. */
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

/* The runtime this name owns. The shim keys its instances by owner, so the
   same name answers with the same instance for as long as the page lives --
   which is what carries the adapton store across an edit. */
let instance_of_name = (name: string): int =>
  switch (shim("claim", [|js_int(0), js_string(name)|])) {
  | exception _ => 0
  | claimed =>
    claimed
    |> Js_of_ocaml.Js.Unsafe.coerce
    |> Js_of_ocaml.Js.float_of_number
    |> int_of_float
  };

/* The adapton semantics an instance runs. Fumola spells these `#simple` and
   `#graphical`, and the tile writes them as the variants `$simple` and
   `$graphical`, so the two stay the same word.

   Graphical is the default, as it is Fumola's and as fumola_new's was. */
type mode =
  | Simple
  | Graphical;

let mode_source =
  fun
  | Simple => "simple"
  | Graphical => "graphical";

/* Setting the mode an instance already has is a no-op; setting a different
   one *resets the instance*, discarding the adapton store. That is why the
   mode is written beside the instance name rather than somewhere a program
   could change it in passing, and why running with no mode written does not
   set one: it would reset an instance another expression had configured. */
let ensure_mode = (instance_id: int, mode: mode): unit =>
  switch (
    shim(
      "ensureMode",
      [|js_int(instance_id), js_string(mode_source(mode))|],
    )
  ) {
  | exception _ => ()
  | _ => ()
  };

/* Run a program in an instance and hand back its JSON. Used both for the
   program itself and, by FumolaValue, for reading what a pointer points at. */
let eval_in = (instance_id: int, program: string): Yojson.Safe.t =>
  switch (
    switch (shim("evalTop", [|js_int(instance_id), js_string(program)|])) {
    | exception _ => None
    | r => Some(r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string)
    }
  ) {
  | None => `Null
  | Some(response) =>
    switch (Yojson.Safe.from_string(response)) {
    | exception _ => `Null
    | json => json
    }
  };

/* Why a program could not produce a Hazel value. A half-written program is a
   syntax error on nearly every keystroke, so whether the failure was
   syntactic is carried separately: the editor has better ways to say that
   than a mark on the expression. The tile route should see far fewer of them
   than the livelit did, since Hazel's own parser now builds the program --
   a syntax error here means the printer emitted something Fumola rejects,
   which is a bug in FumolaPrint rather than in the user's program. */
type failure = {
  syntax: bool,
  message: string,
};

let unprintable = (body: FumolaTermBase.t): option(string) =>
  Fumola.has_hole(body)
    ? Some(
        switch (Fumola.why_unprintable(body)) {
        | Some(why) => why
        | None => "the program is incomplete"
        },
      )
    : None;

/* The mode, written as a Fumola variant. A hole means "leave this instance's
   mode alone", which is not the same as asking for the default: setting a
   mode an instance does not already have resets it, and one expression must
   not silently discard the store another has been building. */
let rec mode_of = (mode: FumolaTermBase.t): result(option(mode), string) =>
  switch (Annotated.term_of(mode)) {
  | Variant("simple", None) => Ok(Some(Simple))
  | Variant("graphical", None) => Ok(Some(Graphical))
  | Hole(_) => Ok(None)
  /* A mode can come from Hazel, so that an instance can be configured once
     and its mode referred to rather than repeated. Hazel spells the two the
     way the livelit did, as the constructors Simple and Graphical.

     Read from the expression rather than from its value, which is the limit
     of where this runs: a constructor written in place is read, and a
     variable bound to one is not, because elaboration happens before
     evaluation and nothing has substituted it yet. See the note in
     src/language/fumola/README.md. */
  | Hazel(e) => mode_of_hazel(e)
  | Paren(m) => mode_of(m)
  | _ =>
    Error(
      "a Fumola instance is $simple or $graphical, or a Hazel expression "
      ++ "giving Simple or Graphical",
    )
  }

and mode_of_hazel = (e: TermBase.Exp.t): result(option(mode), string) =>
  switch (e.term) {
  | Parens(inner)
  | Asc(inner, _) => mode_of_hazel(inner)
  | Constructor("Simple", _) => Ok(Some(Simple))
  | Constructor("Graphical", _) => Ok(Some(Graphical))
  | EmptyHole => Ok(None)
  | Var(x) =>
    Error(
      "the mode is "
      ++ x
      ++ ", whose value is not known here: a Fumola program runs before "
      ++ "anything is substituted, so write Simple or Graphical in place",
    )
  | _ => Error("a Fumola mode from Hazel is Simple or Graphical")
  };

/* The instance name, as text. The Name sort admits only an identifier, so
   anything else means the name position is still a hole. */
let name_of = (name: FumolaTermBase.t): option(string) =>
  switch (Annotated.term_of(name)) {
  | Var(x) => Some(x)
  | _ => None
  };

let run =
    (
      ~ana: TermBase.Typ.t,
      ~tools: FumolaTools.t,
      name: FumolaTermBase.t,
      mode: FumolaTermBase.t,
      body: FumolaTermBase.t,
    )
    : result(TermBase.Exp.t, failure) =>
  switch (name_of(name)) {
  | None =>
    Error({
      syntax: false,
      message: "this Fumola program names no instance",
    })
  | Some(instance_name) =>
    switch (mode_of(mode)) {
    | Error(message) =>
      Error({
        syntax: false,
        message,
      })
    | Ok(mode) =>
      switch (unprintable(body)) {
      | Some(message) =>
        Error({
          syntax: false,
          message,
        })
      | None =>
        let program = Fumola.of_exp(body);
        let instance_id = instance_of_name(instance_name);
        Option.iter(ensure_mode(instance_id), mode);
        switch (eval_in(instance_id, program)) {
        | `Null =>
          Error({
            syntax: false,
            message: "no Fumola runtime available",
          })
        | `Assoc(obj) as json =>
          switch (List.assoc_opt("ok", obj)) {
          | Some(`Bool(true)) =>
            switch (
              FumolaValue.exp_of_json(
                ~instance_id,
                ~eval=eval_in(instance_id),
                ~ana,
                ~tools,
                json,
              )
            ) {
            | Ok(exp) => Ok(exp)
            | Error(message) =>
              Error({
                syntax: false,
                message,
              })
            }
          | _ =>
            Error({
              syntax:
                List.assoc_opt("kind", obj) == Some(`String("syntax")),
              message:
                switch (List.assoc_opt("error", obj)) {
                | Some(`String(message)) => message
                | _ => "the Fumola program did not produce a value"
                },
            })
          }
        | _ =>
          Error({
            syntax: false,
            message: "could not read the Fumola runtime's response",
          })
        };
      }
    }
  };
