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
[@deriving (show({with_path: false}), sexp, yojson)]
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
/* The mode last written in the program text for an instance, by instance.

   Not the instance's actual mode -- the runtime owns that. This is only what
   the tile said the last time it ran, and it is here so a run can tell a
   DECLARATION from a re-run of the same declaration.

   A run used to set the declared mode every time. That is a no-op whenever
   nothing changed, so it looked free, and it was not: the panel's `reset`
   buttons set a mode too, and the re-run that a reset schedules came along
   one step later and set the declared one back. Asking a $graphical cell to
   come back as simple emptied the store, made it simple, and then ran the
   program -- which declared $graphical, which reset it again and recorded a
   graph. The button worked and was undone before anything could show it. */
let last_declared: Hashtbl.t(int, mode) = Hashtbl.create(8);

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

/* Put an instance back to its pristine state, dropping the adapton store and
   everything the page has run in it. Answers whether it happened: a runtime
   that is not loaded, or an instance that was never realized, is a no. */
let reset_instance = (~mode: option(mode)=?, name: string): bool => {
  let instance_id = instance_of_name(name);
  let reset =
    switch (shim("reset", [|js_int(instance_id)|])) {
    | exception _ => false
    | answer => Js_of_ocaml.Js.to_bool(Js_of_ocaml.Js.Unsafe.coerce(answer))
    };
  /* The snapshot a reset restores carries the mode the instance was given,
     so coming back as the other one is a second step. Setting a mode an
     instance already has is a no-op, so asking for the one it already had
     costs nothing.

     `last_declared` is deliberately left alone: it records what the PROGRAM
     said, and the program has not changed. That is what lets this mode
     survive the re-run a reset schedules -- the re-run sees its own
     declaration unchanged and says nothing, so the reader's choice stands
     until the tile itself is edited. */
  if (reset) {
    Option.iter(ensure_mode(instance_id), mode);
  };
  reset;
};

/* The moment the next run will happen at. Counts runs rather than edits: two
   passes over one edit are two moments, which is the point -- telling them
   apart is what the count is for. */
let moment = ref(0);

/* A program, sent at a given moment.

   Three spellings matter here and none of them is obvious.

   The index is a BARE number, so the time is a `Symbol::Nat` and is ordered.
   `hazel(`3) would make the argument a QuotedAst, which Fumola leaves
   deliberately incomparable, and every run would become its own island with
   nothing visible between them.

   The navigation takes a nullary expression, so the time needs parentheses of
   its own: `goto time `hazel(3)` is a syntax error.

   The braces after a navigation are a block position already, so the program
   needs no `do` of its own -- which matters, because outside a nest position
   the same braces would be an object literal, and both parse. */
let at_moment = (n: int, program: string): string =>
  Printf.sprintf("do goto time (`hazel(%d)) { %s }", n, program);

/* Reads go at the LATEST moment, not at `Now`.

   `Now` is the bottom of Fumola's time order: every named moment can see it
   and it can see none of them. Since every run now happens at a named moment,
   a reader left at `Now` would answer nothing at all. A read at T answers with
   the write at the greatest comparable T' <= T, so reading at the latest
   moment is what makes a reader see everything that has happened. */
let at_now = (program: string): string => at_moment(moment^, program);

/* Run a program in an instance and hand back its JSON. Used both for the
   program itself and, by FumolaValue, for reading what a pointer points at. */
let eval_at = (instance_id: int, at: string): Yojson.Safe.t =>
  switch (
    switch (shim("evalTop", [|js_int(instance_id), js_string(at)|])) {
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

/* Everything else reads, so everything else goes at the latest moment. */
let eval_in = (instance_id: int, program: string): Yojson.Safe.t =>
  eval_at(instance_id, at_now(program));

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

     By the time this runs the escape holds a value, so `hazel m end` with m
     bound to Graphical reads the same as `hazel Graphical end`. */
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
  | Asc(inner, _)
  /* The wrappers evaluation puts around a value; see FumolaSource. */
  | Closure(_, inner)
  | Filter(_, inner) => mode_of_hazel(inner)
  | Constructor("Simple", _) => Ok(Some(Simple))
  | Constructor("Graphical", _) => Ok(Some(Graphical))
  | EmptyHole => Ok(None)
  /* A name still standing here is one evaluation could not resolve, which
     for a bound variable would already have been reported as unbound. */
  | Var(x) => Error(x ++ " is not bound to a Fumola mode")
  | _ => Error("a Fumola mode from Hazel is Simple or Graphical")
  };

/* The instance name, as text. The Name sort admits only an identifier, so
   anything else means the name position is still a hole. */
let name_of = (name: FumolaTermBase.t): option(string) =>
  switch (Annotated.term_of(name)) {
  | Var(x) => Some(x)
  | _ => None
  };

/* The same, for anything that wants to say which instance a step belonged to
   and has nowhere to put "it named none". */
let instance_name = (name: FumolaTermBase.t): string =>
  switch (name_of(name)) {
  | Some(x) => x
  | None => "?"
  };

/* Which of Hazel's passes is running this program.

   Hazel runs a Fumola quote from more than one place, and only one of them is
   the program happening; the rest are the editor asking what the next step
   would be, or whether something is a value, and answering by running the
   program again. Measured: docs/hazel-effect-schedule.md, three runs to an
   edit.

   The pass is recorded as a TIME rather than as a cell, because a pass is a
   moment and not a thing. Fumola's times are ordered where ordering means
   something and unordered where it does not, and both halves are load-bearing
   here:

     `hazel(1) < `hazel(2)        Symbol::Nat arguments compare numerically
     `hazel(1) vs `step(2)        different heads: incomparable, by design,
                                  "to express certain kinds of independence /
                                  parallelism in the time ordering"

   And a read at time T answers with the write at the greatest comparable
   T' <= T. So a run at `hazel(n) sees everything every earlier run did, its
   own writes leave the earlier moments untouched, and the whole sequence is a
   revision history rather than one mutable store. Checked against a live
   runtime, every direction.

   `Now` is the bottom of that order -- every named time can see it and it can
   see none of them -- so it is left to the meta level and nothing is run
   there.

   Two spellings matter and neither is obvious. The index must be a BARE
   number: `hazel(1) makes it Symbol::Nat and ordered, while `hazel(`1) makes
   it a QuotedAst, which is deliberately incomparable and would silently give
   every run its own island. And the navigation takes a nullary expression, so
   the time needs its own parentheses: goto time (`hazel(1)). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type pass =
  | Eval
  | Step
  | Decompose
  | ValueCheck;

let pass_name =
  fun
  | Eval => "eval"
  | Step => "step"
  | Decompose => "decompose"
  | ValueCheck => "valueCheck";

let next_moment = () => {
  incr(moment);
  moment^;
};

/* Which pass each moment was, so the panel can label a moment with the pass
   that made it. The store holds the moment; the name of the pass is Hazel's
   business and stays here. Not persisted: the moments start again at zero
   whenever the page does, and so does the store. */
let moment_pass: Hashtbl.t(int, string) = Hashtbl.create(64);

let claim_moment = (pass: pass): int => {
  incr(moment);
  Hashtbl.replace(moment_pass, moment^, pass_name(pass));
  moment^;
};

let pass_of_moment = (n: int): option(string) =>
  Hashtbl.find_opt(moment_pass, n);

/* Anything sent to an instance goes at a moment, reads included.

   A read at `Now` would answer nothing now that every run happens at a named
   moment: `Now` is the bottom of the order and can see none of them. Reading
   at the LATEST moment is what makes a reader see everything, since a read at
   T answers with the write at the greatest comparable T' <= T. */
let run =
    (
      ~ana: TermBase.Typ.t,
      ~tools: FumolaTools.t,
      ~pass: pass,
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
        let at = claim_moment(pass);
        let instance_id = instance_of_name(instance_name);
        /* Declare the mode when the declaration is new or has changed, which
           is when it means something. An unchanged declaration says nothing
           the instance has not already been told, and saying it anyway would
           overrule whoever spoke last -- which, after a reset, is the
           reader. Leaving the mode slot a hole still says nothing at all. */
        Option.iter(
          mode =>
            if (Hashtbl.find_opt(last_declared, instance_id) != Some(mode)) {
              ensure_mode(instance_id, mode);
              Hashtbl.replace(last_declared, instance_id, mode);
            },
          mode,
        );
        switch (eval_at(instance_id, at_moment(at, program))) {
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
