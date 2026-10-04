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

/* What a named cell holds, for a RemoteRef's pull (docs/remote-refs.md):
   `peek`, so the read is not recorded as a dependency, and the value shaped
   into Hazel at [ana], as a `fumola ... end` result is. None when there is no
   runtime, no such cell yet, or the value does not read at [ana]: then there
   is nothing to write. Called only once a run has finished, so the instance
   is settled (its stack is empty). */
let peek_cell =
    (~instance: string, ~cell: string, ~ana: TermBase.Typ.t)
    : option(TermBase.Exp.t) => {
  let instance_id = instance_of_name(instance);
  /* Fumola's own spelling of a symbol, a leading backtick only: the
     closing one is the tile's (`count` on a slide is `count here). */
  switch (eval_in(instance_id, "peek(`" ++ cell ++ ")!")) {
  | `Assoc(obj) as json when List.assoc_opt("ok", obj) == Some(`Bool(true)) =>
    switch (
      FumolaValue.exp_of_json(
        ~instance_id,
        ~eval=eval_in(instance_id),
        ~ana,
        ~tools=FumolaTools.unknown,
        json,
      )
    ) {
    | Ok(exp) => Some(exp)
    | Error(_) => None
    }
  | _ => None
  };
};

/* Which mode an instance is running, asked of the instance rather than
   remembered. Adapton/fumola#134 added the prim for this reader in
   particular: what Hazel last *set* is a different question, and the two
   part company in the case a reader most wants an answer -- immediately
   after a reset into the other mode.

   None means "say nothing", and covers three cases that all deserve silence
   rather than a guess. There is no runtime. There is one, but it predates
   #134, and answers `ok: false` with "there is no prim called adaptonMode" --
   a live case and not a hypothetical, since Hazel pins no runtime version and
   reads whatever fumola.org is serving. Or the answer is not a mode, which
   would be a runtime disagreeing with itself.

   The two spellings are Fumola's own, which is why they are read back through
   `mode_source` rather than written out again: `adaptonReset` takes these
   words and `adaptonMode` answers with them, and a second copy here could
   drift from both. */
let mode_of_instance_local = (name: string): option(mode) =>
  switch (eval_in(instance_of_name(name), "prim \"adaptonMode\" ()")) {
  | `Assoc(fields) =>
    switch (List.assoc_opt("ok", fields), List.assoc_opt("value", fields)) {
    | (Some(`Bool(true)), Some(`Assoc(value))) =>
      switch (List.assoc_opt("name", value)) {
      | Some(`String(spelled)) =>
        List.find_opt(
          mode => mode_source(mode) == spelled,
          [Simple, Graphical],
        )
      | _ => None
      }
    | _ => None
    }
  | _ => None
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
/* Debug, for ^fumola_wip's readout: how many times a quote has run in each
   instance. Each run also writes the count straight onto the readout's DOM,
   as the data-live-runs attribute that its CSS shows, so the count moves even
   when the livelit is not redrawn -- which is the question the readout is
   there to answer. No document (the test runner, the worker): no write. */
let runs: Hashtbl.t(string, int) = Hashtbl.create(8);

let runs_of = (name: string): int =>
  Option.value(Hashtbl.find_opt(runs, name), ~default=0);

let is_plain_name = (name: string): bool =>
  name != ""
  && String.for_all(
       c =>
         c >= 'a'
         && c <= 'z'
         || c >= 'A'
         && c <= 'Z'
         || c >= '0'
         && c <= '9'
         || c == '_',
       name,
     );

let note_run = (name: string): unit => {
  let n = runs_of(name) + 1;
  Hashtbl.replace(runs, name, n);
  if (is_plain_name(name)) {
    switch (
      Js_of_ocaml.Js.Optdef.to_option(
        Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "document"),
      )
    ) {
    | exception _
    | None => ()
    | Some(doc) =>
      switch (
        Js_of_ocaml.Js.Unsafe.meth_call(
          doc,
          "querySelectorAll",
          [|js_string("[data-fumola-runs=\"" ++ name ++ "\"]")|],
        )
      ) {
      | exception _ => ()
      | els =>
        let count: int =
          Js_of_ocaml.Js.Unsafe.get(els, "length")
          |> Js_of_ocaml.Js.float_of_number
          |> int_of_float;
        for (i in 0 to count - 1) {
          let el =
            Js_of_ocaml.Js.Unsafe.meth_call(els, "item", [|js_int(i)|]);
          let () =
            Js_of_ocaml.Js.Unsafe.meth_call(
              el,
              "setAttribute",
              [|js_string("data-live-runs"), js_string(string_of_int(n))|],
            );
          ();
        };
      }
    };
  };
};

/* What the last run in each instance printed, for ^fumola_wip's Printed
   pane. The runtime drains its print buffer into every reply, as `printed`,
   and leaves the key out when there was nothing; each run replaces the last,
   as Fumola's web player does. Fumola keeps a `\n` in a text literal as the
   two characters, so they are turned back into a line break here. */
let printed: Hashtbl.t(string, list(string)) = Hashtbl.create(8);

let unescape_newlines = (s: string): string => {
  let b = Buffer.create(String.length(s));
  let n = String.length(s);
  let rec go = i =>
    if (i < n) {
      if (s.[i] == '\\' && i + 1 < n && s.[i + 1] == 'n') {
        Buffer.add_char(b, '\n');
        go(i + 2);
      } else {
        Buffer.add_char(b, s.[i]);
        go(i + 1);
      };
    };
  go(0);
  Buffer.contents(b);
};

let note_printed = (name: string, reply: Yojson.Safe.t): unit =>
  Hashtbl.replace(
    printed,
    name,
    switch (reply) {
    | `Assoc(obj) =>
      switch (List.assoc_opt("printed", obj)) {
      | Some(`List(lines)) =>
        List.filter_map(
          fun
          | `String(s) => Some(unescape_newlines(s))
          | _ => None,
          lines,
        )
      | _ => []
      }
    | _ => []
    },
  );

let printed_of = (name: string): list(string) =>
  Option.value(Hashtbl.find_opt(printed, name), ~default=[]);

/* The outline of every top-level force in an instance: Adapton's Outline
   value tree, one tree per force, for FumolaOutline to draw -- the forest
   Adapton.IntoText would write as text and Fumola's web player draws as
   HTML. Evaluated on a scratch branch, since computing an outline memoises
   into the store; and at the latest moment, as every read is (see at_now):
   at Now the runs' thunks are invisible and Outline's peekInfo finds null.
   A $simple instance keeps no graph, so its forest is always empty. */
let outlines_local = (name: string): result(list(Yojson.Safe.t), string) =>
  switch (
    shim(
      "evalScratch",
      [|
        js_int(instance_of_name(name)),
        js_string(at_now("Adapton.Outline.outlines()")),
      |],
    )
  ) {
  | exception _ => Error("no Fumola runtime available")
  | r =>
    switch (
      Yojson.Safe.from_string(
        r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string,
      )
    ) {
    | exception _ => Error("could not read the Fumola runtime's response")
    | `Assoc(obj) =>
      switch (
        List.assoc_opt("ok", obj),
        List.assoc_opt("tag", obj),
        List.assoc_opt("value", obj),
      ) {
      | (Some(`Bool(true)), Some(`String("List")), Some(`List(trees))) =>
        Ok(trees)
      | _ =>
        Error(
          switch (List.assoc_opt("error", obj)) {
          | Some(`String(message)) => message
          | _ => "the outline did not come back as a list"
          },
        )
      }
    | _ => Error("could not read the Fumola runtime's response")
    }
  };

/* The last run in each instance, for ^fumola_wip to show: the program
   exactly as it was sent -- printed from the tiles, so its parentheses say
   how the tiles grouped -- and what came back, a value or the runtime's
   error. A run that fails part way leaves nothing in the store to show, so
   without this the reader sees only the run before it. */
type last_run = {
  /* Empty when the code could not be printed, so nothing was sent. */
  program: string,
  outcome: result(string, string),
};

/* Kept per instance and per use: two uses sharing an instance would
   otherwise each show whichever of them ran last. */
let last_runs: Hashtbl.t((string, string), last_run) = Hashtbl.create(8);

/* Which use a program came from. A ^fumola_wip program's first step writes
   `name(`input), so the name is there without printing anything; any other
   program belongs to no use. */
let owner_of = (body: FumolaTermBase.t): string =>
  switch (body.term) {
  | FumolaGrammar.Block([first, ..._]) =>
    switch (first.term) {
    | FumolaGrammar.DExp(put) =>
      switch (put.term) {
      | FumolaGrammar.Put(cell, _) =>
        switch (cell.term) {
        | FumolaGrammar.Ap(head, _) =>
          switch (head.term) {
          | FumolaGrammar.QuotedId(name) => name
          | _ => ""
          }
        | _ => ""
        }
      | _ => ""
      }
    | _ => ""
    }
  | _ => ""
  };

let last_run_of = (~instance: string, ~name: string): option(last_run) =>
  Hashtbl.find_opt(last_runs, (instance, name));

/* REMOTE INSTANCES: an instance may live on the Internet Computer instead of
   in the page -- the canister of docs/internet-computer-backend.md, which
   runs the same Fumola runtime (fumola_wasm_common) behind
   `POST /i/<instance>/<op>`. The program travels as the text this page would
   have run, the canister runs it, and the reply is the same JSON a local run
   answers, so it becomes a Hazel value the same way.

   Where an instance lives is the instance's, not a use's: ^fumola_wip's
   `remote` box says it, at expansion, for its instance. Two uses of one
   instance that disagree are settled by the later one, since every
   expansion happens before any run. */
let remote_instances: Hashtbl.t(string, unit) = Hashtbl.create(4);

let set_remote = (instance: string, remote: bool): unit =>
  remote
    ? Hashtbl.replace(remote_instances, instance, ())
    : Hashtbl.remove(remote_instances, instance);

let is_remote = (instance: string): bool =>
  Hashtbl.mem(remote_instances, instance);

/* Where an instance is read from. A name says nothing about it -- a page
   and the canister can each hold an instance of the same name -- so a
   reader that knows (the Fumola panel's list) says; one that does not
   takes where this page's programs run it. */
[@deriving (show({with_path: false}), sexp, yojson)]
type place =
  | Page
  | Canister;

let place_of = (~place: option(place)=?, instance: string): place =>
  switch (place) {
  | Some(p) => p
  | None => is_remote(instance) ? Canister : Page
  };

/* A run here is synchronous and a call to the canister is not, so the reply
   is kept by what was asked -- the instance, its declared mode, and the
   program as printed, before its moment is added, which changes every
   run -- and a run that finds no reply yet asks for one and says so. The
   reply's arrival is announced as `fumola-remote-reply`, which Main.re
   treats as the runtime arriving: the program is run again, and this time
   the reply is here. */
let remote_replies: Hashtbl.t((string, string), Yojson.Safe.t) =
  Hashtbl.create(16);

/* How many program replies each remote instance has given: what the
   instance holds changes only with one, so a side query of it
   (remote_query) is good until the next. */
let remote_generation: Hashtbl.t(string, int) = Hashtbl.create(4);
let generation_of = (instance: string): int =>
  Option.value(Hashtbl.find_opt(remote_generation, instance), ~default=0);
let remote_asked: Hashtbl.t((string, string), unit) = Hashtbl.create(16);

/* The canister's store, read as an instance: Hazel's saved data, which the
   canister evaluates against but never empties, so it has no reset. */
let store_instance = "hazelStore";

/* Where a canister instance's last reset has got to, for the panel to say.
   A reset that works rebuilds the same graph, so without this it looks
   exactly like a button that did nothing. Ran and Not_run are cleared after
   a few seconds; the others last until the next step. */
type reset_status =
  | Resetting
  | Rerunning
  | Asked
  | Ran
  | Not_run
  | Failed(string);
let remote_resets: Hashtbl.t(string, reset_status) = Hashtbl.create(4);
let reset_status = (instance: string): option(reset_status) =>
  Hashtbl.find_opt(remote_resets, instance);

/* A redraw and nothing more, as a side query's answer asks for. */
let redraw = () =>
  ignore(
    Js_of_ocaml.Js.Unsafe.js_expr(
      "window.dispatchEvent(new Event('fumola-remote-query'))",
    ),
  );
let after = (ms: int, f: unit => unit) =>
  ignore(
    Js_of_ocaml.Js.Unsafe.fun_call(
      Js_of_ocaml.Js.Unsafe.js_expr("setTimeout"),
      [|
        Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.wrap_callback(f)),
        Js_of_ocaml.Js.Unsafe.inject(ms),
      |],
    ),
  );
let set_reset_status = (instance: string, status: option(reset_status)) => {
  switch (status) {
  | Some(s) => Hashtbl.replace(remote_resets, instance, s)
  | None => Hashtbl.remove(remote_resets, instance)
  };
  redraw();
};
/* Clear a status after a while, unless a later step has replaced it. */
let clear_later = (instance: string, status: reset_status) =>
  after(6000, () =>
    if (reset_status(instance) == Some(status)) {
      set_reset_status(instance, None);
    }
  );

let remote_reply =
    (~instance: string, ~mode: option(mode), ~at: int, program: string)
    : option(Yojson.Safe.t) => {
  let mode_text = Option.fold(~none="", ~some=mode_source, mode);
  let key = (instance, mode_text ++ "\n" ++ program);
  switch (Hashtbl.find_opt(remote_replies, key)) {
  | Some(reply) => Some(reply)
  | None =>
    if (!Hashtbl.mem(remote_asked, key)) {
      Hashtbl.replace(remote_asked, key, ());
      if (reset_status(instance) == Some(Rerunning)) {
        Hashtbl.replace(remote_resets, instance, Asked);
      };
      let on_reply = (text: Js_of_ocaml.Js.t(Js_of_ocaml.Js.js_string)) => {
        let reply =
          switch (Yojson.Safe.from_string(Js_of_ocaml.Js.to_string(text))) {
          | exception _ =>
            `Assoc([
              ("ok", `Bool(false)),
              ("error", `String("the canister's reply was not JSON")),
            ])
          | json => json
          };
        Hashtbl.replace(remote_replies, key, reply);
        Hashtbl.remove(remote_asked, key);
        if (reset_status(instance) == Some(Asked)) {
          Hashtbl.replace(remote_resets, instance, Ran);
          clear_later(instance, Ran);
        };
        Hashtbl.replace(
          remote_generation,
          instance,
          generation_of(instance) + 1,
        );
      };
      switch (
        Js_of_ocaml.Js.Optdef.to_option(
          Js_of_ocaml.Js.Unsafe.get(
            Js_of_ocaml.Js.Unsafe.global,
            "hazelFumolaRemote",
          ),
        )
      ) {
      | exception _
      | None =>
        Hashtbl.replace(
          remote_replies,
          key,
          `Assoc([
            ("ok", `Bool(false)),
            (
              "error",
              `String(
                "this build has no canister to run on: remote runs need the Internet Computer build",
              ),
            ),
          ]),
        )
      | Some(call) =>
        ignore(
          Js_of_ocaml.Js.Unsafe.fun_call(
            call,
            [|
              js_string(instance),
              js_string(mode_text),
              js_string(at_moment(at, program)),
              Js_of_ocaml.Js.Unsafe.inject(
                Js_of_ocaml.Js.wrap_callback(on_reply),
              ),
            |],
          ),
        )
      };
    };
    Hashtbl.find_opt(remote_replies, key);
  };
};

/* Asked of a remote instance on the side, not run as its program: a watch
   pane's history (`prim "adaptonPeekHistory" ()` through eval_scratch, which
   keeps nothing), its mode, its stats. The answer is kept until the
   instance's next program reply (remote_generation); until a fresh one
   arrives, the last one stands in, so a pane redrawn mid-question shows
   what it showed rather than flickering to empty. None: never answered. */
let remote_queries:
  Hashtbl.t((string, string, string), (int, Yojson.Safe.t)) =
  Hashtbl.create(8);
let remote_querying: Hashtbl.t((string, string, string), unit) =
  Hashtbl.create(8);

let js_global = (name: string) =>
  switch (
    Js_of_ocaml.Js.Optdef.to_option(
      Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, name),
    )
  ) {
  | exception _ => None
  | f => f
  };

let json_of_reply = (text: Js_of_ocaml.Js.t(Js_of_ocaml.Js.js_string)) =>
  switch (Yojson.Safe.from_string(Js_of_ocaml.Js.to_string(text))) {
  | exception _ =>
    `Assoc([
      ("ok", `Bool(false)),
      ("error", `String("the canister's reply was not JSON")),
    ])
  | json => json
  };

let no_canister =
  `Assoc([
    ("ok", `Bool(false)),
    (
      "error",
      `String(
        "this build has no canister: remote instances need the Internet Computer build",
      ),
    ),
  ]);

let remote_query =
    (~instance: string, ~op: string, ~name: option(string)=?, body: string)
    : option(Yojson.Safe.t) => {
  /* Kept by [name] when the body changes from run to run (a read at the
     latest moment) but the question does not. */
  let key = (instance, op, Option.value(name, ~default=body));
  let generation = generation_of(instance);
  let cached = Hashtbl.find_opt(remote_queries, key);
  switch (cached) {
  | Some((g, reply)) when g == generation => Some(reply)
  | _ =>
    if (!Hashtbl.mem(remote_querying, key)) {
      Hashtbl.replace(remote_querying, key, ());
      let on_reply = text => {
        Hashtbl.replace(
          remote_queries,
          key,
          (generation, json_of_reply(text)),
        );
        Hashtbl.remove(remote_querying, key);
      };
      switch (js_global("hazelFumolaRemoteQuery")) {
      | None =>
        Hashtbl.replace(remote_queries, key, (generation, no_canister));
        Hashtbl.remove(remote_querying, key);
      | Some(call) =>
        ignore(
          Js_of_ocaml.Js.Unsafe.fun_call(
            call,
            [|
              js_string(instance),
              js_string(op),
              js_string(body),
              Js_of_ocaml.Js.Unsafe.inject(
                Js_of_ocaml.Js.wrap_callback(on_reply),
              ),
            |],
          ),
        )
      };
    };
    Option.map(snd, Hashtbl.find_opt(remote_queries, key));
  };
};

/* The canister's GET /stats: its heap, its store's DCG, each instance. Its
   store changes with every save from any page, which no reply here says, so
   it is asked again when the last answer is over [max_age_ms] old -- on a
   redraw, which is when anyone is looking. */
let now_ms = (): float =>
  Js_of_ocaml.Js.float_of_number(
    Js_of_ocaml.Js.Unsafe.js_expr("Date.now()"),
  );

let canister_stats_cache: ref(option((float, Yojson.Safe.t))) = ref(None);
let canister_stats_asking = ref(false);

let canister_stats = (~max_age_ms=3000., ()): option(Yojson.Safe.t) => {
  let now = now_ms();
  let fresh =
    switch (canister_stats_cache^) {
    | Some((at, _)) => now -. at < max_age_ms
    | None => false
    };
  if (!fresh && ! canister_stats_asking^) {
    switch (js_global("hazelBackendStats")) {
    | None => canister_stats_cache := Some((now, no_canister))
    | Some(call) =>
      canister_stats_asking := true;
      let on_reply = text => {
        canister_stats_cache := Some((now_ms(), json_of_reply(text)));
        canister_stats_asking := false;
      };
      ignore(
        Js_of_ocaml.Js.Unsafe.fun_call(
          call,
          [|
            Js_of_ocaml.Js.Unsafe.inject(
              Js_of_ocaml.Js.wrap_callback(on_reply),
            ),
          |],
        ),
      );
    };
  };
  Option.map(snd, canister_stats_cache^);
};

/* The two reads a watch pane makes of an instance besides its history,
   wherever the instance lives. A remote one is asked on the side
   (remote_query), and until it answers the pane says so. */
let mode_of_instance = (~place=?, name: string): option(mode) =>
  if (place_of(~place?, name) == Canister) {
    switch (remote_query(~instance=name, ~op="mode", "")) {
    | Some(`Assoc(fields)) =>
      switch (List.assoc_opt("mode", fields)) {
      | Some(`String(spelled)) =>
        List.find_opt(
          mode => mode_source(mode) == spelled,
          [Simple, Graphical],
        )
      | _ => None
      }
    | _ => None
    };
  } else {
    mode_of_instance_local(name);
  };

let outlines = (~place=?, name: string): result(list(Yojson.Safe.t), string) =>
  if (place_of(~place?, name) == Canister) {
    switch (
      remote_query(
        ~instance=name,
        ~op="eval_scratch",
        ~name="outlines",
        at_now("Adapton.Outline.outlines()"),
      )
    ) {
    | None => Error("asking the canister for this instance's outline...")
    | Some(`Assoc(obj)) =>
      switch (
        List.assoc_opt("ok", obj),
        List.assoc_opt("tag", obj),
        List.assoc_opt("value", obj),
      ) {
      | (Some(`Bool(true)), Some(`String("List")), Some(`List(trees))) =>
        Ok(trees)
      | _ =>
        Error(
          switch (List.assoc_opt("error", obj)) {
          | Some(`String(message)) => message
          | _ => "the outline did not come back as a list"
          },
        )
      }
    | Some(_) => Error("could not read the canister's response")
    };
  } else {
    outlines_local(name);
  };

/* Ask a canister instance's side questions afresh: what it holds can
   change with no reply here to say so (another page ran it), and a reader
   looking at it can say when to look again. */
let refresh_remote = (instance: string): unit =>
  Hashtbl.replace(remote_generation, instance, generation_of(instance) + 1);

/* The instances this page's runtime holds, by name, with their stats
   (window.fumola.instances, prebundle.js). None without a runtime. */
let local_instances = (): option(Yojson.Safe.t) =>
  switch (shim("instances", [||])) {
  | exception _ => None
  | r =>
    switch (
      Yojson.Safe.from_string(
        r |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string,
      )
    ) {
    | exception _ => None
    | json => Some(json)
    }
  };

/* Reset a remote instance, as the watch pane's G and S do a local one:
   empty it on the canister, give it the mode asked for, forget the replies
   kept for it, and announce a reply so the page's programs run again --
   asking the canister afresh, now that nothing is kept. In that order, each
   step on the last one's answer, so the re-run cannot overtake the reset.

   Each step's answer is read: a refusal (the store's reset is a 404) stops
   the reset and says why, rather than carrying on as if it had worked. If
   no program here asks within a few seconds of the re-run, nothing on this
   page runs the instance, and the panel says that instead of waiting. */
let reset_remote = (~mode: option(mode)=?, name: string): unit =>
  switch (js_global("hazelFumolaRemoteQuery")) {
  | None => ()
  | Some(call) =>
    let refusal = (text: string): option(string) =>
      switch (Yojson.Safe.from_string(text)) {
      | exception _ => Some("the canister's reply was not JSON")
      | `Assoc(fields)
          when List.assoc_opt("ok", fields) == Some(`Bool(false)) =>
        switch (List.assoc_opt("error", fields)) {
        | Some(`String(e)) => Some(e)
        | _ => Some("the canister refused")
        }
      | _ => None
      };
    let ask = (op, body, k) =>
      ignore(
        Js_of_ocaml.Js.Unsafe.fun_call(
          call,
          [|
            js_string(name),
            js_string(op),
            js_string(body),
            Js_of_ocaml.Js.Unsafe.inject(
              Js_of_ocaml.Js.wrap_callback(
                (text: Js_of_ocaml.Js.t(Js_of_ocaml.Js.js_string)) =>
                switch (refusal(Js_of_ocaml.Js.to_string(text))) {
                | Some(why) => set_reset_status(name, Some(Failed(why)))
                | None => k()
                }
              ),
            ),
          |],
        ),
      );
    let forget_and_rerun = () => {
      let keys =
        Hashtbl.fold(
          ((i, _) as key, _, acc) => i == name ? [key, ...acc] : acc,
          remote_replies,
          [],
        );
      List.iter(Hashtbl.remove(remote_replies), keys);
      Hashtbl.replace(remote_generation, name, generation_of(name) + 1);
      Hashtbl.replace(remote_resets, name, Rerunning);
      after(3000, () =>
        if (reset_status(name) == Some(Rerunning)) {
          set_reset_status(name, Some(Not_run));
          clear_later(name, Not_run);
        }
      );
      ignore(
        Js_of_ocaml.Js.Unsafe.js_expr(
          "window.dispatchEvent(new Event('fumola-remote-reply'))",
        ),
      );
    };
    set_reset_status(name, Some(Resetting));
    ask("reset", "", () =>
      switch (mode) {
      | Some(mode) =>
        ask("ensure_mode", mode_source(mode), forget_and_rerun)
      | None => forget_and_rerun()
      }
    );
  };

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
        /* Nothing is sent, but the reason still replaces the last outcome:
           otherwise the use keeps showing a run of code it no longer has. */
        Hashtbl.replace(
          last_runs,
          (instance_name, owner_of(body)),
          {
            program: "",
            outcome: Error(message),
          },
        );
        Error({
          syntax: false,
          message,
        });
      | None =>
        let program = Fumola.of_exp(body);
        let at = claim_moment(pass);
        let instance_id = instance_of_name(instance_name);
        /* Declare the mode when the declaration is new or has changed, which
           is when it means something. An unchanged declaration says nothing
           the instance has not already been told, and saying it anyway would
           overrule whoever spoke last -- which, after a reset, is the
           reader. Leaving the mode slot a hole still says nothing at all. */
        let remote = is_remote(instance_name);
        Option.iter(
          mode =>
            if (!remote
                && Hashtbl.find_opt(last_declared, instance_id) != Some(mode)) {
              ensure_mode(instance_id, mode);
              Hashtbl.replace(last_declared, instance_id, mode);
            },
          mode,
        );
        note_run(instance_name);
        /* A remote instance's mode travels with each program, and the
           canister's ensure_mode is the same no-op for an unchanged one. */
        let reply =
          remote
            ? switch (
                remote_reply(~instance=instance_name, ~mode, ~at, program)
              ) {
              | Some(reply) => reply
              | None => `String("pending")
              }
            : eval_at(instance_id, at_moment(at, program));
        note_printed(instance_name, reply);
        let outcome =
          switch (reply) {
          | `Null =>
            Error({
              syntax: false,
              message: "no Fumola runtime available",
            })
          | `String("pending") =>
            Error({
              syntax: false,
              message: "running on the canister...",
            })
          | `Assoc(obj) as json =>
            switch (List.assoc_opt("ok", obj)) {
            | Some(`Bool(true)) =>
              switch (
                FumolaValue.exp_of_json(
                  ~instance_id,
                  /* What a remote value points at is on the canister, and
                     this read is synchronous: a pointer arrives as its
                     name, unread. */
                  ~eval=remote ? _ => `Null : eval_in(instance_id),
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
        Hashtbl.replace(
          last_runs,
          (instance_name, owner_of(body)),
          {
            program,
            outcome:
              switch (outcome) {
              | Ok(e) => Ok(FumolaValue.describe_value(e))
              | Error({message, _}) => Error(message)
              },
          },
        );
        outcome;
      }
    }
  };
