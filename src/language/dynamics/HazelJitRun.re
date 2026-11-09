/* (^ticket(722); Matthew, 2026-11-08: "Compiled run first", "Yes, check then wire") A slide's run, compiled by
   hazel-jit, with each livelit use's model a parameter: at a livelit's event, the slide's program runs as JS with the
   new model given to its parameter, no new code, and every livelit's view answer is kept, for the page to draw
   straight from its JS (HazelJitDOM) before the worker's run comes back. In Node, on the Witch slide, such a step is
   2.3 to 2.5 ms against the interpreter's 684 ms (^report(45), the whole run, compiled).

   The page tells this module the program it draws livelits from (note_program, from ProjectorView); it is compiled
   soon after (a timeout), or at the first event that needs it, once for each program. A form hazel-jit does not
   compile becomes a stub that throws if the run reaches it (HazelJit.survey); then, or on any other failure, there are
   no answers, and the page draws from the run's samples as before. Behind the Compiled views setting
   (HazelJitValue.enabled). */
open Util;
open Js_of_ocaml;
module U = Js.Unsafe;

type compiled = {
  program: Exp.t,
  fn: U.any, /* new Function("$R", src) */
  types: HazelJitValue.types,
  /* each livelit use's projector id with its model's parameter (Id.to_string both); only literal models have one */
  param_of: Hashtbl.t(string, string),
  /* the models given so far, by parameter, kept between events: a drag's steps do not change the program */
  params: U.any,
};

/* The program the page draws livelits from, and its compilation (or why there is none) */
let program: ref(option(Exp.t)) = ref(None);
let compiled: ref(option(result(compiled, string))) = ref(None);

/* The latest run's answers: each livelit use's view answer, by projector id; the projector whose event ran it; and a
   count of runs, for the page's caches */
let answers: Hashtbl.t(string, U.any) = Hashtbl.create(64);
let answer_types: ref(option(HazelJitValue.types)) = ref(None);
let source: ref(option(Id.t)) = ref(None);
let runs: ref(int) = ref(0);
/* (^ticket(722), the drag fast path, cut B) which livelit uses' answers the latest step changed, each answer
   compared as JSON with the one before (its handlers, functions, leave no trace there), for the page to draw again
   only those; and a count of the times the answers were retired, which moves every livelit's version */
let printed: Hashtbl.t(string, string) = Hashtbl.create(64);
let changed: ref(list(string)) = ref([]);
let retired: ref(int) = ref(0);
let retire = (): unit => {
  Hashtbl.reset(answers);
  Hashtbl.reset(printed);
  source := None;
  incr(retired);
};
let print_js = (v: U.any): string =>
  switch (
    Js.Optdef.to_option(
      U.fun_call(U.js_expr("JSON.stringify"), [|U.inject(v)|]),
    )
  ) {
  | Some(s) => Js.to_string(s)
  | None => "undefined"
  | exception _ => "?" ++ string_of_int(Random.bits())
  };

let warned: Hashtbl.t(string, unit) = Hashtbl.create(4);
/* Each time counted with its reason, for a check in a browser to read (HazelJitValue.count); warned in the console
   once */
let warn = (why: string) => {
  HazelJitValue.count(~why, "the slide's run not used");
  if (!Hashtbl.mem(warned, why)) {
    Hashtbl.add(warned, why, ());
    ignore(
      U.meth_call(
        U.get(U.global, "console"),
        "warn",
        [|
          U.inject(
            Js.string("hazel-jit: the slide's run is not compiled: " ++ why),
          ),
        |],
      ),
    );
  };
};

/* A slide is elaborated without its livelits' views in the run: the page gives DefStatics only the cells' pins as
   probes (Program.probe_ids), so Statics' Projector case does not fold a use's view in, and each user livelit use is
   still the expansion UserLivelit.mk_expand_dot_def makes, match def.expand { Functional(f) => f(model) | Macro(_) =>
   ... }. (The CLI's --views probed every use, so the Node numbers ran a program the page never had: Firefox run
   42548426910 counted "0 of the program's livelit uses" with a parameter.) Here each such use gets its view folded in
   as Statics folds a probed use's (UserLivelit.instrument_view), for this compiled run only: the model bound once to
   %model, a parameter when it is a literal, and def.view(%model) sampled at the projector's id. A use already folded
   (probed) has another shape and is left as it is. */
let rec strip = (e: Exp.t): Exp.t =>
  switch (Exp.term_of(e)) {
  | Parens(e)
  | Asc(e, _) => strip(e)
  | _ => e
  };
/* A macro livelit's use is expanded at elaboration (Statics' macro case): let %macro_model = model in the body's
   closure applied to each splice's value. It holds no definition to call view on, so the page notes each use's
   definition as it draws it (LivelitProj: the context's user_def, closed, the term its events run), by projector id;
   Firefox run 42549941977 found the Witch's ^picture, the slide's one macro use, the only one not folded. */
let defs: Hashtbl.t(string, Exp.t) = Hashtbl.create(64);
let note_def = (id: Id.t, def: Exp.t): unit =>
  if (HazelJitValue.enabled^) {
    Hashtbl.replace(defs, Id.to_string(id), def);
  };
let expansion_parts =
    (~projector: Id.t, e: Exp.t): result((Exp.t, Exp.t), string) =>
  switch (Exp.term_of(strip(e))) {
  | Match(scrut, [(_, body), ..._]) =>
    switch (Exp.term_of(strip(scrut)), Exp.term_of(strip(body))) {
    | (Dot(def, l), Ap(Forward, _, model))
        when HazelJit.label_of(l) == Some("expand") =>
      Ok((def, model))
    | _ => Error("a match of another shape")
    }
  | Let(p, model, _) when HazelJit.pat_is("%macro_model", p) =>
    switch (Hashtbl.find_opt(defs, Id.to_string(projector))) {
    | Some(def) => Ok((def, model))
    | None => Error("a macro livelit's use, its definition not yet noted")
    }
  | _ =>
    let s = Exp.show(strip(e));
    Error(
      "a livelit use's expansion of another shape: "
      ++ String.sub(s, 0, min(120, String.length(s))),
    );
  };
let fold_views = (e: Exp.t): Exp.t => {
  let folded = ref(0);
  let e =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (continue(e)) {
          | {term: Projector(data, inner), _} as e'
              when data.Grammar.kind == ProjectorKind.Livelit =>
            switch (expansion_parts(~projector=Exp.rep_id(e'), inner)) {
            | Ok((def, model)) =>
              incr(folded);
              {
                ...e',
                term:
                  Projector(
                    data,
                    UserLivelit.instrument_view(
                      ~projector_id=Exp.rep_id(e),
                      ~def,
                      ~model,
                      inner,
                    ),
                  ),
              };
            | Error(why) =>
              HazelJitValue.count(~why, "the slide's run: a use not folded");
              e';
            }
          | e' => e'
          },
      e,
    );
  HazelJitValue.count(
    ~why=string_of_int(folded^) ++ " livelit uses' views folded in",
    "the slide's run compiled",
  );
  e;
};

let compile = (e: Exp.t): result(compiled, string) =>
  Util.BenchMarks.time("hazel-jit: the run compiled", () => {
    HazelJit.params_on := true;
    HazelJit.survey := Some(Hashtbl.create(4));
    Hashtbl.reset(HazelJit.param_of_projector);
    HazelJit.last_param := None;
    let r =
      switch (HazelJit.compile_full(~env=Builtins.env_init, fold_views(e))) {
      | c =>
        switch (
          U.fun_call(
            U.pure_js_expr(
              "(function (src) { return new Function('$R', src); })",
            ),
            [|U.inject(Js.string(c.src))|],
          )
        ) {
        | fn =>
          Ok({
            program: e,
            fn,
            types: HazelJitValue.types_of(c),
            param_of: Hashtbl.copy(HazelJit.param_of_projector),
            params: U.obj([||]),
          })
        | exception exn => Error(HazelJitValue.message(exn))
        }
      | exception (HazelJit.Unsupported(why)) => Error(why)
      | exception exn => Error(HazelJitValue.message(exn))
      };
    HazelJit.params_on := false;
    HazelJit.survey := None;
    r;
  });

/* The program's compilation, made now if it has not been */
let current = (): option(compiled) =>
  switch (program^) {
  | None => None
  | Some(e) =>
    let c =
      switch (compiled^) {
      | Some(Ok(c)) when c.program === e => Ok(c)
      | Some(Error(_) as err) => err
      | _ =>
        let c = compile(e);
        compiled := Some(c);
        c;
      };
    switch (c) {
    | Ok(c) => Some(c)
    | Error(why) =>
      warn(why);
      None;
    };
  };

/* The page draws livelits from this program now. Compiled soon after, off this frame. */
let note_program = (e: Exp.t): unit =>
  if (HazelJitValue.enabled^) {
    switch (program^) {
    | Some(p) when p === e => ()
    | _ =>
      program := Some(e);
      compiled := None;
      ignore(
        U.fun_call(
          U.get(U.global, "setTimeout"),
          [|
            U.inject(
              Js.wrap_callback(() =>
                switch (program^) {
                | Some(p) when p === e => ignore(current())
                | _ => ()
                }
              ),
            ),
            U.inject(Js.number_of_float(0.)),
          |],
        ),
      );
    };
  };

/* A livelit's event: its new model given to its parameter and the program run; every livelit's view answer kept.
   The number of answers, or why there are none. */
let step = (~projector: Id.t, ~model: Exp.t): result(int, string) =>
  if (! HazelJitValue.enabled^) {
    Error("compiled views are off");
  } else {
    changed := [];
    HazelJitValue.count("the slide's run: a livelit's event");
    switch (current()) {
    | None =>
      HazelJitValue.count(
        ~why=
          switch (program^) {
          | None => "the page has not said which program it draws from"
          | Some(_) => "the program did not compile (its reason counted too)"
          },
        "the slide's run not used",
      );
      Error("no compiled run");
    | Some(c) =>
      switch (Hashtbl.find_opt(c.param_of, Id.to_string(projector))) {
      | None =>
        let why =
          Printf.sprintf(
            "a livelit's model is not a parameter of the run (%d of the program's livelit uses have one)",
            Hashtbl.length(c.param_of),
          );
        warn(why);
        Error(why);
      | Some(mid) =>
        switch (HazelJitValue.to_js(model)) {
        | exception exn =>
          let why =
            "a livelit's new model has no JS form: "
            ++ HazelJitValue.message(exn);
          warn(why);
          Error(why);
        | m =>
          U.set(c.params, Js.string(mid), m);
          let r = Lazy.force(HazelJitValue.runtime);
          let samples = U.obj([||]);
          U.set(r, "params", c.params);
          U.set(r, "samples", samples);
          let ran =
            Util.BenchMarks.time("hazel-jit: the run", () =>
              switch (U.fun_call(c.fn, [|r|])) {
              | _ => Ok()
              | exception exn => Error(HazelJitValue.message(exn))
              }
            );
          U.set(r, "params", Js.null);
          U.set(r, "samples", Js.null);
          switch (ran) {
          | Error(why) =>
            warn(why);
            Error(why);
          | Ok () =>
            let before = Hashtbl.copy(printed);
            Hashtbl.reset(answers);
            Hashtbl.reset(printed);
            let keys: Js.t(Js.js_array(Js.t(Js.js_string))) =
              U.fun_call(U.js_expr("Object.keys"), [|U.inject(samples)|]);
            Array.iter(
              (k: Js.t(Js.js_string)) =>
                Hashtbl.replace(
                  answers,
                  Js.to_string(k),
                  U.get(samples, k),
                ),
              Js.to_array(keys),
            );
            let diff = ref([]);
            Hashtbl.iter(
              (k, v) => {
                let p = print_js(v);
                Hashtbl.replace(printed, k, p);
                if (Hashtbl.find_opt(before, k) != Some(p)) {
                  diff := [k, ...diff^];
                };
              },
              answers,
            );
            Hashtbl.iter(
              (k, _) =>
                if (!Hashtbl.mem(printed, k)) {
                  diff := [k, ...diff^];
                },
              before,
            );
            changed := diff^;
            answer_types := Some(c.types);
            source := Some(projector);
            incr(runs);
            HazelJitValue.count("the slide's run answered");
            Util.BenchMarks.mark(
              ~extra=[
                (
                  "views",
                  U.inject(
                    Js.number_of_float(
                      float_of_int(Hashtbl.length(answers)),
                    ),
                  ),
                ),
              ],
              "hazel-jit: views answered",
            );
            Ok(Hashtbl.length(answers));
          };
        }
      }
    };
  };

/* The worker's run has caught up with this livelit's model (LivelitProj, as the livelit draws): the answers its event made
   are retired, and every livelit draws from the run's samples again */
let caught_up = (~projector: Id.t): unit =>
  switch (source^) {
  | Some(s) when Id.to_string(s) == Id.to_string(projector) => retire()
  | _ => ()
  };

/* The latest run's answer for this livelit use, while [fresh] says the run it came from is still ahead of the worker's */
let answer =
    (~fresh: Id.t => bool, id: Id.t): option((U.any, HazelJitValue.types)) =>
  switch (source^, answer_types^) {
  | (Some(src), Some(types)) when fresh(src) =>
    switch (Hashtbl.find_opt(answers, Id.to_string(id))) {
    | Some(js) => Some((js, types))
    | None => None
    }
  | (Some(_), _) =>
    /* the worker's run has caught up: the answers are stale */
    retire();
    None;
  | _ => None
  };
