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
  /* the latest program found to be this one but for its livelits' models (^report(50)'s next step 3), the one it was
     compiled from at first */
  program: Exp.t,
  /* that program with each parameter's model left out, and every livelit use's projector id in order, to tell whether
     a new program is the same but for those models (erase) */
  erased: Exp.t,
  uses: list(string),
  fn: U.any, /* new Function("$R", src) */
  types: HazelJitValue.types,
  /* each livelit use's projector id with its model's parameter (Id.to_string both); only literal models have one */
  param_of: Hashtbl.t(string, string),
  /* the models given so far, by parameter, kept between events: a drag's steps do not change the program */
  params: U.any,
  /* each parameter's value as the run last had it (the program's literal, or the latest step's model), as JSON */
  given: Hashtbl.t(string, string),
};

/* The program the page draws livelits from, and its compilation (or why the program it was tried on has none) */
let program: ref(option(Exp.t)) = ref(None);
let compiled: ref(option(result(compiled, (Exp.t, string)))) = ref(None);

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

/* (^report(50)'s next step 3: compile once, not again after a step) A step commits its new model to the program, so
   the page has a new program after each one, and it was compiled again, 126 ms of the page's thread on the Witch
   (build 8). But the compiled program takes each literal model as a parameter: a program that is the same but for
   those models runs as the old one with the new models given. Each use's model is found where fold_views finds it, in
   the use's expansion, and left out (a placeholder in its place), the rest compared as Exp.fast_equal does (ids
   aside), and every livelit use's projector id compared too, since the compiled program samples views by them. */
let model_of = (e: Exp.t): option(Exp.t) =>
  switch (Exp.term_of(strip(e))) {
  | Match(scrut, [(_, body), ..._]) =>
    switch (Exp.term_of(strip(scrut)), Exp.term_of(strip(body))) {
    | (Dot(_, l), Ap(Forward, _, model))
        when HazelJit.label_of(l) == Some("expand") =>
      Some(model)
    | _ => None
    }
  | Let(p, model, _) when HazelJit.pat_is("%macro_model", p) => Some(model)
  | _ => None
  };
/* the same expansion with [m] for its model */
let set = (e: Exp.t, term: Exp.term): Exp.t => {
  ...e,
  term,
};
let rec with_model = (e: Exp.t, m: Exp.t): option(Exp.t) =>
  switch (Exp.term_of(e)) {
  | Parens(x) => Option.map(x => set(e, Parens(x)), with_model(x, m))
  | Asc(x, t) => Option.map(x => set(e, Asc(x, t)), with_model(x, m))
  | Match(scrut, [(p, body), ...rest]) =>
    Option.map(
      body => set(e, Match(scrut, [(p, body), ...rest])),
      with_arg(body, m),
    )
  | Let(p, _, body) when HazelJit.pat_is("%macro_model", p) =>
    Some(set(e, Let(p, m, body)))
  | _ => None
  }
and with_arg = (e: Exp.t, m: Exp.t): option(Exp.t) =>
  switch (Exp.term_of(e)) {
  | Parens(x) => Option.map(x => set(e, Parens(x)), with_arg(x, m))
  | Asc(x, t) => Option.map(x => set(e, Asc(x, t)), with_arg(x, m))
  | Ap(Forward, f, _) => Some(set(e, Ap(Forward, f, m)))
  | _ => None
  };
let left_out: Exp.t =
  Exp.fresh(Atom(String("%a livelit's model, a parameter of the run")));
/* The program with each parameter's model left out; every livelit use's projector id in order; and each parameter's
   use with its model */
let erase =
    (~param_of: Hashtbl.t(string, string), e: Exp.t)
    : (Exp.t, list(string), list((string, Exp.t))) => {
  let uses = ref([]);
  let models = ref([]);
  let keep = (_, x) => x;
  let e =
    Exp.map_term(
      ~f_pat=keep,
      ~f_typ=keep,
      ~f_tpat=keep,
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (continue(e)) {
          | {term: Projector(data, inner), _} as e'
              when data.Grammar.kind == ProjectorKind.Livelit =>
            let pid = Id.to_string(Exp.rep_id(e'));
            uses := [pid, ...uses^];
            switch (Hashtbl.mem(param_of, pid), model_of(inner)) {
            | (true, Some(model)) =>
              switch (with_model(inner, left_out)) {
              | Some(inner) =>
                models := [(pid, model), ...models^];
                {
                  ...e',
                  term: Projector(data, inner),
                };
              | None => e'
              }
            | _ => e'
            };
          | e' => e'
          },
      e,
    );
  (e, List.rev(uses^), List.rev(models^));
};

let compile = (e: Exp.t): result(compiled, string) => {
  /* (^report(51)) its two parts' times and the source's size, noted after (the term's nodes counted outside the time) */
  let timed = ref(None);
  let r =
    Util.BenchMarks.time("hazel-jit: the run compiled", () => {
      HazelJit.params_on := true;
      HazelJit.survey := Some(Hashtbl.create(4));
      Hashtbl.reset(HazelJit.param_of_projector);
      HazelJit.last_param := None;
      let t0 = HazelJitValue.now();
      let r =
        switch (HazelJit.compile_full(~env=Builtins.env_init, fold_views(e))) {
        | c =>
          let t1 = HazelJitValue.now();
          switch (
            U.fun_call(
              U.pure_js_expr(
                "(function (src) { return new Function('$R', src); })",
              ),
              [|U.inject(Js.string(c.src))|],
            )
          ) {
          | fn =>
            timed :=
              Some((
                t1 -. t0,
                HazelJitValue.now() -. t1,
                String.length(c.src),
              ));
            let param_of = Hashtbl.copy(HazelJit.param_of_projector);
            let (erased, uses, models) = erase(~param_of, e);
            let given = Hashtbl.create(64);
            List.iter(
              ((pid, model)) =>
                Hashtbl.replace(
                  given,
                  Hashtbl.find(param_of, pid),
                  switch (HazelJitValue.to_js(model)) {
                  | js => print_js(js)
                  | exception _ => "?" ++ string_of_int(Random.bits())
                  },
                ),
              models,
            );
            Ok({
              program: e,
              erased,
              uses,
              fn,
              types: HazelJitValue.types_of(c),
              param_of,
              params: U.obj([||]),
              given,
            });
          | exception exn => Error(HazelJitValue.message(exn))
          };
        | exception (HazelJit.Unsupported(why)) => Error(why)
        | exception exn => Error(HazelJitValue.message(exn))
        };
      HazelJit.params_on := false;
      HazelJit.survey := None;
      r;
    });
  switch (timed^) {
  | Some((to_js_ms, js_ms, chars)) =>
    HazelJitValue.note_compile(
      ~what="a slide's whole run",
      ~to_js_ms,
      ~js_ms,
      ~chars,
      ~nodes=HazelJitValue.nodes_of(e),
    )
  | None => ()
  };
  r;
};

/* [c] for the program [e], when [e] is the same but for its parameters' models, each new model given to its
   parameter; and whether any parameter's value changed (the answers then hold for the models before) */
/* (^report(51)) for the check only: how much of the new program is the
   very objects of the old one (its top-most shared subterms, and the nodes
   outside them), to see whether a comparison that skips shared subterms by
   identity would visit only what an edit changed */
let note_sharing = (old_e: Exp.t, new_e: Exp.t): unit =>
  if (Util.BenchMarks.on()) {
    let set = U.new_obj(U.get(U.global, "WeakSet"), [||]);
    let keep = (_, x) => x;
    let _ =
      Exp.map_term(
        ~f_pat=keep,
        ~f_typ=keep,
        ~f_tpat=keep,
        ~f_exp=
          (continue, e: Exp.t) => {
            ignore(U.meth_call(set, "add", [|U.inject(e)|]));
            continue(e);
          },
        old_e,
      );
    let shared = ref(0);
    let fresh = ref(0);
    let _ =
      Exp.map_term(
        ~f_pat=keep,
        ~f_typ=keep,
        ~f_tpat=keep,
        ~f_exp=
          (continue, e: Exp.t) =>
            if (Js.to_bool(U.meth_call(set, "has", [|U.inject(e)|]))) {
              incr(shared);
              e;
            } else {
              incr(fresh);
              continue(e);
            },
        new_e,
      );
    Util.BenchMarks.mark(
      ~extra=[
        ("shared_subterms", Util.BenchMarks.num(float_of_int(shared^))),
        ("fresh_nodes", Util.BenchMarks.num(float_of_int(fresh^))),
        ("same_root", Util.BenchMarks.num(old_e === new_e ? 1. : 0.)),
      ],
      "hazel-jit: the program's sharing with the one compared before",
    );
  };

let reuse = (c: compiled, e: Exp.t): option((compiled, bool)) => {
  note_sharing(c.program, e);
  Util.BenchMarks.time(
    "hazel-jit: the program compared, its livelits' models left out",
    () => {
      let (erased, uses, models) =
        Util.BenchMarks.time("hazel-jit: the comparison: models left out", () =>
          erase(~param_of=c.param_of, e)
        );
      if (uses != c.uses
          || !
               Util.BenchMarks.time(
                 "hazel-jit: the comparison: the rest equal", () =>
                 Exp.fast_equal(c.erased, erased)
               )) {
        None;
      } else {
        switch (
          List.fold_left(
            (changed, (pid, model)) => {
              let mid = Hashtbl.find(c.param_of, pid);
              let js = HazelJitValue.to_js(model);
              let p = print_js(js);
              if (Hashtbl.find_opt(c.given, mid) == Some(p)) {
                changed;
              } else {
                U.set(c.params, Js.string(mid), js);
                Hashtbl.replace(c.given, mid, p);
                true;
              };
            },
            false,
            models,
          )
        ) {
        | changed =>
          Some((
            {
              ...c,
              program: e,
            },
            changed,
          ))
        | exception _ => None
        };
      };
    },
  );
};

/* The compilation, when it still runs the program the page draws from: the same program, or one the same but for
   livelits' models, now given to its parameters. The answers are retired when they no longer hold: the program
   changed otherwise, or a parameter's value did (an undo, an edit of a model's text). Never compiles. */
let kept = (): option(compiled) =>
  switch (program^, compiled^) {
  | (Some(e), Some(Ok(c))) when c.program === e => Some(c)
  | (Some(e), Some(Ok(c))) =>
    switch (reuse(c, e)) {
    | Some((c, changed)) =>
      HazelJitValue.count(
        ~why=
          changed
            ? "a livelit's model changed outside a step: answers retired"
            : "the models the run was given",
        "the slide's run kept, the program the same but for its livelits' models",
      );
      if (changed) {
        retire();
      };
      compiled := Some(Ok(c));
      Some(c);
    | None =>
      HazelJitValue.count(
        "the slide's run compiled again: the program changed",
      );
      compiled := None;
      retire();
      None;
    }
  | _ => None
  };

/* The program's compilation, made now if it has not been */
let current = (): option(compiled) =>
  switch (kept()) {
  | Some(c) => Some(c)
  | None =>
    switch (program^) {
    | None => None
    | Some(e) =>
      let c =
        switch (compiled^) {
        | Some(Error((p, why))) when p === e => Error(why)
        | _ =>
          let c = compile(e);
          compiled :=
            Some(
              switch (c) {
              | Ok(c) => Ok(c)
              | Error(why) => Error((e, why))
              },
            );
          c;
        };
      switch (c) {
      | Ok(c) => Some(c)
      | Error(why) =>
        warn(why);
        None;
      };
    }
  };

/* Asked for a redraw when the answers are retired after the page has drawn (in note_program's timeout): set by the page
   (LivelitProj, through AppBridge.livelit_repaint); nothing in the CLI and tests */
let on_retire: ref(unit => unit) = ref(() => ());

/* The page draws livelits from this program now. Compared or compiled soon after, off this frame, after the paint
   (run 42651798712: compared in the view, its 29 to 47 ms came before the paint) */
let note_program = (e: Exp.t): unit =>
  if (HazelJitValue.enabled^) {
    switch (program^) {
    | Some(p) when p === e => ()
    | _ =>
      program := Some(e);
      ignore(
        U.fun_call(
          U.get(U.global, "setTimeout"),
          [|
            U.inject(
              Js.wrap_callback(() =>
                switch (program^) {
                | Some(p) when p === e =>
                  let before = retired^;
                  ignore(current());
                  if (retired^ != before) {
                    on_retire^();
                  };
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

/* (^report(50)'s next step 2) the livelit whose event the compiled run answered last, by projector id: cleared as an
   event begins (LivelitProj), so the event's commit knows the page can draw it before the edit */
let answered_for: ref(option(string)) = ref(None);

/* (^report(50)'s next step 4) when the compiled run last answered a livelit's event, for the page's request to the
   worker that the event's commit makes: held, since the livelits are already drawn (WorkerClient) */
let answered_at: ref(float) = ref(neg_infinity);
let now_ms = (): float => Js.float_of_number(U.js_expr("Date.now()"));
/* Whether the compiled run answered a livelit's event in the last [ms]; asked once a request (the flag is cleared) */
let take_answered = (~ms: float): bool => {
  let r = now_ms() -. answered_at^ <= ms;
  answered_at := neg_infinity;
  r;
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
          Hashtbl.replace(c.given, mid, print_js(m));
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
            answered_at := now_ms();
            answered_for := Some(Id.to_string(projector));
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

/* The worker's run has caught up with this livelit's model (LivelitProj, as the livelit draws). Before ^report(50)'s
   next step 4, the answers its event made were retired then, and every livelit was drawn again from the run's samples,
   to the same picture. Now they are kept while the compilation still runs the page's program (kept): they are its
   answers, with every model as the program has it. */
let caught_up = (~projector: Id.t): unit =>
  switch (source^) {
  | Some(s) when Id.to_string(s) == Id.to_string(projector) =>
    switch (program^, compiled^) {
    | (Some(p), Some(Ok(_))) =>
      /* the same program, or a new one note_program's timeout will compare */
      ignore(p);
      HazelJitValue.count("the worker caught up: answers kept");
    | _ => retire()
    }
  | _ => ()
  };

/* The latest run's answer for this livelit use: while the event that ran it is still the newest word on its livelit's
   model (its optimistic entry is there: [fresh]), or while the compilation still runs the page's program (kept, which
   compares a new program; note_program's timeout compares it after the paint). (Before ^report(50)'s next step 4: only
   while [fresh], and until the worker caught up.) */
let answer =
    (~fresh: Id.t => bool, id: Id.t): option((U.any, HazelJitValue.types)) =>
  switch (source^, answer_types^) {
  | (Some(src), Some(types)) =>
    switch (fresh(src) ? Some() : Option.map(_ => (), kept()), source^) {
    | (Some (), Some(_)) =>
      Hashtbl.find_opt(answers, Id.to_string(id))
      |> Option.map(js => (js, types))
    | (None, Some(_)) =>
      retire();
      None;
    | (_, None) => None
    }
  | (Some(_), None) =>
    retire();
    None;
  | (None, _) => None
  };
