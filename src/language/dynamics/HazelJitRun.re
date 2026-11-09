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

let compile = (e: Exp.t): result(compiled, string) =>
  Util.BenchMarks.time("hazel-jit: the run compiled", () => {
    HazelJit.params_on := true;
    HazelJit.survey := Some(Hashtbl.create(4));
    Hashtbl.reset(HazelJit.param_of_projector);
    HazelJit.last_param := None;
    let r =
      switch (HazelJit.compile_full(~env=Builtins.env_init, e)) {
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
            Hashtbl.reset(answers);
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
    Hashtbl.reset(answers);
    source := None;
    None;
  | _ => None
  };
