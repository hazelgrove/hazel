/* (^ticket(766)) Mixed mode, a first step: the interpreter keeps running the program, around its holes, with probes and
   the stepper as they are, and calls the program's complete functions compiled, as it calls builtins.

   Which functions: a let at the top of the program (a chain of lets and type aliases, before its first other form), or
   a member of a module bound there, that binds a name to a function whose whole type is declared,
   let f : A -> B = fun ... , with no unknown in it. The
   declared result type reads the compiled answer back (a JS number does not say whether it was an Int or a Float). Each
   is compiled with the ones accepted before it and the type aliases among them, so it may call them; a function that
   names anything else (an earlier let that is not such a function, say) is not compiled, and the reason kept.

   How it is called: the program is rewritten so that the interpreter still evaluates each such definition to its own
   value (a closure), then hands that value to a builtin made at run time (Builtins.made_at_run_time), which keeps it
   and gives back another, the compiled function, which the rest of the program sees under the name. Applying that
   builtin carries the argument into JS, runs the compiled function under a budget, and carries the answer back by the
   declared result type. On any stop (an argument that will not carry, a hole in it say; an invalid operation; the
   budget; an answer that will not carry back) it falls back for that call alone: it gives back the application of the
   interpreter's own closure to the same argument, and the interpreter evaluates that as it would have anyway.
   Evaluation has no effects, so running a call again is safe.

   Not yet: a setting in the app, and anything but the evaluator tests' sweep (Test_HazelJit_Agree) and Test_HazelJit
   using it. A function's own probes, and the stepper's view of its calls, are the interpreter's no longer once it is
   compiled: one call is one builtin step. A type error inside a compiled function is not looked for here. */
open Js_of_ocaml;
module U = Js.Unsafe;

/* what the last rewrite compiled, by name, and why each other candidate was not */
let compiled_names: ref(list(string)) = ref([]);
let not_compiled: ref(list((string, string))) = ref([]);
/* calls made through a compiled function, and those that fell back to the interpreter, since the counts were reset */
let calls = ref(0);
let fallbacks = ref(0);
let fallback_reasons: Hashtbl.t(string, int) = Hashtbl.create(8);
let reset_counts = () => {
  calls := 0;
  fallbacks := 0;
  Hashtbl.reset(fallback_reasons);
};

/* a compiled call's budget, in the worker: the calls compiled code may make, and its time */
let fuel = 50_000_000;
let ms = 1000.;
let with_budget = (f: unit => 'a): 'a => {
  let r = Lazy.force(HazelJitValue.runtime);
  let off = () => ignore(U.meth_call(r, "unlimit", [||]));
  ignore(U.meth_call(r, "limit", [|U.inject(fuel), U.inject(ms)|]));
  switch (f()) {
  | v =>
    off();
    v;
  | exception exn =>
    off();
    raise(exn);
  };
};

let rec strip = (e: Exp.t): Exp.t =>
  switch (Exp.term_of(e)) {
  | Parens(e)
  | Asc(e, _) => strip(e)
  | _ => e
  };
let rec is_function = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Fun(_) => true
  | Parens(e)
  | Asc(e, _)
  | FixF(_, e, _) => is_function(e)
  | _ => false
  };
let rec name_of = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Var(x) => Some(x)
  | Parens(p)
  | Asc(p, _) => name_of(p)
  | _ => None
  };
/* a declared function type's parameter and result */
let rec arrow_parts = (t: Typ.t): option((Typ.t, Typ.t)) =>
  switch (Typ.term_of(t)) {
  | Parens(t) => arrow_parts(t)
  | Arrow(a, b) => Some((a, b))
  | _ => None
  };
let has_unknown = (t: Typ.t): bool => {
  let found = ref(false);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, t) => {
          switch (Typ.term_of(t)) {
          | Unknown(_) => found := true
          | _ => ()
          };
          cont(t);
        },
      t,
    );
  found^;
};
/* the declared type's parameter and result: an ascription on the name, or around the definition */
let declared_result = (p: Pat.t, def: Exp.t): option((Typ.t, Typ.t)) => {
  let rec of_pat = (p: Pat.t) =>
    switch (Pat.term_of(p)) {
    | Parens(p) => of_pat(p)
    | Asc(p, t) =>
      switch (arrow_parts(t)) {
      | Some(_) as r when !has_unknown(t) => r
      | _ => of_pat(p)
      }
    | _ => None
    };
  let rec of_exp = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | Parens(e) => of_exp(e)
    | Asc(e, t) =>
      switch (arrow_parts(t)) {
      | Some(_) as r when !has_unknown(t) => r
      | _ => of_exp(e)
      }
    | _ => None
    };
  switch (of_pat(p)) {
  | Some(_) as r => r
  | None => of_exp(def)
  };
};

type entry = {
  fn: U.any,
  ts: HazelJitValue.types,
  param: Typ.t,
  result: Typ.t,
  closure: ref(option(Exp.t)),
};

let fall_back = (why: string, entry: entry, arg: Exp.t): option(Exp.t) => {
  incr(fallbacks);
  Hashtbl.replace(
    fallback_reasons,
    why,
    1 + Option.value(~default=0, Hashtbl.find_opt(fallback_reasons, why)),
  );
  /* the argument cast to the declared parameter type, as the interpreter alone has it at the call (let inc : Int -> Int =
     ... in inc(?) is (? : Int) + 1; CI 11-17), unless it carries a cast already */
  let arg =
    switch (Exp.term_of(arg)) {
    | Asc(_) => arg
    | _ => Exp.fresh(Asc(arg, entry.param))
    };
  switch (entry.closure^) {
  | Some(c) => Some(Exp.fresh(Ap(Forward, c, arg)))
  | None => None
  };
};

/* the compiled function applied: into JS, run, back by the declared result type; or the interpreter's own closure */
let call = (entry: entry, arg: Exp.t): option(Exp.t) => {
  incr(calls);
  switch (HazelJitValue.to_js(arg)) {
  | exception _ =>
    fall_back("an argument that does not carry into JS", entry, arg)
  | js_arg =>
    switch (with_budget(() => U.fun_call(entry.fn, [|js_arg|]))) {
    | exception exn =>
      let m = HazelJitValue.message(exn);
      let why = String.length(m) > 40 ? String.sub(m, 0, 40) ++ "..." : m;
      fall_back("the compiled run stopped: " ++ why, entry, arg);
    | v =>
      switch (HazelJitValue.of_js(entry.ts, Some(entry.result), v)) {
      | exception _ =>
        fall_back("an answer that does not carry back", entry, arg)
      | e => Some(e)
      }
    }
  };
};

let made = ref(0);

/* The program rewritten for mixed mode, and the names compiled. The candidates are the lets at its top, and the members
   of its modules (each module's own, seen with what is in scope there); the chain compiled for each is the ones
   accepted before it, with the type aliases among them, ending in its name. */
let rewrite = (program: Exp.t): Exp.t => {
  compiled_names := [];
  not_compiled := [];
  Hashtbl.reset(Builtins.made_at_run_time);
  /* the accepted definitions in scope, newest first: a let's pattern and definition, or a type alias */
  let chain: ref(list(Exp.t => Exp.t)) = ref([]);
  let unit_for = (last: Exp.t): Exp.t =>
    List.fold_left((body, wrap) => wrap(body), last, chain^);
  /* a definition of a function with its whole type declared, compiled: the pattern and definition that replace it */
  let accept = (p: Pat.t, def: Exp.t): option((Pat.t, Exp.t)) =>
    switch (name_of(p), declared_result(p, def), is_function(strip(def))) {
    | (Some(f), Some((param, result)), true) =>
      let unit = Exp.fresh(Let(p, def, Exp.fresh(Var(f)))) |> unit_for;
      switch (HazelJit.compile_full(~env=Builtins.env_init, unit)) {
      | exception (HazelJit.Unsupported(why)) =>
        not_compiled := [(f, why), ...not_compiled^];
        None;
      | c =>
        switch (HazelJitValue.instantiate(c.src)) {
        | exception exn =>
          not_compiled :=
            [
              (f, "its JS did not run: " ++ HazelJitValue.message(exn)),
              ...not_compiled^,
            ];
          None;
        | fn =>
          incr(made);
          let n = string_of_int(made^);
          let entry = {
            fn,
            ts: HazelJitValue.types_of(c),
            param,
            result,
            closure: ref(None),
          };
          let compiled_name = "%jit:" ++ f ++ ":" ++ n;
          let keep_name = "%jit-keep:" ++ f ++ ":" ++ n;
          Hashtbl.replace(Builtins.made_at_run_time, compiled_name, arg =>
            call(entry, arg)
          );
          Hashtbl.replace(
            Builtins.made_at_run_time,
            keep_name,
            closure => {
              entry.closure := Some(closure);
              Some(Exp.fresh(BuiltinFun(compiled_name)));
            },
          );
          chain := [(b => Exp.fresh(Let(p, def, b))), ...chain^];
          compiled_names := [f, ...compiled_names^];
          /* (a bare name: the definition's own ascription, kept, cast the builtin's answers again, and a stuck answer
             showed the cast, (10 / 0 : Int), where the interpreter alone has none; CI 11-17) */
          Some((
            Pat.fresh(Var(f)),
            Exp.fresh(Ap(Forward, Exp.fresh(BuiltinFun(keep_name)), def)),
          ));
        }
      };
    | (Some(f), None, true) =>
      not_compiled :=
        [(f, "its whole type is not declared"), ...not_compiled^];
      None;
    | _ => None
    };
  let rec go = (e: Exp.t): Exp.t =>
    switch (Exp.term_of(e)) {
    | TyAlias(tp, t, body) =>
      chain := [(b => Exp.fresh(TyAlias(tp, t, b))), ...chain^];
      {
        ...e,
        term: TyAlias(tp, t, go(body)),
      };
    | Let(p, def, body) =>
      switch (accept(p, def)) {
      | Some((p', def')) => {
          ...e,
          term: Let(p', def', go(body)),
        }
      | None => {
          ...e,
          term: Let(p, into(def), go(body)),
        }
      }
    | _ => e
    }
  /* a module's members, each module seen with what is in scope where it is; the chain as it was after it */
  and into = (def: Exp.t): Exp.t =>
    switch (Exp.term_of(def)) {
    | Parens(d) => {
        ...def,
        term: Parens(into(d)),
      }
    | Asc(d, t) => {
        ...def,
        term: Asc(into(d), t),
      }
    | Module(items) =>
      let saved = chain^;
      let items = List.map(member, items);
      chain := saved;
      {
        ...def,
        term: Module(items),
      };
    | _ => def
    }
  and member = (item: Mod.t): Mod.t =>
    switch (item.term) {
    | ModLet(p, def) =>
      switch (accept(p, def)) {
      | Some((p', def')) => {
          ...item,
          term: ModLet(p', def'),
        }
      | None => {
          ...item,
          term: ModLet(p, into(def)),
        }
      }
    | ModType(tp, t) =>
      chain := [(b => Exp.fresh(TyAlias(tp, t, b))), ...chain^];
      item;
    | ModuleMod(mp, def) => {
        ...item,
        term: ModuleMod(mp, into(def)),
      }
    | _ => item
    };
  let out = go(program);
  compiled_names := List.rev(compiled_names^);
  not_compiled := List.rev(not_compiled^);
  out;
};
