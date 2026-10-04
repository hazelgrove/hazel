open BuiltinsUtil;

/* Built-in functions for Hazel. */

let builtins =
  List.map(fn_builtin, BuiltinsBase.misc_fns)
  @ List.map(fn_builtin, BuiltinsBase.string_fns)
  @ List.map(fn_builtin, BuiltinsBase.pair_fns)
  @ List.map(of_atom_builtin, Atom.converter_builtins)
  @ List.map(fn_builtin, BuiltinsADT.ord_builtins)
  @ List.map(of_atom_builtin, Operators.builtins)
  @ List.map(hazel_fn_builtin, BuiltinsList.builtins)
  @ List.map(hazel_fn_builtin, BuiltinsADT.builtins)
  @ List.map(fn_builtin, BuiltinsBase.numeric_fns)
  @ List.map(const_builtin, BuiltinsBase.numeric_constants)
  @ List.map(const_builtin, BuiltinsADT.module_builtins)
  @ List.map(const_builtin, BuiltinsADT.monad_ops)
  @ List.map(fn_builtin, BuiltinsTupleOperations.builtins)
  @ List.map(fn_builtin, BuiltinsColor.builtins)
  @ [
    fn_builtin(BuiltinsADT.splice_value),
    fn_builtin(BuiltinsADT.fill_quote),
  ];

let builtins =
  List.sort(
    (a: builtin, b: builtin) =>
      String.compare(name_of_builtin(b), name_of_builtin(a)),
    builtins,
  );

/* Check for accidental duplicates */
let _ = to_map(builtins);

let ctx_entries =
  List.map(ctx_entry_of_builtin, builtins)
  @ List.map(entry => Ctx.LivelitEntry(entry), Livelit.livelits)
  @ BuiltinsADT.constructor_entries
  /* Product types, so they get no constructors -- just names the config
     slide can annotate with. */
  @ List.map(
      ((name, typ)) => BuiltinsADT.create_type_alias(name, typ),
      BuiltinsColorScheme.type_aliases,
    );

/* Every statics ctx ends in this list; index it once (see Ctx.tail_index). */
let () = Ctx.index_tail(ctx_entries);

let ctx_init: option(Operators.mode) => Ctx.t =
  use_mode => {
    use_mode,
    entries: ctx_entries,
  };

let forms_init: forms = List.filter_map(form_of_builtin, builtins);

let env_init: Environment.t(Exp.t) =
  builtins
  |> List.map(imp_of_builtin)
  |> List.fold_left(Environment.extend, Environment.empty);

let closure_env: Environment.t(Exp.t) = env_init;

/* The names a term refers to that env_init binds: every Var in it, bound
   in the term or not, so it may include a few too many, never too few. */
let builtin_names_in = (e: Exp.t): list(Var.t) => {
  let names = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Var(x) when Environment.lookup(env_init, x) != None =>
            names := [x, ...names^]
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  List.sort_uniq(compare, names^);
};

/* env_init cut down to what a closed term needs: the builtins it names,
   and theirs in turn, in env_init's order, so each lookup finds what it
   found in env_init. A Macro use's expansion is closed over the builtin
   environment for hygiene; the whole of it is ~116 KB marshaled to the
   eval worker, and a body names a handful of it. Memoized on the names. */
let env_for_cache: Hashtbl.t(list(Var.t), Environment.t(Exp.t)) =
  Hashtbl.create(8);
let env_for = (e: Exp.t): Environment.t(Exp.t) => {
  let direct = builtin_names_in(e);
  switch (Hashtbl.find_opt(env_for_cache, direct)) {
  | Some(env) => env
  | None =>
    let rec close = (seen, todo) =>
      switch (todo) {
      | [] => seen
      | [x, ...todo] when List.mem(x, seen) => close(seen, todo)
      | [x, ...todo] =>
        let deps =
          switch (Environment.lookup(env_init, x)) {
          | Some(v) => builtin_names_in(v)
          | None => []
          };
        close([x, ...seen], deps @ todo);
      };
    let needed = close([], direct);
    let env =
      Environment.to_list(env_init)
      |> List.filter(((x, _)) => List.mem(x, needed))
      |> Environment.of_list;
    Hashtbl.replace(env_for_cache, direct, env);
    env;
  };
};
