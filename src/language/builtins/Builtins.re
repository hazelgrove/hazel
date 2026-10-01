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
  @ List.map(fn_builtin, BuiltinsTupleOperations.builtins)
  @ List.map(fn_builtin, BuiltinsColor.builtins);

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

/* built once: of_entries is O(n) (a List.length) and ctx_init runs per
   statics run. one record per mode, so memos keyed on context identity
   (Statics.mk) can hit */
let ctx_init_base: Ctx.t = Ctx.of_entries(~use_mode=None, ctx_entries);
let ctx_init: option(Operators.mode) => Ctx.t = {
  let by_mode = Hashtbl.create(5);
  use_mode =>
    switch (Hashtbl.find_opt(by_mode, use_mode)) {
    | Some(ctx) => ctx
    | None =>
      let ctx = Ctx.set_use_mode(ctx_init_base, use_mode);
      Hashtbl.replace(by_mode, use_mode, ctx);
      ctx;
    };
};

let forms_init: forms = List.filter_map(form_of_builtin, builtins);

let env_init: Environment.t(Exp.t) =
  builtins
  |> List.map(imp_of_builtin)
  |> List.fold_left(Environment.extend, Environment.empty);

let closure_env: Environment.t(Exp.t) = env_init;
