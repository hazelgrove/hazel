open BuiltinsUtil;

/* Built-in functions for Hazel. */

let builtins =
  List.map(~f=fn_builtin, BuiltinsBase.misc_fns)
  @ List.map(~f=fn_builtin, BuiltinsBase.string_fns)
  @ List.map(~f=fn_builtin, BuiltinsBase.pair_fns)
  @ List.map(~f=of_atom_builtin, Atom.converter_builtins)
  @ List.map(~f=fn_builtin, BuiltinsADT.ord_builtins)
  @ List.map(~f=of_atom_builtin, Operators.builtins)
  @ List.map(~f=hazel_fn_builtin, BuiltinsList.builtins)
  @ List.map(~f=hazel_fn_builtin, BuiltinsADT.builtins)
  @ List.map(~f=fn_builtin, BuiltinsBase.numeric_fns)
  @ List.map(~f=const_builtin, BuiltinsBase.numeric_constants)
  @ List.map(~f=fn_builtin, BuiltinsTupleOperations.builtins)
  @ List.map(~f=fn_builtin, BuiltinsColor.builtins);

let builtins =
  List.sort(
    ~compare=
      (a: builtin, b: builtin) =>
        String.compare(name_of_builtin(b), name_of_builtin(a)),
    builtins,
  );

/* Check for accidental duplicates */
let _ = to_map(builtins);

let ctx_entries =
  List.map(~f=ctx_entry_of_builtin, builtins)
  @ List.map(~f=entry => Ctx.LivelitEntry(entry), Livelit.livelits)
  @ BuiltinsADT.constructor_entries
  /* Product types, so they get no constructors -- just names the config
     slide can annotate with. */
  @ List.map(
      ~f=((name, typ)) => BuiltinsADT.create_type_alias(name, typ),
      BuiltinsColorScheme.type_aliases,
    );

/* built once: of_entries is O(n) (a List.length) and ctx_init runs per
   statics run. one record per mode, so memos keyed on context identity
   (Statics.mk) can hit */
let ctx_init_base: Ctx.t = Ctx.of_entries(~use_mode=None, ctx_entries);
let ctx_init: option(Operators.mode) => Ctx.t = {
  let by_mode = Stdlib.Hashtbl.create(5);
  use_mode =>
    switch (Stdlib.Hashtbl.find_opt(by_mode, use_mode)) {
    | Some(ctx) => ctx
    | None =>
      let ctx = Ctx.set_use_mode(ctx_init_base, use_mode);
      Stdlib.Hashtbl.replace(by_mode, use_mode, ctx);
      ctx;
    };
};

let forms_init: forms = List.filter_map(~f=form_of_builtin, builtins);

let env_init: Environment.t(Exp.t) =
  builtins
  |> List.map(~f=imp_of_builtin)
  |> List.fold_left(~f=Environment.extend, ~init=Environment.empty);

let closure_env: Environment.t(Exp.t) = env_init;
