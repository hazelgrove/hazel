open BuiltinsUtil;

/* Built-in functions for Hazel. */

let builtins =
  List.map(fn_builtin, BuiltinsBase.misc_fns)
  @ List.map(fn_builtin, BuiltinsBase.string_fns)
  @ List.map(fn_builtin, BuiltinsBase.pair_fns)
  @ List.map(of_atom_builtin, Atom.converter_builtins)
  @ List.map(fn_builtin, BuiltinsADT.ord_builtins)
  @ List.map(of_atom_builtin, Operators.builtins)
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

let forms_init: forms = List.filter_map(form_of_builtin, builtins);

let env_base: Environment.t(Exp.t) =
  builtins
  |> List.map(imp_of_builtin)
  |> List.fold_left(Environment.extend, Environment.empty);

/* The Hazel-source standard library (Library.install): its context entries
   go on top of the builtins', its evaluated bindings on top of env_base */
let library_entries: ref(list(Ctx.entry)) = ref([]);
let library_env: ref(Environment.t(Exp.t)) = ref(env_base);

/* one record per mode, so memos keyed on context identity (Statics.mk,
   DefStatics) hit until the library changes */
let ctx_by_mode: Hashtbl.t(option(Operators.mode), Ctx.t) =
  Hashtbl.create(5);

let install_library =
    (~entries: list(Ctx.entry), ~env: Environment.t(Exp.t)): unit => {
  library_entries := entries;
  library_env := env;
  Hashtbl.reset(ctx_by_mode);
};

let ctx_init = (use_mode: option(Operators.mode)): Ctx.t =>
  switch (Hashtbl.find_opt(ctx_by_mode, use_mode)) {
  | Some(ctx) => ctx
  | None =>
    let ctx = Ctx.of_entries(~use_mode, library_entries^ @ ctx_entries);
    Hashtbl.replace(ctx_by_mode, use_mode, ctx);
    ctx;
  };

let env_init = (): Environment.t(Exp.t) => library_env^;
