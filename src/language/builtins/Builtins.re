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

let ctx_init: option(Operators.mode) => Ctx.t =
  use_mode => {
    use_mode,
    entries: ctx_entries,
  };

let forms_init: forms = List.filter_map(~f=form_of_builtin, builtins);

/* (^ticket(766)) builtins made at run time: mixed mode's compiled functions (HazelJitMixed), each a name and what it
   does with its argument, looked up when forms_init has no such name. Empty unless mixed mode rewrote the program. */
let made_at_run_time: Stdlib.Hashtbl.t(string, DHExp.t => option(DHExp.t)) =
  Stdlib.Hashtbl.create(16);

let env_init: Environment.t(Exp.t) =
  builtins
  |> List.map(~f=imp_of_builtin)
  |> List.fold_left(~f=Environment.extend, ~init=Environment.empty);

let closure_env: Environment.t(Exp.t) = env_init;
