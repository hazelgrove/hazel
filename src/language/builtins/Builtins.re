open BuiltinsUtil;

/* Built-in functions for Hazel.
   Update src/menhirParser/Lexer.mll when any new builtin is added */

let builtins =
  List.map(fn_builtin, BuiltinsBase.string_fns)
  @ List.map(fn_builtin, BuiltinsBase.pair_fns)
  @ List.map(of_atom_builtin, Atom.converter_builtins)
  @ List.map(of_atom_builtin, Operators.builtins)
  @ List.map(hazel_fn_builtin, BuiltinsList.builtins)
  @ List.map(hazel_fn_builtin, BuiltinsADT.builtins)
  @ List.map(fn_builtin, BuiltinsBase.numeric_fns)
  @ List.map(const_builtin, BuiltinsBase.numeric_constants)
  @ List.map(fn_builtin, BuiltinsTupleOperations.builtins);

let builtins: list(builtin) = [];

/* Check for accidental duplicates */
let _ = to_map(builtins);

let ctx_entries =
  List.map(ctx_entry_of_builtin, builtins)
  @ List.map(entry => Ctx.LivelitEntry(entry), Livelit.livelits)
  @ BuiltinsADT.constructor_entries;

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
