open BuiltinsUtil;

/* Built-in functions for Hazel.
   Update src/menhirParser/Lexer.mll when any new builtin is added */

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
  @ List.map(fn_builtin, BuiltinsTupleOperations.builtins);

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

let closure_env: Environment.t(Exp.t) = env_init;

/* Ids minted for builtin implementation terms (e.g. fold_left's internal
 * recursion and application nodes). When user functions are applied inside
 * a builtin (a fold/map callback), these ids appear as call-stack frame
 * ids, even though they are never present in any user program's statics.
 * Substitution and fix-unrolling copy nodes without re-minting ids, so the
 * init-time set covers every frame id evaluation can produce from them. */
let internal_ids: Lazy.t(Id.Set.t) =
  lazy(
    Environment.to_list(env_init)
    |> List.fold_left(
         (acc, (_, imp)) => {
           let acc = ref(acc);
           let _ =
             Exp.map_term(
               ~f_exp=
                 (continue, e) => {
                   acc := Id.Set.add(Exp.rep_id(e), acc^);
                   continue(e);
                 },
               imp,
             );
           acc^;
         },
         Id.Set.empty,
       )
  );

let is_internal_id = (id: Id.t): bool =>
  Id.Set.mem(id, Lazy.force(internal_ids));
