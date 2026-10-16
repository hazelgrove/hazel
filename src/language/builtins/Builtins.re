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

/* The standard library's source reaches every other builtin as
   `$prim.name` and names it publicly (plans/stdlib.md §4). The tuple
   operations stay top-level: their typing rules (custom_statics) would
   not survive projection. */
let prim = "$prim";

let (top_level, prims) =
  List.partition(
    fun
    | Fn({custom_statics: Some(_), _})
    | Fn({name: "from_lvs", _}) => true
    | _ => false,
    builtins,
  );

let prim_entry: Ctx.entry =
  Ctx.VarEntry({
    name: prim,
    typ:
      Fresh.Typ.prod(
        List.map(
          b =>
            Fresh.Typ.tup_label(
              Fresh.Typ.label(name_of_builtin(b)),
              typ_of_builtin(b),
            ),
          prims,
        ),
      ),
    id: Id.invalid,
    custom_statics: None,
  });

let prim_value: Exp.t =
  Fresh.Exp.tuple(
    List.map(
      b => {
        let (name, imp) = imp_of_builtin(b);
        Fresh.Exp.tup_label(Fresh.Exp.label(name), imp);
      },
      prims,
    ),
  );

/* BuiltinFun(name)'s type, for statics: most names are not in the ctx */
let builtin_typ: string => option(Typ.t) = {
  let table = Hashtbl.create(List.length(builtins));
  List.iter(
    b => Hashtbl.replace(table, name_of_builtin(b), typ_of_builtin(b)),
    builtins,
  );
  Hashtbl.find_opt(table);
};

let ctx_entries =
  [prim_entry]
  @ List.map(ctx_entry_of_builtin, top_level)
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
  [(prim, prim_value), ...List.map(imp_of_builtin, top_level)]
  |> List.fold_left(Environment.extend, Environment.empty);

/* The Hazel-source standard library (Library.install): its context entries
   go on top of the builtins', its values on top of env_base */
let library_entries: ref(list(Ctx.entry)) = ref([]);
let library_names: ref(Util.Maps.StringMap.t(unit)) =
  ref(Util.Maps.StringMap.empty);
/* env_base with just these library names bound: each definition is
   evaluated when first needed, so a program pays for what it uses however
   large the library grows */
let library_env_for: ref(list(Var.t) => Environment.t(Exp.t)) =
  ref(_ => env_base);

/* one record per mode, so memos keyed on context identity (Statics.mk,
   DefStatics) hit until the library changes */
let ctx_by_mode: Hashtbl.t(option(Operators.mode), Ctx.t) =
  Hashtbl.create(5);

/* the library's modules, once evaluated, by name and first item's id
   (evaluation rebuilds a module value's node but keeps its items' ids): a
   module shown (a result stuck inside List.map, a sample) shows as its
   name */
let library_modules: ref(list((Var.t, Id.t))) = ref([]);

let install_library =
    (
      ~entries: list(Ctx.entry),
      ~names: list(Var.t),
      ~env_for: list(Var.t) => Environment.t(Exp.t),
    )
    : unit => {
  library_entries := entries;
  library_names :=
    List.fold_left(
      (m, x) => Util.Maps.StringMap.add(x, (), m),
      Util.Maps.StringMap.empty,
      names,
    );
  library_env_for := env_for;
  library_modules := [];
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

let is_library_name = (x: Var.t): bool =>
  Util.Maps.StringMap.mem(x, library_names^);

/* the names [d] mentions that [keep] accepts, once each: a superset of its
   free ones */
let mentioned = (~keep: Var.t => bool, d: Exp.t): list(Var.t) => {
  let seen = Hashtbl.create(16);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (e.term) {
          | Var(x) when keep(x) => Hashtbl.replace(seen, x, ())
          | _ => ()
          };
          cont(e);
        },
      d,
    );
  Hashtbl.fold((x, (), acc) => [x, ...acc], seen, []);
};

/* what [d] evaluates against: the builtins and the library names it
   mentions (a mention a binding shadows only costs its evaluation) */
let env_for = (d: Exp.t): Environment.t(Exp.t) =>
  library_env_for^(mentioned(~keep=is_library_name, d));

/* the whole library, every definition evaluated */
let env_init = (): Environment.t(Exp.t) =>
  library_env_for^(
    Util.Maps.StringMap.fold(
      (x, (), acc) => [x, ...acc],
      library_names^,
      [],
    ),
  );

/* whether env_init binds [name], without evaluating the library */
let predefined = (name: Var.t): bool =>
  Environment.lookup(env_base, name) != None || is_library_name(name);

let library_module_name = (v: Exp.t): option(Var.t) =>
  switch (v.term) {
  | Module([item, ..._]) =>
    let id = Mod.rep_id(item);
    List.find_map(
      ((x, first)) => Id.equal(first, id) ? Some(x) : None,
      library_modules^,
    );
  | _ => None
  };

/* without the library: what the library's own source is checked against */
let ctx_base: option(Operators.mode) => Ctx.t = {
  let by_mode = Hashtbl.create(5);
  use_mode =>
    switch (Hashtbl.find_opt(by_mode, use_mode)) {
    | Some(ctx) => ctx
    | None =>
      let ctx = Ctx.of_entries(~use_mode, ctx_entries);
      Hashtbl.replace(by_mode, use_mode, ctx);
      ctx;
    };
};
