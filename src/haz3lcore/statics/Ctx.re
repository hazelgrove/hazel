open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Token.t,
  id: Id.t,
  typ: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | TVarEntry({
      name: Token.t,
      id: Id.t,
      kind: Kind.t,
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_item = {
  id: Id.t,
  mode: Typ.mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co_entry = list(co_item);

[@deriving (show({with_path: false}), sexp, yojson)]
type co = VarMap.t_(co_entry);

let lookup_tvar = (ctx: t, t: Token.t) =>
  List.find_map(
    fun
    | TVarEntry({name, kind, _}) when name == t => Some(kind)
    | _ => None,
    ctx,
  );

let rec lookup_tvar_idx = (~i=0, ctx: t, x: Token.t) => {
  switch (ctx) {
  | [] => None
  | [TVarEntry({name, _}), ..._] when Token.compare(name, x) == 0 =>
    Some(i)
  | [TVarEntry(_), ...ctx] => lookup_tvar_idx(ctx, x, ~i=i + 1)
  | [_entry, ...ctx] => lookup_tvar_idx(ctx, x, ~i)
  };
};

let is_tvar = (ctx: t, name: Token.t) =>
  // switch (
  //   List.assoc_opt(name, BuiltinADTs.adts),
  //   Ctx.lookup_tvar(ctx, name),
  // ) {
  // | (Some(_), _)
  // | (_, Some(_)) => true
  // | _ => false
  // };
  switch (lookup_tvar(ctx, name)) {
  | Some(_) => true
  | None => false
  };

let get_id =
  fun
  | VarEntry({id, _}) => id
  | TVarEntry({id, _}) => id;
let empty: t = VarMap.empty;

let extend = (entry: entry, ctx: t): t => [entry, ...ctx];

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    entry =>
      switch (entry) {
      | VarEntry(var) =>
        if (var.name == name) {
          Some(var);
        } else {
          None;
        }
      | TVarEntry(_) => None
      },
    ctx,
  );

let subtract_typ = (ctx: t, free: co): co =>
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_var(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        Util.ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
      ),
    );
  };
};

/* Note: this currently shadows in the case of duplicates */
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, typ_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));

// let adts: t => list((string, list(Typ.tagged))) =
//   List.filter_map(
//     fun
//     | VarEntry(_) => None
//     | TVarEntry({name, kind, _}) =>
//       switch (kind) {
//       | Singleton(Rec({item: LabelSum(ts), _})) => Some((name, ts))
//       | Singleton(LabelSum(ts)) => Some((name, ts))
//       | _ => None
//       },
//   );

// let adt_tag_and_typ =
//     (name: Token.t, {tag, typ}: Typ.tagged): (Token.t, Typ.t) => (
//   tag,
//   switch (typ) {
//   | Prod([]) => Var(name)
//   | _ => Arrow(typ, Var(name))
//   },
// );

// let get_tags = (adts: list(Typ.adt)): list((Token.t, Typ.t)) =>
//   adts
//   |> List.map(((name, adt)) => List.map(adt_tag_and_typ(name), adt))
//   |> List.flatten;

// let builtin_adt_tags = get_tags(BuiltinADTs.adts);

// // Check builtin type names are unique
// assert(Util.ListUtil.are_duplicates(List.map(fst, BuiltinADTs.adts)));
// // Check builtin tag names are unique
// assert(Util.ListUtil.are_duplicates(List.map(fst, builtin_adt_tags)));

let lookup_tag = (_ctx, _name) => None;
// let lookup_tag = (ctx, name) =>
// switch (List.assoc_opt(name, builtin_adt_tags)) {
// | Some(typ) => Some(typ)
// | None => List.assoc_opt(name, ctx |> adts |> get_tags)
// };
