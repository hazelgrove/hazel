open Sexplib.Std;
open Util.OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry({
      name: Token.t,
      id: Id.t,
      typ: Typ.t,
    })
  | TVarEntry({
      name: Token.t,
      id: Id.t,
      kind: Kind.t,
    });

let get_id = (entry: entry) =>
  switch (entry) {
  | VarEntry({id, _}) => id
  | TVarEntry({id, _}) => id
  };

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

let empty: t = VarMap.empty;

let extend = (entry: entry, ctx: t): t => [entry, ...ctx];

let lookup_var = (ctx: t, t: Token.t) =>
  List.find_map(
    fun
    | VarEntry({name, typ, _}) when name == t => Some(typ)
    | _ => None,
    ctx,
  );

let lookup_tvar = (ctx: t, t: Token.t) =>
  List.find_map(
    fun
    | TVarEntry({name, kind, _}) when name == t => Some(kind)
    | _ => None,
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

//TODO(andrew): is this correct in the case of duplicates?
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
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));

let adts: t => list((string, list(Typ.tagged))) =
  List.filter_map(
    fun
    | VarEntry(_) => None
    | TVarEntry({name, kind, _}) =>
      switch (kind) {
      | Singleton(Rec(_, LabelSum(ts))) => Some((name, ts))
      | Singleton(LabelSum(ts)) => Some((name, ts))
      | _ => None
      },
  );

let adt_tag_and_typ =
    (name: Token.t, {tag, typ}: Typ.tagged): (Token.t, Typ.t) => (
  tag,
  switch (typ) {
  | Prod([]) => Var(name)
  | _ => Arrow(typ, Var(name))
  },
);

let get_tags = (adts: list(Typ.adt)): list((Token.t, Typ.t)) =>
  adts
  |> List.map(((name, adt)) => List.map(adt_tag_and_typ(name), adt))
  |> List.flatten;

let builtin_adt_tags = get_tags(BuiltinADTs.adts);

// Check builtin type names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, BuiltinADTs.adts)));
// Check builtin tag names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, builtin_adt_tags)));

let lookup_tag = (ctx, name) =>
  switch (List.assoc_opt(name, builtin_adt_tags)) {
  | Some(typ) => Some(typ)
  | None => List.assoc_opt(name, ctx |> adts |> get_tags)
  };

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join =
        (~d=[], ~resolve=false, ctx, ty1: Typ.t, ty2: Typ.t): option(Typ.t) => {
  let join' = join(~d, ctx);
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(Typ.join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Rec(x1, t1), Rec(x2, t2)) => join(~d=[(x1, x2), ...d], ctx, t1, t2)
  | (Rec(_), _) => None
  | (Var(n1), Var(n2)) =>
    if (Typ.type_var_eq(d, n1, n2)) {
      Some(Var(n1));
    } else {
      let* Singleton(ty1) = lookup_tvar(ctx, n1);
      let* Singleton(ty2) = lookup_tvar(ctx, n2);
      let+ ty_join = join'(ty1, ty2);
      resolve ? ty_join : Var(n1);
    }
  | (Var(name), ty)
  | (ty, Var(name)) =>
    let* Singleton(ty_name) = lookup_tvar(ctx, name);
    let+ ty_join = join'(ty_name, ty);
    resolve ? ty_join : Var(name);
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join'(ty1_1, ty2_1), join'(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (List.map2(join', tys1, tys2) |> Util.OptUtil.sequence) {
      | None => None
      | Some(tys) => Some(Prod(tys))
      };
    }
  | (Prod(_), _) => None
  | (LabelSum(tys1), LabelSum(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      let (tys1, tys2) = (Typ.sort_tagged(tys1), Typ.sort_tagged(tys2));
      switch (
        List.map2(
          (t1: Typ.tagged, t2: Typ.tagged) =>
            t1.tag == t2.tag ? join'(t1.typ, t2.typ) : None,
          tys1,
          tys2,
        )
        |> Util.OptUtil.sequence
      ) {
      | None => None
      | Some(tys) =>
        Some(
          LabelSum(
            List.map2(
              (t1: Typ.tagged, typ) => Typ.{tag: t1.tag, typ},
              tys1,
              tys,
            ),
          ),
        )
      };
    }
  | (LabelSum(_), _) => None
  | (Sum(ty1_1, ty1_2), Sum(ty2_1, ty2_2)) =>
    switch (join'(ty1_1, ty2_1), join'(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join'(ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };
};

let join_all = (ctx, ts: list(Typ.t)): option(Typ.t) =>
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ctx, ty), acc),
    Some(Unknown(Internal)),
    ts,
  );
