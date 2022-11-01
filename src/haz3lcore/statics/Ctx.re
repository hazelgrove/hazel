open Sexplib.Std;

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

let empty = VarMap.empty;

let extend = (entry: entry, ctx: t) => [entry, ...ctx];

let lookup_var = (ctx: t, x) =>
  List.find_map(
    entry =>
      switch (entry) {
      | VarEntry({name, typ, _}) =>
        if (name == x) {
          Some(typ);
        } else {
          None;
        }
      | TVarEntry(_) => None
      },
    ctx,
  );

let lookup_tvar = (ctx: t, x) =>
  List.find_map(
    entry =>
      switch (entry) {
      | TVarEntry({name, kind, _}) =>
        if (name == x) {
          Some(kind);
        } else {
          None;
        }
      | VarEntry(_) => None
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
  List.filter_map(entry =>
    switch (entry) {
    | VarEntry(_) => None
    | TVarEntry({name, kind, _}) =>
      switch (kind) {
      | Type(LabelSum(ts)) => Some((name, ts))
      | _ => None
      }
    }
  );

let tags = (ctx: t): list((string, Typ.t)) =>
  ctx |> adts |> BuiltinADTs.tags;

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown */
let rec join = (ctx, ty1: Typ.t, ty2: Typ.t): option(Typ.t) =>
  switch (ty1, ty2) {
  | (Var(n1), Var(n2)) =>
    //TODO(andrew)
    switch (lookup_tvar(ctx, n1), lookup_tvar(ctx, n2)) {
    | (Some(Type(ty1)), Some(Type(ty2))) => join(ctx, ty1, ty2)
    | _ when n1 == n2 => Some(ty1) //BuiltIn ADT case (deprecate?)
    | _ => None
    }
  | (Var(n1), ty2)
  | (ty2, Var(n1)) =>
    switch (lookup_tvar(ctx, n1)) {
    | Some(Type(ty1)) => join(ctx, ty1, ty2)
    | _ => None
    }
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(Typ.join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join(ctx, ty1_1, ty2_1), join(ctx, ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (List.map2(join(ctx), tys1, tys2) |> Util.OptUtil.sequence) {
      | None => None
      | Some(tys) => Some(Prod(tys))
      };
    }
  | (Prod(_), _) => None
  | (LabelSum(tys1), LabelSum(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (
        List.map2(
          (t1: Typ.tagged, t2: Typ.tagged) =>
            t1.tag == t2.tag ? join(ctx, t1.typ, t2.typ) : None,
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
    switch (join(ctx, ty1_1, ty2_1), join(ctx, ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join(ctx, ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all = (ctx, ts: list(Typ.t)): option(Typ.t) =>
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ctx, ty), acc),
    Some(Unknown(Internal)),
    ts,
  );
