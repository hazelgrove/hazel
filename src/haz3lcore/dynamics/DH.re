open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type consistency =
  | Consistent
  | Inconsistent(MetaVar.t, HoleInstanceId.t);

module rec DHExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    // TODO: Add IDs
    /* TODO: ADD:
        UnOp
        TyAlias [and ignore]
        Parens
       */
    // TODO: Work out how to reconcile the invalids
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(DHExp.t))
    | StaticErrorHole(Id.t, t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t) // TODO: Remove, use info_map      /// --------------------------------------------------------------------------------------------------------
    | InvalidOperation(t, InvalidOperationError.t) // Warning will robinson
    | FailedCast(t, Typ.t, Typ.t) // TODO: Add to TermBase
    | Closure([@show.opaque] ClosureEnvironment.t, t) // > UEXP
    | Filter(DHFilter.t, t) // DONE [UEXP TO BE CHANGED]
    | Var(Var.t) // DONE [ALREADY]
    | Seq(t, t) // DONE [ALREADY]
    | Let(DHPat.t, t, t) // DONE [ALREADY]
    | FixF(DHPat.t, Typ.t, t) // TODO: surface fix
    | Fun(
        DHPat.t,
        Typ.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      ) // TODO: Use info_map for Typ.t
    | Ap(TermBase.UExp.ap_direction, t, t) // TODO: Add reverse application
    | ApBuiltin(string, t) // DONE [TO ADD TO UEXP] TODO: Add a loooong comment here
    | BuiltinFun(string) // DONE [TO ADD TO UEXP]
    | Test(KeywordID.t, t) // TODO: ! ID
    | Bool(bool) // DONE
    | Int(int) // DONE
    | Float(float) // DONE
    | String(string) // DONE
    | BinOp(TermBase.UExp.op_bin, t, t) // DONE
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t)) // TODO: afaict the first three arguments here are never used? 3rd one might be info_map
    | Cons(t, t) // DONE [ALREADY]
    | ListConcat(t, t) // DONE [ALREADY]
    | Tuple(list(t)) // DONE [ALREADY]
    | Constructor(string) // DONE [ALREADY]
    | Match(consistency, t, list((DHPat.t, t)))
    | Cast(t, Typ.t, Typ.t) // TODO: Add to uexp or remove
    | If(consistency, t, t, t)
  and t; // TODO: CONSISTENCY? from statics

  let rep_id: t => Id.t;
  let term_of: t => term;
  let fast_copy: (Id.t, t) => t;
  // All children of term must have expression-unique ids.
  let fresh: term => t;
  let mk: (list(Id.t), term) => t;
  let unwrap: t => (term, term => t);

  let fresh_cast: (t, Typ.t, Typ.t) => t;

  let apply_casts: (t, list((Typ.t, Typ.t))) => t;
  let strip_casts: t => t;

  let repair_ids: t => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    /* Hole types */
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(DHExp.t))
    | StaticErrorHole(Id.t, t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    /* Generalized closures */
    | Closure(ClosureEnvironment.t, t)
    | Filter(DHFilter.t, t)
    /* Other expressions forms */
    | Var(Var.t)
    | Seq(t, t)
    | Let(DHPat.t, t, t)
    | FixF(DHPat.t, Typ.t, t)
    | Fun(
        DHPat.t,
        Typ.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | Ap(TermBase.UExp.ap_direction, t, t)
    | ApBuiltin(string, t)
    | BuiltinFun(string)
    | Test(KeywordID.t, t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | BinOp(TermBase.UExp.op_bin, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
    | Cons(t, t)
    | ListConcat(t, t)
    | Tuple(list(t))
    | Constructor(string)
    | Match(consistency, t, list((DHPat.t, t)))
    | Cast(t, Typ.t, Typ.t)
    | If(consistency, t, t, t)
  and t = {
    /* invariant: nonempty, TODO: what happens to later ids in DHExp */
    ids: list(Id.t),
    /*TODO: Verify: Always false in UExp, if an expression has been copied as part of
      evaluation (e.g. fun x -> x + x), then this will be flagged as true. This means
      the ids should be replaced after evaluation. */
    copied: bool,
    term,
  };

  let rep_id = ({ids, _}) => List.hd(ids);
  let term_of = ({term, _}) => term;
  let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};
  // All children of term must have expression-unique ids.
  let fresh = term => {
    {ids: [Id.mk()], copied: false, term};
  };
  let unwrap = ({ids, term, copied}) => (
    term,
    term => {ids, term, copied},
  );

  let mk = (ids, term) => {
    {ids, copied: true, term};
  };

  // All children of d must have expression-unique ids.
  let fresh_cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
    if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
      d;
    } else {
      fresh(Cast(d, t1, t2));
    };

  let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
    List.fold_left((d, (ty1, ty2)) => fresh_cast(d, ty1, ty2), d, casts);

  let rec repair_ids = (require: bool, d: t) => {
    let child_require = require || d.copied;
    let repair_ids = repair_ids(child_require);
    let term = term_of(d);
    let rewrap = term => {
      ids:
        child_require
          ? {
            let id = Id.mk();
            [id];
          }
          : d.ids,
      copied: false,
      term,
    };
    (
      switch (term) {
      | EmptyHole
      | FreeVar(_)
      | Invalid(_)
      | Var(_)
      | BuiltinFun(_)
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | Constructor(_) => term
      | StaticErrorHole(static_id, d1) =>
        StaticErrorHole(static_id, repair_ids(d1))
      | InvalidOperation(d1, x) => InvalidOperation(repair_ids(d1), x)
      | FailedCast(d1, t1, t2) => FailedCast(repair_ids(d1), t1, t2)
      | Closure(env, d1) => Closure(env, repair_ids(d1))
      | Filter(flt, d1) => Filter(flt, repair_ids(d1))
      | Seq(d1, d2) => Seq(repair_ids(d1), repair_ids(d2))
      | Let(dp, d1, d2) => Let(dp, repair_ids(d1), repair_ids(d2))
      | FixF(f, t, d1) => FixF(f, t, repair_ids(d1))
      | Fun(dp, t, d1, env, f) => Fun(dp, t, repair_ids(d1), env, f)
      | Ap(dir, d1, d2) => Ap(dir, repair_ids(d1), repair_ids(d2))
      | ApBuiltin(s, d1) => ApBuiltin(s, repair_ids(d1))
      | Test(id, d1) => Test(id, repair_ids(d1))
      | BinOp(op, d1, d2) => BinOp(op, repair_ids(d1), repair_ids(d2))
      | ListLit(mv, mvi, t, ds) =>
        ListLit(mv, mvi, t, List.map(repair_ids, ds))
      | Cons(d1, d2) => Cons(repair_ids(d1), repair_ids(d2))
      | ListConcat(d1, d2) => ListConcat(repair_ids(d1), repair_ids(d2))
      | Tuple(ds) => Tuple(List.map(repair_ids, ds))
      | MultiHole(ds) => MultiHole(List.map(repair_ids, ds))
      | Match(c, d1, rls) =>
        Match(
          c,
          repair_ids(d1),
          List.map(((p, d)) => (p, repair_ids(d)), rls),
        )
      | Cast(d1, t1, t2) => Cast(repair_ids(d1), t1, t2)
      | If(c, d1, d2, d3) =>
        If(c, repair_ids(d1), repair_ids(d2), repair_ids(d3))
      }
    )
    |> rewrap;
  };

  let repair_ids = repair_ids(false);

  // Also strips static error holes - kinda like unelaboration
  let rec strip_casts = d => {
    let (term, rewrap) = unwrap(d);
    switch (term) {
    | Closure(ei, d) => Closure(ei, strip_casts(d)) |> rewrap
    | Cast(d, _, _) => strip_casts(d)
    | FailedCast(d, _, _) => strip_casts(d)
    | Tuple(ds) => Tuple(ds |> List.map(strip_casts)) |> rewrap
    | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2)) |> rewrap
    | ListConcat(d1, d2) =>
      ListConcat(strip_casts(d1), strip_casts(d2)) |> rewrap
    | ListLit(a, b, c, ds) =>
      ListLit(a, b, c, List.map(strip_casts, ds)) |> rewrap
    | MultiHole(ds) => MultiHole(List.map(strip_casts, ds)) |> rewrap
    | StaticErrorHole(_, d) => strip_casts(d)
    | Seq(a, b) => Seq(strip_casts(a), strip_casts(b)) |> rewrap
    | Filter(f, b) =>
      Filter(DHFilter.strip_casts(f), strip_casts(b)) |> rewrap
    | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c)) |> rewrap
    | FixF(a, b, c) => FixF(a, b, strip_casts(c)) |> rewrap
    | Fun(a, b, c, e, d) => Fun(a, b, strip_casts(c), e, d) |> rewrap
    | Ap(dir, a, b) => Ap(dir, strip_casts(a), strip_casts(b)) |> rewrap
    | Test(id, a) => Test(id, strip_casts(a)) |> rewrap
    | ApBuiltin(fn, args) => ApBuiltin(fn, strip_casts(args)) |> rewrap
    | BuiltinFun(fn) => BuiltinFun(fn) |> rewrap
    | BinOp(a, b, c) => BinOp(a, strip_casts(b), strip_casts(c)) |> rewrap
    | Match(c, a, rules) =>
      Match(
        c,
        strip_casts(a),
        List.map(((k, v)) => (k, strip_casts(v)), rules),
      )
      |> rewrap
    | EmptyHole as d
    | FreeVar(_) as d
    | Invalid(_) as d
    | Var(_) as d
    | Bool(_) as d
    | Int(_) as d
    | Float(_) as d
    | String(_) as d
    | Constructor(_) as d
    | InvalidOperation(_) as d => d |> rewrap
    | If(consistent, c, d1, d2) =>
      If(consistent, strip_casts(c), strip_casts(d1), strip_casts(d2))
      |> rewrap
    };
  };

  let rec fast_equal = ({term: d1, _}, {term: d2, _}): bool => {
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (Var(_), _)
    /* TODO: Not sure if this is right... */
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (Constructor(_), _) => d1 == d2
    | (String(s1), String(s2)) => String.equal(s1, s2)
    | (String(_), _) => false

    /* Non-hole forms: recurse */
    | (Test(id1, d1), Test(id2, d2)) => id1 == id2 && fast_equal(d1, d2)
    | (Seq(d11, d21), Seq(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Filter(f1, d1), Filter(f2, d2)) =>
      DHFilter.fast_equal(f1, f2) && fast_equal(d1, d2)
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1, None, s1), Fun(dp2, ty2, d2, None, s2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2) && s1 == s2
    | (
        Fun(dp1, ty1, d1, Some(env1), s1),
        Fun(dp2, ty2, d2, Some(env2), s2),
      ) =>
      dp1 == dp2
      && ty1 == ty2
      && fast_equal(d1, d2)
      && ClosureEnvironment.id_equal(env1, env2)
      && s1 == s2
    | (Ap(dir1, d11, d21), Ap(dir2, d12, d22)) =>
      dir1 == dir2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Cons(d11, d21), Cons(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (ListConcat(d11, d21), ListConcat(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Tuple(ds1), Tuple(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (ApBuiltin(f1, d1), ApBuiltin(f2, d2)) => f1 == f2 && d1 == d2
    | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
    | (ListLit(_, _, _, ds1), ListLit(_, _, _, ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (BinOp(op1, d11, d21), BinOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (Match(c1, s1, rs1), Match(c2, s2, rs2)) =>
      c1 == c2
      && fast_equal(s1, s2)
      && List.length(rs2) == List.length(rs2)
      && List.for_all2(
           ((k1, v1), (k2, v2)) => k1 == k2 && fast_equal(v1, v2),
           rs1,
           rs2,
         )
    | (If(c1, d11, d12, d13), If(c2, d21, d22, d23)) =>
      c1 == c2
      && fast_equal(d11, d21)
      && fast_equal(d12, d22)
      && fast_equal(d13, d23)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Seq(_), _)
    | (Filter(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Test(_), _)
    | (Ap(_), _)
    | (ApBuiltin(_), _)
    | (BuiltinFun(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (BinOp(_), _)
    | (Cast(_), _)
    | (FailedCast(_), _)
    | (InvalidOperation(_), _)
    | (If(_), _)
    | (Match(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (EmptyHole, EmptyHole) => true
    | (MultiHole(ds1), MultiHole(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (StaticErrorHole(sid1, d1), StaticErrorHole(sid2, d2)) =>
      sid1 == sid2 && d1 == d2
    | (FreeVar(u1, i1, x1), FreeVar(u2, i2, x2)) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (Invalid(text1), Invalid(text2)) => text1 == text2
    | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
      ClosureEnvironment.id_equal(sigma1, sigma2) && fast_equal(d1, d2)
    | (EmptyHole, _)
    | (MultiHole(_), _)
    | (StaticErrorHole(_), _)
    | (FreeVar(_), _)
    | (Invalid(_), _)
    | (Closure(_), _) => false
    };
  };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(DHExp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(DHExp.t);
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (EnvironmentId.t, Environment.t) => t;

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, DHExp.t));

  let of_environment: Environment.t => t;

  let id_equal: (t, t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(DHExp.t);
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, DHExp.t)) => t;
  let extend_keep_id: (t, (Var.t, DHExp.t)) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, DHExp.t)) => DHExp.t, t) => t;
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;
  let filter: (((Var.t, DHExp.t)) => bool, t) => t;
  let filter_keep_id: (((Var.t, DHExp.t)) => bool, t) => t;
  let fold: (((Var.t, DHExp.t), 'b) => 'b, 'b, t) => 'b;

  let without_keys: (list(Var.t), t) => t;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let wrap: (EnvironmentId.t, Environment.t) => t;

    let id_of: t => EnvironmentId.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (EnvironmentId.t, Environment.t);

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
    let (sexp_of_t, t_of_sexp) =
      StructureShareSexp.structure_share_here(id_of, sexp_of_t, t_of_sexp);
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = map => {
    let ei = Id.mk();
    wrap(ei, map);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(EnvironmentId.invalid, Environment.empty);

  let without_keys = keys => update(Environment.without_keys(keys));
}

and Filter: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pat: DHExp.t,
    act: FilterAction.t,
  };

  let mk: (DHExp.t, FilterAction.t) => t;

  let map: (DHExp.t => DHExp.t, t) => t;

  let strip_casts: t => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pat: DHExp.t,
    act: FilterAction.t,
  };

  let mk = (pat: DHExp.t, act: FilterAction.t): t => {pat, act};

  let map = (f: DHExp.t => DHExp.t, filter: t): t => {
    ...filter,
    pat: f(filter.pat),
  };

  let fast_equal = (f1: t, f2: t): bool => {
    DHExp.fast_equal(f1.pat, f2.pat) && f1.act == f2.act;
  };
  let strip_casts = (f: t): t => {...f, pat: f.pat |> DHExp.strip_casts};
}

and DHFilter: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(Filter.t)
    | Residue(int, FilterAction.t);
  let fast_equal: (t, t) => bool;
  let strip_casts: t => t;
  let map: (DHExp.t => DHExp.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(Filter.t)
    | Residue(int, FilterAction.t);
  let fast_equal = (f1: t, f2: t) => {
    switch (f1, f2) {
    | (Filter(flt1), Filter(flt2)) => Filter.fast_equal(flt1, flt2)
    | (Residue(idx1, act1), Residue(idx2, act2)) =>
      idx1 == idx2 && act1 == act2
    | _ => false
    };
  };
  let strip_casts = f => {
    switch (f) {
    | Filter(flt) => Filter(Filter.strip_casts(flt))
    | Residue(idx, act) => Residue(idx, act)
    };
  };
  let map = (mapper, filter) => {
    switch (filter) {
    | Filter(flt) => Filter(Filter.map(mapper, flt))
    | Residue(idx, act) => Residue(idx, act)
    };
  };
}

and FilterEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Filter.t);

  let extends: (Filter.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Filter.t);

  let extends = (flt, env) => [flt, ...env];
};
