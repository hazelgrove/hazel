open Sexplib.Std;

module rec DHExp: {
  module BinBoolOp: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | And
      | Or;
  };

  module BinIntOp: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Minus
      | Plus
      | Times
      | Power
      | Divide
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals;
  };

  module BinFloatOp: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FPower
      | FDivide
      | FLessThan
      | FLessThanOrEqual
      | FGreaterThan
      | FGreaterThanOrEqual
      | FEquals;
  };

  module BinStringOp: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | SEquals;
  };

  module BinListOp: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | LConcat;
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    | Closure(ClosureEnvironment.t, t)
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | TestLit(KeywordID.t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | BinStringOp(BinStringOp.t, t, t)
    | BinListOp(BinListOp.t, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t, list(t))
    | Cons(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | Tag(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string: t => string;

  let mk_tuple: list(t) => t;

  let cast: (t, Typ.t, Typ.t) => t;

  let apply_casts: (t, list((Typ.t, Typ.t))) => t;
  let strip_casts: t => t;

  let fast_equal: (t, t) => bool;
} = {
  module BinBoolOp = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | And
      | Or;
  };

  module BinIntOp = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Minus
      | Plus
      | Times
      | Power
      | Divide
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals;
  };

  module BinFloatOp = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FPower
      | FDivide
      | FLessThan
      | FLessThanOrEqual
      | FGreaterThan
      | FGreaterThanOrEqual
      | FEquals;
  };

  module BinStringOp = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | SEquals;
  };

  module BinListOp = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | LConcat;
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* Hole types */
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    /* Generalized closures */
    | Closure(ClosureEnvironment.t, t)
    /* Other expressions forms */
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | TestLit(KeywordID.t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | BinStringOp(BinStringOp.t, t, t)
    | BinListOp(BinListOp.t, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t, list(t))
    | Cons(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | Tag(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string = (d: t): string =>
    switch (d) {
    | EmptyHole(_, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _) => "NonEmptyHole"
    | ExpandingKeyword(_, _, _) => "ExpandingKeyword"
    | FreeVar(_, _, _) => "FreeVar"
    | InvalidText(_) => "InvalidText"
    | BoundVar(_) => "BoundVar"
    | Sequence(_, _) => "Sequence"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _, _) => "Fun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | ApBuiltin(_, _) => "ApBuiltin"
    | TestLit(_) => "TestLit"
    | BoolLit(_) => "BoolLit"
    | IntLit(_) => "IntLit"
    | FloatLit(_) => "FloatLit"
    | StringLit(_) => "StringLit"
    | BinBoolOp(_, _, _) => "BinBoolOp"
    | BinIntOp(_, _, _) => "BinIntOp"
    | BinFloatOp(_, _, _) => "BinFloatOp"
    | BinStringOp(_, _, _) => "BinStringOp"
    | BinListOp(_, _, _) => "BinListOp"
    | ListLit(_) => "ListLit"
    | Cons(_, _) => "Cons"
    | Tuple(_) => "Tuple"
    | Prj(_) => "Prj"
    | Inj(_, _, _) => "Inj"
    | Tag(_) => "Tag"
    | ConsistentCase(_) => "ConsistentCase"
    | InconsistentBranches(_, _, _) => "InconsistentBranches"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    | InvalidOperation(_) => "InvalidOperation"
    };

  let mk_tuple: list(t) => t =
    fun
    | []
    | [_] => failwith("mk_tuple: expected at least 2 elements")
    | xs => Tuple(xs);

  let cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
    if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
      d;
    } else {
      Cast(d, t1, t2);
    };

  let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
    List.fold_left((d, (ty1, ty2)) => cast(d, ty1, ty2), d, casts);

  let rec strip_casts =
    fun
    | Closure(ei, d) => Closure(ei, strip_casts(d))
    | Cast(d, _, _) => strip_casts(d)
    | FailedCast(d, _, _) => strip_casts(d)
    | Inj(ty, side, d) => Inj(ty, side, strip_casts(d))
    | Tuple(ds) => Tuple(ds |> List.map(strip_casts))
    | Prj(d, n) => Prj(strip_casts(d), n)
    | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
    | ListLit(a, b, c, d, ds) =>
      ListLit(a, b, c, d, List.map(strip_casts, ds))
    | NonEmptyHole(err, u, i, d) => NonEmptyHole(err, u, i, strip_casts(d))
    | Sequence(a, b) => Sequence(strip_casts(a), strip_casts(b))
    | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c))
    | FixF(a, b, c) => FixF(a, b, strip_casts(c))
    | Fun(a, b, c, d) => Fun(a, b, strip_casts(c), d)
    | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
    | ApBuiltin(fn, args) => ApBuiltin(fn, List.map(strip_casts, args))
    | BinBoolOp(a, b, c) => BinBoolOp(a, strip_casts(b), strip_casts(c))
    | BinIntOp(a, b, c) => BinIntOp(a, strip_casts(b), strip_casts(c))
    | BinFloatOp(a, b, c) => BinFloatOp(a, strip_casts(b), strip_casts(c))
    | BinStringOp(a, b, c) =>
      BinStringOp(a, strip_casts(b), strip_casts(c))
    | BinListOp(a, b, c) => BinListOp(a, strip_casts(b), strip_casts(c))
    | ConsistentCase(Case(a, rs, b)) =>
      ConsistentCase(
        Case(strip_casts(a), List.map(strip_casts_rule, rs), b),
      )
    | InconsistentBranches(u, i, Case(scrut, rules, n)) =>
      InconsistentBranches(
        u,
        i,
        Case(strip_casts(scrut), List.map(strip_casts_rule, rules), n),
      )
    | EmptyHole(_) as d
    | ExpandingKeyword(_) as d
    | FreeVar(_) as d
    | InvalidText(_) as d
    | BoundVar(_) as d
    | TestLit(_) as d
    | BoolLit(_) as d
    | IntLit(_) as d
    | FloatLit(_) as d
    | StringLit(_) as d
    | Tag(_) as d
    | InvalidOperation(_) as d => d
  and strip_casts_rule = (Rule(a, d)) => Rule(a, strip_casts(d));

  let rec fast_equal = (d1: t, d2: t): bool => {
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (BoundVar(_), _)
    /* TODO: Not sure if this is right... */
    | (TestLit(_), _)
    | (BoolLit(_), _)
    | (IntLit(_), _)
    | (FloatLit(_), _)
    | (Tag(_), _) => d1 == d2
    | (StringLit(s1), StringLit(s2)) => String.equal(s1, s2)
    | (StringLit(_), _) => false

    /* Non-hole forms: recurse */
    | (Sequence(d11, d21), Sequence(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1, s1), Fun(dp2, ty2, d2, s2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2) && s1 == s2
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Tuple(ds1), Tuple(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (Prj(d1, n), Prj(d2, m)) => n == m && fast_equal(d1, d2)
    | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
      f1 == f2 && List.for_all2(fast_equal, args1, args2)
    | (ListLit(_, _, _, _, ds1), ListLit(_, _, _, _, ds2)) =>
      List.for_all2(fast_equal, ds1, ds2)
    | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinStringOp(op1, d11, d21), BinStringOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinListOp(op1, d11, d21), BinListOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
      ty1 == ty2 && side1 == side2 && fast_equal(d1, d2)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (ConsistentCase(case1), ConsistentCase(case2)) =>
      fast_equal_case(case1, case2)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Sequence(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    | (ApBuiltin(_), _)
    | (Cons(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (Prj(_), _)
    | (BinBoolOp(_), _)
    | (BinIntOp(_), _)
    | (BinFloatOp(_), _)
    | (BinStringOp(_), _)
    | (BinListOp(_), _)
    | (Inj(_), _)
    | (Cast(_), _)
    | (FailedCast(_), _)
    | (InvalidOperation(_), _)
    | (ConsistentCase(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (EmptyHole(u1, i1), EmptyHole(u2, i2)) => u1 == u2 && i1 == i2
    | (NonEmptyHole(reason1, u1, i1, d1), NonEmptyHole(reason2, u2, i2, d2)) =>
      reason1 == reason2 && u1 == u2 && i1 == i2 && fast_equal(d1, d2)
    | (ExpandingKeyword(u1, i1, kw1), ExpandingKeyword(u2, i2, kw2)) =>
      u1 == u2 && i1 == i2 && kw1 == kw2
    | (FreeVar(u1, i1, x1), FreeVar(u2, i2, x2)) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (InvalidText(u1, i1, text1), InvalidText(u2, i2, text2)) =>
      u1 == u2 && i1 == i2 && text1 == text2
    | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
      ClosureEnvironment.id_equal(sigma1, sigma2) && fast_equal(d1, d2)
    | (
        InconsistentBranches(u1, i1, case1),
        InconsistentBranches(u2, i2, case2),
      ) =>
      u1 == u2 && i1 == i2 && fast_equal_case(case1, case2)
    | (EmptyHole(_), _)
    | (NonEmptyHole(_), _)
    | (ExpandingKeyword(_), _)
    | (FreeVar(_), _)
    | (InvalidText(_), _)
    | (Closure(_), _)
    | (InconsistentBranches(_), _) => false
    };
  }
  and fast_equal_case = (Case(d1, rules1, i1), Case(d2, rules2, i2)) => {
    fast_equal(d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         (Rule(dp1, d1), Rule(dp2, d2)) =>
           dp1 == dp2 && fast_equal(d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;
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

  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  let id_equal: (t, t) => bool;

  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(DHExp.t);
  let contains: (t, Var.t) => bool;
  let update:
    (Environment.t => Environment.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend:
    (t, (Var.t, DHExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let extend_keep_id: (t, (Var.t, DHExp.t)) => t;
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let union_keep_id: (t, t) => t;
  let map:
    (((Var.t, DHExp.t)) => DHExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;
  let filter:
    (((Var.t, DHExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let filter_keep_id: (((Var.t, DHExp.t)) => bool, t) => t;
  let fold: (((Var.t, DHExp.t), 'b) => 'b, 'b, t) => 'b;

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
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = (map, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    (wrap(ei, map), eig);
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
};
