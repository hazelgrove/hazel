open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type if_consistency =
  | ConsistentIf
  | InconsistentIf;

module rec DHExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    | Closure([@opaque] ClosureEnvironment.t, t)
    | Filter(DHFilter.t, t)
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | TypFun(Term.UTPat.t, t, option(Var.t))
    | TypAp(t, Typ.t)
    | Ap(t, t)
    | ApBuiltin(string, t)
    | BuiltinFun(string)
    | Test(KeywordID.t, t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(TermBase.UExp.op_bin_bool, t, t)
    | BinIntOp(TermBase.UExp.op_bin_int, t, t)
    | BinFloatOp(TermBase.UExp.op_bin_float, t, t)
    | BinStringOp(TermBase.UExp.op_bin_string, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
    | Cons(t, t)
    | ListConcat(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Constructor(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | IfThenElse(if_consistency, t, t, t) // use bool tag to track if branches are consistent
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
  let has_arrow: t => bool;

  let assign_name_if_none: (t, option(Var.t)) => t;
  let ty_subst: (Typ.t, TypVar.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* Hole types */
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    /* Generalized closures */
    | Closure(ClosureEnvironment.t, t)
    | Filter(DHFilter.t, t)
    /* Other expressions forms */
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, Typ.t, t)
    | Fun(DHPat.t, Typ.t, t, option(Var.t))
    | TypFun(Term.UTPat.t, t, option(Var.t))
    | TypAp(t, Typ.t)
    | Ap(t, t)
    | ApBuiltin(string, t)
    | BuiltinFun(string)
    | Test(KeywordID.t, t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(TermBase.UExp.op_bin_bool, t, t)
    | BinIntOp(TermBase.UExp.op_bin_int, t, t)
    | BinFloatOp(TermBase.UExp.op_bin_float, t, t)
    | BinStringOp(TermBase.UExp.op_bin_string, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
    | Cons(t, t)
    | ListConcat(t, t)
    | Tuple(list(t))
    | Prj(t, int)
    | Constructor(string)
    | ConsistentCase(case)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | IfThenElse(if_consistency, t, t, t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string = (d: t): string =>
    switch (d) {
    | EmptyHole(_, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _) => "NonEmptyHole"
    | FreeVar(_, _, _) => "FreeVar"
    | InvalidText(_) => "InvalidText"
    | BoundVar(_) => "BoundVar"
    | Sequence(_, _) => "Sequence"
    | Filter(_, _) => "Filter"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _, _) => "Fun"
    | TypFun(_) => "TypFun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | TypAp(_) => "TypAp"
    | ApBuiltin(_, _) => "ApBuiltin"
    | BuiltinFun(_) => "BuiltinFun"
    | Test(_) => "Test"
    | BoolLit(_) => "BoolLit"
    | IntLit(_) => "IntLit"
    | FloatLit(_) => "FloatLit"
    | StringLit(_) => "StringLit"
    | BinBoolOp(_, _, _) => "BinBoolOp"
    | BinIntOp(_, _, _) => "BinIntOp"
    | BinFloatOp(_, _, _) => "BinFloatOp"
    | BinStringOp(_, _, _) => "BinStringOp"
    | ListLit(_) => "ListLit"
    | Cons(_, _) => "Cons"
    | ListConcat(_, _) => "ListConcat"
    | Tuple(_) => "Tuple"
    | Prj(_) => "Prj"
    | Constructor(_) => "Constructor"
    | ConsistentCase(_) => "ConsistentCase"
    | InconsistentBranches(_, _, _) => "InconsistentBranches"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    | InvalidOperation(_) => "InvalidOperation"
    | IfThenElse(_, _, _, _) => "IfThenElse"
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
    | Tuple(ds) => Tuple(ds |> List.map(strip_casts))
    | Prj(d, n) => Prj(strip_casts(d), n)
    | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
    | ListConcat(d1, d2) => ListConcat(strip_casts(d1), strip_casts(d2))
    | ListLit(a, b, c, ds) => ListLit(a, b, c, List.map(strip_casts, ds))
    | NonEmptyHole(err, u, i, d) => NonEmptyHole(err, u, i, strip_casts(d))
    | Sequence(a, b) => Sequence(strip_casts(a), strip_casts(b))
    | Filter(f, b) => Filter(DHFilter.strip_casts(f), strip_casts(b))
    | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c))
    | FixF(a, b, c) => FixF(a, b, strip_casts(c))
    | Fun(a, b, c, d) => Fun(a, b, strip_casts(c), d)
    | TypFun(a, b, c) => TypFun(a, strip_casts(b), c)
    | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
    | TypAp(a, b) => TypAp(strip_casts(a), b)
    | Test(id, a) => Test(id, strip_casts(a))
    | ApBuiltin(fn, args) => ApBuiltin(fn, strip_casts(args))
    | BuiltinFun(fn) => BuiltinFun(fn)
    | BinBoolOp(a, b, c) => BinBoolOp(a, strip_casts(b), strip_casts(c))
    | BinIntOp(a, b, c) => BinIntOp(a, strip_casts(b), strip_casts(c))
    | BinFloatOp(a, b, c) => BinFloatOp(a, strip_casts(b), strip_casts(c))
    | BinStringOp(a, b, c) =>
      BinStringOp(a, strip_casts(b), strip_casts(c))
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
    | FreeVar(_) as d
    | InvalidText(_) as d
    | BoundVar(_) as d
    | BoolLit(_) as d
    | IntLit(_) as d
    | FloatLit(_) as d
    | StringLit(_) as d
    | Constructor(_) as d
    | InvalidOperation(_) as d => d
    | IfThenElse(consistent, c, d1, d2) =>
      IfThenElse(
        consistent,
        strip_casts(c),
        strip_casts(d1),
        strip_casts(d2),
      )
  and strip_casts_rule = (Rule(a, d)) => Rule(a, strip_casts(d));

  let rec fast_equal = (d1: t, d2: t): bool => {
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (BoundVar(_), _)
    /* TODO: Not sure if this is right... */
    | (BoolLit(_), _)
    | (IntLit(_), _)
    | (FloatLit(_), _)
    | (Constructor(_), _) => d1 == d2
    | (StringLit(s1), StringLit(s2)) => String.equal(s1, s2)
    | (StringLit(_), _) => false

    /* Non-hole forms: recurse */
    | (Test(id1, d1), Test(id2, d2)) => id1 == id2 && fast_equal(d1, d2)
    | (Sequence(d11, d21), Sequence(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Filter(f1, d1), Filter(f2, d2)) =>
      DHFilter.fast_equal(f1, f2) && fast_equal(d1, d2)
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1, s1), Fun(dp2, ty2, d2, s2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2) && s1 == s2
    | (TypFun(_tpat1, d1, s1), TypFun(_tpat2, d2, s2)) =>
      _tpat1 == _tpat2 && fast_equal(d1, d2) && s1 == s2
    | (TypAp(d1, ty1), TypAp(d2, ty2)) => fast_equal(d1, d2) && ty1 == ty2
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (ListConcat(d11, d21), ListConcat(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Tuple(ds1), Tuple(ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (Prj(d1, n), Prj(d2, m)) => n == m && fast_equal(d1, d2)
    | (ApBuiltin(f1, d1), ApBuiltin(f2, d2)) => f1 == f2 && d1 == d2
    | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
    | (ListLit(_, _, _, ds1), ListLit(_, _, _, ds2)) =>
      List.length(ds1) == List.length(ds2)
      && List.for_all2(fast_equal, ds1, ds2)
    | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinStringOp(op1, d11, d21), BinStringOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (ConsistentCase(case1), ConsistentCase(case2)) =>
      fast_equal_case(case1, case2)
    | (IfThenElse(c1, d11, d12, d13), IfThenElse(c2, d21, d22, d23)) =>
      c1 == c2
      && fast_equal(d11, d21)
      && fast_equal(d12, d22)
      && fast_equal(d13, d23)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Sequence(_), _)
    | (Filter(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Test(_), _)
    | (Ap(_), _)
    | (TypAp(_), _)
    | (ApBuiltin(_), _)
    | (BuiltinFun(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (ListLit(_), _)
    | (Tuple(_), _)
    | (Prj(_), _)
    | (BinBoolOp(_), _)
    | (BinIntOp(_), _)
    | (BinFloatOp(_), _)
    | (BinStringOp(_), _)
    | (Cast(_), _)
    | (FailedCast(_), _)
    | (InvalidOperation(_), _)
    | (IfThenElse(_), _)
    | (ConsistentCase(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (EmptyHole(u1, i1), EmptyHole(u2, i2)) => u1 == u2 && i1 == i2
    | (NonEmptyHole(reason1, u1, i1, d1), NonEmptyHole(reason2, u2, i2, d2)) =>
      reason1 == reason2 && u1 == u2 && i1 == i2 && fast_equal(d1, d2)
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

  let rec has_arrow =
    fun
    | Fun(_, _, _, _)
    | BuiltinFun(_) => true
    | EmptyHole(_)
    | FreeVar(_)
    | InvalidText(_)
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | Constructor(_)
    | InvalidOperation(_) => false
    | Closure(_, d)
    | Cast(d, _, _)
    | FailedCast(d, _, _)
    | Prj(d, _)
    | NonEmptyHole(_, _, _, d)
    | Filter(_, d)
    | FixF(_, _, d)
    | TypFun(_, d, _)
    | TypAp(d, _)
    | Test(_, d)
    | ApBuiltin(_, d)
    | ConsistentCase(Case(d, _, _))
    | InconsistentBranches(_, _, Case(d, _, _)) => has_arrow(d)
    | Tuple(ds)
    | ListLit(_, _, _, ds) => ds |> List.exists(has_arrow)
    | Cons(d1, d2)
    | ListConcat(d1, d2)
    | Sequence(d1, d2)
    | Let(_, d1, d2)
    | Ap(d1, d2)
    | BinBoolOp(_, d1, d2)
    | BinIntOp(_, d1, d2)
    | BinFloatOp(_, d1, d2)
    | BinStringOp(_, d1, d2) => has_arrow(d1) || has_arrow(d2)
    | IfThenElse(_, d1, d2, d3) =>
      has_arrow(d1) || has_arrow(d2) || has_arrow(d3);

  let assign_name_if_none = (t, name) =>
    switch (t) {
    | Fun(arg, ty, body, None) => Fun(arg, ty, body, name)
    | TypFun(utpat, body, None) => TypFun(utpat, body, name)
    | _ => t
    };

  let rec ty_subst = (s: Typ.t, x: TypVar.t, exp: DHExp.t): t => {
    let re = e2 => ty_subst(s, x, e2);
    let t_re = ty => Typ.subst(s, x, ty);
    switch (exp) {
    | Cast(t, t1, t2) => Cast(re(t), t_re(t1), t_re(t2))
    | FixF(arg, ty, body) => FixF(arg, t_re(ty), re(body))
    | Fun(arg, ty, body, var) => Fun(arg, t_re(ty), re(body), var)
    | TypAp(tfun, ty) => TypAp(re(tfun), t_re(ty))
    | ListLit(mv, mvi, t, lst) =>
      ListLit(mv, mvi, t_re(t), List.map(re, lst))
    | TypFun(utpat, body, var) =>
      switch (Term.UTPat.tyvar_of_utpat(utpat)) {
      | Some(x') when x == x' => exp
      | _ =>
        /* Note that we do not have to worry about capture avoidance, since s will always be closed. */
        TypFun(utpat, re(body), var)
      }
    | NonEmptyHole(errstat, mv, hid, t) =>
      NonEmptyHole(errstat, mv, hid, re(t))
    | Test(id, t) => Test(id, re(t))
    | InconsistentBranches(mv, hid, case) =>
      InconsistentBranches(mv, hid, ty_subst_case(s, x, case))
    | Closure(ce, t) => Closure(ce, re(t))
    | Sequence(t1, t2) => Sequence(re(t1), re(t2))
    | Let(dhpat, t1, t2) => Let(dhpat, re(t1), re(t2))
    | Ap(t1, t2) => Ap(re(t1), re(t2))
    | ApBuiltin(s, args) => ApBuiltin(s, re(args))
    | BinBoolOp(op, t1, t2) => BinBoolOp(op, re(t1), re(t2))
    | BinIntOp(op, t1, t2) => BinIntOp(op, re(t1), re(t2))
    | BinFloatOp(op, t1, t2) => BinFloatOp(op, re(t1), re(t2))
    | BinStringOp(op, t1, t2) => BinStringOp(op, re(t1), re(t2))
    | Cons(t1, t2) => Cons(re(t1), re(t2))
    | ListConcat(t1, t2) => ListConcat(re(t1), re(t2))
    | Tuple(args) => Tuple(List.map(re, args))
    | Prj(t, n) => Prj(re(t), n)
    | ConsistentCase(case) => ConsistentCase(ty_subst_case(s, x, case))
    | InvalidOperation(t, err) => InvalidOperation(re(t), err)
    | Filter(filt, exp) => Filter(DHFilter.map(re, filt), re(exp))
    | IfThenElse(consis, i, t, e) =>
      IfThenElse(consis, re(i), re(t), re(e))

    | BuiltinFun(_)
    | EmptyHole(_)
    | FreeVar(_, _, _)
    | InvalidText(_, _, _)
    | Constructor(_)
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | FailedCast(_, _, _) => exp
    };
  }
  and ty_subst_case = (s, x, Case(t, rules, n)) =>
    Case(
      ty_subst(s, x, t),
      List.map(
        (DHExp.Rule(dhpat, t)) => DHExp.Rule(dhpat, ty_subst(s, x, t)),
        rules,
      ),
      n,
    );
  //TODO: Inconsistent cases: need to check again for inconsistency?
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
