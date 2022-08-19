open Sexplib.Std;

module rec DHExp: {
  module BinBoolOp: {
    [@deriving sexp]
    type t =
      | And
      | Or;
    let of_op: Operators_Exp.t => option(t);
    let to_op: t => Operators_Exp.t;
  };

  module BinIntOp: {
    [@deriving sexp]
    type t =
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals;
    let of_op: Operators_Exp.t => option((t, HTyp.t));
    let to_op: t => Operators_Exp.t;
  };

  module BinFloatOp: {
    [@deriving sexp]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals;
    let of_op: Operators_Exp.t => option((t, HTyp.t));
    let to_op: t => Operators_Exp.t;
  };

  [@deriving sexp]
  type t_('env) =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(
        ErrStatus.HoleReason.t,
        MetaVar.t,
        HoleInstanceId.t,
        t_('env),
      )
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case_('env))
    | Closure('env, t_('env))
    | BoundVar(Var.t)
    | Let(DHPat.t, t_('env), t_('env))
    | FixF(Var.t, HTyp.t, t_('env))
    | Fun(DHPat.t, HTyp.t, t_('env))
    | Ap(t_('env), t_('env))
    | ApBuiltin(string, list(t_('env)))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t_('env), t_('env))
    | BinIntOp(BinIntOp.t, t_('env), t_('env))
    | BinFloatOp(BinFloatOp.t, t_('env), t_('env))
    | ListNil(HTyp.t)
    | Cons(t_('env), t_('env))
    | Inj(HTyp.t, InjSide.t, t_('env))
    | Pair(t_('env), t_('env))
    | Triv
    | ConsistentCase(case_('env))
    | Cast(t_('env), HTyp.t, HTyp.t)
    | FailedCast(t_('env), HTyp.t, HTyp.t)
    | InvalidOperation(t_('env), InvalidOperationError.t)
  and case_('env) =
    | Case(t_('env), list(rule_('env)), int)
  and rule_('env) =
    | Rule(DHPat.t, t_('env));

  [@deriving sexp]
  type t = t_(ClosureEnvironment.t);
  type case = case_(ClosureEnvironment.t);
  type rule = rule_(ClosureEnvironment.t);

  [@deriving sexp]
  type t' = t_(EnvironmentId.t);
  type case' = case_(EnvironmentId.t);
  type rule' = rule_(EnvironmentId.t);

  let constructor_string: t_('env) => string;

  let mk_tuple: list(t_('env)) => t_('env);

  let cast: (t_('env), HTyp.t, HTyp.t) => t_('env);

  let apply_casts: (t_('env), list((HTyp.t, HTyp.t))) => t_('env);

  let fast_equal_: (('env, 'env) => bool, t_('env), t_('env)) => bool;
  let fast_equal: (t, t) => bool;
  let fast_equal': (t', t') => bool;

  let of_t: t => (t', EnvironmentIdMap.t(Environment.t'));
} = {
  module BinBoolOp = {
    [@deriving sexp]
    type t =
      | And
      | Or;

    let of_op = (op: UHExp.operator): option(t) =>
      switch (op) {
      | And => Some(And)
      | Or => Some(Or)
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (op: t): UHExp.operator =>
      switch (op) {
      | And => And
      | Or => Or
      };
  };

  module BinIntOp = {
    [@deriving sexp]
    type t =
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals;

    let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
      switch (op) {
      | Minus => Some((Minus, Int))
      | Plus => Some((Plus, Int))
      | Times => Some((Times, Int))
      | Divide => Some((Divide, Int))
      | LessThan => Some((LessThan, Bool))
      | GreaterThan => Some((GreaterThan, Bool))
      | Equals => Some((Equals, Bool))
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals
      | And
      | Or
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (bio: t): UHExp.operator =>
      switch (bio) {
      | Minus => Minus
      | Plus => Plus
      | Times => Times
      | Divide => Divide
      | LessThan => LessThan
      | GreaterThan => GreaterThan
      | Equals => Equals
      };
  };

  module BinFloatOp = {
    [@deriving sexp]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals;

    let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
      switch (op) {
      | FPlus => Some((FPlus, Float))
      | FMinus => Some((FMinus, Float))
      | FTimes => Some((FTimes, Float))
      | FDivide => Some((FDivide, Float))
      | FLessThan => Some((FLessThan, Bool))
      | FGreaterThan => Some((FGreaterThan, Bool))
      | FEquals => Some((FEquals, Bool))
      | Plus
      | Minus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals
      | And
      | Or
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (bfo: t): UHExp.operator =>
      switch (bfo) {
      | FPlus => FPlus
      | FMinus => FMinus
      | FTimes => FTimes
      | FDivide => FDivide
      | FLessThan => FLessThan
      | FGreaterThan => FGreaterThan
      | FEquals => FEquals
      };
  };

  [@deriving sexp]
  type t_('env) =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(
        ErrStatus.HoleReason.t,
        MetaVar.t,
        HoleInstanceId.t,
        t_('env),
      )
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case_('env))
    | Closure('env, t_('env))
    | BoundVar(Var.t)
    | Let(DHPat.t, t_('env), t_('env))
    | FixF(Var.t, HTyp.t, t_('env))
    | Fun(DHPat.t, HTyp.t, t_('env))
    | Ap(t_('env), t_('env))
    | ApBuiltin(string, list(t_('env)))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t_('env), t_('env))
    | BinIntOp(BinIntOp.t, t_('env), t_('env))
    | BinFloatOp(BinFloatOp.t, t_('env), t_('env))
    | ListNil(HTyp.t)
    | Cons(t_('env), t_('env))
    | Inj(HTyp.t, InjSide.t, t_('env))
    | Pair(t_('env), t_('env))
    | Triv
    | ConsistentCase(case_('env))
    | Cast(t_('env), HTyp.t, HTyp.t)
    | FailedCast(t_('env), HTyp.t, HTyp.t)
    | InvalidOperation(t_('env), InvalidOperationError.t)
  and case_('env) =
    | Case(t_('env), list(rule_('env)), int)
  and rule_('env) =
    | Rule(DHPat.t, t_('env));

  [@deriving sexp]
  type t = t_(ClosureEnvironment.t);
  type case = case_(ClosureEnvironment.t);
  type rule = rule_(ClosureEnvironment.t);

  [@deriving sexp]
  type t' = t_(EnvironmentId.t);
  type case' = case_(EnvironmentId.t);
  type rule' = rule_(EnvironmentId.t);

  let constructor_string = (d: t_('env)): string =>
    switch (d) {
    | EmptyHole(_, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _) => "NonEmptyHole"
    | ExpandingKeyword(_, _, _) => "ExpandingKeyword"
    | FreeVar(_, _, _) => "FreeVar"
    | InvalidText(_) => "InvalidText"
    | BoundVar(_) => "BoundVar"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _) => "Fun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | ApBuiltin(_, _) => "ApBuiltin"
    | BoolLit(_) => "BoolLit"
    | IntLit(_) => "IntLit"
    | FloatLit(_) => "FloatLit"
    | BinBoolOp(_, _, _) => "BinBoolOp"
    | BinIntOp(_, _, _) => "BinIntOp"
    | BinFloatOp(_, _, _) => "BinFloatOp"
    | ListNil(_) => "ListNil"
    | Cons(_, _) => "Cons"
    | Inj(_, _, _) => "Inj"
    | Pair(_, _) => "Pair"
    | Triv => "Triv"
    | ConsistentCase(_) => "ConsistentCase"
    | InconsistentBranches(_, _, _) => "InconsistentBranches"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    | InvalidOperation(_) => "InvalidOperation"
    };

  let rec mk_tuple: list(t_('env)) => t_('env) =
    fun
    | [] => failwith("mk_tuple: expected at least 1 element")
    | [d] => d
    | [d, ...ds] => Pair(d, mk_tuple(ds));

  let cast = (d: t_('env), t1: HTyp.t, t2: HTyp.t): t_('env) =>
    if (HTyp.eq(t1, t2)) {
      d;
    } else {
      Cast(d, t1, t2);
    };

  let apply_casts = (d: t_('env), casts: list((HTyp.t, HTyp.t))): t_('env) =>
    List.fold_left(
      (d, c: (HTyp.t, HTyp.t)) => {
        let (ty1, ty2) = c;
        cast(d, ty1, ty2);
      },
      d,
      casts,
    );

  let rec fast_equal_ =
          (
            env_fast_equal_: ('env, 'env) => bool,
            d1: t_('env),
            d2: t_('env),
          )
          : bool =>
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (BoundVar(_), _)
    | (BoolLit(_), _)
    | (IntLit(_), _)
    | (FloatLit(_), _)
    | (ListNil(_), _)
    | (Triv, _) => d1 == d2

    /* Non-hole forms: recurse */
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2
      && fast_equal_(env_fast_equal_, d11, d12)
      && fast_equal_(env_fast_equal_, d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal_(env_fast_equal_, d1, d2)
    | (Fun(dp1, ty1, d1), Fun(dp2, ty2, d2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal_(env_fast_equal_, d1, d2)
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22))
    | (Pair(d11, d21), Pair(d12, d22)) =>
      fast_equal_(env_fast_equal_, d11, d12)
      && fast_equal_(env_fast_equal_, d21, d22)
    | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
      f1 == f2 && List.for_all2(fast_equal_(env_fast_equal_), args1, args2)
    | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
      op1 == op2
      && fast_equal_(env_fast_equal_, d11, d12)
      && fast_equal_(env_fast_equal_, d21, d22)
    | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
      op1 == op2
      && fast_equal_(env_fast_equal_, d11, d12)
      && fast_equal_(env_fast_equal_, d21, d22)
    | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
      op1 == op2
      && fast_equal_(env_fast_equal_, d11, d12)
      && fast_equal_(env_fast_equal_, d21, d22)
    | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
      ty1 == ty2 && side1 == side2 && fast_equal_(env_fast_equal_, d1, d2)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal_(env_fast_equal_, d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal_(env_fast_equal_, d1, d2) && reason1 == reason2
    | (ConsistentCase(case1), ConsistentCase(case2)) =>
      fast_equal_case_(env_fast_equal_, case1, case2)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    | (ApBuiltin(_), _)
    | (Cons(_), _)
    | (Pair(_), _)
    | (BinBoolOp(_), _)
    | (BinIntOp(_), _)
    | (BinFloatOp(_), _)
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
      reason1 == reason2
      && u1 == u2
      && i1 == i2
      && fast_equal_(env_fast_equal_, d1, d2)
    | (ExpandingKeyword(u1, i1, kw1), ExpandingKeyword(u2, i2, kw2)) =>
      u1 == u2 && i1 == i2 && kw1 == kw2
    | (FreeVar(u1, i1, x1), FreeVar(u2, i2, x2)) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (InvalidText(u1, i1, text1), InvalidText(u2, i2, text2)) =>
      u1 == u2 && i1 == i2 && text1 == text2
    | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
      env_fast_equal_(sigma1, sigma2) && fast_equal_(env_fast_equal_, d1, d2)
    | (
        InconsistentBranches(u1, i1, case1),
        InconsistentBranches(u2, i2, case2),
      ) =>
      u1 == u2 && i1 == i2 && fast_equal_case_(env_fast_equal_, case1, case2)
    | (EmptyHole(_), _)
    | (NonEmptyHole(_), _)
    | (ExpandingKeyword(_), _)
    | (FreeVar(_), _)
    | (InvalidText(_), _)
    | (Closure(_), _)
    | (InconsistentBranches(_), _) => false
    }
  and fast_equal_case_ =
      (env_fast_equal_, Case(d1, rules1, i1), Case(d2, rules2, i2)) =>
    fast_equal_(env_fast_equal_, d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         (Rule(dp1, d1), Rule(dp2, d2)) =>
           dp1 == dp2 && fast_equal_(env_fast_equal_, d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;

  let fast_equal = (d1: t, d2: t) =>
    fast_equal_(ClosureEnvironment.id_equal, d1, d2);

  let fast_equal' = (d1: t', d2: t') =>
    fast_equal_(EnvironmentId.equal, d1, d2);

  module OfTMonad =
    StateMonad.Make({
      type t = EnvironmentIdMap.t(Environment.t');
    });

  let of_t = (d: t): (t', EnvironmentIdMap.t(Environment.t')) => {
    open OfTMonad;
    open OfTMonad.Syntax;

    let rec of_t = d =>
      switch (d) {
      | EmptyHole(u, i) => EmptyHole(u, i) |> return
      | NonEmptyHole(reason, u, i, d') =>
        let+ d' = of_t(d');
        NonEmptyHole(reason, u, i, d');
      | ExpandingKeyword(u, i, k) => ExpandingKeyword(u, i, k) |> return
      | FreeVar(u, i, x) => FreeVar(u, i, x) |> return
      | InvalidText(u, i, text) => InvalidText(u, i, text) |> return
      | BoundVar(x) => BoundVar(x) |> return
      | Let(dp, d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        Let(dp, d1, d2);
      | FixF(f, ty, body) =>
        let+ body = of_t(body);
        FixF(f, ty, body);
      | Fun(dp, dp_ty, body) =>
        let+ body = of_t(body);
        Fun(dp, dp_ty, body);
      | Closure(env, d') =>
        let id = env |> ClosureEnvironment.id_of;
        let* map =
          env
          |> ClosureEnvironment.map_of
          |> Environment.to_listo
          |> List.map(((x, d'')) => of_t(d'') >>| (d'' => (x, d'')))
          |> sequence
          >>| Environment.of_list;
        let* () = modify(envs => EnvironmentIdMap.add(id, map, envs));
        let+ d' = of_t(d');
        Closure(id, d');
      | Ap(d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        Ap(d1, d2);
      | ApBuiltin(ident, args) =>
        let+ args = args |> List.map(of_t) |> sequence;
        ApBuiltin(ident, args);
      | BoolLit(b) => BoolLit(b) |> return
      | IntLit(n) => IntLit(n) |> return
      | FloatLit(f) => FloatLit(f) |> return
      | BinBoolOp(op, d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        BinBoolOp(op, d1, d2);
      | BinIntOp(op, d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        BinIntOp(op, d1, d2);
      | BinFloatOp(op, d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        BinFloatOp(op, d1, d2);
      | ListNil(ty) => ListNil(ty) |> return
      | Cons(d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        Cons(d1, d2);
      | Inj(side, other_ty, d') =>
        let+ d' = of_t(d');
        Inj(side, other_ty, d');
      | Pair(d1, d2) =>
        let* d1 = of_t(d1);
        let+ d2 = of_t(d2);
        Pair(d1, d2);
      | Triv => Triv |> return
      | ConsistentCase(case) =>
        let+ case = of_t_case(case);
        ConsistentCase(case);
      | InconsistentBranches(u, i, case) =>
        let+ case = of_t_case(case);
        InconsistentBranches(u, i, case);
      | Cast(d', ty1, ty2) =>
        let+ d' = of_t(d');
        Cast(d', ty1, ty2);
      | FailedCast(d', ty1, ty2) =>
        let+ d' = of_t(d');
        FailedCast(d', ty1, ty2);
      | InvalidOperation(d', err) =>
        let+ d' = of_t(d');
        InvalidOperation(d', err);
      }
    and of_t_case =
        (Case(scrut, rules, n): DHExp.case): OfTMonad.t(DHExp.case') => {
      let* scrut = of_t(scrut);
      let+ rules = rules |> List.map(of_t_rules) |> sequence;
      Case(scrut, rules, n);
    }
    and of_t_rules = (Rule(dp, body): DHExp.rule): OfTMonad.t(DHExp.rule') => {
      let+ body = of_t(body);
      Rule(dp, body);
    };

    let (map, d) = of_t(d, EnvironmentIdMap.empty);
    (d, map);
  };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving sexp]
  type nonrec t_('env) = t_(DHExp.t_('env));

  [@deriving sexp]
  type t = t_(ClosureEnvironment.t);

  [@deriving sexp]
  type t' = t_(EnvironmentId.t);
} = {
  include VarBstMap.Ordered;

  [@deriving sexp]
  type nonrec t_('env) = t_(DHExp.t_('env));

  [@deriving sexp]
  type t = t_(ClosureEnvironment.t);

  [@deriving sexp]
  type t' = t_(EnvironmentId.t);
}

and ClosureEnvironment: {
  [@deriving sexp]
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

  let placeholder: unit => t;
} = {
  module Inner: {
    [@deriving sexp]
    type t;

    let wrap: (EnvironmentId.t, Environment.t) => t;

    let id_of: t => EnvironmentId.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving sexp]
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

  let placeholder = () => wrap(EnvironmentId.invalid, Environment.empty);
};
