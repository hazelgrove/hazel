open Sexplib.Std;

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
type t =
  | EmptyHole(MetaVar.t, HoleClosureId.t, evalenv)
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      HoleClosureId.t,
      evalenv,
      t,
    )
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, HoleClosureId.t, evalenv, ExpandingKeyword.t)
  | FreeVar(MetaVar.t, HoleClosureId.t, evalenv, Var.t)
  | InvalidText(MetaVar.t, HoleClosureId.t, evalenv, string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | Lam(DHPat.t, HTyp.t, t)
  | Closure(evalenv, DHPat.t, HTyp.t, t)
  | Ap(t, t)
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(HTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | ConsistentCase(case)
  | InconsistentBranches(MetaVar.t, HoleClosureId.t, evalenv, case)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t)
and environment = VarMap.t_(t)
and evalenv =
  | Env(EvalEnvId.t, VarMap.t_(result))
  | UnreachedEnv
and result =
  | BoxedValue(t)
  | Indet(t);

let constructor_string = (d: t): string =>
  switch (d) {
  | EmptyHole(_, _, _) => "EmptyHole"
  | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
  | Keyword(_, _, _, _) => "Keyword"
  | FreeVar(_, _, _, _) => "FreeVar"
  | InvalidText(_) => "InvalidText"
  | BoundVar(_) => "BoundVar"
  | Let(_, _, _) => "Let"
  | Lam(_, _, _) => "Lam"
  | Closure(_, _, _, _) => "Closure"
  | Ap(_, _) => "Ap"
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
  | InconsistentBranches(_, _, _, _) => "InconsistentBranches"
  | Cast(_, _, _) => "Cast"
  | FailedCast(_, _, _) => "FailedCast"
  | InvalidOperation(_) => "InvalidOperation"
  };

let rec mk_tuple: list(t) => t =
  fun
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [d] => d
  | [d, ...ds] => Pair(d, mk_tuple(ds));

let cast = (d: t, t1: HTyp.t, t2: HTyp.t): t =>
  if (HTyp.eq(t1, t2)) {
    d;
  } else {
    Cast(d, t1, t2);
  };

let apply_casts = (d: t, casts: list((HTyp.t, HTyp.t))): t =>
  List.fold_left(
    (d, c: (HTyp.t, HTyp.t)) => {
      let (ty1, ty2) = c;
      cast(d, ty1, ty2);
    },
    d,
    casts,
  );

/* Helper for fast_equals. */
let evalenv_equals = (sigma1: evalenv, sigma2: evalenv): bool => {
  switch (sigma1, sigma2) {
  | (Env(ei1, _), Env(ei2, _)) => ei1 == ei2
  | _ => false
  };
};

let rec fast_equals = (d1: t, d2: t): bool => {
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
    dp1 == dp2 && fast_equals(d11, d12) && fast_equals(d21, d22)
  | (Lam(dp1, ty1, d1), Lam(dp2, ty2, d2)) =>
    dp1 == dp2 && ty1 == ty2 && fast_equals(d1, d2)
  | (Ap(d11, d21), Ap(d12, d22))
  | (Cons(d11, d21), Cons(d12, d22))
  | (Pair(d11, d21), Pair(d12, d22)) =>
    fast_equals(d11, d12) && fast_equals(d21, d22)
  | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
    op1 == op2 && fast_equals(d11, d12) && fast_equals(d21, d22)
  | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
    op1 == op2 && fast_equals(d11, d12) && fast_equals(d21, d22)
  | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
    op1 == op2 && fast_equals(d11, d12) && fast_equals(d21, d22)
  | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
    ty1 == ty2 && side1 == side2 && fast_equals(d1, d2)
  | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
  | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
    fast_equals(d1, d2) && ty11 == ty12 && ty21 == ty22
  | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
    fast_equals(d1, d2) && reason1 == reason2
  | (ConsistentCase(case1), ConsistentCase(case2)) =>
    fast_equals_case(case1, case2)
  | (Let(_), _)
  | (Lam(_), _)
  | (Ap(_), _)
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
  | (EmptyHole(u1, i1, sigma1), EmptyHole(u2, i2, sigma2)) =>
    u1 == u2 && i1 == i2 && evalenv_equals(sigma1, sigma2)
  | (
      NonEmptyHole(reason1, u1, i1, sigma1, d1),
      NonEmptyHole(reason2, u2, i2, sigma2, d2),
    ) =>
    reason1 == reason2
    && u1 == u2
    && i1 == i2
    && evalenv_equals(sigma1, sigma2)
    && fast_equals(d1, d2)
  | (Keyword(u1, i1, sigma1, kw1), Keyword(u2, i2, sigma2, kw2)) =>
    u1 == u2 && i1 == i2 && evalenv_equals(sigma1, sigma2) && kw1 == kw2
  | (FreeVar(u1, i1, sigma1, x1), FreeVar(u2, i2, sigma2, x2)) =>
    u1 == u2 && i1 == i2 && evalenv_equals(sigma1, sigma2) && x1 == x2
  | (InvalidText(u1, i1, sigma1, text1), InvalidText(u2, i2, sigma2, text2)) =>
    u1 == u2 && i1 == i2 && evalenv_equals(sigma1, sigma2) && text1 == text2
  | (Closure(sigma1, dp1, ty1, d1), Closure(sigma2, dp2, ty2, d2)) =>
    evalenv_equals(sigma1, sigma2) && dp1 == dp2 && ty1 == ty2 && d1 == d2
  | (
      InconsistentBranches(u1, i1, sigma1, case1),
      InconsistentBranches(u2, i2, sigma2, case2),
    ) =>
    u1 == u2
    && i1 == i2
    && evalenv_equals(sigma1, sigma2)
    && fast_equals_case(case1, case2)
  | (EmptyHole(_), _)
  | (NonEmptyHole(_), _)
  | (Keyword(_), _)
  | (FreeVar(_), _)
  | (InvalidText(_), _)
  | (Closure(_), _)
  | (InconsistentBranches(_), _) => false
  };
}
and fast_equals_case = (Case(d1, rules1, i1), Case(d2, rules2, i2)) => {
  fast_equals(d1, d2)
  && List.for_all2(
       (Rule(dp1, d1), Rule(dp2, d2)) => dp1 == dp2 && fast_equals(d1, d2),
       rules1,
       rules2,
     )
  && i1 == i2;
};
