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
  | EmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(t))
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      t,
    )
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, VarMap.t_(t), ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | FixF(Var.t, HTyp.t, t)
  | Fun(DHPat.t, HTyp.t, t, string)
  | Ap(t, t)
  | ApBuiltin(string, list(t))
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
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t);

let constructor_string = (d: t): string =>
  switch (d) {
  | EmptyHole(_, _, _) => "EmptyHole"
  | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
  | Keyword(_, _, _, _) => "Keyword"
  | FreeVar(_, _, _, _) => "FreeVar"
  | InvalidText(_) => "InvalidText"
  | BoundVar(_) => "BoundVar"
  | Let(_, _, _) => "Let"
  | FixF(_, _, _) => "FixF"
  | Fun(_, _, _, _) => "Fun"
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
