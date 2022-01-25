// open Sexplib.Std;

module BinBoolOp = {
  // [@deriving sexp]
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
  // [@deriving sexp]
  type t =
    | Minus
    | Plus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  let of_op = (op: UHExp.operator): option((t, DHTyp.t)) =>
    switch (op) {
    | Minus => Some((Minus, DHTyp.wrap(Int)))
    | Plus => Some((Plus, DHTyp.wrap(Int)))
    | Times => Some((Times, DHTyp.wrap(Int)))
    | Divide => Some((Divide, DHTyp.wrap(Int)))
    | LessThan => Some((LessThan, DHTyp.wrap(Bool)))
    | GreaterThan => Some((GreaterThan, DHTyp.wrap(Bool)))
    | Equals => Some((Equals, DHTyp.wrap(Bool)))
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
  // [@deriving sexp]
  type t =
    | FPlus
    | FMinus
    | FTimes
    | FDivide
    | FLessThan
    | FGreaterThan
    | FEquals;

  let of_op = (op: UHExp.operator): option((t, DHTyp.t)) =>
    switch (op) {
    | FPlus => Some((FPlus, DHTyp.wrap(Float)))
    | FMinus => Some((FMinus, DHTyp.wrap(Float)))
    | FTimes => Some((FTimes, DHTyp.wrap(Float)))
    | FDivide => Some((FDivide, DHTyp.wrap(Float)))
    | FLessThan => Some((FLessThan, DHTyp.wrap(Bool)))
    | FGreaterThan => Some((FGreaterThan, DHTyp.wrap(Bool)))
    | FEquals => Some((FEquals, DHTyp.wrap(Bool)))
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

// [@deriving sexp]
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
  | TyAlias(TPat.t, DHTyp.t, Kind.t(DHTyp.t), t)
  | FixF(Var.t, DHTyp.t, t)
  | Lam(DHPat.t, DHTyp.t, t)
  | Ap(t, t)
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(DHTyp.t)
  | Cons(t, t)
  | Inj(DHTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | ConsistentCase(case)
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | Cast(t, DHTyp.t, DHTyp.t)
  | FailedCast(Contexts.t, t, DHTyp.t, DHTyp.t)
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
  | TyAlias(_) => "TyAlias"
  | FixF(_, _, _) => "FixF"
  | Lam(_, _, _) => "Lam"
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
  | FailedCast(_, _, _, _) => "FailedCast"
  | InvalidOperation(_) => "InvalidOperation"
  };

let rec mk_tuple: list(t) => t =
  fun
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [d] => d
  | [d, ...ds] => Pair(d, mk_tuple(ds));

let cast = (d: t, dty1: DHTyp.t, dty2: DHTyp.t): t =>
  DHTyp.equivalent(dty1, dty2) ? d : Cast(d, dty1, dty2);

let apply_casts = (d: t, casts: list((DHTyp.t, DHTyp.t))): t =>
  List.fold_left(
    (d, (dty1, dty2): (DHTyp.t, DHTyp.t)) => {cast(d, dty1, dty2)},
    d,
    casts,
  );
