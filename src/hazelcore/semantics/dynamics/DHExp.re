open Sexplib.Std;

module BinIntOp = {
  [@deriving sexp]
  type t =
    | Minus
    | Plus
    | Times
    | LessThan
    | GreaterThan
    | Equals;

  let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
    switch (op) {
    | Minus => Some((Minus, Int))
    | Plus => Some((Plus, Int))
    | Times => Some((Times, Int))
    | LessThan => Some((LessThan, Bool))
    | GreaterThan => Some((GreaterThan, Bool))
    | Equals => Some((Equals, Bool))
    | FPlus
    | FMinus
    | FTimes
    | FLessThan
    | FGreaterThan
    | FEquals
    | And
    | Or
    | Space
    | Cons
    | Comma
    | PlusPlus => None
    };

  let to_op = (bio: t): UHExp.operator =>
    switch (bio) {
    | Minus => Minus
    | Plus => Plus
    | Times => Times
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
    | FLessThan
    | FGreaterThan
    | FEquals;

  let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
    switch (op) {
    | FPlus => Some((FPlus, Float))
    | FMinus => Some((FMinus, Float))
    | FTimes => Some((FTimes, Float))
    | FLessThan => Some((FLessThan, Bool))
    | FGreaterThan => Some((FGreaterThan, Bool))
    | FEquals => Some((FEquals, Bool))
    | Plus
    | Minus
    | Times
    | LessThan
    | GreaterThan
    | Equals
    | And
    | Or
    | Space
    | Cons
    | Comma
    | PlusPlus => None
    };

  let to_op = (bfo: t): UHExp.operator =>
    switch (bfo) {
    | FPlus => FPlus
    | FMinus => FMinus
    | FTimes => FTimes
    | FLessThan => FLessThan
    | FGreaterThan => FGreaterThan
    | FEquals => FEquals
    };
};

module BinStrOp = {
  [@deriving sexp]
  type t =
    | PlusPlus;

  let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
    switch (op) {
    | PlusPlus => Some((PlusPlus, String))
    | Minus
    | Plus
    | Times
    | LessThan
    | GreaterThan
    | Equals
    | And
    | Or
    | Space
    | Cons
    | Comma
    | FPlus
    | FMinus
    | FTimes
    | FLessThan
    | FGreaterThan
    | FEquals => None
    };

  let to_op = (bso: t): UHExp.operator =>
    switch (bso) {
    | PlusPlus => PlusPlus
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
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | FixF(Var.t, HTyp.t, t)
  | Lam(DHPat.t, HTyp.t, t)
  | Ap(t, t)
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | StringLit(string)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | BinStrOp(BinStrOp.t, t, t)
  | And(t, t)
  | Or(t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(HTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | Case(t, list(rule), int)
  | Subscript(t, t, t)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | FailedSubscript(t)
and rule =
  | Rule(DHPat.t, t);

let constructor_string = (d: t): string =>
  switch (d) {
  | EmptyHole(_, _, _) => "EmptyHole"
  | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
  | Keyword(_, _, _, _) => "Keyword"
  | FreeVar(_, _, _, _) => "FreeVar"
  | BoundVar(_) => "BoundVar"
  | Let(_, _, _) => "Let"
  | FixF(_, _, _) => "FixF"
  | Lam(_, _, _) => "Lam"
  | Subscript(_, _, _) => "Subscript"
  | Ap(_, _) => "Ap"
  | BoolLit(_) => "BoolLit"
  | IntLit(_) => "IntLit"
  | FloatLit(_) => "FloatLit"
  | StringLit(_) => "StringLit"
  | BinIntOp(_, _, _) => "BinIntOp"
  | BinFloatOp(_, _, _) => "BinFloatOp"
  | BinStrOp(_, _, _) => "BinStrOp"
  | And(_, _) => "And"
  | Or(_, _) => "Or"
  | ListNil(_) => "ListNil"
  | Cons(_, _) => "Cons"
  | Inj(_, _, _) => "Inj"
  | Pair(_, _) => "Pair"
  | Triv => "Triv"
  | Case(_, _, _) => "Case"
  | Cast(_, _, _) => "Cast"
  | FailedCast(_, _, _) => "FailedCast"
  | FailedSubscript(_) => "FailedSubscript"
  };

let rec make_tuple: list(t) => t =
  fun
  | [] => failwith("make_tuple: expected at least 1 element")
  | [d] => d
  | [d, ...ds] => Pair(d, make_tuple(ds));

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
