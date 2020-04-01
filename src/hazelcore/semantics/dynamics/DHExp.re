open Sexplib.Std;

[@deriving sexp]
type bin_num_op =
  | Minus
  | Plus
  | Times
  | LessThan
  | GreaterThan
  | Equals;

let of_op = (op: UHExp.operator): option((bin_num_op, HTyp.t)) =>
  switch (op) {
  | Minus => Some((Minus, Num))
  | Plus => Some((Plus, Num))
  | Times => Some((Times, Num))
  | LessThan => Some((LessThan, Bool))
  | GreaterThan => Some((GreaterThan, Bool))
  | Equals => Some((Equals, Bool))
  | And
  | Or
  | Space
  | Cons
  | Comma => None
  };

let to_op = (bno: bin_num_op): UHExp.operator =>
  switch (bno) {
  | Minus => Minus
  | Plus => Plus
  | Times => Times
  | LessThan => LessThan
  | GreaterThan => GreaterThan
  | Equals => Equals
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
  | NumLit(int)
  | BinNumOp(bin_num_op, t, t)
  | And(t, t)
  | Or(t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(HTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | Case(t, list(rule), int)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
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
  | Ap(_, _) => "Ap"
  | BoolLit(_) => "BoolLit"
  | NumLit(_) => "NumLit"
  | BinNumOp(_, _, _) => "BinNumOp"
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
