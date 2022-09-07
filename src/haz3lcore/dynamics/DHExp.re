open Sexplib.Std;

module BinBoolOp = {
  [@deriving sexp]
  type t =
    | And
    | Or;
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
  | ExpandingKeyword(
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      ExpandingKeyword.t,
    )
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | FixF(Var.t, HTyp.t, t)
  | Fun(DHPat.t, HTyp.t, t)
  | Ap(t, t)
  | ApBuiltin(string, list(t))
  | BoolLit(bool)
  | IntLit(int)
  | Sequence(t, t)
  | TestLit(KeywordID.t)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListLit(
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      ListErrStatus.t,
      HTyp.t,
      list(t),
    )
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

let empty = EmptyHole(0, 0, []);

// let rec strip_casts: t => t =
//   fun
//   | Cast(d, _, _) => strip_casts(d)
//   | FailedCast(d, _, _) => strip_casts(d)
//   | Inj(a, b, d) => Inj(a, b, strip_casts(d))
//   | Pair(d1, d2) => Pair(strip_casts(d1), strip_casts(d2))
//   | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2))
//   | NonEmptyHole(err, b, c, d, e) =>
//     NonEmptyHole(err, b, c, d, strip_casts(e))
//   | Let(a, b, c) => Let(a, strip_casts(b), strip_casts(c))
//   | FixF(a, b, c) => FixF(a, b, strip_casts(c))
//   | Fun(a, b, c) => Fun(a, b, strip_casts(c))
//   | Ap(a, b) => Ap(strip_casts(a), strip_casts(b))
//   | Sequence(a, b) => Sequence(strip_casts(a), strip_casts(b))
//   | BinBoolOp(a, b, c) => BinBoolOp(a, strip_casts(b), strip_casts(c))
//   | BinIntOp(a, b, c) => BinIntOp(a, strip_casts(b), strip_casts(c))
//   | BinFloatOp(a, b, c) => BinFloatOp(a, strip_casts(b), strip_casts(c))
//   | ConsistentCase(Case(a, rs, b)) =>
//     ConsistentCase(Case(strip_casts(a), List.map(strip_casts_rule, rs), b))
//   | InconsistentBranches(c, d, e, Case(a, rs, b)) =>
//     InconsistentBranches(
//       c,
//       d,
//       e,
//       Case(strip_casts(a), List.map(strip_casts_rule, rs), b),
//     )
//   | ListLit(a, b, c, d, e, ele_list) =>
//     ListLit(a, b, c, d, e, List.map(strip_casts, ele_list))
//   | EmptyHole(_) as d
//   | ExpandingKeyword(_) as d
//   | FreeVar(_) as d
//   | InvalidText(_) as d
//   | BoundVar(_) as d
//   | Triv as d
//   | TestLit(_) as d
//   | BoolLit(_) as d
//   | IntLit(_) as d
//   | FloatLit(_) as d
//   | InvalidOperation(_) as d => d
// and strip_casts_rule: rule => rule =
//   (Rule(a, d)) => Rule(a, strip_casts(d));

let constructor_string = (d: t): string =>
  switch (d) {
  | EmptyHole(_, _, _) => "EmptyHole"
  | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
  | ExpandingKeyword(_, _, _, _) => "ExpandingKeyword"
  | FreeVar(_, _, _, _) => "FreeVar"
  | InvalidText(_) => "InvalidText"
  | BoundVar(_) => "BoundVar"
  | Let(_, _, _) => "Let"
  | FixF(_, _, _) => "FixF"
  | Fun(_, _, _) => "Fun"
  | Ap(_, _) => "Ap"
  | ApBuiltin(_, _) => "ApBuiltin"
  | BoolLit(_) => "BoolLit"
  | IntLit(_) => "IntLit"
  | Sequence(_, _) => "Sequence"
  | TestLit(_) => "TestLit"
  | FloatLit(_) => "FloatLit"
  | BinBoolOp(_, _, _) => "BinBoolOp"
  | BinIntOp(_, _, _) => "BinIntOp"
  | BinFloatOp(_, _, _) => "BinFloatOp"
  | ListLit(_, _, _, _, _, _) => "ListLit"
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
  switch (d, t2) {
  | (ListLit(_, _, _, _, _, []), List(_)) =>
    //HACK(andrew, cyrus)
    d
  | _ =>
    if (HTyp.eq(t1, t2)) {
      d;
    } else {
      Cast(d, t1, t2);
    }
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
