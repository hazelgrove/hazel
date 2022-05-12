open Sexplib.Std;

// TODO: Separate variants for ops on indet forms.
[@deriving sexp]
type bin_bool_op =
  | And
  | Or;

[@deriving sexp]
type bin_int_op =
  | Minus
  | Plus
  | Times
  | Divide
  | LessThan
  | GreaterThan
  | Equals;

[@deriving sexp]
type bin_float_op =
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | FLessThan
  | FGreaterThan
  | FEquals;

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
  | Keyword(MetaVar.t, MetaVarInst.t, VarMap.t_(t), ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | BoundVar(Var.t)
  | Let(IHPat.t, t, t)
  | LetRec(Var.t, HTyp.t, IHPat.t, t, t)
  | Lam(IHPat.t, HTyp.t, t)
  | Ap(t, t)
  | ApBuiltin(Var.t, list(t))
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(bin_bool_op, t, t)
  | BinIntOp(bin_int_op, t, t)
  | BinFloatOp(bin_float_op, t, t)
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
  | Rule(IHPat.t, t);
