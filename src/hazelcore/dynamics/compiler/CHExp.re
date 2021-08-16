open DHExp;
open Sexplib.Std;

[@deriving sexp]
type t =
  | Rectangle(t, t)
  | BuiltIn(builtin)
  | Meta(MetaVar.t, MetaVarInst.t, VarMap.t_(t), meta_exp)
  | BoundVar(Var.t)
  | FixF(Var.t, option(HTyp.t), t)
  | Lam(DHPat.t, option(HTyp.t), t)
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
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and meta_exp =
  | InconsistentBranches(t)
  | Keyword(ExpandingKeyword.t)
  | FreeVar(Var.t)
  | InvalidText(string)
  | EmptyHole
  | NonEmptyHole(ErrStatus.HoleReason.t, t)
and builtin =
  | DoesNotMatch
  | Indet
  | UnpackSum(InjSide.t)
  | UnpackProd(int)
  | UnpackCons
  | UnpackNil
  | Select(int)
  | IfEq;
