open DHExp;
open Sexplib.Std;

[@deriving sexp]
type t =
  | BuiltIn(builtin)
  | Meta(MetaVar.t, MetaVarInst.t, VarMap.t_(t), meta_exp)
  | BoundVar(Var.t)
  | FixF(Var.t, HTyp.t, t)
  | Lam(DHPat.t, HTyp.t, t)
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
  | Rectangle(t, t)
  | DoesNotMatch
  | Indet
  | UnpackSum(InjSide.t)
  | UnpackPair(int)
  | UnpackTriv
  | UnpackCons
  | UnpackNil
  | Select(int)
  | IfEq;
