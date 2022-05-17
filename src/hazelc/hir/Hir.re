open Sexplib.Std;

[@deriving sexp]
type bin_bool_op =
  | OpAnd
  | OpOr;

[@deriving sexp]
type bin_int_op =
  | OpMinus
  | OpPlus
  | OpTimes
  | OpDivide
  | OpLessThan
  | OpGreaterThan
  | OpEquals;

[@deriving sexp]
type bin_float_op =
  | OpFPlus
  | OpFMinus
  | OpFTimes
  | OpFDivide
  | OpFLessThan
  | OpFGreaterThan
  | OpFEquals;

[@deriving sexp]
type has_indet = bool;

[@deriving sexp]
type expr = {
  expr_kind,
  expr_ty: HTyp.t,
  expr_indet: has_indet,
}

[@deriving sexp]
and case = {
  case_kind,
  case_ty: HTyp.t,
  case_indet: has_indet,
}

[@deriving sexp]
and case_kind =
  | ECase(expr, list(rule), int)

[@deriving sexp]
and rule = {
  rule_kind,
  rule_ty: HTyp.t,
  rule_indet: has_indet,
}

and rule_kind =
  | ERule(pat, expr)

[@deriving sexp]
and expr_kind =
  | EEmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(expr))
  | ENonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(expr),
      expr,
    )
  | EKeyword(MetaVar.t, MetaVarInst.t, VarMap.t_(expr), ExpandingKeyword.t)
  | EFreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(expr), Var.t)
  | EInvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(expr), string)
  | EBoundVar(Var.t)
  | ELet(pat, expr, expr)
  | ELetRec(Var.t, HTyp.t, pat, expr, expr)
  | ELam(pat, HTyp.t, expr)
  | EAp(expr, expr)
  | EApBuiltin(Var.t, list(expr))
  | EBoolLit(bool)
  | EIntLit(int)
  | EFloatLit(float)
  | EBinBoolOp(bin_bool_op, expr, expr)
  | EBinIntOp(bin_int_op, expr, expr)
  | EBinFloatOp(bin_float_op, expr, expr)
  | EListNil(HTyp.t)
  | ECons(expr, expr)
  | EInj(HTyp.t, InjSide.t, expr)
  | EPair(expr, expr)
  | ETriv
  | EConsistentCase(case)
  | EInconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(expr), case)
  | ECast(expr, HTyp.t, HTyp.t)
  | EFailedCast(expr, HTyp.t, HTyp.t)
  | EInvalidOperation(expr, InvalidOperationError.t)

[@deriving sexp]
and pat = {
  pat_kind,
  pat_indet: has_indet,
}

[@deriving sexp]
and pat_kind =
  | PEmptyHole(MetaVar.t, MetaVarInst.t)
  | PNonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, pat)
  | PWild
  | PKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | PInvalidText(MetaVar.t, MetaVarInst.t, string)
  | PVar(Var.t)
  | PIntLit(int)
  | PFloatLit(float)
  | PBoolLit(bool)
  | PInj(InjSide.t, pat)
  | PListNil
  | PCons(pat, pat)
  | PPair(pat, pat)
  | PTriv
  | PAp(pat, pat);
