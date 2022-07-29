/**
  This module defines the high-level intermediate representation, which has an
  expression form.
 */

module Label = ExprLabel;
module RuleLabel = RuleLabel;

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
type t = {
  kind,
  label: ExprLabel.t,
}

[@deriving sexp]
and kind =
  /* Holes */
  | EEmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(t))
  | ENonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      t,
    )
  | EKeyword(MetaVar.t, MetaVarInst.t, VarMap.t_(t), ExpandingKeyword.t)
  | EFreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | EInvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | EInvalidOperation(t, InvalidOperationError.t)
  /* Casts */
  | ECast(t, HTyp.t, HTyp.t)
  | EFailedCast(t, HTyp.t, HTyp.t)
  /* Case */
  | EConsistentCase(case)
  | EInconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  /* Let bindings */
  | ELet(Pat.t, t, t)
  | ELetRec(Var.t, Pat.t, HTyp.t, t, t)
  /* Function */
  | EFun(Pat.t, HTyp.t, t)
  /* Application */
  | EAp(t, t)
  | EApBuiltin(Var.t, list(t))
  /* Binary operations */
  | EBinBoolOp(bin_bool_op, t, t)
  | EBinIntOp(bin_int_op, t, t)
  | EBinFloatOp(bin_float_op, t, t)
  /* Pair */
  | EPair(t, t)
  /* Cons */
  | ECons(t, t)
  /* Sum injection */
  | EInj(HTyp.t, InjSide.t, t)
  /* Immediate tessions; we can deduce the type and indet-ness of these trivially. */
  | EBoundVar(HTyp.t, Var.t)
  | EBoolLit(bool)
  | EIntLit(int)
  | EFloatLit(float)
  | ENil(HTyp.t)
  | ETriv

[@deriving sexp]
and case = {case_kind}

/* FIXME: Remove int */
[@deriving sexp]
and case_kind =
  | ECase(t, list(rule), int)

[@deriving sexp]
and rule = {
  rule_kind,
  rule_label: RuleLabel.t,
}

[@deriving sexp]
and rule_kind =
  | ERule(Pat.t, t);
