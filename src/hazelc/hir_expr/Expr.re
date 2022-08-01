open Sexplib.Std;

open Holes;

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
  | EEmptyHole(MetaVar.t, MetaVarInst.t, Ident.Map.t(t))
  | ENonEmptyHole(HoleReason.t, MetaVar.t, MetaVarInst.t, Ident.Map.t(t), t)
  | EKeyword(MetaVar.t, MetaVarInst.t, Ident.Map.t(t), ExpandingKeyword.t)
  | EFreeVar(MetaVar.t, MetaVarInst.t, Ident.Map.t(t), Ident.t)
  | EInvalidText(MetaVar.t, MetaVarInst.t, Ident.Map.t(t), string)
  | EInvalidOperation(t, InvalidOperationError.t)
  /* Casts */
  | ECast(t, Typ.t, Typ.t)
  | EFailedCast(t, Typ.t, Typ.t)
  /* Case */
  | EConsistentCase(case)
  | EInconsistentBranches(MetaVar.t, MetaVarInst.t, Ident.Map.t(t), case)
  /* Let bindings */
  | ELet(Pat.t, t, t)
  | ELetRec(Ident.t, Pat.t, Typ.t, t, t)
  /* Function */
  | EFun(Pat.t, Typ.t, t)
  /* Application */
  | EAp(t, t)
  | EApBuiltin(Ident.t, list(t))
  /* Binary operations */
  | EBinBoolOp(bin_bool_op, t, t)
  | EBinIntOp(bin_int_op, t, t)
  | EBinFloatOp(bin_float_op, t, t)
  /* Pair */
  | EPair(t, t)
  /* Cons */
  | ECons(t, t)
  /* Sum injection */
  | EInj(Typ.t, InjSide.t, t)
  /* Immediate expressions */
  /* FIXME: Remove type. */
  | EBoundVar(Typ.t, Ident.t)
  | EBoolLit(bool)
  | EIntLit(int)
  | EFloatLit(float)
  | ENil(Typ.t)
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
