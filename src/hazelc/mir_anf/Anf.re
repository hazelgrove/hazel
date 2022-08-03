/**
 * This module defines a linearized intermediate representation.
 */
open Sexplib.Std;

open Holes;

module ExprLabel = ExprLabel;
module RuleLabel = RuleLabel;
module StmtLabel = StmtLabel;

[@deriving sexp]
type bin_op =
  | OpAnd
  | OpOr
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  | OpLessThan
  | OpGreaterThan
  | OpEquals
  | OpFPlus
  | OpFMinus
  | OpFTimes
  | OpFDivide
  | OpFLessThan
  | OpFGreaterThan
  | OpFEquals;

[@deriving sexp]
type constant =
  | ConstInt(int)
  | ConstFloat(float)
  | ConstBool(bool)
  | ConstNil(Typ.t)
  | ConstTriv

[@deriving sexp]
and imm = {
  imm_kind,
  imm_ty: Typ.t,
  imm_complete: Complete.t,
  imm_label: ExprLabel.t,
}

[@deriving sexp]
and imm_kind =
  | IConst(constant)
  | IVar(Ident.t)

[@deriving sexp]
and comp = {
  comp_kind,
  comp_ty: Typ.t,
  comp_complete: Complete.t,
  comp_label: ExprLabel.t,
}

[@deriving sexp]
and comp_kind =
  | CImm(imm)
  | CBinOp(bin_op, imm, imm)
  | CAp(imm, imm)
  | CFun(Pat.t, block)
  | CCons(imm, imm)
  | CPair(imm, imm)
  | CInj(inj_side, imm)
  | CCase(imm, list(rule))
  | CEmptyHole(MetaVar.t, MetaVarInst.t, sigma)
  | CNonEmptyHole(HoleReason.t, MetaVar.t, MetaVarInst.t, sigma, imm)
  | CCast(imm, Typ.t, Typ.t)

[@deriving sexp]
and sigma = Ident.Map.t(imm)

[@deriving sexp]
and inj_side =
  | CInjL
  | CInjR

[@deriving sexp]
and rule = {
  rule_pat: Pat.t,
  rule_branch: block,
  rule_complete: Complete.t,
  rule_label: RuleLabel.t,
}

[@deriving sexp]
and stmt = {
  stmt_kind,
  stmt_complete: Complete.t,
  stmt_label: StmtLabel.t,
}

[@deriving sexp]
and stmt_kind =
  | SLet(Ident.t, comp)
  | SLetRec(Ident.t, comp)

[@deriving sexp]
and block = {
  block_body: (list(stmt), imm),
  block_ty: Typ.t,
  block_complete: Complete.t,
  block_label: ExprLabel.t,
};
