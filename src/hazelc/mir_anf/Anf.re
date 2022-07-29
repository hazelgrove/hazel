/**
 * This module defines a linearized intermediate representation.
 */
open Sexplib.Std;

[@deriving sexp]
type complete = Complete.t;

[@deriving sexp]
type label = AnfLabel.t;

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
type pat = {
  pat_kind,
  pat_complete: complete,
  pat_label: label,
}

[@deriving sexp]
and pat_kind =
  | PWild
  | PVar(Var.t)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PInj(inj_side, pat)
  | PCons(pat, pat)
  | PPair(pat, pat)
  | PTriv

[@deriving sexp]
and constant =
  | ConstInt(int)
  | ConstFloat(float)
  | ConstBool(bool)
  | ConstNil(HTyp.t)
  | ConstTriv

[@deriving sexp]
and imm = {
  imm_kind,
  imm_ty: HTyp.t,
  imm_complete: complete,
  imm_label: label,
}

[@deriving sexp]
and imm_kind =
  | IConst(constant)
  | IVar(Var.t)

[@deriving sexp]
and comp = {
  comp_kind,
  comp_ty: HTyp.t,
  comp_complete: complete,
  comp_label: label,
}

[@deriving sexp]
and comp_kind =
  | CImm(imm)
  | CBinOp(bin_op, imm, imm)
  | CAp(imm, imm)
  | CFun(pat, block)
  | CCons(imm, imm)
  | CPair(imm, imm)
  | CInj(inj_side, imm)
  | CCase(imm, list(rule))
  | CEmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(imm))
  | CNonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(imm),
      imm,
    )
  | CCast(imm, HTyp.t, HTyp.t)

[@deriving sexp]
and inj_side =
  | CInjL
  | CInjR

[@deriving sexp]
and rule = {
  rule_pat: pat,
  rule_branch: block,
  rule_complete: complete,
  rule_label: label,
}

[@deriving sexp]
and stmt = {
  stmt_kind,
  stmt_complete: complete,
  stmt_label: label,
}

[@deriving sexp]
and stmt_kind =
  | SLet(pat, comp)
  | SLetRec(Var.t, comp)

[@deriving sexp]
and block = {
  block_body: (list(stmt), imm),
  block_ty: HTyp.t,
  block_complete: complete,
  block_label: label,
};
