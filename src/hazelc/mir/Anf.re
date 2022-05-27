/**
 * This module defines a linearized intermediate representation.
 */
open Sexplib.Std;

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
type has_indet = bool;

[@deriving sexp]
type pat = {
  pat_kind,
  pat_indet: has_indet,
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
  imm_indet: has_indet,
}

[@deriving sexp]
and imm_kind =
  | IConst(constant)
  | IVar(Var.t)

[@deriving sexp]
and comp = {
  comp_kind,
  comp_ty: HTyp.t,
  comp_indet: has_indet,
}

[@deriving sexp]
and comp_kind =
  | CImm(imm)
  | CBinOp(bin_op, imm, imm)
  | CAp(imm, list(imm))
  | CLam(list(pat), prog)
  | CCons(imm, imm)
  | CPair(imm, imm)
  | CInj(inj_side, imm)
  | CCase(imm, list(rule))
  | CEmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(comp))
  | CNonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(comp),
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
  rule_branch: prog,
  rule_indet: has_indet,
}

[@deriving sexp]
and stmt = {
  stmt_kind,
  stmt_indet: has_indet,
}

[@deriving sexp]
and stmt_kind =
  | SLet(pat, comp)
  | SLetRec(Var.t, comp)

[@deriving sexp]
and prog = {
  prog_body,
  prog_ty: HTyp.t,
  prog_indet: has_indet,
}

[@deriving sexp]
and prog_body = (list(stmt), comp);

module Imm = {
  let mk_var = (x: Var.t, c: comp): imm => {
    imm_kind: IVar(x),
    imm_ty: c.comp_ty,
    imm_indet: c.comp_indet,
  };
};

module Comp = {
  let mk_imm = (im: imm): comp => {
    comp_kind: CImm(im),
    comp_ty: im.imm_ty,
    comp_indet: im.imm_indet,
  };
};

module Prog = {
  let mk = (body: list(stmt), c: comp): prog => {
    prog_body: (body, c),
    prog_ty: c.comp_ty,
    prog_indet: c.comp_indet,
  };
};
