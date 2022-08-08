/**
  Linearized form with explicit completeness conversions.
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
  | ConstNil(Typ.t_)
  | ConstTriv

[@deriving sexp]
and imm = {
  imm_kind,
  imm_label: ExprLabel.t,
}

[@deriving sexp]
and imm_kind =
  | IConst(constant)
  | IVar(Ident.t)

[@deriving sexp]
and comp = {
  comp_kind,
  comp_label: ExprLabel.t,
}

[@deriving sexp]
and comp_kind =
  | CImm(imm)
  | CBinNC(bin_op, imm, imm)
  | CBinNI(bin_op, imm, imm)
  | /** Application of a necessarily complete function.  */
    CApNC(imm, imm)
  | /** Application of a necessarily incomplete function. */
    CApNI(imm, imm)
  | /** Function that takes a necessarily complete argument. */
    CFunNC(
      (Ident.t, Typ.t_),
      block,
    )
  | /** Function that takes a necessarily incomplete argument. */
    CFunNI(
      (Ident.t, Typ.t_),
      block,
    )
  | CConsNC(imm, imm)
  | CConsNI(imm, imm)
  | CPairNC(imm, imm)
  | CPairNI(imm, imm)
  | CInjNC(Typ.t_, inj_side, imm)
  | CInjNI(Typ.t_, inj_side, imm)
  | CCase(imm, list(rule))
  | CEHole(MetaVar.t, MetaVarInst.t, sigma)
  | /**
      Wrap a necessarily complete expression into an non-empty hole.
     */
    CNEHole(
      HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      sigma,
      imm,
    )
  | /**
      Cast a necessarily complete expression from one base type to another.
     */
    CCastNC(
      imm,
      Typ.t_,
      Typ.t_,
    )
  | /**
      Cast a necessarily incomplete expression from one base type to another.
     */
    CCastNI(
      imm,
      Typ.t_,
      Typ.t_,
    )
  | /**
      Wrap a necessarily complete expression into a necessarily incomplete one.
     */
    CNIWrapNC(
      imm,
    )
  | /**
      Wrap a necessarily complete expression into an indeterminately incomplete
      one.
     */
    CIIWrapNC(
      imm,
    )
  | /**
      Wrap a necessarily incomplete expression into an indeterminately
      incomplete one.
     */
    CIIWrapNI(
      imm,
    )

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
  rule_label: RuleLabel.t,
}

[@deriving sexp]
and stmt = {
  stmt_kind,
  stmt_label: StmtLabel.t,
}

[@deriving sexp]
and stmt_kind =
  | SLet(Ident.t, comp)
  | SLetRec(Ident.t, (Ident.t, Typ.t), (Typ.t, block))
  | /**
      Branching construct that takes an indeterminately incomplete result and
      branches based on its completeness at runtime.
     */
    SSwitch(
      imm,
      Ident.t,
      block,
      block,
    )

[@deriving sexp]
and block = {
  block_body: (list(stmt), imm),
  block_label: ExprLabel.t,
};
