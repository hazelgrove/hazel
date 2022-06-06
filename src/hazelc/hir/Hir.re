/*
   This module defines the high-level intermediate representation, which has an
   expression form.
 */
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
type expr = {expr_kind}

[@deriving sexp]
and expr_kind =
  /* Holes */
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
  | EInvalidOperation(expr, InvalidOperationError.t)
  /* Casts */
  | ECast(expr, HTyp.t, HTyp.t)
  | EFailedCast(expr, HTyp.t, HTyp.t)
  /* Case */
  | EConsistentCase(case)
  | EInconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(expr), case)
  /* Let bindings */
  | ELet(pat, expr, expr)
  | ELetRec(Var.t, pat, HTyp.t, expr, expr)
  /* Lambda */
  | ELam(pat, HTyp.t, expr)
  /* Application */
  | EAp(expr, expr)
  | EApBuiltin(Var.t, list(expr))
  /* Binary operations */
  | EBinBoolOp(bin_bool_op, expr, expr)
  | EBinIntOp(bin_int_op, expr, expr)
  | EBinFloatOp(bin_float_op, expr, expr)
  /* Pair */
  | EPair(expr, expr)
  /* Cons */
  | ECons(expr, expr)
  /* Sum injection */
  | EInj(HTyp.t, InjSide.t, expr)
  /* Immediate expressions; we can deduce the type and indet-ness of these trivially. */
  | EBoundVar(HTyp.t, Var.t)
  | EBoolLit(bool)
  | EIntLit(int)
  | EFloatLit(float)
  | ENil(HTyp.t)
  | ETriv

[@deriving sexp]
and case = {case_kind}

[@deriving sexp]
and case_kind =
  | ECase(expr, list(rule), int)

[@deriving sexp]
and rule = {rule_kind}

[@deriving sexp]
and rule_kind =
  | ERule(pat, expr)

[@deriving sexp]
and pat = {pat_kind}

[@deriving sexp]
and pat_kind =
  /* Holes */
  | PEmptyHole(MetaVar.t, MetaVarInst.t)
  | PNonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, pat)
  | PKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | PInvalidText(MetaVar.t, MetaVarInst.t, string)
  /* Non-holes */
  | PAp(pat, pat)
  | PPair(pat, pat)
  | PCons(pat, pat)
  | PInj(InjSide.t, pat)
  | PWild
  | PVar(Var.t)
  | PIntLit(int)
  | PFloatLit(float)
  | PBoolLit(bool)
  | PNil
  | PTriv;
