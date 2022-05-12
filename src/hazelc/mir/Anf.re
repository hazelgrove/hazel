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
type pat =
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
  | ConstNil
  | ConstTriv

[@deriving sexp]
and imm = {imm_kind}

[@deriving sexp]
and imm_kind =
  | IConst(constant)
  | IVar(Var.t)

[@deriving sexp]
and comp = {comp_kind}

[@deriving sexp]
and comp_kind =
  | CImm(imm)
  | CBinOp(bin_op, imm, imm)
  | CAp(imm, list(imm))
  | CLam(list(pat), prog)
  | CCons(imm, imm)
  | CPair(imm, imm)
  | CInj(inj_side, imm)
  | CEmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(comp))
  | CNonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(comp),
      imm,
    )

[@deriving sexp]
and inj_side =
  | CInjL
  | CInjR

[@deriving sexp]
and stmt = {stmt_kind}

[@deriving sexp]
and stmt_kind =
  | SLet(pat, rec_flag, comp)

[@deriving sexp]
and rec_flag =
  | NoRec
  | Rec

[@deriving sexp]
and prog = {prog_body}

[@deriving sexp]
and prog_body = (list(stmt), comp);
