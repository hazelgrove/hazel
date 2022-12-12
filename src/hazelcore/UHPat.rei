exception FreeVarInPat;

[@deriving sexp]
type operator = Operators_Pat.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | TypeAnn(ErrStatus.t, operand, UHTyp.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | IntLit(ErrStatus.t, string)
  | FloatLit(ErrStatus.t, string)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Parenthesized(t)
  | Inj(ErrStatus.t, InjSide.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);

[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

let var: (~err: ErrStatus.t=?, ~var_err: VarErrStatus.t=?, Var.t) => operand;

let wild: (~err: ErrStatus.t=?, unit) => operand;

let boollit: (~err: ErrStatus.t=?, bool) => operand;

let intlit: (~err: ErrStatus.t=?, string) => operand;

let floatlit: (~err: ErrStatus.t=?, string) => operand;

let listnil: (~err: ErrStatus.t=?, unit) => operand;

let get_tuple_elements: skel => list(skel);

let mk_tuple: (~err: ErrStatus.t=?, list(skel)) => skel;

let new_InvalidText: (IDGen.t, string) => (operand, IDGen.t) /* helper function for constructing a new empty hole */;

let new_EmptyHole: IDGen.t => (operand, IDGen.t);

let is_EmptyHole: operand => bool;

let get_err_status: t => ErrStatus.t;

let get_err_status_opseq: t => ErrStatus.t;

let get_err_status_operand: operand => ErrStatus.t;

let set_err_status: (ErrStatus.t, t) => t;

let set_err_status_opseq: (ErrStatus.t, t) => t;

let set_err_status_operand: (ErrStatus.t, operand) => operand /* put p in a new hole, if it is not already in a hole */;

let mk_inconsistent: (IDGen.t, t) => (t, IDGen.t);

let mk_inconsistent_opseq: (IDGen.t, t) => (t, IDGen.t);

let mk_inconsistent_operand: (IDGen.t, operand) => (operand, IDGen.t);

let text_operand: (IDGen.t, TextShape.t) => (operand, IDGen.t);

let associate: seq => Skel.t(Operators_Pat.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let is_complete: t => bool;
