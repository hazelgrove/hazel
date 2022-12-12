[@deriving sexp]
type operator = Operators_Exp.t;

[@deriving sexp]
type t = block
and block = list(line)
and line =
  | EmptyLine
  | CommentLine(string)
  | LetLine(UHPat.t, t)
  | ExpLine(opseq)
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | IntLit(ErrStatus.t, string)
  | FloatLit(ErrStatus.t, string)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Fun(ErrStatus.t, UHPat.t, t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(CaseErrStatus.t, t, rules)
  | Parenthesized(t)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);

[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

type affix = Seq.affix(operand, operator);

let letline: (UHPat.t, t) => line;

let var: (~err: ErrStatus.t=?, ~var_err: VarErrStatus.t=?, Var.t) => operand;

let intlit: (~err: ErrStatus.t=?, string) => operand;

let floatlit: (~err: ErrStatus.t=?, string) => operand;

let boollit: (~err: ErrStatus.t=?, bool) => operand;

let lam: (~err: ErrStatus.t=?, UHPat.t, t) => operand;

let case: (~err: CaseErrStatus.t=?, t, rules) => operand;

let listnil: (~err: ErrStatus.t=?, unit) => operand;

module Line: {
  let prune_empty_hole: line => line;

  let force_get_opseq: line => opseq;
};

module Lines: {let prune_empty_holes: list(line) => list(line);};

module Block: {
  let wrap': opseq => block;

  let wrap: operand => block;

  let num_lines: block => int;

  let prune_empty_hole_lines: block => block;

  let split_conclusion: block => option((list(line), opseq));
};

let get_tuple_elements: skel => list(skel);

let mk_tuple: (~err: ErrStatus.t=?, list(skel)) => skel;

let new_InvalidText: (IDGen.t, string) => (operand, IDGen.t) /* helper function for constructing a new empty hole */;

let new_EmptyHole: IDGen.t => (operand, IDGen.t);

let is_EmptyHole: operand => bool;

let empty_rule: IDGen.t => (rule, IDGen.t);

let get_err_status: t => ErrStatus.t;

let get_err_status_block: t => ErrStatus.t;

let get_err_status_opseq: opseq => ErrStatus.t;

let get_err_status_operand: operand => ErrStatus.t;

let set_err_status_opseq: (ErrStatus.t, opseq) => opseq;

let set_err_status_operand: (ErrStatus.t, operand) => operand;

let mk_inconsistent_opseq: (IDGen.t, opseq) => (opseq, IDGen.t);

let mk_inconsistent_operand: (IDGen.t, operand) => (operand, IDGen.t);

let text_operand: (IDGen.t, TextShape.t) => (operand, IDGen.t);

let associate: seq => Skel.t(Operators_Exp.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let is_complete: t => bool;
