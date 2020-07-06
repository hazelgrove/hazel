[@deriving sexp]
type operator = Operators_Exp.t;

// TODO
// type t =
// /* laid out vertically */
// | V(block)
// /* laid out horizontally */
// | H(opseq)
[@deriving sexp]
type t = block
// TODO
// block = (bool /* user newline */, list(line))
and block = list(line)
and line =
  | EmptyLine
  | CommentLine(string)
  | LetLine(UHPat.t, option(UHTyp.t), t)
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
  | Lam(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(CaseErrStatus.t, t, rules)
  | Parenthesized(t)
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t)
and splice_info = SpliceInfo.t(t)
and splice_map = SpliceInfo.splice_map(t);

[@deriving sexp]
type skel = OpSeq.skel(operator);

[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

type affix = Seq.affix(operand, operator);

let find_line: t => line;

let find_line_block: t => line;

let find_line_line: line => line;

let letline: (UHPat.t, ~ann: UHTyp.t=?, t) => line;

let var: (~err: ErrStatus.t=?, ~var_err: VarErrStatus.t=?, Var.t) => operand;

let intlit: (~err: ErrStatus.t=?, string) => operand;

let floatlit: (~err: ErrStatus.t=?, string) => operand;

let boollit: (~err: ErrStatus.t=?, bool) => operand;

let lam: (~err: ErrStatus.t=?, UHPat.t, ~ann: UHTyp.t=?, t) => operand;

let case: (~err: CaseErrStatus.t=?, t, rules) => operand;

let listnil: (~err: ErrStatus.t=?, unit) => operand;

module Line: {
  let prune_empty_hole: line => line;

  let get_opseq: line => option(opseq);

  let force_get_opseq: line => opseq;
};

module Lines: {let prune_empty_holes: list(line) => list(line);};

module Block: {
  let wrap': opseq => block;

  let wrap: operand => block;

  let num_lines: block => int;

  let prune_empty_hole_lines: block => block;

  let split_conclusion: block => option((list(line), opseq));

  let force_split_conclusion: block => (list(line), opseq);

  let join_conclusion: (list(line), opseq) => block;
};

let get_tuple_elements: skel => list(skel);

let mk_tuple: (~err: ErrStatus.t=?, list(skel)) => skel;

let new_InvalidText: (MetaVarGen.t, string) => (operand, MetaVarGen.t);

/* helper function for constructing a new empty hole */
let new_EmptyHole: MetaVarGen.t => (operand, MetaVarGen.t);

let is_EmptyHole: operand => bool;

let empty_rule: MetaVarGen.t => (rule, MetaVarGen.t);

/* put e in the specified hole */
let get_err_status: t => ErrStatus.t;

let get_err_status_block: t => ErrStatus.t;

let get_err_status_opseq: opseq => ErrStatus.t;

let get_err_status_operand: operand => ErrStatus.t;

let set_err_status: (ErrStatus.t, t) => t;

let set_err_status_block: (ErrStatus.t, t) => block;

let set_err_status_opseq: (ErrStatus.t, opseq) => opseq;

let set_err_status_operand: (ErrStatus.t, operand) => operand;

let is_inconsistent: operand => bool;

/* put e in a new hole, if it is not already in a hole */
let mk_inconsistent: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_block: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_opseq: (MetaVarGen.t, opseq) => (opseq, MetaVarGen.t);

let mk_inconsistent_operand:
  (MetaVarGen.t, operand) => (operand, MetaVarGen.t);

let drop_outer_parentheses: operand => t;

let text_operand: (MetaVarGen.t, TextShape.t) => (operand, MetaVarGen.t);

let associate: seq => Skel.t(Operators_Exp.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let is_complete_line: (line, bool) => bool;

let is_complete_block: (block, bool) => bool;

let is_complete_rule: (rule, bool) => bool;

let is_complete_rules: (rules, bool) => bool;

let is_complete_operand: (operand, bool) => bool;

let is_complete: (t, bool) => bool;
