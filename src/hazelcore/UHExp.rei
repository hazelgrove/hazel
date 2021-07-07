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
  | Lam(ErrStatus.t, UHPat.t, t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(CaseErrStatus.t, t, rules)
  | Parenthesized(t)
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
  | Label(LabelErrStatus.t, Label.t)
  | Prj(ErrStatus.t, operand, Label.t)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t)
and splice_info = SpliceInfo.t(t)
and splice_map = SpliceInfo.splice_map(t)
and binding = string; // placeholder

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

let label: (~err: LabelErrStatus.t=?, Label.t) => operand;

let prj: (~err: ErrStatus.t=?, operand, Label.t) => operand;

let lam: (~err: ErrStatus.t=?, UHPat.t, ~ann: UHTyp.t=?, t) => operand;

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

let get_projected_skel: (list(skel), seq, Label.t) => option(skel);

let mk_tuple: (~err: ErrStatus.t=?, list(skel)) => skel;

let set_duplicate_tuple_labels:
  (list(skel), list(HTyp.t), seq, MetaVarGen.t, Label.t) =>
  (seq, list(HTyp.t), bool);

let find_and_set_dupe_labels_tuple:
  (list(skel), list(HTyp.t), seq, MetaVarGen.t) => (seq, list(HTyp.t));

let find_and_clear_dupe_holes_labels_tuple:
  (list(skel), list(HTyp.t), seq, MetaVarGen.t) => (seq, list(HTyp.t));

let new_InvalidText: (MetaVarGen.t, string) => (operand, MetaVarGen.t);

/* helper function for constructing a new empty hole */
let new_EmptyHole: MetaVarGen.t => (operand, MetaVarGen.t);

let is_EmptyHole: operand => bool;

let empty_rule: MetaVarGen.t => (rule, MetaVarGen.t);

let get_err_status: t => ErrStatus.t;

let get_err_status_block: t => ErrStatus.t;

let get_err_status_opseq: opseq => ErrStatus.t;

let get_err_status_operand: operand => ErrStatus.t;

let set_err_status_opseq: (ErrStatus.t, opseq) => opseq;

let set_err_status_operand: (ErrStatus.t, operand) => operand;

let is_inconsistent: operand => bool;

let mk_inconsistent_opseq: (MetaVarGen.t, opseq) => (opseq, MetaVarGen.t);

let mk_inconsistent_operand:
  (MetaVarGen.t, operand) => (operand, MetaVarGen.t);

let text_operand: (MetaVarGen.t, TextShape.t) => (operand, MetaVarGen.t);

let associate: seq => Skel.t(Operators_Exp.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let is_complete: t => bool;
