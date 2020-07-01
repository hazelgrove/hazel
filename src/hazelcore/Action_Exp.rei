let operator_of_shape: Action_common.operator_shape => option(UHExp.operator);

let shape_of_operator: UHExp.operator => option(Action_common.operator_shape);

let has_Comma: ZExp.zopseq => bool;

let mk_and_syn_fix_OpSeq:
  (Contexts.t, MetaVarGen.t, UHExp.seq) => (UHExp.opseq, HTyp.t, MetaVarGen.t);

let mk_and_ana_fix_OpSeq:
  (Contexts.t, MetaVarGen.t, UHExp.seq, HTyp.t) => (UHExp.opseq, MetaVarGen.t);

let mk_and_syn_fix_ZOpSeq:
  (Contexts.t, MetaVarGen.t, ZExp.zseq) => (ZExp.t, HTyp.t, MetaVarGen.t);

let mk_and_ana_fix_ZOpSeq:
  (Contexts.t, MetaVarGen.t, ZExp.zseq, HTyp.t) => (ZExp.t, MetaVarGen.t);

/**
     * Used to construct an expression from an opseq suffix that
     * follows a keyword when the user hits space after the keyword.
     * If the first operation is a space, then what follows the space
     * becomes the new expression. Otherwise, a new hole is generated,
     * prepended to the suffix, and the reuslting opseq becomes the
     * new expression.
     */
let keyword_suffix_to_opseq:
  (Seq.affix(UHExp.operand, UHExp.operator), MetaVarGen.t) =>
  (UHExp.opseq, MetaVarGen.t);

let keyword_action: ExpandingKeyword.t => Action_common.t;

let delete_operator:
  Seq.operator_surround(UHExp.operand, UHExp.operator) =>
  ZSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator);

let construct_operator_before_zoperand:
  (
    MetaVarGen.t,
    UHExp.operator,
    ZExp.zoperand,
    Seq.operand_surround(UHExp.operand, UHExp.operator)
  ) =>
  (
    ZSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator),
    MetaVarGen.t,
  );

let construct_operator_after_zoperand:
  (
    MetaVarGen.t,
    UHExp.operator,
    ZExp.zoperand,
    Seq.operand_surround(UHExp.operand, UHExp.operator)
  ) =>
  (
    ZSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator),
    MetaVarGen.t,
  );

let complete_tuple:
  (MetaVarGen.t, OpSeq.t(UHExp.operand, UHExp.operator), HTyp.t) =>
  (
    ZOpSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator),
    MetaVarGen.t,
  );

let lines_of_prefix:
  (MetaVarGen.t, UHExp.affix) => (list(UHExp.line), MetaVarGen.t);

let lines_of_suffix:
  (MetaVarGen.t, UHExp.affix) => (list(UHExp.line), MetaVarGen.t);

let resurround:
  (MetaVarGen.t, UHExp.t, ZExp.operand_surround) => (UHExp.t, MetaVarGen.t);

let resurround_z:
  (MetaVarGen.t, ZExp.t, ZExp.operand_surround) => (ZExp.t, MetaVarGen.t);

type expanding_result = {
  kw: ExpandingKeyword.t,
  u_gen: MetaVarGen.t,
  prefix: list(UHExp.line),
  subject: UHExp.t,
  suffix: list(UHExp.line),
};

type line_success =
  | LineExpands(expanding_result)
  | LineDone((ZExp.zblock, Contexts.t, MetaVarGen.t));

type syn_done = (ZExp.t, HTyp.t, MetaVarGen.t);

type syn_success =
  | SynExpands(expanding_result)
  | SynDone(syn_done);

let mk_SynExpandsToCase:
  (
    ~u_gen: MetaVarGen.t,
    ~prefix: list(UHExp.line)=?,
    ~suffix: list(UHExp.line)=?,
    ~scrut: UHExp.t,
    unit
  ) =>
  syn_success;

let mk_SynExpandsToLet:
  (
    ~u_gen: MetaVarGen.t,
    ~prefix: list(UHExp.line)=?,
    ~suffix: list(UHExp.line)=?,
    ~def: UHExp.t,
    unit
  ) =>
  syn_success;

let wrap_in_SynDone:
  Action_common.Outcome.t(syn_done) => Action_common.Outcome.t(syn_success);

type ana_done = (ZExp.t, MetaVarGen.t);

type ana_success =
  | AnaExpands(expanding_result)
  | AnaDone(ana_done);

let mk_AnaExpandsToCase:
  (
    ~u_gen: MetaVarGen.t,
    ~prefix: list(UHExp.line)=?,
    ~suffix: list(UHExp.line)=?,
    ~scrut: UHExp.t,
    unit
  ) =>
  ana_success;

let mk_AnaExpandsToLet:
  (
    ~u_gen: MetaVarGen.t,
    ~prefix: list(UHExp.line)=?,
    ~suffix: list(UHExp.line)=?,
    ~def: UHExp.t,
    unit
  ) =>
  ana_success;

let wrap_in_AnaDone:
  Action_common.Outcome.t(ana_done) => Action_common.Outcome.t(ana_success);

let zcase_of_scrut_and_suffix:
  (MetaVarGen.t, UHExp.t, list(UHExp.line)) => (ZExp.zoperand, MetaVarGen.t);

let mk_syn_text:
  (Contexts.t, MetaVarGen.t, int, string) =>
  Action_common.Outcome.t(syn_success);

let mk_ana_text:
  (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_insert_text:
  (Contexts.t, MetaVarGen.t, (int, string), string) =>
  Action_common.Outcome.t(syn_success);

let ana_insert_text:
  (Contexts.t, MetaVarGen.t, (int, string), string, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_backspace_text:
  (Contexts.t, MetaVarGen.t, int, string) =>
  Action_common.Outcome.t(syn_success);

let ana_backspace_text:
  (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_delete_text:
  (Contexts.t, MetaVarGen.t, int, string) =>
  Action_common.Outcome.t(syn_success);

let ana_delete_text:
  (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_split_text:
  (Contexts.t, MetaVarGen.t, int, Action_common.operator_shape, string) =>
  Action_common.Outcome.t(syn_success);

let ana_split_text:
  (
    Contexts.t,
    MetaVarGen.t,
    int,
    Action_common.operator_shape,
    string,
    HTyp.t
  ) =>
  Action_common.Outcome.t(ana_success);

let syn_move:
  (Contexts.t, Action_common.t, (ZExp.t, HTyp.t, MetaVarGen.t)) =>
  Action_common.Outcome.t(syn_success);

let ana_move:
  (Contexts.t, Action_common.t, (ZExp.t, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_perform:
  (Contexts.t, Action_common.t, syn_done) => Action_common.Outcome.t(syn_done);

let syn_perform_block:
  (Contexts.t, Action_common.t, (ZExp.t, HTyp.t, MetaVarGen.t)) =>
  Action_common.Outcome.t(syn_success);

let syn_perform_line:
  (Contexts.t, Action_common.t, (ZExp.zline, MetaVarGen.t)) =>
  Action_common.Outcome.t(line_success);

let syn_perform_opseq:
  (Contexts.t, Action_common.t, (ZExp.zopseq, HTyp.t, MetaVarGen.t)) =>
  Action_common.Outcome.t(syn_success);

let syn_perform_operand:
  (Contexts.t, Action_common.t, (ZExp.zoperand, HTyp.t, MetaVarGen.t)) =>
  Action_common.Outcome.t(syn_success);

let syn_perform_rules:
  (Contexts.t, Action_common.t, (ZExp.zrules, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t((ZExp.zrules, MetaVarGen.t));

let ana_perform_rules:
  (
    Contexts.t,
    Action_common.t,
    (ZExp.zrules, MetaVarGen.t),
    HTyp.t,
    HTyp.t
  ) =>
  Action_common.Outcome.t((ZExp.zrules, MetaVarGen.t));

let ana_perform:
  (Contexts.t, Action_common.t, ana_done, HTyp.t) =>
  Action_common.Outcome.t(ana_done);

let ana_perform_block:
  (Contexts.t, Action_common.t, (ZExp.t, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let ana_perform_opseq:
  (Contexts.t, Action_common.t, (ZExp.zopseq, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let ana_perform_operand:
  (Contexts.t, Action_common.t, (ZExp.zoperand, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let ana_perform_subsume:
  (Contexts.t, Action_common.t, (ZExp.zoperand, MetaVarGen.t), HTyp.t) =>
  Action_common.Outcome.t(ana_success);
