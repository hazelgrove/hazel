let operator_of_shape: Action_common.operator_shape => option(UHPat.operator);

let shape_of_operator: UHPat.operator => Action_common.operator_shape;

let has_Comma: ZPat.zopseq => bool;

type syn_success = (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

type ana_success = (ZPat.t, Contexts.t, MetaVarGen.t);

let mk_and_syn_fix_ZOpSeq:
  (Contexts.t, MetaVarGen.t, ZPat.zseq) => syn_success;

let mk_and_ana_fix_ZOpSeq:
  (Contexts.t, MetaVarGen.t, ZPat.zseq, HTyp.t) => ana_success;

let mk_syn_result:
  (Contexts.t, MetaVarGen.t, ZPat.t) => Action_common.Outcome.t(syn_success);

let mk_ana_result:
  (Contexts.t, MetaVarGen.t, ZPat.t, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

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

let delete_operator:
  Seq.operator_surround(UHPat.operand, UHPat.operator) =>
  ZSeq.t(UHPat.operand, UHPat.operator, ZPat.zoperand, ZPat.zoperator);

let construct_operator_before_zoperand:
  (
    MetaVarGen.t,
    UHPat.operator,
    ZPat.zoperand,
    Seq.operand_surround(UHPat.operand, UHPat.operator)
  ) =>
  (
    ZSeq.t(UHPat.operand, UHPat.operator, ZPat.zoperand, ZPat.zoperator),
    MetaVarGen.t,
  );

let construct_operator_after_zoperand:
  (
    MetaVarGen.t,
    UHPat.operator,
    ZPat.zoperand,
    Seq.operand_surround(UHPat.operand, UHPat.operator)
  ) =>
  (
    ZSeq.t(UHPat.operand, UHPat.operator, ZPat.zoperand, ZPat.zoperator),
    MetaVarGen.t,
  );

let complete_tuple:
  (MetaVarGen.t, OpSeq.t(UHPat.operand, UHPat.operator), HTyp.t) =>
  (
    ZOpSeq.t(UHPat.operand, UHPat.operator, ZPat.zoperand, ZPat.zoperator),
    MetaVarGen.t,
  );

let resurround_z: (ZPat.t, ZPat.operand_surround) => ZPat.zseq;

let syn_move:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t) =>
  Action_common.Outcome.t(syn_success);

let ana_move:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let syn_perform:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t) =>
  Action_common.Outcome.t(syn_success);

let syn_perform_opseq:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t) =>
  Action_common.Outcome.t(syn_success);

let syn_perform_operand:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.zoperand) =>
  Action_common.Outcome.t(syn_success);

let ana_perform:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let ana_perform_opseq:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.t, HTyp.t) =>
  Action_common.Outcome.t(ana_success);

let ana_perform_operand:
  (Contexts.t, MetaVarGen.t, Action_common.t, ZPat.zoperand, HTyp.t) =>
  Action_common.Outcome.t(ana_success);
