type cursor_term = CursorInfo_common.cursor_term;

type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term: ZExp.t => cursor_term;

let extract_from_zline: ZExp.zline => cursor_term;

let extract_from_zexp_operand: ZExp.zoperand => cursor_term;

let extract_from_zrules: ZExp.zrules => cursor_term;

let extract_from_zrule: ZExp.zrule => cursor_term;

let extract_from_zexp_opseq: ZExp.zopseq => cursor_term;

let extract_from_zexp_zseq:
  ZSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator) =>
  cursor_term;

let get_zoperand: ZExp.t => option(zoperand);

let get_zoperand_from_zexp: ZExp.t => option(zoperand);

let get_zoperand_from_zline: ZExp.zline => option(zoperand);

let get_zoperand_from_zexp_opseq: ZExp.zopseq => option(zoperand);

let get_zoperand_from_zexp_operand: ZExp.zoperand => option(zoperand);

let get_zoperand_from_zrules: ZExp.zrules => option(zoperand);

let get_zoperand_from_zrule: ZExp.zrule => option(zoperand);

let get_outer_zrules: ZExp.t => option(ZExp.zrules);

let get_outer_zrules_from_zexp:
  (ZExp.t, option(ZExp.zrules)) => option(ZExp.zrules);

let get_outer_zrules_from_zline:
  (ZExp.zline, option(ZExp.zrules)) => option(ZExp.zrules);

let get_outer_zrules_from_zexp_opseq:
  (ZExp.zopseq, option(ZExp.zrules)) => option(ZExp.zrules);

let get_outer_zrules_from_zexp_operand:
  (ZExp.zoperand, option(ZExp.zrules)) => option(ZExp.zrules);

let get_outer_zrules_from_zrules: ZExp.zrules => option(ZExp.zrules);

let get_outer_zrules_from_zrule:
  (ZExp.zrule, option(ZExp.zrules)) => option(ZExp.zrules);

let cursor_on_outer_expr:
  ZExp.zoperand => option((ErrStatus.t, VarErrStatus.t));

let caret_is_after_zoperand: ZExp.t => bool;

let caret_is_before_zoperand: ZExp.t => bool;

let adjacent_is_emptyline: ZExp.t => (bool, bool);

let syn_cursor_info:
  (~steps: CursorPath_common.steps=?, Contexts.t, ZExp.t) =>
  option(CursorInfo_common.t);

let syn_cursor_info_zblock:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.t) =>
  option(CursorInfo_common.t);

let syn_cursor_info_line:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zline) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let syn_cursor_info_zopseq:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zopseq) =>
  option(CursorInfo_common.t);
// handle n-tuples:
// cannot simply defer to syn_cursor_info_skel here
// because it assumes binary tupling -- this would
// cause sub-tuples to synthesize sub-product types,
// but we want all comma operators in an opseq to
// show the complete product type

let syn_cursor_info_skel:
  (~steps: CursorPath_common.steps, Contexts.t, UHExp.skel, ZExp.zseq) =>
  option(CursorInfo_common.t);

let syn_cursor_info_zoperand:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zoperand) =>
  option(CursorInfo_common.t);

let ana_cursor_info:
  (~steps: CursorPath_common.steps=?, Contexts.t, ZExp.t, HTyp.t) =>
  option(CursorInfo_common.t);

let ana_cursor_info_zblock:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.t, HTyp.t) =>
  option(CursorInfo_common.t);

let ana_cursor_info_zopseq:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zopseq, HTyp.t) =>
  option(CursorInfo_common.t);

let ana_cursor_info_skel:
  (
    ~steps: CursorPath_common.steps,
    Contexts.t,
    UHExp.skel,
    ZExp.zseq,
    HTyp.t
  ) =>
  option(CursorInfo_common.t);

let ana_cursor_info_zoperand:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zoperand, HTyp.t) =>
  option(CursorInfo_common.t);

let syn_cursor_info_rule:
  (
    ~steps: CursorPath_common.steps,
    Contexts.t,
    ZExp.zrule,
    HTyp.t,
    CursorInfo_common.join_of_branches,
    int
  ) =>
  option(CursorInfo_common.t);

let ana_cursor_info_rule:
  (~steps: CursorPath_common.steps, Contexts.t, ZExp.zrule, HTyp.t, HTyp.t) =>
  option(CursorInfo_common.t);
