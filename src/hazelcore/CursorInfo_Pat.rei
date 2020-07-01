type cursor_term = CursorInfo_common.cursor_term;

type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term: ZPat.t => cursor_term;

let extract_cursor_pat_zseq:
  ZSeq.t(UHPat.operand, UHPat.operator, ZPat.zoperand, ZPat.zoperator) =>
  cursor_term;

let extract_from_zpat_operand: ZPat.zoperand => cursor_term;

let get_zoperand_from_zpat: ZPat.t => option(zoperand);

let get_zoperand_from_zpat_opseq: ZPat.t => option(zoperand);

let get_zoperand_from_zpat_operand: ZPat.zoperand => option(zoperand);

let syn_cursor_info:
  (~steps: CursorPath_common.steps=?, Contexts.t, ZPat.t) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let syn_cursor_info_zopseq:
  (~steps: CursorPath_common.steps, Contexts.t, ZPat.t) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));
// handle n-tuples:
// cannot simply defer to ana_cursor_info_skel here
// because it assumes binary tupling -- this would
// cause sub-tuples to synthesize sub-product types,
// but we want all comma operators in an opseq to
// show the complete product type

let syn_cursor_info_skel:
  (~steps: CursorPath_common.steps, Contexts.t, UHPat.skel, ZPat.zseq) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let syn_cursor_info_zoperand:
  (~steps: CursorPath_common.steps, Contexts.t, ZPat.zoperand) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let ana_cursor_info:
  (~steps: CursorPath_common.steps, Contexts.t, ZPat.t, HTyp.t) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let ana_cursor_info_zopseq:
  (~steps: CursorPath_common.steps, Contexts.t, ZPat.t, HTyp.t) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));
// handle n-tuples:
// cannot simply defer to ana_cursor_info_skel here
// because it assumes binary tupling -- this would
// cause sub-tuples to synthesize sub-product types,
// but we want all comma operators in an opseq to
// show the complete product type
let ana_cursor_info_skel:
  (
    ~steps: CursorPath_common.steps,
    Contexts.t,
    UHPat.skel,
    ZPat.zseq,
    HTyp.t
  ) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));

let ana_cursor_info_zoperand:
  (~steps: CursorPath_common.steps, Contexts.t, ZPat.zoperand, HTyp.t) =>
  option(CursorInfo_common.deferrable(CursorInfo_common.t));
