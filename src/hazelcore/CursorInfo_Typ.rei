type cursor_term = CursorInfo_common.cursor_term;

type zoperand = CursorInfo_common.zoperand;

let extract_cursor_term: ZTyp.t => cursor_term;

let extract_from_ztyp_operand: ZTyp.zoperand => cursor_term;

let get_zoperand_from_ztyp: ZTyp.t => option(zoperand);

let get_zoperand_from_ztyp_opseq: ZTyp.t => option(zoperand);

let get_zoperand_from_ztyp_operand: ZTyp.zoperand => option(zoperand);

let cursor_info:
  (~steps: 'a, Contexts.t, ZTyp.t) => option(CursorInfo_common.t);
