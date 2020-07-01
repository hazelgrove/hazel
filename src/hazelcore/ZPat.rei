[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator)
and zoperand =
  | CursorP(CursorPosition.t, UHPat.operand)
  | ParenthesizedZ(t)
  | InjZ(ErrStatus.t, InjSide.t, t)
and zoperator = (CursorPosition.t, UHPat.operator);

type operand_surround = Seq.operand_surround(UHPat.operand, UHPat.operator);

type operator_surround = Seq.operator_surround(UHPat.operand, UHPat.operator);

type zseq = ZSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator);

let valid_cursors_operand: UHPat.operand => list(CursorPosition.t);

let valid_cursors_operator: UHPat.operator => list(CursorPosition.t);

let is_valid_cursor_operand: (CursorPosition.t, UHPat.operand) => bool;

let is_valid_cursor_operator: (CursorPosition.t, UHPat.operator) => bool;

let set_err_status: (ErrStatus.t, t) => t;

let set_err_status_zopseq: (ErrStatus.t, t) => t;

let set_err_status_zoperand: (ErrStatus.t, zoperand) => zoperand;

let mk_inconsistent: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_zopseq: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_zoperand:
  (MetaVarGen.t, zoperand) => (zoperand, MetaVarGen.t);

let erase: t => UHPat.t;

let erase_zopseq: t => UHPat.t;

let erase_zseq:
  ZSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator) =>
  OpSeq.seq(UHPat.operand, UHPat.operator);

let erase_zoperand: zoperand => UHPat.operand;

let erase_zoperator: zoperator => UHPat.operator;

let mk_ZOpSeq:
  ZSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator) =>
  ZOpSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator);

let is_before: t => bool;

let is_before_zopseq: t => bool;

let is_before_zoperand: zoperand => bool;

let is_before_zoperator: zoperator => bool;

let is_after: t => bool;

let is_after_zopseq: t => bool;

let is_after_zoperand: zoperand => bool;

let is_after_zoperator: zoperator => bool;

let place_before: UHPat.t => t;

let place_before_opseq: UHPat.t => t;

let place_before_operand: UHPat.operand => zoperand;

let place_before_operator: UHPat.operator => option(zoperator);

let place_after: UHPat.t => t;

let place_after_opseq: UHPat.t => t;

let place_after_operand: UHPat.operand => zoperand;

let place_after_operator: UHPat.operator => option(zoperator);

let place_cursor_operand:
  (CursorPosition.t, UHPat.operand) => option(zoperand);

let place_cursor_operator:
  (CursorPosition.t, UHPat.operator) => option(zoperator);

/* helper function for constructing a new empty hole */
let new_EmptyHole: MetaVarGen.t => (zoperand, MetaVarGen.t);

let is_inconsistent: t => bool;

let move_cursor_left_zoperator: zoperator => option(zoperator);

let move_cursor_left: t => option(t);

let move_cursor_left_zopseq: t => option(t);

let move_cursor_left_zoperand: zoperand => option(zoperand);

let move_cursor_right_zoperator: zoperator => option(zoperator);

let move_cursor_right: t => option(t);

let move_cursor_right_zopseq: t => option(t);

let move_cursor_right_zoperand: zoperand => option(zoperand);
