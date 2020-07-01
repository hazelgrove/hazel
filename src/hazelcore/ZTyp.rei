[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
and zoperand =
  | CursorT(CursorPosition.t, UHTyp.operand)
  | ParenthesizedZ(t)
  | ListZ(t)
and zoperator = (CursorPosition.t, UHTyp.operator);

type operand_surround = Seq.operand_surround(UHTyp.operand, UHTyp.operator);

type operator_surround = Seq.operator_surround(UHTyp.operand, UHTyp.operator);

type zseq = ZSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator);

let valid_cursors_operand: UHTyp.operand => list(CursorPosition.t);

let valid_cursors_operator: UHTyp.operator => list(CursorPosition.t);

let is_valid_cursor_operand: (CursorPosition.t, UHTyp.operand) => bool;

let is_valid_cursor_operator: (CursorPosition.t, UHTyp.operator) => bool;

let erase_zoperator: (('a, 'b)) => 'b;

let erase: t => UHTyp.t;

let erase_zopseq: t => UHTyp.t;

let erase_zoperand: zoperand => UHTyp.operand;

let mk_ZOpSeq: zseq => zopseq;

let erase_zseq:
  ZSeq.t(UHTyp.operand, 'a, zoperand, ('b, 'a)) => Seq.t(UHTyp.operand, 'a);

let is_before: t => bool;

let is_before_zopseq: t => bool;

let is_before_zoperand: zoperand => bool;

let is_before_zoperator: zoperator => bool;

let is_after: t => bool;

let is_after_zopseq: t => bool;

let is_after_zoperand: zoperand => bool;

let is_after_zoperator: zoperator => bool;

let place_before: UHTyp.t => t;

let place_before_opseq: UHTyp.t => t;

let place_before_operand: UHTyp.operand => zoperand;

let place_before_operator: UHTyp.operator => option(zoperator);

let place_after: UHTyp.t => t;

let place_after_opseq: UHTyp.t => t;

let place_after_operand: UHTyp.operand => zoperand;

let place_after_operator: UHTyp.operator => option(zoperator);

let place_cursor_operand:
  (CursorPosition.t, UHTyp.operand) => option(zoperand);

let place_cursor_operator:
  (CursorPosition.t, UHTyp.operator) => option(zoperator);

let move_cursor_left_zoperator: zoperator => option(zoperator);

let move_cursor_left: t => option(t);

let move_cursor_left_zopseq: t => option(t);

let move_cursor_left_zoperand: zoperand => option(zoperand);

let move_cursor_right_zoperator: zoperator => option(zoperator);

let move_cursor_right: t => option(t);

let move_cursor_right_zopseq: t => option(t);

let move_cursor_right_zoperand: zoperand => option(zoperand);
