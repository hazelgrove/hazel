[@deriving sexp]
type t = zblock
and zblock = ZList.t(zline, UHExp.line)
and zline =
  | CursorL(CursorPosition.t, UHExp.line)
  | ExpLineZ(zopseq)
  | LetLineZP(ZPat.t, option(UHTyp.t), UHExp.t)
  | LetLineZA(UHPat.t, ZTyp.t, UHExp.t)
  | LetLineZE(UHPat.t, option(UHTyp.t), t)
and zopseq = ZOpSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator)
and zoperand =
  | CursorE(CursorPosition.t, UHExp.operand)
  | ParenthesizedZ(t)
  | LamZP(ErrStatus.t, ZPat.t, option(UHTyp.t), UHExp.t)
  | LamZA(ErrStatus.t, UHPat.t, ZTyp.t, UHExp.t)
  | LamZE(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | InjZ(ErrStatus.t, InjSide.t, t)
  | CaseZE(CaseErrStatus.t, t, list(UHExp.rule))
  | CaseZR(CaseErrStatus.t, UHExp.t, zrules)
  | ApPaletteZ(
      ErrStatus.t,
      PaletteName.t,
      SerializedModel.t,
      ZSpliceInfo.t(UHExp.t, t),
    )
and zoperator = (CursorPosition.t, UHExp.operator)
and zrules = ZList.t(zrule, UHExp.rule)
and zrule =
  | CursorR(CursorPosition.t, UHExp.rule)
  | RuleZP(ZPat.t, UHExp.t)
  | RuleZE(UHPat.t, t);

type operand_surround = Seq.operand_surround(UHExp.operand, UHExp.operator);

type operator_surround = Seq.operator_surround(UHExp.operand, UHExp.operator);

type zseq = ZSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator);

let prune_type_annotation: zoperand => zoperand;

let line_can_be_swapped: zline => bool;

let valid_cursors_line: UHExp.line => list(CursorPosition.t);

let valid_cursors_operator: UHExp.operator => list(CursorPosition.t);

let valid_cursors_operand: UHExp.operand => list(CursorPosition.t);

let valid_cursors_rule: UHExp.rule => list(CursorPosition.t);

let is_valid_cursor_line: (CursorPosition.t, UHExp.line) => bool;

let is_valid_cursor_operand: (CursorPosition.t, UHExp.operand) => bool;

let is_valid_cursor_operator: (CursorPosition.t, UHExp.operator) => bool;

let is_valid_cursor_rule: (CursorPosition.t, UHExp.rule) => bool;

module ZLine: {let force_get_zopseq: zline => zopseq;};

module ZBlock: {
  let wrap': zopseq => zblock;
  let wrap: zoperand => zblock;
};

let is_before: t => bool;

let is_before_zblock: t => bool;

let is_before_zline: zline => bool;

let is_before_zopseq: zopseq => bool;

let is_before_zoperand: zoperand => bool;

let is_before_zrule: zrule => bool;

let is_before_zoperator: zoperator => bool;

let is_after: t => bool;

let is_after_zblock: t => bool;

let is_after_zline: zline => bool;

let is_after_zopseq: zopseq => bool;

let is_after_zoperand: zoperand => bool;

let is_after_zrule: zrule => bool;

let is_after_zoperator: zoperator => bool;

let is_outer: t => bool;

let is_outer_zblock: t => bool;

let is_outer_zline: zline => bool;

let is_outer_zopseq: zopseq => bool;

let is_outer_zoperand: zoperand => bool;

let place_before: UHExp.t => t;

let place_before_block: UHExp.t => t;

let place_before_line: UHExp.line => zline;

let place_before_opseq: UHExp.opseq => zopseq;

let place_before_operand: UHExp.operand => zoperand;

let place_before_rule: UHExp.rule => zrule;

let place_before_operator: UHExp.operator => option(zoperator);

let place_after: UHExp.t => t;

let place_after_block: UHExp.t => zblock;

let place_after_line: UHExp.line => zline;

let place_after_opseq: UHExp.opseq => zopseq;

let place_after_operand: UHExp.operand => zoperand;

let place_after_rule: UHExp.rule => zrule;

let place_after_operator: UHExp.operator => option(zoperator);

let place_cursor_operator:
  (CursorPosition.t, UHExp.operator) => option(zoperator);

let place_cursor_operand:
  (CursorPosition.t, UHExp.operand) => option(zoperand);

let place_cursor_line: (CursorPosition.t, UHExp.line) => option(zline);

let place_cursor_rule: (CursorPosition.t, UHExp.rule) => option(zrule);

let prune_empty_hole_line: zline => zline;

let prune_empty_hole_lines: zblock => zblock;

let erase: t => UHExp.t;

let erase_zblock: t => UHExp.block;

let erase_zline: zline => UHExp.line;

let erase_zopseq: zopseq => UHExp.opseq;

let erase_zoperator: zoperator => UHExp.operator;

let erase_zoperand: zoperand => UHExp.operand;

let erase_zrules: zrules => UHExp.rules;

let erase_zrule: zrule => UHExp.rule;

let erase_zseq:
  ZSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator) =>
  Seq.t(UHExp.operand, UHExp.operator);

let mk_ZOpSeq:
  ZSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator) =>
  ZOpSeq.t(UHExp.operand, UHExp.operator, zoperand, zoperator);

let get_err_status: t => ErrStatus.t;

let get_err_status_zblock: t => ErrStatus.t;

let get_err_status_zopseq: zopseq => ErrStatus.t;

let get_err_status_zoperand: zoperand => ErrStatus.t;

let set_err_status: (ErrStatus.t, t) => t;

let set_err_status_zblock: (ErrStatus.t, t) => zblock;

let set_err_status_zopseq: (ErrStatus.t, zopseq) => zopseq;

let set_err_status_zoperand: (ErrStatus.t, zoperand) => zoperand;

let mk_inconsistent: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_zblock: (MetaVarGen.t, t) => (t, MetaVarGen.t);

let mk_inconsistent_zopseq: (MetaVarGen.t, zopseq) => (zopseq, MetaVarGen.t);

let mk_inconsistent_zoperand:
  (MetaVarGen.t, zoperand) => (zoperand, MetaVarGen.t);

let new_EmptyHole: MetaVarGen.t => (zoperand, MetaVarGen.t);

let cursor_on_outer_expr: zoperand => option((UHExp.t, CursorPosition.t));

let empty_zrule: MetaVarGen.t => (zrule, MetaVarGen.t);

let is_inconsistent: zoperand => bool;

let move_cursor_left: t => option(t);

let move_cursor_left_zblock: t => option(t);

let move_cursor_left_zline: zline => option(zline);

let move_cursor_left_zopseq: zopseq => option(zopseq);

let move_cursor_left_zoperator: zoperator => option(zoperator);

let move_cursor_left_zoperand: zoperand => option(zoperand);

let move_cursor_left_zrules: zrules => option(zrules);

let move_cursor_left_zrule: zrule => option(zrule);

let move_cursor_right: t => option(t);

let move_cursor_right_zblock: t => option(t);

let move_cursor_right_zline: zline => option(zline);

let move_cursor_right_zopseq: zopseq => option(zopseq);

let move_cursor_right_zoperator: zoperator => option(zoperator);

let move_cursor_right_zoperand: zoperand => option(zoperand);

let move_cursor_right_zrules: zrules => option(zrules);

let move_cursor_right_zrule: zrule => option(zrule);

let cursor_on_EmptyHole: t => option(MetaVar.t);

let cursor_on_EmptyHole_zblock: t => option(MetaVar.t);

let cursor_on_EmptyHole_zline: zline => option(MetaVar.t);

let cursor_on_EmptyHole_zopseq: zopseq => option(MetaVar.t);

let cursor_on_EmptyHole_zoperand: zoperand => option(MetaVar.t);

let cursor_on_EmptyHole_zrule: zrule => option(MetaVar.t);
