let cons': (int, CursorPath_common.t) => CursorPath_common.t;

let of_z: ZExp.t => CursorPath_common.t;

let of_zblock: ZExp.t => CursorPath_common.t;

let of_zline: ZExp.zline => CursorPath_common.t;

let of_zopseq: ZExp.zopseq => CursorPath_common.t;

let of_zoperand: ZExp.zoperand => CursorPath_common.t;

let of_zoperator: ZExp.zoperator => CursorPath_common.t;

let of_zrules: ZExp.zrules => CursorPath_common.t;

let of_zrule: ZExp.zrule => CursorPath_common.t;

let follow: (CursorPath_common.t, UHExp.t) => option(ZExp.t);

let follow_block: (CursorPath_common.t, UHExp.t) => option(ZExp.t);

let follow_line: (CursorPath_common.t, UHExp.line) => option(ZExp.zline);

let follow_opseq: (CursorPath_common.t, UHExp.opseq) => option(ZExp.zopseq);

let follow_operator:
  (CursorPath_common.t, UHExp.operator) => option(ZExp.zoperator);

let follow_operand:
  (CursorPath_common.t, UHExp.operand) => option(ZExp.zoperand);

let follow_rules: (CursorPath_common.t, UHExp.rules) => option(ZExp.zrules);

let follow_rule: (CursorPath_common.t, UHExp.rule) => option(ZExp.zrule);

let of_steps:
  (CursorPath_common.steps, ~side: Side.t=?, UHExp.t) =>
  option(CursorPath_common.t);

let of_steps_block:
  (CursorPath_common.steps, ~side: Side.t, UHExp.t) =>
  option(CursorPath_common.t);

let of_steps_line:
  (CursorPath_common.steps, ~side: Side.t, UHExp.line) =>
  option(CursorPath_common.t);

let of_steps_opseq:
  (CursorPath_common.steps, ~side: Side.t, UHExp.opseq) =>
  option(CursorPath_common.t);

let of_steps_operator:
  (CursorPath_common.steps, ~side: Side.t, UHExp.operator) =>
  option(CursorPath_common.t);

let of_steps_operand:
  (CursorPath_common.steps, ~side: Side.t, UHExp.operand) =>
  option(CursorPath_common.t);

let of_steps_rule:
  (CursorPath_common.steps, ~side: Side.t, UHExp.rule) =>
  option(CursorPath_common.t);

let hole_desc: MetaVar.t => CursorPath_common.hole_desc;

let holes_err:
  (ErrStatus.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_case_err:
  (
    CaseErrStatus.t,
    CursorPath_common.rev_steps,
    CursorPath_common.hole_list
  ) =>
  CursorPath_common.hole_list;

let holes_verr:
  (VarErrStatus.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes:
  (UHExp.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_block:
  (UHExp.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_line:
  (UHExp.line, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_operand:
  (UHExp.operand, CursorPath_common.steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_rule:
  (UHExp.rule, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_z:
  (ZExp.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zblock:
  (ZExp.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zline:
  (ZExp.zline, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zopseq:
  (ZExp.zopseq, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zoperand:
  (ZExp.zoperand, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zrule:
  (ZExp.zrule, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let prev_hole_steps_z: ZExp.t => option(CursorPath_common.steps);

let prev_hole_steps_zline: ZExp.zline => option(CursorPath_common.steps);

let next_hole_steps_z: ZExp.t => option(CursorPath_common.steps);

let next_hole_steps_zline: ZExp.zline => option(CursorPath_common.steps);
