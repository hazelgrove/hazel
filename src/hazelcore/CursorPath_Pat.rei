let of_z: ZPat.t => CursorPath_common.t;

let of_zopseq: ZPat.t => CursorPath_common.t;

let of_zoperand: ZPat.zoperand => CursorPath_common.t;

let of_zoperator: (('a, 'b)) => (list('c), 'a);

let follow: (CursorPath_common.t, UHPat.t) => option(ZPat.t);

let follow_opseq: (CursorPath_common.t, UHPat.t) => option(ZPat.t);

let follow_operand:
  (CursorPath_common.t, UHPat.operand) => option(ZPat.zoperand);

let follow_operator:
  (CursorPath_common.t, UHPat.operator) => option(ZPat.zoperator);

let of_steps:
  (CursorPath_common.steps, ~side: Side.t=?, UHPat.t) =>
  option(CursorPath_common.t);

let of_steps_opseq:
  (CursorPath_common.steps, ~side: Side.t, UHPat.t) =>
  option(CursorPath_common.t);

let of_steps_operand:
  (CursorPath_common.steps, ~side: Side.t, UHPat.operand) =>
  option(CursorPath_common.t);

let of_steps_operator:
  (CursorPath_common.steps, ~side: Side.t, UHPat.operator) =>
  option(CursorPath_common.t);

let hole_desc: MetaVar.t => CursorPath_common.hole_desc;

let holes:
  (UHPat.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_operand:
  (UHPat.operand, CursorPath_common.steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_z:
  (ZPat.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zopseq:
  (ZPat.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zoperand:
  (ZPat.zoperand, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;
