let of_z: ZTyp.t => CursorPath_common.t;

let of_zopseq: ZTyp.t => CursorPath_common.t;

let of_zoperand: ZTyp.zoperand => CursorPath_common.t;

let of_zoperator: (('a, 'b)) => (list('c), 'a);

let follow: (CursorPath_common.t, UHTyp.t) => option(ZTyp.t);

let follow_opseq: (CursorPath_common.t, UHTyp.t) => option(ZTyp.t);

let follow_operand:
  (CursorPath_common.t, UHTyp.operand) => option(ZTyp.zoperand);

let follow_operator:
  (CursorPath_common.t, UHTyp.operator) => option(ZTyp.zoperator);

let of_steps:
  (CursorPath_common.steps, ~side: Side.t=?, UHTyp.t) =>
  option(CursorPath_common.t);

let of_steps_opseq:
  (CursorPath_common.steps, ~side: Side.t, UHTyp.t) =>
  option(CursorPath_common.t);

let of_steps_operand:
  (CursorPath_common.steps, ~side: Side.t, UHTyp.operand) =>
  option(CursorPath_common.t);

let of_steps_operator:
  (CursorPath_common.steps, ~side: Side.t, UHTyp.operator) =>
  option(CursorPath_common.t);

let hole_desc: 'a => CursorPath_common.hole_desc;

let is_space: 'a => bool;

let holes:
  (UHTyp.t, CursorPath_common.rev_steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_operand:
  (UHTyp.operand, CursorPath_common.steps, CursorPath_common.hole_list) =>
  CursorPath_common.hole_list;

let holes_z:
  (ZTyp.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zopseq:
  (ZTyp.t, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;

let holes_zoperand:
  (ZTyp.zoperand, CursorPath_common.rev_steps) => CursorPath_common.zhole_list;
