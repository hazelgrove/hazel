let operator_of_shape: Action_common.operator_shape => option(UHTyp.operator);

let shape_of_operator: UHTyp.operator => Action_common.operator_shape;

let construct_operator:
  (UHTyp.operator, ZTyp.zoperand, ZTyp.operand_surround) => ZTyp.zopseq;

let move: (Action_common.t, ZTyp.t) => Action_common.Outcome.t(ZTyp.t);

let perform: (Action_common.t, ZTyp.t) => Action_common.Outcome.t(ZTyp.t);

let perform_opseq:
  (Action_common.t, ZTyp.t) => Action_common.Outcome.t(ZTyp.t);

let perform_operand:
  (Action_common.t, ZTyp.zoperand) => Action_common.Outcome.t(ZTyp.t);
