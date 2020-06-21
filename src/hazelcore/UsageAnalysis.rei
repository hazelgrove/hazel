[@deriving sexp]
type uses_list = list(CursorPath_common.steps);

let binds_var: (Var.t, UHPat.t) => bool;

let binds_var_opseq: (Var.t, UHPat.t) => bool;

let binds_var_operand: (Var.t, UHPat.operand) => bool;

let find_uses: (~steps: CursorPath_common.steps, Var.t, UHExp.t) => uses_list;

let find_uses_block:
  (~offset: int=?, ~steps: CursorPath_common.steps, Var.t, UHExp.t) =>
  uses_list;

let find_uses_line:
  (~steps: list(ChildIndex.t), Var.t, UHExp.line) => (uses_list, bool);

let find_uses_opseq:
  (~steps: list(ChildIndex.t), Var.t, UHExp.opseq) => uses_list;

let find_uses_operand:
  (~steps: CursorPath_common.steps, Var.t, UHExp.operand) => uses_list;

let find_uses_rule:
  (~steps: list(ChildIndex.t), Var.t, UHExp.rule) => uses_list;
