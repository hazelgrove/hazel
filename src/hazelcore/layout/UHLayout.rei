open Pretty;

type t = Layout.t(UHAnnot.t);

let has_child: t => bool;

let has_inline_OpenChild: t => bool;

let has_para_OpenChild: t => bool;

let find_and_decorate_caret: (~path: CursorPath_common.t, t) => option(t);

// TODO rename to current term
let find_and_decorate_cursor:
  (~steps: CursorPath_common.steps, t) => option(t);

let find_and_decorate_var_def:
  (~steps: CursorPath_common.steps, t) => option(t);

let find_and_decorate_var_use:
  (~steps: CursorPath_common.steps, t) => option(t);
