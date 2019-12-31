[@deriving sexp]
type t = Layout.t(TermTag.t);

let has_inline_OpenChild: t => bool;

let has_para_OpenChild: t => bool;

// TODO find_and_decorate_pattern_usage

let find_and_decorate_caret: (~path: CursorPath.t, t) => option(t);

// TODO rename to current term
let find_and_decorate_cursor: (~steps: CursorPath.steps, t) => option(t);

let path_of_caret_position: (int, int, t) => option(CursorPath.t);
