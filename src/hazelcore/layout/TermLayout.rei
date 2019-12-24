[@deriving sexp]
type t = Layout.t(TermTag.t);

let find_and_decorate_caret: (~path: CursorPath.t, t) => option(t);

// TODO rename to current term
let find_and_decorate_cursor: (~steps: CursorPath.steps, t) => option(t) /* TODO find_and_decorate_pattern_usag*/;
