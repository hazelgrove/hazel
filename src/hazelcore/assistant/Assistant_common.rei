[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

let extract_vars: (Context.t, HTyp.t) => list((Var.t, HTyp.t));

let fun_vars: (Context.t, HTyp.t) => VarMap.t(HTyp.t);

let get_type: CursorInfo.t => option(HTyp.t);

let get_mode: CursorInfo.t => mode;

let valid_assistant_term: CursorInfo.cursor_term => bool;

let term_to_str: CursorInfo.cursor_term => string;
