let extract_vars: (Contexts.t, HTyp.t) => VarCtx.t;

let fun_vars: (Contexts.t, HTyp.t) => VarCtx.t;

let get_type: CursorInfo.t => option(HTyp.t);

let valid_assistant_term: CursorInfo.cursor_term => bool;

let type_to_str: HTyp.t => string;

let term_to_str: CursorInfo.cursor_term => string;
