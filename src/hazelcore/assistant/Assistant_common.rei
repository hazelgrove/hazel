let extract_vars: (Contexts.t, HTyp.t) => VarCtx.t;

let fun_vars: (Contexts.t, HTyp.t) => VarCtx.t;

let get_type: CursorInfo.t => option(HTyp.t);

let type_to_str: HTyp.t => string;
