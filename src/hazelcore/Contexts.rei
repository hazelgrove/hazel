[@deriving sexp]
type t = (VarCtx.t, PaletteCtx.t);

let empty: (list('a), list('b));

let gamma: t => VarCtx.t;

let extend_gamma: (t, (Var.t, HTyp.t)) => t;

let gamma_union: (t, VarCtx.t) => t;

let gamma_contains: (t, Var.t) => bool;

let palette_ctx: t => PaletteCtx.t;
