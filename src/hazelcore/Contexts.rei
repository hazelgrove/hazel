[@deriving sexp]
type t;

let empty: t;

let extend_gamma: (t, (Var.t, HTyp.t)) => t;

let gamma: t => VarCtx.t;

let palette_ctx: t => PaletteCtx.t;

let tyvars: t => TyVarCtx.t;

let extend_tyvars: (t, (TyId.t, Kind.t)) => t;
