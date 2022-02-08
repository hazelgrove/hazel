[@deriving sexp]
type t;

let empty: t;

let extend_gamma: (t, (Var.t, HTyp.t)) => t;

let gamma: t => VarCtx.t;

let palette_ctx: t => PaletteCtx.t;

let tyvars: t => TyVarCtx.t;
// let replace_tyvars: (t, TyVarCtx.t) => t;

let bind_tyvar: (t, TyVar.Name.t, Kind.t) => t;
let bind_tyhole: (t, MetaVar.t, Kind.t) => t;

// let extend_tyvars: (TyVarCtx.Vars.binding, t) => t;

// let tyholes: t => TyVarCtx.Holes.t;

// let extend_tyholes: (MetaVar.t, Kind.t, t) => t;
