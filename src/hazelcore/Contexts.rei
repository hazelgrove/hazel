[@deriving sexp]
type t;

let empty: t;

let extend_gamma: (t, (Var.t, HTyp.t)) => t;

let gamma: t => VarCtx.t;

let palette_ctx: t => PaletteCtx.t;

let typing: t => TyCtx.t;

let bind_tyvar: (TyVar.Name.t, Kind.t, t) => t;

// let extend_tyvars: (TyCtx.Vars.binding, t) => t;

// let tyholes: t => TyCtx.Holes.t;

// let extend_tyholes: (MetaVar.t, Kind.t, t) => t;
