[@deriving sexp]
type t = (VarCtx.t, PaletteCtx.t, TyVarCtx.t);
let empty = (VarCtx.empty, PaletteCtx.empty, TyVarCtx.empty);

let gamma = ((gamma, _, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, palette_ctx, tyvars) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx, tyvars);
};

let palette_ctx = ((_, palette_ctx, _): t): PaletteCtx.t => palette_ctx;

let tyvars = ((_, _, tyvars): t): TyVarCtx.t => tyvars;
let extend_tyvars =
    ((gamma, palette_ctx, tyvars): t, name: string, kind: Kind.t) => {
  (gamma, palette_ctx, TyVarCtx.bind(tyvars, name, kind));
};
