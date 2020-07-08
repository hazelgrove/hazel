[@deriving sexp]
type t = (VarCtx.t, PaletteCtx.t);
let empty = (VarCtx.empty, PaletteCtx.empty);

let gamma = ((gamma, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, palette_ctx) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx);
};
let gamma_union = (ctx: t, gamma': VarCtx.t): t => {
  let (gamma, palette_ctx) = ctx;
  let gamma'' = VarCtx.union(gamma, gamma');
  (gamma'', palette_ctx);
};
let gamma_contains = (ctx: t, x: Var.t): bool =>
  VarCtx.contains(gamma(ctx), x);

let palette_ctx = ((_, palette_ctx): t): PaletteCtx.t => palette_ctx;
