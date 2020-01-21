[@deriving sexp]
type t('a) = (VarCtx.t('a), PaletteCtx.t);
let empty = (VarCtx.empty, PaletteCtx.empty);

let gamma = ((gamma, _): t('a)): VarCtx.t('a) => gamma;
let extend_gamma = (ctx: t('a), binding: (Var.t, (HTyp.t, 'a))): t('a) => {
  let (gamma, palette_ctx) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx);
};
let gamma_union = (ctx: t('a), gamma': VarCtx.t('a)): t('a) => {
  let (gamma, palette_ctx) = ctx;
  let gamma'' = VarCtx.union(gamma, gamma');
  (gamma'', palette_ctx);
};
let gamma_contains = (ctx: t('a), x: Var.t): bool =>
  VarCtx.contains(gamma(ctx), x);

let palette_ctx = ((_, palette_ctx): t('a)): PaletteCtx.t => palette_ctx;
