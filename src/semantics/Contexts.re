type t = (VarCtx.t, PaletteCtx.t);
let gamma =
  fun
  | (gamma, _) => gamma;
let extend_gamma = (contexts, binding) => {
  let (gamma, palette_ctx) = contexts;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx);
};
let gamma_union = (contexts, gamma') => {
  let (gamma, palette_ctx) = contexts;
  let gamma'' = VarCtx.union(gamma, gamma');
  (gamma'', palette_ctx);
};
let gamma_contains = (contexts, x) => VarCtx.contains(gamma(contexts), x);

let palette_ctx = ((_, palette_ctx)) => palette_ctx;
