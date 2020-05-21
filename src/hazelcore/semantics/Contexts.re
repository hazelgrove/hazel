[@deriving sexp]
type t = {
  vars: VarCtx.t,
  tvars: TVarCtx.t,
  palettes: PaletteCtx.t
}
let empty = {
  vars: VarCtx.empty,
  tvars: TVarCtx.empty,
  palettes: PaletteCtx.empty
}

let gamma = (ctx: t): VarCtx.t => ctx.vars;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  ...ctx,
  vars: VarCtx.extend(ctx.vars, binding)
};
let gamma_union = (ctx: t, gamma': VarCtx.t): t => {
  ...ctx,
  vars: VarCtx.union(ctx.vars, gamma')
}
let gamma_contains = (ctx: t, x: Var.t): bool =>
  VarCtx.contains(gamma(ctx), x);

let delta = (ctx: t): TVarCtx.t => ctx.tvars;
let extend_delta = (ctx: t, binding: (Vat.t, Kint.t)): t => {
  ...ctx,
  tvars: TVarCtx.extend(ctx.tvars, binding)
};
let delta_union = (ctx: t, delta': TVarCtx.t): t => {
  ...ctx,
  tvars: TVarCtx.union(ctx.tvars, delta')
}
let delta_contains = (ctx: t, x: Var.t): bool => 
  TVarCtx.contains(delta(ctx), x)

let palette_ctx = (ctx: t): PaletteCtx.t => ctx.palettes
