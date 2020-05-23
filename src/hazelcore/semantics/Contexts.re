[@deriving sexp]
type t = {
  vars: VarCtx.t,
  tyvars: TyVarCtx.t,
  palettes: PaletteCtx.t
}
let empty = {
  vars: VarCtx.empty,
  tyvars: TyVarCtx.empty,
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

let tyvars = (ctx: t): TyVarCtx.t = ctx.tyvar;

let palette_ctx = (ctx: t): PaletteCtx.t => ctx.palettes
