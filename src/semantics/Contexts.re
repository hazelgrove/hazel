type t = {
  tvars: TVarCtx.t,
  vars: VarCtx.t,
  palettes: PaletteCtx.t,
};

let empty = {
  tvars: TVarCtx.empty,
  palettes: PaletteCtx.empty,
  vars: VarCtx.empty,
};

let extend_vars = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  {...ctx, vars: VarCtx.extend(ctx.vars, binding)};
};

let extend_tvars = (ctx: t, t: Var.t): t => {
  {...ctx, tvars: TVarCtx.extend(ctx.tvars, t)};
};

let union_vars = (ctx: t, vars': VarCtx.t): t => {
  {...ctx, vars: VarCtx.union(ctx.vars, vars')};
};

let contains_vars = (ctx: t, x: Var.t): bool => VarCtx.contains(ctx.vars, x);
