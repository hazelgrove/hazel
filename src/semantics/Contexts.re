[@deriving sexp]
type t = (VarCtx.t, LivelitCtx.t);
let empty = (VarCtx.empty, LivelitCtx.empty);

let gamma = ((gamma, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, livelit_ctx) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', livelit_ctx);
};
let gamma_union = (ctx: t, gamma': VarCtx.t): t => {
  let (gamma, livelit_ctx) = ctx;
  let gamma'' = VarCtx.union(gamma, gamma');
  (gamma'', livelit_ctx);
};
let gamma_contains = (ctx: t, x: Var.t): bool =>
  VarCtx.contains(gamma(ctx), x);

let livelit_ctx = ((_, livelit_ctx): t): LivelitCtx.t => livelit_ctx;
