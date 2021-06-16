[@deriving sexp]
type t'('closed_llarg) = (VarCtx.t, LivelitCtx.t('closed_llarg));
[@deriving sexp]
type t = t'(HTyp.t);
let empty = (BuiltinFunctions.ctx, BuiltinLivelits.ctx);

let gamma = ((gamma, _): t'('a)): VarCtx.t => gamma;
let extend_gamma = (ctx: t'('a), binding: (Var.t, HTyp.t)): t'('a) => {
  let (gamma, livelit_ctx) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', livelit_ctx);
};
let gamma_union = (ctx: t'('a), gamma': VarCtx.t): t'('a) => {
  let (gamma, livelit_ctx) = ctx;
  let gamma'' = VarCtx.union(gamma, gamma');
  (gamma'', livelit_ctx);
};
let gamma_contains = (ctx: t'('a), x: Var.t): bool =>
  VarCtx.contains(gamma(ctx), x);

let livelit_ctx = ((_, livelit_ctx): t'('a)): LivelitCtx.t('a) => livelit_ctx;
