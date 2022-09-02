[@deriving sexp]
type t = VarCtx.t;
let initial = Builtins.ctx;

let gamma = (gamma: t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let gamma = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  gamma';
};
