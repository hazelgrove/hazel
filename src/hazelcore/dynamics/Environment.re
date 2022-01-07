[@deriving sexp]
type t = DHExp.environment;
include VarMap;

let id_env = (ctx: VarCtx.t): t =>
  VarMap.map(
    xt => {
      let (x, _) = xt;
      DHExp.BoundVar(x);
    },
    ctx,
  );
