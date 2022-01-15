[@deriving sexp]
type t = DHExp.evalenv;
include VarMap;

module EvalEnvCtx = {
  type t = int;

  let empty = 0;
  let next = (n: t): (t, t) => (n + 1, n);
};

let id_of_evalenv = ((ei, _): t): option(int) => ei;
let env_of_evalenv = ((_, ee): t): VarMap.t_(DHExp.t) => ee;

let empty_unnumbered = (None, VarMap.empty);
let unnumbered_evalenv_of_env = (e: Environment.t): t => (None, e);

let empty = (ec: EvalEnvCtx.t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, (Some(ei), VarMap.empty));
};

let is_empty = (env: t) => VarMap.is_empty(env_of_evalenv(env));

let extend = (ec: EvalEnvCtx.t, env: t, xa): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, (Some(ei), VarMap.extend(env_of_evalenv(env), xa)));
};

let union = (ec: EvalEnvCtx.t, env1: t, env2: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (
    ec,
    (Some(ei), VarMap.union(env_of_evalenv(env2), env_of_evalenv(env1))),
  );
};

let lookup = (env: t, x) => VarMap.lookup(env_of_evalenv(env), x);

let contains = (env: t, x) => VarMap.contains(env_of_evalenv(env), x);

let map = (ec: EvalEnvCtx.t, f, env: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, (Some(ei), VarMap.map(f, env_of_evalenv(env))));
};

let filter = (ec: EvalEnvCtx.t, f, env: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, (Some(ei), VarMap.filter(f, env_of_evalenv(env))));
};

let length = (env: t): int => List.length(env_of_evalenv(env));

let to_list = env_of_evalenv;
