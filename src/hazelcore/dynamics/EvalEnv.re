exception KeepIdOnUnreachedEnvironment;

[@deriving sexp]
type t = DHExp.evalenv;

module EvalEnvCtx = {
  type t = int;

  let sexp_of_t = Sexplib.Std.sexp_of_int;
  let t_of_sexp = Sexplib.Std.int_of_sexp;

  let empty = (-1);

  /* ec => (ec', ei) */
  let next = (n: t): (t, t) => (n + 1, n);
};

let id_of_evalenv = (env: t): option(int) =>
  switch (env) {
  | Env(ei, _) => Some(ei)
  | UnreachedEnv => None
  };

let env_of_evalenv = (env: t): VarMap.t_(DHExp.t) =>
  switch (env) {
  | Env(_, ee) => ee
  | UnreachedEnv => VarMap.empty
  };

let empty: (EvalEnvCtx.t, t) = {
  let (ec, ei) = EvalEnvCtx.next(EvalEnvCtx.empty);
  let env: t = Env(ei, VarMap.empty);
  (ec, env);
};

let unreached: t = UnreachedEnv;

let is_empty = (env: t) => VarMap.is_empty(env_of_evalenv(env));

let extend =
    (ec: EvalEnvCtx.t, env: t, xa: VarMap.t__(DHExp.t)): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.extend(env_of_evalenv(env), xa)));
};

let union = (ec: EvalEnvCtx.t, env1: t, env2: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.union(env_of_evalenv(env1), env_of_evalenv(env2))));
};

let union_from_env =
    (ec: EvalEnvCtx.t, env1: t, env2: Environment.t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.union(env_of_evalenv(env1), env2)));
};

let union_with_env =
    (ec: EvalEnvCtx.t, env1: Environment.t, env2: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.union(env1, env_of_evalenv(env2))));
};

let lookup = (env: t, x) => VarMap.lookup(env_of_evalenv(env), x);

let contains = (env: t, x) => VarMap.contains(env_of_evalenv(env), x);

let map = (ec: EvalEnvCtx.t, f, env: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.map(f, env_of_evalenv(env))));
};

let map_keep_id = (f, env: t): t => {
  switch (id_of_evalenv(env)) {
  | Some(ei) => Env(ei, VarMap.map(f, env_of_evalenv(env)))
  | None => raise(KeepIdOnUnreachedEnvironment)
  };
};

let filter = (ec: EvalEnvCtx.t, f, env: t): (EvalEnvCtx.t, t) => {
  let (ec, ei) = EvalEnvCtx.next(ec);
  (ec, Env(ei, VarMap.filter(f, env_of_evalenv(env))));
};

let length = (env: t): int => List.length(env_of_evalenv(env));

let to_list = env_of_evalenv;
