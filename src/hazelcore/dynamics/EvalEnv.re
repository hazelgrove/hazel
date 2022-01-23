exception InvalidEvalEnvType;

[@deriving sexp]
type t = DHExp.evalenv
and result = DHExp.result
and result_map = VarMap.t_(result);

let id_of_evalenv = (env: t): option(int) =>
  switch (env) {
  | Env(ei, _) => Some(ei)
  | UnreachedEnv => None
  };

let environment_of_evalenv = (env: t): Environment.t =>
  (
    switch (env) {
    | Env(_, ee) => ee
    | UnreachedEnv => VarMap.empty
    }
  )
  |> List.map(((x, res: result)) =>
       switch (res) {
       | Indet(d)
       | BoxedValue(d) => (x, d)
       }
     );

let result_map_of_evalenv = (env: t): result_map =>
  switch (env) {
  | Env(_, ee) => ee
  | UnreachedEnv => VarMap.empty
  };

let empty: (EvalEnvIdGen.t, t) = {
  let (ec, ei) = EvalEnvIdGen.next(EvalEnvIdGen.empty);
  let env: t = Env(ei, VarMap.empty);
  (ec, env);
};

let unreached: t = UnreachedEnv;

let is_empty = (env: t) => VarMap.is_empty(result_map_of_evalenv(env));

let extend =
    (ec: EvalEnvIdGen.t, env: t, xa: VarMap.t__(result))
    : (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, Env(ei, VarMap.extend(result_map_of_evalenv(env), xa)));
};

let union = (ec: EvalEnvIdGen.t, env1: t, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (
    ec,
    Env(
      ei,
      VarMap.union(
        result_map_of_evalenv(env1),
        result_map_of_evalenv(env2),
      ),
    ),
  );
};

let union_from_env =
    (ec: EvalEnvIdGen.t, env1: t, env2: result_map): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, Env(ei, VarMap.union(result_map_of_evalenv(env1), env2)));
};

let union_with_env =
    (ec: EvalEnvIdGen.t, env1: result_map, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, Env(ei, VarMap.union(env1, result_map_of_evalenv(env2))));
};

let lookup = (env: t, x) => VarMap.lookup(result_map_of_evalenv(env), x);

let contains = (env: t, x) =>
  VarMap.contains(result_map_of_evalenv(env), x);

let map = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, Env(ei, VarMap.map(f, result_map_of_evalenv(env))));
};

let map_keep_id = (f, env: t): t => {
  switch (id_of_evalenv(env)) {
  | Some(ei) => Env(ei, VarMap.map(f, result_map_of_evalenv(env)))
  | None => raise(InvalidEvalEnvType)
  };
};

let filter = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, Env(ei, VarMap.filter(f, result_map_of_evalenv(env))));
};

let length = (env: t): int => List.length(result_map_of_evalenv(env));

let to_list = result_map_of_evalenv;
