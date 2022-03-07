[@deriving sexp]
type t = DHExp.evalenv;

[@deriving sexp]
type result_map = VarBstMap.t(EvaluatorResult.t);

let id_of_evalenv = ((ei, _): t): EvalEnvId.t => ei;

let environment_of_evalenv = ((_, result_map): t): Environment.t =>
  result_map
  |> VarBstMap.bindings
  |> List.map(((x, res: EvaluatorResult.t)) =>
       switch (res) {
       | Indet(d)
       | BoxedValue(d) => (x, d)
       }
     );

let result_map_of_evalenv = ((_, result_map): t): result_map => result_map;

let alist_of_evalenv =
    ((_, result_map): t): list((Var.t, EvaluatorResult.t)) =>
  result_map |> VarBstMap.bindings;

let empty: (EvalEnvIdGen.t, t) = {
  let (ec, ei) = EvalEnvIdGen.next(EvalEnvIdGen.empty);
  let env: t = (ei, VarBstMap.empty);
  (ec, env);
};

let is_empty = (env: t) => VarBstMap.is_empty(result_map_of_evalenv(env));

let equals = (env1: t, env2: t): bool =>
  id_of_evalenv(env1) == id_of_evalenv(env2);

let extend =
    (ec: EvalEnvIdGen.t, env: t, (x, a): (Var.t, EvaluatorResult.t))
    : (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarBstMap.add(x, a, result_map_of_evalenv(env))));
};

let union = (ec: EvalEnvIdGen.t, env1: t, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (
    ec,
    (
      ei,
      VarBstMap.union(
        (_, dr, _) => Some(dr),
        result_map_of_evalenv(env1),
        result_map_of_evalenv(env2),
      ),
    ),
  );
};

let union_from_env =
    (ec: EvalEnvIdGen.t, env1: t, env2: result_map): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (
    ec,
    (
      ei,
      VarBstMap.union(
        (_, dr, _) => Some(dr),
        result_map_of_evalenv(env1),
        env2,
      ),
    ),
  );
};

let union_with_env =
    (ec: EvalEnvIdGen.t, env1: result_map, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (
    ec,
    (
      ei,
      VarBstMap.union(
        (_, dr, _) => Some(dr),
        env1,
        result_map_of_evalenv(env2),
      ),
    ),
  );
};

let lookup = (env: t, x) =>
  env |> result_map_of_evalenv |> VarBstMap.find_opt(x);

let contains = (env: t, x) =>
  env |> result_map_of_evalenv |> VarBstMap.mem(x);

let map = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarBstMap.mapi(f, result_map_of_evalenv(env))));
};

let map_keep_id = (f, env: t): t => (
  id_of_evalenv(env),
  VarBstMap.mapi(f, result_map_of_evalenv(env)),
);

let filter = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarBstMap.filter(f, result_map_of_evalenv(env))));
};

let length = (env: t): int =>
  VarBstMap.cardinal(result_map_of_evalenv(env));

let to_list = (env: t): list((Var.t, EvaluatorResult.t)) =>
  env |> result_map_of_evalenv |> VarBstMap.bindings;

let placeholder = ((-1), VarBstMap.empty);
