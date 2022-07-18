[@deriving sexp]
type t = DHExp.evalenv;

[@deriving sexp]
type result_map = VarBstMap.t_(EvaluatorResult.t);

let id_of_evalenv = ((ei, _): t): EvalEnvId.t => ei;

let environment_of_evalenv = ((_, result_map): t): Environment.t =>
  result_map
  |> VarBstMap.to_list
  |> List.map(((x, res: EvaluatorResult.t)) =>
       switch (res) {
       | Indet(d)
       | BoxedValue(d) => (x, d)
       }
     );

let result_map_of_evalenv = ((_, result_map): t): result_map => result_map;

let alist_of_evalenv =
    ((_, result_map): t): list((Var.t, EvaluatorResult.t)) =>
  result_map |> VarBstMap.to_list;

let empty: (EvaluatorState.t, t) = {
  let (es, ei) = EvaluatorState.initial |> EvaluatorState.next_env_id;
  let env: t = (ei, VarBstMap.empty);
  (es, env);
};

let is_empty = (env: t) => VarBstMap.is_empty(result_map_of_evalenv(env));

let equals = (env1: t, env2: t): bool =>
  id_of_evalenv(env1) == id_of_evalenv(env2);

let extend =
    (es: EvaluatorState.t, env: t, (x, a): (Var.t, EvaluatorResult.t))
    : (EvaluatorState.t, t) => {
  let (es, ei) = es |> EvaluatorState.next_env_id;
  (es, (ei, VarBstMap.extend(result_map_of_evalenv(env), (x, a))));
};

let union = (es: EvaluatorState.t, env1: t, env2: t): (EvaluatorState.t, t) => {
  let (es, ei) = es |> EvaluatorState.next_env_id;
  (
    es,
    (
      ei,
      VarBstMap.union(
        result_map_of_evalenv(env1),
        result_map_of_evalenv(env2),
      ),
    ),
  );
};

let lookup = (env: t, x) =>
  env |> result_map_of_evalenv |> (map => VarBstMap.lookup(map, x));

let contains = (env: t, x) =>
  env |> result_map_of_evalenv |> (map => VarBstMap.contains(map, x));

let map = (es: EvaluatorState.t, f, env: t): (EvaluatorState.t, t) => {
  let (es, ei) = es |> EvaluatorState.next_env_id;
  (
    es,
    (ei, VarBstMap.map(((x, r)) => f(x, r), result_map_of_evalenv(env))),
  );
};

let map_keep_id = (f, env: t): t => (
  id_of_evalenv(env),
  VarBstMap.map(((x, r)) => f(x, r), result_map_of_evalenv(env)),
);

let filter = (es: EvaluatorState.t, f, env: t): (EvaluatorState.t, t) => {
  let (es, ei) = es |> EvaluatorState.next_env_id;
  (
    es,
    (
      ei,
      VarBstMap.filter(((x, r)) => f(x, r), result_map_of_evalenv(env)),
    ),
  );
};

let length = (env: t): int => VarBstMap.length(result_map_of_evalenv(env));

let to_list = (env: t): list((Var.t, EvaluatorResult.t)) =>
  env |> result_map_of_evalenv |> VarBstMap.to_list;

let placeholder = ((-1), VarBstMap.empty);
