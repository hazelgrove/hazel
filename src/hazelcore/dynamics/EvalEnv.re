[@deriving sexp]
type t = DHExp.evalenv;

[@deriving sexp]
type result_map = VarBstMap.t(EvaluatorResult.t);

/* Environment with a special `EvalEnvId.t` of zero. */
let empty: t = (EvalEnvId.empty, VarBstMap.empty);

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

let is_empty = (env: t) => VarBstMap.is_empty(result_map_of_evalenv(env));

let equals = (env1: t, env2: t): bool =>
  id_of_evalenv(env1) == id_of_evalenv(env2);

let extend =
    (es: EvalState.t, env: t, (x, a): (Var.t, EvaluatorResult.t))
    : (EvalState.t, t) => {
  let (es, ei) = es |> EvalState.next_evalenvid;
  (es, (ei, VarBstMap.add(x, a, result_map_of_evalenv(env))));
};

let union = (es: EvalState.t, env1: t, env2: t): (EvalState.t, t) => {
  let (es, ei) = es |> EvalState.next_evalenvid;
  (
    es,
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

let lookup = (env: t, x) =>
  env |> result_map_of_evalenv |> VarBstMap.find_opt(x);

let contains = (env: t, x) =>
  env |> result_map_of_evalenv |> VarBstMap.mem(x);

let map = (es: EvalState.t, f, env: t): (EvalState.t, t) => {
  let (es, ei) = es |> EvalState.next_evalenvid;
  (es, (ei, VarBstMap.mapi(f, result_map_of_evalenv(env))));
};

let map_keep_id = (f, env: t): t => (
  id_of_evalenv(env),
  VarBstMap.mapi(f, result_map_of_evalenv(env)),
);

let filter = (es: EvalState.t, f, env: t): (EvalState.t, t) => {
  let (es, ei) = es |> EvalState.next_evalenvid;
  (es, (ei, VarBstMap.filter(f, result_map_of_evalenv(env))));
};

let length = (env: t): int =>
  VarBstMap.cardinal(result_map_of_evalenv(env));

let to_list = (env: t): list((Var.t, EvaluatorResult.t)) =>
  env |> result_map_of_evalenv |> VarBstMap.bindings;

let placeholder = ((-1), VarBstMap.empty);
