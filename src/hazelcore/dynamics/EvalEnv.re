[@deriving sexp]
type t = DHExp.evalenv
and result = DHExp.result
and result_map = VarMap.t_(result);

let id_of_evalenv = ((ei, _): t): EvalEnvId.t => ei;

let environment_of_evalenv = ((_, result_map): t): Environment.t =>
  result_map
  |> List.map(((x, res: result)) =>
       switch (res) {
       | Indet(d)
       | BoxedValue(d) => (x, d)
       }
     );

let result_map_of_evalenv = ((_, result_map): t): result_map => result_map;

let empty: (EvalEnvIdGen.t, t) = {
  let (ec, ei) = EvalEnvIdGen.next(EvalEnvIdGen.empty);
  let env: t = (ei, VarMap.empty);
  (ec, env);
};

let is_empty = (env: t) => VarMap.is_empty(result_map_of_evalenv(env));

let equals = (env1: t, env2: t): bool =>
  id_of_evalenv(env1) == id_of_evalenv(env2);

let extend =
    (ec: EvalEnvIdGen.t, env: t, xa: VarMap.t__(result))
    : (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarMap.extend(result_map_of_evalenv(env), xa)));
};

let union = (ec: EvalEnvIdGen.t, env1: t, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (
    ec,
    (
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
  (ec, (ei, VarMap.union(result_map_of_evalenv(env1), env2)));
};

let union_with_env =
    (ec: EvalEnvIdGen.t, env1: result_map, env2: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarMap.union(env1, result_map_of_evalenv(env2))));
};

let lookup = (env: t, x) => VarMap.lookup(result_map_of_evalenv(env), x);

let contains = (env: t, x) =>
  VarMap.contains(result_map_of_evalenv(env), x);

let map = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarMap.map(f, result_map_of_evalenv(env))));
};

let map_keep_id = (f, env: t): t => (
  id_of_evalenv(env),
  VarMap.map(f, result_map_of_evalenv(env)),
);

let filter = (ec: EvalEnvIdGen.t, f, env: t): (EvalEnvIdGen.t, t) => {
  let (ec, ei) = EvalEnvIdGen.next(ec);
  (ec, (ei, VarMap.filter(f, result_map_of_evalenv(env))));
};

let length = (env: t): int => List.length(result_map_of_evalenv(env));

let to_list = result_map_of_evalenv;

let placeholder = ((-1), []);
