/* EvalEnv is an environment (mapping variables to expressions) that
   are used during evaluation. It is different from Environment.t in two ways:

   1. It maps Var.t to Evaluator.result (rather than to DHExp.result)
   2. Each EvalEnv has an ID associated with it (if evaluation reaches it),
      or is unreachable. (i.e., a placeholder environment)

   Environment.t may be useful in certain cases, namely pattern matching,
   when an evaluated result is not needed. EvalEnv is used for environents
   during evaluation, including in closures. EvalEnvs are numbered
   so that operations on them (e.g., during hole numbering) can be memoized;
   the id allows for quick equality checking and allows environments to be
   comparable (e.g., so that they can be stored in a map).

   Both EvalEnv.t and Environment.t are often named sigma (usually for hole
   environments) or env.

   This mimicks the VarMap interface on the extended EvalEnv.t type. Most
   operations require an EvaluatorState.t parameter, which is used to generate
   unique ID's for each environment, and is created using EvalEnv.empty
   (at the beginning of evaluation).
   */

[@deriving sexp]
type map = DHExp.closure_map;

[@deriving sexp]
type t = DHExp.closure_env;

/**
  [id_of env] is the id of [env].
 */
let id_of: t => EnvironmentId.t;

/**
  [map_of env] is the {!type:map} of [env].
 */
let map_of: t => map;

/**
  [to_list env] is the list of bindings in [env].
 */
let to_list: t => list((Var.t, DHExp.result));

/**
  [to_environment] is the environment containing the bindings in [env].
 */
let to_environment: t => Environment.t;

/**
  [equal env1 env2] is [true] if and only if [env1] and [env2] have the same
  id.
 */
let equal: (t, t) => bool;

/**
  [empty eig] is [(env, eig')] where [env] is an empty environment.
 */
let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);

/**
  [is_empty env] is [true] if and only if [env] has no bindings.
 */
let is_empty: t => bool;

/**
  [length env] is the number of bindings in [env].
 */
let length: t => int;

/**
  [lookup env x] is [Some d] if [d] is bound for [x] in [env] and [None] otherwise.
 */
let lookup: (t, Var.t) => option(DHExp.result);

/**
  [contains env x] is [true] if [x] is bound in [env].
 */
let contains: (t, Var.t) => bool;

/**
  [extend env (x, d) eig] is [(env', eig')], where [env'] is [env] extended
  with the binding of [d] for [x].
 */
let extend:
  (t, (Var.t, DHExp.result), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

/**
  [union env1 env2 eig] is [(env, eig)] where [env] is [env2] extended with
  [env1]. See {!val:VarBstMap.union}.
 */
let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

/**
  [map f env eig] is [(env', eig')] where [env'] contains the bindings of
  [env] mapped by [f].
 */
let map:
  (((Var.t, DHExp.result)) => DHExp.result, t, EnvironmentIdGen.t) =>
  (t, EnvironmentIdGen.t);

/**
  [map_keep_id] is [map], but the id of the given environment is maintained.

  (This is used when transforming an environment, such as in the closure ->
  lambda stage after evaluation. More functions may be added like this
  as-needed for similar purposes.)
 */
let map_keep_id: (((Var.t, DHExp.result)) => DHExp.result, t) => t;

/**
  [filter f env eig] is [(env', eig')] where [env'] contains the bindings of
  [env] filtered by [f].
 */
let filter:
  (((Var.t, DHExp.result)) => bool, t, EnvironmentIdGen.t) =>
  (t, EnvironmentIdGen.t);

/**
  Placeholder used in DHCode. Is identified by an invalid EnvironmentId.t, only
  used for display purposes.
 */
let placeholder: t;
