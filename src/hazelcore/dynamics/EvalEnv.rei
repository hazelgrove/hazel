/* EvalEnv is an environment (mapping variables to expressions) that
   are used during evaluation. It is different from Environment.t in two ways:

   1. It maps Var.t to Evaluator.result (rather than to DHExp.t)
   2. Each EvalEnv has an ID associated with it (if evaluation reaches it),
      or is unreachable. (i.e., a placeholder environment)

   Environment.t may be useful in certain cases, namely pattern matching,
   when an evaluated result is not needed. EvalEnv is used for environents
   during evaluation, including hole environments. EvalEnvs are numbered
   so that operations on them (e.g., during hole numbering) can be memoized.

   Both EvalEnv.t and Environment.t are often named sigma (usually for hole
   environments) or env.

   This mimicks the VarMap interface on the extended EvalEnv.t type. Most
   operations require an EvalEnvIdGen.t parameter, which is used to generate
   unique ID's for each environment, and is created using EvalEnv.empty
   (at the beginning of evaluation).
   */

exception InvalidEvalEnvType;

[@deriving sexp]
type t = DHExp.evalenv
and result = DHExp.result
and result_map = VarMap.t_(result);

let id_of_evalenv: t => option(int);
let result_map_of_evalenv: t => result_map;
let environment_of_evalenv: t => Environment.t;

let empty: (EvalEnvIdGen.t, t);
let unreached: t;
let is_empty: t => bool;
let length: t => int;
let to_list: t => list(VarMap.t__(result));
let lookup: (t, Var.t) => option(result);
let contains: (t, Var.t) => bool;

/* these functions require an EvalEnvIdGen.t because they generate a new
   EvalEnv.t */
let extend: (EvalEnvIdGen.t, t, VarMap.t__(result)) => (EvalEnvIdGen.t, t);
let map:
  (EvalEnvIdGen.t, VarMap.t__(result) => result, t) => (EvalEnvIdGen.t, t);
let filter:
  (EvalEnvIdGen.t, VarMap.t__(result) => bool, t) => (EvalEnvIdGen.t, t);

/* union(new_env, env) extends env with new_env (same argument order
   as in VarMap.union) */
let union: (EvalEnvIdGen.t, t, t) => (EvalEnvIdGen.t, t);
let union_from_env:
  (EvalEnvIdGen.t, t, VarMap.t_(result)) => (EvalEnvIdGen.t, t);
let union_with_env:
  (EvalEnvIdGen.t, VarMap.t_(result), t) => (EvalEnvIdGen.t, t);

/* same as map, but doesn't assign a new ID. (This is used when
   transforming an environment, such as in the closure->lambda stage
   after evaluation. More functions may be added like this as-needed
   for similar purposes.) */
let map_keep_id: (VarMap.t__(result) => result, t) => t;
