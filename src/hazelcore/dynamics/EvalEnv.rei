/* EvalEnv is a specialized environment (mapping from variables
   to expressions) that are used during evaluation. It is different
   from Environment.t in two ways:

   1. It maps Var.t to Evaluator.result (rather than to DHExp.t)
   2. Each EvalEnv has an ID associated with it (if evaluation reaches it),
      or is unreachable. (i.e., a placeholder environment)

   (TODO: change mapping from DHExp.t to Evaluator.result)

   Environment.t may be useful in certain cases, namely pattern matching,
   when an evaluated result is not needed. EvalEnv is used for environents
   during evaluation, including hole environments. EvalEnvs are numbered
   so that operations on them (e.g., during hole numbering) can be memoized.

   This mimicks the VarMap interface on the extended EvalEnv.t type. Most
   operations require an EvalEnvCtx.t parameter, which is used to generate
   unique ID's for each environment, and is created using EvalEnv.empty
   (at the beginning of evaluation).
   */

module EvalEnvCtx: {
  [@deriving sexp]
  type t;
};

[@deriving sexp]
type t = DHExp.evalenv;

let id_of_evalenv: t => option(int);
let env_of_evalenv: t => Environment.t;

let empty: (EvalEnvCtx.t, t);
let unreached: t;
let is_empty: t => bool;
let length: t => int;
let to_list: t => list(VarMap.t__(DHExp.t));
let lookup: (t, Var.t) => option(DHExp.t);
let contains: (t, Var.t) => bool;

/* these functions require an EvalEnvCtx.t because they generate a new
   EvalEnv.t */
let extend: (EvalEnvCtx.t, t, VarMap.t__(DHExp.t)) => (EvalEnvCtx.t, t);
let map:
  (EvalEnvCtx.t, VarMap.t__(DHExp.t) => DHExp.t, t) => (EvalEnvCtx.t, t);
let filter:
  (EvalEnvCtx.t, VarMap.t__(DHExp.t) => bool, t) => (EvalEnvCtx.t, t);

/* union(new_env, env) extends env with new_env (same argument order
   as in VarMap.union) */
let union: (EvalEnvCtx.t, t, t) => (EvalEnvCtx.t, t);
let union_from_env: (EvalEnvCtx.t, t, Environment.t) => (EvalEnvCtx.t, t);
let union_with_env: (EvalEnvCtx.t, Environment.t, t) => (EvalEnvCtx.t, t);

/* same as map, but doesn't assign a new ID. (This is used when
   transforming an environment, such as in the closure->lambda stage
   after evaluation. More functions may be added like this as-needed
   for similar purposes.) */
let map_keep_id: (VarMap.t__(DHExp.t) => DHExp.t, t) => t;
