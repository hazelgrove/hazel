/* EvalEnv is an environment (mapping variables to expressions) that
   are used during evaluation. It is different from Environment.t in two ways:

   1. It maps Var.t to Evaluator.result (rather than to DHExp.t)
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
   operations require an EvalEnvIdGen.t parameter, which is used to generate
   unique ID's for each environment, and is created using EvalEnv.empty
   (at the beginning of evaluation).
   */

[@deriving sexp]
type t = DHExp.evalenv
and result_map = VarBstMap.t(EvaluatorResult.t);

let id_of_evalenv: t => EvalEnvId.t;
let result_map_of_evalenv: t => result_map;
let environment_of_evalenv: t => Environment.t;
let alist_of_evalenv: t => list((Var.t, EvaluatorResult.t));

let empty: (EvalEnvIdGen.t, t);
let is_empty: t => bool;
let length: t => int;
let to_list: t => list((Var.t, EvaluatorResult.t));
let lookup: (t, Var.t) => option(EvaluatorResult.t);
let contains: (t, Var.t) => bool;

/* Equals only needs to check environment ID's.
   (faster than structural equality checking.) */
let equals: (t, t) => bool;

/* these functions require an EvalEnvIdGen.t because they generate a new
   EvalEnv.t */
let extend:
  (EvalEnvIdGen.t, t, (Var.t, EvaluatorResult.t)) => (EvalEnvIdGen.t, t);
let map:
  (EvalEnvIdGen.t, (Var.t, EvaluatorResult.t) => EvaluatorResult.t, t) =>
  (EvalEnvIdGen.t, t);
let filter:
  (EvalEnvIdGen.t, (Var.t, EvaluatorResult.t) => bool, t) =>
  (EvalEnvIdGen.t, t);

/* union(new_env, env) extends env with new_env (same argument order
   as in VarMap.union) */
let union: (EvalEnvIdGen.t, t, t) => (EvalEnvIdGen.t, t);

/* same as map, but doesn't assign a new ID. (This is used when
   transforming an environment, such as in the closure->lambda stage
   after evaluation. More functions may be added like this as-needed
   for similar purposes.) */
let map_keep_id: ((Var.t, EvaluatorResult.t) => EvaluatorResult.t, t) => t;

/* Placeholder used in DHCode. Is identified by an invalid
   EvalEnvId.t, only used for display purposes. */
let placeholder: t;
