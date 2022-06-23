/* Identifier for an EvalEnv.t.EvalState

   `EvalState.next_env_id` should generate a unique `EvalEnv.t`
   for each `EvalEnv.t`.
   */
[@deriving sexp]
type t = int;

/* Id of first environment */
let initial: t;
