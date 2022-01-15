[@deriving sexp]
type t = (option(int), VarMap.t_(DHExp.t));

module EvalEnvCtx: {
  type t = int;

  let empty: t;
  let next: t => (t, t);
};

let id_of_evalenv: t => option(int);
let env_of_evalenv: t => VarMap.t_(DHExp.t);

let empty_unnumbered: t;
let unnumbered_evalenv_of_env: Environment.t => t;

let empty: EvalEnvCtx.t => (EvalEnvCtx.t, t);
let is_empty: t => bool;
let extend: (EvalEnvCtx.t, t, VarMap.t__(DHExp.t)) => (EvalEnvCtx.t, t);
let union: (EvalEnvCtx.t, t, t) => (EvalEnvCtx.t, t);
let lookup: (t, Var.t) => option(DHExp.t);
let contains: (t, Var.t) => bool;
let map:
  (EvalEnvCtx.t, VarMap.t__(DHExp.t) => DHExp.t, t) => (EvalEnvCtx.t, t);
let filter:
  (EvalEnvCtx.t, VarMap.t__(DHExp.t) => bool, t) => (EvalEnvCtx.t, t);
let length: t => int;
let to_list: t => VarMap.t_(DHExp.t);
