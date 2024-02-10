/**
  The result of a program evaluation. Includes the {!type:EvaluatorResult.t},
  the {!type:EvaluatorState}, and the tracked hole instance information
  ({!type:HoleInstanceInfo.t}). Constructed by {!val:Program.get_result}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  result: EvaluatorResult.t,
  state: EvaluatorState.t,
  hii: HoleInstanceInfo.t,
  elab: DHExp.t,
};

/* A dummy value for quick error passthrough */
let init = (err: string): t => {
  result: Indet(BoundVar(err)),
  state: EvaluatorState.init,
  hii: HoleInstanceInfo.empty,
  elab: BoundVar(err),
};

let get_dhexp = (r: t) => EvaluatorResult.unbox(r.result);
let get_state = (r: t) => r.state;
