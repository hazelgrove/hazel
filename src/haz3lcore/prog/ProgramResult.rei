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

let init: string => t;

/**
  [get_dhexp r] is the {!type:DHExp.t} in [r].
 */
let get_dhexp: t => DHExp.t;
let get_state: t => EvaluatorState.t;
let get_elab: t => DHExp.t;
