open Util;

/**
  The result of a program evaluation. Includes the {!type:EvaluatorResult.t},
  the {!type:EvaluatorState}, and the tracked hole instance information
  ({!type:HoleInstanceInfo.t}). Constructed by {!val:Program.get_result}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type inner = {
  result: Evaluator.Result.t,
  state: EvaluatorState.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Timeout
  | EvaulatorError(EvaluatorError.t)
  | UnknownException(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Off(Elaborator.Elaboration.t)
  | ResultOk(inner)
  | ResultFail(error)
  | ResultPending;

let get_dhexp = (r: inner) => Evaluator.Result.unbox(r.result);
let get_state = (r: inner) => r.state;
