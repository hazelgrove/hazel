open Sexplib.Std;

/**
  The result of a program evaluation. Includes the {!type:EvaluatorResult.t},
  the {!type:EvaluatorState}, and the tracked hole instance information
  ({!type:HoleInstanceInfo.t}). Constructed by {!val:Program.get_result}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type inner = {
  result: EvaluatorResult.t,
  state: EvaluatorState.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type off =
  | StaticsOff
  | ElaborationOff
  | DynamicsOff(DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type program_eval_error =
  | Timeout
  | EvaulatorError(EvaluatorError.t)
  | UnknownException(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Off(off)
  | ResultOk(inner)
  | ResultFail(program_eval_error)
  | ResultPending;

let get_dhexp = (r: inner) => EvaluatorResult.unbox(r.result);
let get_state = (r: inner) => r.state;
