open Util;

/**
  The result of a program evaluation. Includes the {!type:EvaluatorResult.t},
  the {!type:EvaluatorState}, and the tracked hole instance information
  ({!type:HoleInstanceInfo.t}). Constructed by {!val:Program.get_result}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type inner = {
  result: Exp.t,
  state: EvaluatorState.t,
};

type indet = {results: list(DHExp.t)};

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Timeout
  | EvaulatorError(EvaluatorError.t)
  | UnknownException(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | ResultOk('a)
  | ResultFail(error)
  | ResultPending;

let get_dhexp = (r: inner) => r.result;
let get_state = (r: inner) => r.state;

let map = (f: 'a => 'b, r: t('a)) =>
  switch (r) {
  | ResultOk(a) => ResultOk(f(a))
  | ResultFail(e) => ResultFail(e)
  | ResultPending => ResultPending
  };
