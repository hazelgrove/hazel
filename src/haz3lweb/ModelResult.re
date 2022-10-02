open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type previous = ProgramResult.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type current =
  | ResultOk(ProgramResult.t)
  | ResultFail(ProgramEvaluatorError.t)
  | ResultTimeout
  | ResultPending;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  previous,
  current,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type optional_simple_data = {
  opt_eval_result: option(Haz3lcore.DHExp.t),
  opt_test_results: option(Interface.test_results),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type simple_data = {
  eval_result: Haz3lcore.DHExp.t,
  test_results: Interface.test_results,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = option(simple_data);

let init = previous => {previous, current: ResultPending};

let get_previous = ({previous, _}) => previous;
let put_previous = (previous, cr) => {...cr, previous};
let get_previous_dhexp = cr => cr |> get_previous |> ProgramResult.get_dhexp;

let get_current = ({current, _}) => current;
let get_current_ok = res =>
  res
  |> get_current
  |> (
    fun
    | ResultOk(r) => Some(r)
    | ResultFail(_)
    | ResultTimeout
    | ResultPending => None
  );

let update_current = (current, res) => {
  let res =
    switch (res.current) {
    | ResultOk(r) => put_previous(r, res)
    | ResultFail(_)
    | ResultTimeout
    | ResultPending => res
    };

  let res = {...res, current};
  res;
};

let get_simple = (res: option(t)): simple => {
  let* res = res;
  let prev =
    res |> get_current_ok |> Option.value(~default=get_previous(res));
  let* eval_result = ProgramResult.get_dhexp(prev);
  let+ state = ProgramResult.get_state(prev);
  let test_results =
    state |> Haz3lcore.EvaluatorState.get_tests |> Interface.mk_results;
  {eval_result, test_results};
};

let unwrap_test_results = (simple: simple): option(Interface.test_results) => {
  Option.map(simple_data => simple_data.test_results, simple);
};

let unwrap_eval_result = (simple: simple): option(Haz3lcore.DHExp.t) => {
  Option.map(simple_data => simple_data.eval_result, simple);
};

let unwrap_simple = (simple: simple): optional_simple_data =>
  switch (simple) {
  | None => {opt_eval_result: None, opt_test_results: None}
  | Some({eval_result, test_results}) => {
      opt_eval_result: Some(eval_result),
      opt_test_results: Some(test_results),
    }
  };
