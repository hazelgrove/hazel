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
  print_endline("update_current");
  print_endline("res: " ++ show(res));
  print_endline("current: " ++ show_current(current));
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

type optional_simple_data = {
  opt_eval_result: option(DHExp.t),
  opt_test_results: option(TestResults.test_results),
};

// simple definitions are moved to TestResults

type simple_data =
  TestResults.simple_data = {
    eval_result: DHExp.t,
    test_results: TestResults.test_results,
  };
type simple = TestResults.simple;

let get_simple = (res: option(t)): simple =>
  res
  |> Option.map(res =>
       res |> get_current_ok |> Option.value(~default=get_previous(res))
     )
  |> Option.map(r => {
       let eval_result = r |> ProgramResult.get_dhexp;
       let test_results =
         r
         |> ProgramResult.get_state
         |> EvaluatorState.get_tests
         |> Interface.mk_results;
       {eval_result, test_results};
     });

let unwrap_test_results = TestResults.unwrap_test_results;

let unwrap_eval_result = (simple: simple): option(DHExp.t) => {
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
