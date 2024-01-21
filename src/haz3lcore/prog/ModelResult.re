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
  value: option(DHExp.t),
  elab: option(DHExp.t),
  tests: option(TestResults.test_results),
};

// simple definitions are moved to TestResults

type simple_data =
  TestResults.simple_data = {
    result: DHExp.t,
    elab: DHExp.t,
    test_results: TestResults.test_results,
  };
type simple = TestResults.simple;

let get_simple = (res: option(t)): simple =>
  res
  |> Option.map(res =>
       res |> get_current_ok |> Option.value(~default=get_previous(res))
     )
  |> Option.map(r => {
       let result = r |> ProgramResult.get_dhexp;
       let elab = r |> ProgramResult.get_elab;
       let test_results =
         r
         |> ProgramResult.get_state
         |> EvaluatorState.get_tests
         |> Interface.mk_results;
       {result, test_results, elab};
     });

let unwrap_test_results = TestResults.unwrap_test_results;

let unwrap_eval_result = (simple: simple): option(DHExp.t) => {
  Option.map(simple_data => simple_data.result, simple);
};

let unwrap' = (simp: option(simple_data)): optional_simple_data =>
  switch (simp) {
  | None => {value: None, elab: None, tests: None}
  | Some({result, elab, test_results}) => {
      value: Some(result),
      tests: Some(test_results),
      elab: Some(elab),
    }
  };

let unwrap = (res: option(t)): optional_simple_data =>
  unwrap'(get_simple(res));
