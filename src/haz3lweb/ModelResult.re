open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type previous = list(ProgramResult.t);

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

let init = previous => {previous: [previous], current: ResultPending};

let get_previous = ({previous, _}) => List.hd(previous);
let put_previous = (previous: ProgramResult.t, cr: t) => {
  ...cr,
  previous: [previous, ...cr.previous],
};
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

let get_record = res =>
  switch (get_current_ok(res)) {
  | Some(r) => [r] @ res.previous
  | None => res.previous
  };

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

let step_backward = res => {
  switch (res.previous) {
  | [] => failwith("empty ModelResult.t.previous")
  | [_] => res
  | [hd, ...tl] => {previous: tl, current: ResultOk(hd)}
  };
};

type optional_simple_data = {
  opt_eval_results: list(Haz3lcore.DHExp.t),
  opt_test_results: option(Interface.test_results),
};

type simple_data = {
  eval_results: list(Haz3lcore.DHExp.t),
  test_results: Interface.test_results,
};

type simple = option(simple_data);

let get_simple = (res: option(t)): simple =>
  res
  |> Option.map(res => res |> get_record)
  |> Option.map((r: list(ProgramResult.t)) => {
       let eval_results = r |> List.map(ProgramResult.get_dhexp);
       let test_results =
         List.hd(r)
         |> ProgramResult.get_state
         |> Haz3lcore.EvaluatorState.get_tests
         |> Interface.mk_results;
       {eval_results, test_results};
     });

let unwrap_test_results = (simple: simple): option(Interface.test_results) => {
  Option.map(simple_data => simple_data.test_results, simple);
};

let unwrap_eval_result = (simple: simple): list(Haz3lcore.DHExp.t) => {
  switch (simple) {
  | Some(simple_data) => simple_data.eval_results
  | None => []
  };
};

let unwrap_simple = (simple: simple): optional_simple_data =>
  switch (simple) {
  | None => {opt_eval_results: [], opt_test_results: None}
  | Some({eval_results, test_results}) => {
      opt_eval_results: eval_results,
      opt_test_results: Some(test_results),
    }
  };
