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

type simple = option((Haz3lcore.DHExp.t, Interface.test_results));

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
         |> Haz3lcore.EvaluatorState.get_tests
         |> Interface.mk_results;
       (eval_result, test_results);
     });
