open Sexplib.Std;

[@deriving sexp]
type t = {
  err_holes: list(CursorPath.steps),
  var_err_holes: list(CursorPath.steps),
  var_uses: list(CursorPath.steps),
  tests: list((CursorPath.steps, TestMap.test_report)),
  current_term: option(CursorPath.t),
};

let is_empty = (dpaths: t): bool =>
  // remember to add a case here if you add a new dpaths field
  ListUtil.is_empty(dpaths.err_holes)
  && ListUtil.is_empty(dpaths.var_err_holes)
  && ListUtil.is_empty(dpaths.var_uses)
  && ListUtil.is_empty(dpaths.tests)
  && dpaths.current_term == None;

let take_step = (step: int, dpaths: t): t => {
  let {err_holes, var_err_holes, var_uses, tests, current_term} = dpaths;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let var_uses = var_uses |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  let remove_step' = ((steps, x)) =>
    steps |> remove_step |> Option.map(s => (s, x));
  let tests = tests |> List.filter_map(remove_step');
  {err_holes, var_err_holes, var_uses, tests, current_term};
};

let current = (shape: TermShape.t, dpaths: t): list(UHDecorationShape.t) => {
  let is_current = steps =>
    switch (shape) {
    | SubBlock({hd_index, _}) => steps == [hd_index]
    | NTuple({comma_indices, _}) =>
      List.exists(n => steps == [n], comma_indices)
    | BinOp({op_index, _}) => steps == [op_index]
    | Operand
    | Case
    | Rule => steps == []
    };
  let err_holes =
    dpaths.err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.ErrHole)
    |> Option.to_list;
  let var_err_holes =
    dpaths.var_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarErrHole)
    |> Option.to_list;
  let var_uses =
    dpaths.var_uses
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarUse)
    |> Option.to_list;
  let current_term =
    switch (dpaths.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        UHDecorationShape.CurrentTerm,
      ]
    | _ => []
    };
  let tests =
    dpaths.tests
    |> List.find_opt(((steps, _)) => is_current(steps))
    |> Option.map(((_, lst)) => UHDecorationShape.TestStatus(lst))
    |> Option.to_list;
  List.concat([err_holes, var_err_holes, var_uses, tests, current_term]);
} /*taking precedent on the current term*/;
