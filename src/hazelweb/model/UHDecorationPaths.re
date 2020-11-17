open Sexplib.Std;

[@deriving sexp]
type t = {
  err_holes: list(CursorPath.steps),
  var_err_holes: list(CursorPath.steps),
  var_uses: list(CursorPath.steps),
  current_term: option(CursorPath.t),
  // TODO rename to livelit_expressions
  livelits: list(CursorPath.steps),
};

let is_empty = (ds: t): bool =>
  ListUtil.is_empty(ds.err_holes)
  && ListUtil.is_empty(ds.var_err_holes)
  && ListUtil.is_empty(ds.livelits)
  && ListUtil.is_empty(ds.var_uses)
  && ds.current_term == None;

let take_step = (step: int, decorations: t): t => {
  let {err_holes, var_err_holes, current_term, var_uses, livelits} = decorations;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let livelits = livelits |> List.filter_map(remove_step);
  let var_uses = var_uses |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {err_holes, var_err_holes, var_uses, current_term, livelits};
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
    | Rule
    | FreeLivelit
    | ApLivelit => steps == []
    | LivelitExpression(_) => false
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
  let livelits =
    dpaths.livelits
    |> List.find_opt(steps =>
         switch (shape) {
         | LivelitExpression({hd_index}) => steps == [hd_index]
         // | ApLivelit => steps == []
         | _ => false
         }
       )
    |> Option.map(_ => UHDecorationShape.LivelitExpression)
    |> Option.to_list;
  let current_term =
    switch (dpaths.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        UHDecorationShape.CurrentTerm,
      ]
    | _ => []
    };
  List.concat([err_holes, var_err_holes, var_uses, livelits, current_term]);
};
