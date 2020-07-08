open Sexplib.Std;

[@deriving sexp]
type t = {
  err_holes: list(CursorPath_common.steps),
  var_err_holes: list(CursorPath_common.steps),
  current_term: option(CursorPath_common.t),
  // TODO rename to livelit_expressions
  livelits: list(CursorPath_common.steps),
};

let is_empty = (ds: t): bool =>
  ListUtil.is_empty(ds.err_holes)
  && ListUtil.is_empty(ds.var_err_holes)
  && ListUtil.is_empty(ds.livelits)
  && ds.current_term == None;

let take_step = (step: int, decorations: t): t => {
  let {err_holes, var_err_holes, current_term, livelits} = decorations;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let livelits = livelits |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {err_holes, var_err_holes, current_term, livelits};
};

let current = (term_shape: TermShape.t, decorations: t): list(Decoration.t) => {
  let is_current = steps =>
    switch (term_shape) {
    | SubBlock({hd_index, _}) => steps == [hd_index]
    | NTuple({comma_indices, _}) =>
      List.exists(n => steps == [n], comma_indices)
    | BinOp({op_index, _}) => steps == [op_index]
    | Operand(_)
    | Case(_)
    | Rule
    | Var(_)
    | Invalid
    | FreeLivelit
    | ApLivelit => steps == []
    | LivelitExpression(_) => false
    };
  let err_holes =
    decorations.err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => Decoration.ErrHole)
    |> Option.to_list;
  let var_err_holes =
    decorations.var_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => Decoration.VarErrHole)
    |> Option.to_list;
  let livelits =
    decorations.livelits
    |> List.find_opt(steps =>
         switch (term_shape) {
         | LivelitExpression({hd_index}) => steps == [hd_index]
         // | ApLivelit => steps == []
         | _ => false
         }
       )
    |> Option.map(_ => Decoration.LivelitExpression)
    |> Option.to_list;
  let current_term =
    switch (decorations.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        Decoration.CurrentTerm(term_shape),
      ]
    | _ => []
    };
  List.concat([err_holes, var_err_holes, livelits, current_term]);
};
