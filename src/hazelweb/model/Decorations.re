open Sexplib.Std;

[@deriving sexp]
type t = {
  err_holes: list(CursorPath_common.steps),
  current_term: option(CursorPath_common.t),
};

let is_empty = (ds: t): bool =>
  ListUtil.is_empty(ds.err_holes) && ds.current_term == None;

let take_step = (step: int, decorations: t): t => {
  let {err_holes, current_term} = decorations;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {err_holes, current_term};
};

let current = (decorations: t): list(Decoration.t) => {
  let err_holes =
    decorations.err_holes
    |> List.find_opt((==)([]))
    |> Option.map(_ => Decoration.ErrHole)
    |> Option.to_list;
  let current_term =
    switch (decorations.current_term) {
    | Some(([], cursor)) => [Decoration.CurrentTerm(cursor)]
    | _ => []
    };
  List.concat([err_holes, current_term]);
};
