open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type t = list((CursorPath.steps, UHDecorationShape.t));

let is_empty = (dpaths: t): bool => ListUtil.is_empty(dpaths);

let take_step = (step: int, dpaths: t): t => {
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  List.filter_map(
    ((steps, shape)) => {
      let+ steps' = remove_step(steps);
      (steps', shape);
    },
    dpaths,
  );
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
  List.find_all(((steps, _)) => is_current(steps), dpaths)
  |> List.map(snd);
};
