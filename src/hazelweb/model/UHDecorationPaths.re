open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type t = list(UHDecorationShape.t);

let is_empty = (dpaths: t): bool => ListUtil.is_empty(dpaths);

let take_step = (step: int, dpaths: t): t => {
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  List.filter_map(
    (dpath: UHDecorationShape.t) =>
      switch (dpath) {
      | ErrHole(steps) =>
        let+ steps' = remove_step(steps);
        UHDecorationShape.ErrHole(steps');
      | VarErrHole(steps) =>
        let+ steps' = remove_step(steps);
        UHDecorationShape.VarErrHole(steps');
      | VarUse(steps) =>
        let+ steps' = remove_step(steps);
        UHDecorationShape.VarUse(steps');
      | CurrentTerm((steps, cursor)) =>
        let+ steps' = remove_step(steps);
        UHDecorationShape.CurrentTerm((steps', cursor));
      | ExplanationElems((steps, color)) =>
        let+ steps' = remove_step(steps);
        UHDecorationShape.ExplanationElems((steps', color));
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
  List.find_all(
    dpath =>
      switch (dpath) {
      | UHDecorationShape.ErrHole(steps)
      | VarErrHole(steps)
      | VarUse(steps)
      | CurrentTerm((steps, _))
      | ExplanationElems((steps, _)) => is_current(steps)
      },
    dpaths,
  );
};
