[@deriving sexp]
type operand_strategy =
  | Delete
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertCase
  | WrapApp
  | WrapCase
  | ConvertLit;

[@deriving sexp]
type operand_suggestion = {
  operand: UHExp.operand,
  operand_strategy,
  report: SuggestionReportExp.operand_report,
};

[@deriving sexp]
type t =
  | ReplaceOperand(operand_suggestion);

[@deriving sexp]
type generator' = CursorInfo.t => t;

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((suggestions, g) => g(ci) @ suggestions, [], gs);

let score = (suggestion: t) =>
  switch (suggestion) {
  | ReplaceOperand({report: {scores, _}, _}) =>
    scores
    |> SuggestionReportExp.scores_params
    |> List.map(((score, param)) => param *. score)
    |> List.fold_left((+.), 0.)
  };

let compare: (t, t) => int =
  (a1, a2) => Float.compare(score(a2), score(a1));

let get_action: t => Action.t =
  fun
  | ReplaceOperand({operand, _}) => ReplaceOperand(operand, None);
