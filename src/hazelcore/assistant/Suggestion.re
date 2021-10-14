//open Sexplib.Std;

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
type strategy =
  | ReplaceOperand(operand_strategy, SuggestionReportExp.operand_report);

[@deriving sexp]
type t = {
  strategy,
  action: Action.t,
};

[@deriving sexp]
type generator' = CursorInfo.t => t;

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((suggestions, g) => g(ci) @ suggestions, [], gs);

let score = ({strategy: ReplaceOperand(_, {scores, _}), _}: t) =>
  scores
  |> SuggestionReportExp.scores_params
  |> List.map(((score, param)) => param *. score)
  |> List.fold_left((+.), 0.);

let compare: (t, t) => int =
  (a1, a2) => Float.compare(score(a2), score(a1));

let mk = (~action: Action.t, ~strategy: strategy): t => {strategy, action};
