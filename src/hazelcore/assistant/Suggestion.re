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
  | ReplaceOperand(operand_strategy, UHExp.operand);

[@deriving sexp]
type report =
  | ExpOperand(SuggestionReportExp.report_operand);

[@deriving sexp]
type t = {
  strategy,
  action: Action.t,
  report,
};

[@deriving sexp]
type generator' = CursorInfo.t => t;

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((suggestions, g) => g(ci) @ suggestions, [], gs);

let score = ({report, _}: t) =>
  switch (report) {
  | ExpOperand({scores, _}) =>
    scores
    |> SuggestionReportExp.scores_params
    |> List.map(((score, param)) => param *. score)
    |> List.fold_left((+.), 0.)
  };

let compare: (t, t) => int =
  (a1, a2) => Float.compare(score(a2), score(a1));

let mk = (~action: Action.t, ~strategy: strategy, ~report: report): t => {
  strategy,
  action,
  report,
};
