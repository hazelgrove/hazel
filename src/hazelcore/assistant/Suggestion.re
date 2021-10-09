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
  | ExpOperand(SuggestionReport.report_exp_operand);

[@deriving sexp]
type t = {
  strategy,
  action: Action.t,
  report,
};

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let action_of_strategy: strategy => Action.t =
  fun
  | ReplaceOperand(_, operand) => Action.ReplaceOperand(operand, None);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((sugs, g) => g(ci) @ sugs, [], gs);

let score_params = (score: SuggestionReport.score_exp_operand) => [
  (score.delta_errors, 1.),
  (score.idiomaticity, 1.),
  (score.type_specificity, 1.),
  (score.syntax_conserved, 1.5),
];

let score = ({report, _}: t) =>
  switch (report) {
  | ExpOperand({score, _}) =>
    score
    |> score_params
    |> List.map(((score, param)) => param *. score)
    |> List.fold_left((+.), 0.)
  };

let compare = (a1, a2) => Float.compare(score(a2), score(a1));

let mk_report = (strategy: strategy, ci: CursorInfo.t): report => {
  switch (strategy) {
  | ReplaceOperand(_, operand) =>
    let action = action_of_strategy(strategy);
    ExpOperand(SuggestionReport.mk_exp_operand_report(action, operand, ci));
  };
};

let mk = (~strategy: strategy, ~ci: CursorInfo.t): t => {
  strategy,
  action: action_of_strategy(strategy),
  report: mk_report(strategy, ci),
};
