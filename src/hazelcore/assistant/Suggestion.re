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
type pat_operand_strategy =
  | Delete
  | InsertLit;

[@deriving sexp]
type pat_operand_suggestion = {
  operand: UHPat.operand,
  pat_operand_strategy,
  report: SuggestionReportPat.operand_report_pat,
};

[@deriving sexp]
type typ_operand_strategy =
  | Delete
  | InsertLit;

[@deriving sexp]
type typ_operand_suggestion = {
  operand: UHTyp.operand,
  typ_operand_strategy,
  report: SuggestionReportTyp.operand_report,
};

[@deriving sexp]
type t =
  | ReplaceOperand(operand_suggestion)
  | ReplacePatOperand(pat_operand_suggestion)
  | ReplaceTypOperand(typ_operand_suggestion);

[@deriving sexp]
type generator' = CursorInfo.t => t;

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((suggestions, g) => g(ci) @ suggestions, [], gs);

let scorer = (scores, params) =>
  scores
  |> params
  |> List.map(((score, param)) => param *. score)
  |> List.fold_left((+.), 0.);

let score: t => float =
  fun
  | ReplaceOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportExp.scores_params)
  | ReplacePatOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportPat.scores_params)
  | ReplaceTypOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportTyp.scores_params);

let compare: (t, t) => int =
  (a1, a2) => Float.compare(score(a2), score(a1));

let get_action: t => Action.t =
  fun
  | ReplaceOperand({operand, _}) => ReplaceOperand(Exp(operand, None))
  | ReplacePatOperand({operand, _}) => ReplaceOperand(Pat(operand, None))
  | ReplaceTypOperand({operand, _}) => ReplaceOperand(Typ(operand, None));

let get_sort: t => TermSort.t =
  fun
  | ReplaceOperand(_) => Exp
  | ReplacePatOperand(_) => Pat
  | ReplaceTypOperand(_) => Typ;
