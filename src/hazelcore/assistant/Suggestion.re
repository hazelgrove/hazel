[@deriving sexp]
type operand_strategy =
  | Delete
  | InsertVar
  | InsertLit
  | InsertApp
  | InsertCase
  | WrapCase
  | WrapLit
  | WrapApp
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
  | InsertLit
  | InsertExpType
  | InsertPatType
  | InsertJoin;

[@deriving sexp]
type typ_operand_suggestion = {
  operand: UHTyp.operand,
  typ_operand_strategy,
  report: SuggestionReportTyp.operand_report,
};

[@deriving sexp]
type t =
  | ReplaceExpOperand(operand_suggestion)
  | ReplacePatOperand(pat_operand_suggestion)
  | ReplaceTypOperand(typ_operand_suggestion);

[@deriving sexp]
type generator' = CursorInfo.t => t;

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((suggestions, g) => g(ci) @ suggestions, [], gs);

let action: t => Action.t =
  fun
  | ReplaceExpOperand({operand, _}) => ReplaceOperand(Exp(operand, None))
  | ReplacePatOperand({operand, _}) => ReplaceOperand(Pat(operand, None))
  | ReplaceTypOperand({operand, _}) => ReplaceOperand(Typ(operand, None));

let sort_of: t => TermSort.t =
  fun
  | ReplaceExpOperand(_) => Exp
  | ReplacePatOperand(_) => Pat
  | ReplaceTypOperand(_) => Typ;

let show_text: t => string =
  fun
  | ReplaceExpOperand({report: {show_text, _}, _}) => show_text
  | ReplacePatOperand({report: {show_text, _}, _}) => show_text
  | ReplaceTypOperand({report: {show_text, _}, _}) => show_text;

let show_syntax: t => TermSort.syntax =
  fun
  | ReplaceExpOperand({operand, _}) => Exp(UHExp.Block.wrap(operand))
  | ReplacePatOperand({operand, _}) => Pat(OpSeq.wrap(operand))
  | ReplaceTypOperand({operand, _}) => Typ(OpSeq.wrap(operand));

let result_ty: t => option(HTyp.t) =
  fun
  | ReplaceExpOperand({report: {result_ty, _}, _})
  | ReplacePatOperand({report: {result_ty, _}, _}) => Some(result_ty)
  | ReplaceTypOperand(_) => None;

let scorer = (scores: 'a, parametrize: 'a => list((float, float))) =>
  scores
  |> parametrize
  |> List.map(((score, param)) => param *. score)
  |> List.fold_left((+.), 0.);

let score: t => float =
  fun
  | ReplaceExpOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportExp.scores_params)
  | ReplacePatOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportPat.scores_params)
  | ReplaceTypOperand({report: {scores, _}, _}) =>
    scorer(scores, SuggestionReportTyp.scores_params);

let compare: (t, t) => int =
  (a1, a2) => Float.compare(score(a2), score(a1));
