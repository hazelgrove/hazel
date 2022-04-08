open Sexplib.Std;

[@deriving sexp]
type t = list(Suggestion.t);

let collect_suggestions = (ci: CursorInfo.t): t =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => SuggestionsExp.mk(ci)
  | PatOperand(_) => SuggestionsPat.mk(ci)
  | TypOperand(_) => SuggestionsTyp.mk(ci)
  | _ => []
  };

let suggestion_isnt_noop =
    (cursor_term: CursorInfo.cursor_term, s: Suggestion.t): bool => {
  switch (cursor_term, s) {
  | (ExpOperand(_, op), ReplaceExpOperand({operand: op', _})) =>
    !Assistant_common.equals_operand(op, op')
  | (PatOperand(_, op), ReplacePatOperand({operand: op', _})) =>
    !Assistant_common.equals_pat_operand(op, op')
  | (TypOperand(_, op), ReplaceTypOperand({operand: op', _})) =>
    !Assistant_common.equals_typ_operand(op, op')
  | _ => true
  };
};

let suggestion_result_equals = (s: Suggestion.t, s': Suggestion.t): bool =>
  switch (s, s') {
  | (
      ReplaceExpOperand({operand: op, _}),
      ReplaceExpOperand({operand: op', _}),
    ) =>
    Assistant_common.equals_operand(op, op')
  | (
      ReplacePatOperand({operand: op, _}),
      ReplacePatOperand({operand: op', _}),
    ) =>
    Assistant_common.equals_pat_operand(op, op')
  | (
      ReplaceTypOperand({operand: op, _}),
      ReplaceTypOperand({operand: op', _}),
    ) =>
    Assistant_common.equals_typ_operand(op, op')
  | _ => false
  };

let deduplicate_suggestions: t => t =
  List.fold_left(
    (uniques, s) =>
      List.exists(suggestion_result_equals(s), uniques)
        ? uniques : uniques @ [s],
    [],
  );

let renumber_suggestion_holes = (ctx, u_gen, s: Suggestion.t): Suggestion.t =>
  switch (s) {
  | ReplaceExpOperand({operand, _} as operand_suggestion) =>
    let (operand, _, _) =
      Statics_Exp.syn_fix_holes_operand(
        ctx,
        u_gen - 1,
        ~renumber_empty_holes=true,
        operand,
      );
    ReplaceExpOperand({...operand_suggestion, operand});
  | ReplacePatOperand({operand, _} as operand_suggestion) =>
    let (operand, _, _, _) =
      Statics_Pat.syn_fix_holes_operand(
        ctx,
        u_gen - 1,
        ~renumber_empty_holes=true,
        operand,
      );
    ReplacePatOperand({...operand_suggestion, operand});
  | ReplaceTypOperand(_) => s
  };

let consistent_with_context = (expected_ty: HTyp.t, s: Suggestion.t) =>
  switch (s) {
  | ReplaceExpOperand({report: {result_ty, _}, _}) =>
    HTyp.consistent(expected_ty, result_ty)
  | ReplacePatOperand(_) => true
  | ReplaceTypOperand(_) => true
  };

let dont_make_more_errors = (s: Suggestion.t) =>
  switch (s) {
  | ReplaceExpOperand({report: {scores: {delta_errors, _}, _}, _}) =>
    delta_errors >= 0.
  | ReplacePatOperand(_) => true
  | ReplaceTypOperand(_) => true
  };

let mk =
    (
      {cursor_term, ctx, _} as ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      ty: HTyp.t,
    )
    : list(Suggestion.t) =>
  ci
  |> collect_suggestions
  |> List.filter(dont_make_more_errors)
  |> List.filter(s => Suggestion.score(s) >= (-0.5))
  |> List.filter(suggestion_isnt_noop(cursor_term))
  |> deduplicate_suggestions
  |> List.map(renumber_suggestion_holes(ctx, u_gen))
  |> List.sort(Suggestion.compare)
  |> List.filter(consistent_with_context(ty));
