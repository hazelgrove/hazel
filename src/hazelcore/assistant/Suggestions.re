open Sexplib.Std;

[@deriving sexp]
type suggestion = Suggestion.exp;

[@deriving sexp]
type t = list(suggestion);

let collect_suggestions = (ci: CursorInfo.t): t =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => SuggestionsExpOperand.mk(ci)
  | _ => []
  };

let sort_suggestions = (suggestions: t): t => {
  let int_score = (a: suggestion) =>
    a.score.delta_errors + a.score.idiomaticity + a.score.type_specificity;
  let text_match_multiplier = 1.5;
  let scorer = (a: suggestion) =>
    float_of_int(int_score(a)) +. text_match_multiplier *. a.score.text_match;
  let compare = (a1, a2) => Float.compare(scorer(a2), scorer(a1));
  List.sort(compare, suggestions);
};

let renumber_suggestion_holes =
    (ctx, u_gen, {action, result, _} as s: suggestion): suggestion => {
  let (result, _, _) =
    Statics_Exp.syn_fix_holes(
      ctx,
      u_gen - 1,
      ~renumber_empty_holes=true,
      result,
    );
  let action: Action.t =
    switch (action) {
    | ReplaceOperand(operand, proj_z) =>
      let (operand, _, _) =
        Statics_Exp.syn_fix_holes_operand(
          ctx,
          u_gen - 1,
          ~renumber_empty_holes=true,
          operand,
        );
      ReplaceOperand(operand, proj_z);
    | _ => action
    };
  {...s, result, action};
};

let suggestion_isnt_noop =
    (cursor_term: CursorInfo.cursor_term, s: suggestion): bool => {
  switch (s.result, cursor_term) {
  | ([ExpLine(OpSeq(_, S(op, E)))], ExpOperand(_, op')) =>
    !Assistant_common.equals_operand(op, op')
  | _ => true
  };
};

let get_suggestion_operand = (s: suggestion): option(UHExp.operand) =>
  switch (s.action) {
  | ReplaceOperand(operand, _) => Some(operand)
  | _ => None
  };

let suggestion_result_equals = (s: suggestion, s': suggestion): bool =>
  switch (get_suggestion_operand(s), get_suggestion_operand(s')) {
  | (Some(op), Some(op')) => Assistant_common.equals_operand(op, op')
  | _ => false
  };

let deduplicate_suggestions: t => t =
  List.fold_left(
    (uniques, s) =>
      List.exists(suggestion_result_equals(s), uniques)
        ? uniques : uniques @ [s],
    [],
  );

let mk =
    (
      {cursor_term, ctx, _} as ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      ty: HTyp.t,
    )
    : list(suggestion) =>
  ci
  |> collect_suggestions
  |> List.filter(suggestion_isnt_noop(cursor_term))
  |> deduplicate_suggestions
  |> List.map(renumber_suggestion_holes(ctx, u_gen))
  |> sort_suggestions
  |> List.filter((s: suggestion) => HTyp.consistent(s.result_ty, ty));
