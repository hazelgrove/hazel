open Sexplib.Std;

[@deriving sexp]
type suggestion = SuggestionsExp.suggestion;

[@deriving sexp]
type t = list(suggestion);

let collect_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => SuggestionsExpOperand.mk(ci)
  | _ => []
  };

let sort_suggestions = (suggestions: list(suggestion)): list(suggestion) => {
  let int_score = (a: suggestion) =>
    a.score.delta_errors + a.score.idiomaticity + a.score.type_specificity;
  let text_match_multiplier = 2.;
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

let normalize_operand = (op: UHExp.operand) => {
  let (op, _, _) =
    Statics_Exp.syn_fix_holes_operand(
      Contexts.empty,
      0,
      ~renumber_empty_holes=true,
      op,
    );
  op;
};

let equality_modulo_hole_numbers = (op1: UHExp.operand, op2: UHExp.operand) =>
  normalize_operand(op1) == normalize_operand(op2);

let get_suggestion_operand = ({action, _}: suggestion) =>
  switch (action) {
  | ReplaceOperand(operand, _) => operand
  | _ => EmptyHole(-1)
  };

let suggestion_equals_modulo_hole_numbers = (s, s') =>
  equality_modulo_hole_numbers(
    get_suggestion_operand(s),
    get_suggestion_operand(s'),
  );

let suggestion_isnt_noop =
    (cursor_term: CursorInfo.cursor_term, {result, _}: suggestion) => {
  switch (result, cursor_term) {
  | ([ExpLine(OpSeq(_, S(a, E)))], ExpOperand(_, b)) =>
    !equality_modulo_hole_numbers(a, b)
  | _ => true
  };
};

let deduplicate_suggestions = (suggestions: list(suggestion)) => {
  List.fold_left(
    (uniques, s) =>
      List.exists(suggestion_equals_modulo_hole_numbers(s), uniques)
        ? uniques : uniques @ [s],
    [],
    suggestions,
  );
};

let mk' =
    ({cursor_term, ctx, _} as ci: CursorInfo.t, ~u_gen: MetaVarGen.t)
    : list(suggestion) => {
  collect_suggestions(ci)
  |> List.filter(suggestion_isnt_noop(cursor_term))
  |> deduplicate_suggestions
  |> List.map(renumber_suggestion_holes(ctx, u_gen))
  |> sort_suggestions;
};

let mk =
    (ci: CursorInfo.t, ~u_gen: MetaVarGen.t, ty: HTyp.t): list(suggestion) =>
  ci
  |> mk'(~u_gen)
  |> List.filter((s: suggestion) => HTyp.consistent(s.result_ty, ty));
