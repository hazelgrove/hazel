open Sexplib.Std;

[@deriving sexp]
type t = list(Suggestion.t);

let get_operand = (s: Suggestion.t): option(UHExp.operand) =>
  switch (s.action) {
  | ReplaceOperand(operand, _) => Some(operand)
  | _ => None
  };

let get_operand_props = (s: Suggestion.t): Suggestion.report_exp_operand =>
  switch (s.report) {
  | ExpOperand(props) => props
  };

let consistent_with_context = (expected_ty: HTyp.t, s: Suggestion.t) =>
  HTyp.consistent(expected_ty, get_operand_props(s).ty);

let collect_suggestions = (ci: CursorInfo.t): t =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => SuggestionsExpOperand.mk(ci)
  | _ => []
  };

let renumber_suggestion_holes =
    (ctx, u_gen, {action, report, _} as s: Suggestion.t): Suggestion.t =>
  switch (report) {
  | ExpOperand({operand, _} as props) =>
    let (operand, _, _) =
      Statics_Exp.syn_fix_holes_operand(
        ctx,
        u_gen - 1,
        ~renumber_empty_holes=true,
        operand,
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
    {...s, report: ExpOperand({...props, operand}), action};
  };

let suggestion_isnt_noop =
    (cursor_term: CursorInfo.cursor_term, s: Suggestion.t): bool => {
  switch (cursor_term) {
  | ExpOperand(_, op') =>
    !Assistant_common.equals_operand(get_operand_props(s).operand, op')
  | _ => true
  };
};

let suggestion_result_equals = (s: Suggestion.t, s': Suggestion.t): bool =>
  switch (get_operand(s), get_operand(s')) {
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
    : list(Suggestion.t) =>
  ci
  |> collect_suggestions
  |> List.filter(suggestion_isnt_noop(cursor_term))
  |> deduplicate_suggestions
  |> List.map(renumber_suggestion_holes(ctx, u_gen))
  |> List.sort(Suggestion.compare)
  |> List.filter(consistent_with_context(ty));
