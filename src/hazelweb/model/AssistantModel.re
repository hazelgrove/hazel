open OptUtil.Syntax;

type t = {
  active: bool,
  selection_index: int,
  choice_display_limit: int,
  filter_editor: Program.typ,
};

[@deriving sexp]
type update =
  | Toggle
  | Turn_on
  | Turn_off
  | Set_type_editor(UHTyp.t)
  | Reset
  | Increment_selection_index
  | Decrement_selection_index;

[@deriving sexp]
type suggestion = Assistant_Exp.suggestion;

let init = {
  active: false,
  selection_index: 0,
  choice_display_limit: 6,
  filter_editor: Program.mk_typ_editor(OpSeq.wrap(UHTyp.Hole)),
};

let put_filter_editor = (assistant_model, filter_editor) => {
  ...assistant_model,
  filter_editor,
};

let update_filter_editor = (a: Action.t, new_editor, assistant_model: t): t => {
  let edit_state =
    new_editor
    |> Program.get_edit_state
    |> Program.EditState_Typ.perform_edit_action(a);
  put_filter_editor(assistant_model, {...new_editor, edit_state});
};

let apply_update = (u: update, model: t) =>
  switch (u) {
  | Turn_off => init
  | Turn_on => {...init, active: true}
  | Toggle => {...model, active: !model.active}
  | Reset => {...init, active: model.active}
  | Increment_selection_index => {
      ...model,
      selection_index: model.selection_index + 1,
    }
  | Decrement_selection_index => {
      ...model,
      selection_index: model.selection_index - 1,
    }
  | Set_type_editor(uty) =>
    put_filter_editor(model, Program.mk_typ_editor(uty))
  };

let wrap_index = (index: int, xs: list('a)): int =>
  IntUtil.wrap(index, List.length(xs));

let sort_by_prefix =
    ((prefix: string, index: int), suggestions: list(suggestion))
    : list(suggestion) => {
  let (prefix, _) = StringUtil.split_string(index, prefix);
  let matches =
    List.filter(
      (s: suggestion) => StringUtil.match_prefix(prefix, s.result_text),
      suggestions,
    );
  let compare = (a1: suggestion, a2: suggestion) =>
    String.compare(a1.result_text, a2.result_text);
  // NOTE: sort gooduns if they are nontrivial matches
  let matches = prefix == "" ? matches : List.sort(compare, matches);
  let nonmatches =
    List.filter(
      (a: suggestion) => !StringUtil.match_prefix(prefix, a.result_text),
      suggestions,
    );
  matches @ nonmatches;
};

let get_operand_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | Exp(_) => Assistant_Exp.operand_suggestions(ci)
  | _ => []
  };

let get_operator_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | ExpOp(_) => Assistant_Exp.operator_suggestions(ci)
  | _ => []
  };

let sort_suggestions = (suggestions: list(suggestion)): list(suggestion) => {
  let compare = (a1: suggestion, a2: suggestion) =>
    Int.compare(
      a2.score.delta_errors + a2.score.idiomaticity + a2.score.type_specificity,
      a1.score.delta_errors + a1.score.idiomaticity + a1.score.type_specificity,
    );
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
    | ReplaceAtCursor(operand, proj_z) =>
      let (operand, _, _) =
        Statics_Exp.syn_fix_holes_operand(
          ctx,
          u_gen - 1,
          ~renumber_empty_holes=true,
          operand,
        );
      ReplaceAtCursor(operand, proj_z);
    // TODO| ReplaceOpSeqAroundCursor(zseq)
    | _ => action
    };
  {...s, result, action};
};

let get_suggestions =
    (
      {cursor_term, _ /*syntactic_context, expected_ty, actual_ty, opParent,*/} as ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
    )
    : list(suggestion) => {
  get_operand_suggestions(ci)
  @ get_operator_suggestions(ci)
  |> List.map(renumber_suggestion_holes(ci.ctx, u_gen))
  |> sort_suggestions
  |> sort_by_prefix(
       CursorInfo_common.string_and_index_of_cursor_term(cursor_term),
     );
};

let get_suggestions_of_ty =
    (ci: CursorInfo.t, ~u_gen: MetaVarGen.t, ty: HTyp.t): list(suggestion) =>
  ci
  |> get_suggestions(~u_gen)
  |> List.filter((s: suggestion) => HTyp.consistent(s.res_ty, ty));

let get_action =
    (
      {selection_index, filter_editor, _}: t,
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
    )
    : option(Action.t) => {
  let filter_ty = Program.get_ty(filter_editor);
  let suggestions = get_suggestions_of_ty(~u_gen, ci, filter_ty);
  let selection_index = wrap_index(selection_index, suggestions);
  let+ selection = List.nth_opt(suggestions, selection_index);
  selection.action;
};

let get_display_suggestions =
    (
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      {selection_index, choice_display_limit, filter_editor, _}: t,
    )
    : list(suggestion) => {
  let filter_ty = Program.get_ty(filter_editor);
  let suggestions = get_suggestions_of_ty(~u_gen, ci, filter_ty);
  let selection_index = wrap_index(selection_index, suggestions);
  suggestions
  |> ListUtil.rotate_n(selection_index)
  |> ListUtil.trim(choice_display_limit);
};
