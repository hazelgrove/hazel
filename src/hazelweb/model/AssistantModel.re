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

let get_operand_suggestions = (ci: CursorInfo.pro): list(suggestion) =>
  switch (ci.term) {
  | Exp(_) => Assistant_Exp.operand_suggestions(ci)
  | _ => []
  };

let get_operator_suggestions = (ci: CursorInfo.pro): list(suggestion) =>
  switch (ci.term) {
  | ExpOp(_) => Assistant_Exp.operator_suggestions(ci)
  | _ => []
  };

let sort_suggestions = (suggestions: list(suggestion)): list(suggestion) => {
  let compare = (a1: suggestion, a2: suggestion) =>
    Int.compare(a2.delta_errors + a2.score, a1.delta_errors + a1.score);
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
      {term, syntactic_context, mode, expected_ty, actual_ty, opParent, _} as ci: CursorInfo.pro,
    )
    : list(suggestion) => {
  if (false) {
    //print_endline("ASSISTANT DEBUG:");
    switch (opParent) {
    | None => print_endline("TRAD opParent: None")
    | Some(opp) => P.p("TRAD opParent: %s\n", ZExp.sexp_of_zoperand(opp))
    };
    P.p("TRAD expected_ty: %s\n", HTyp.sexp_of_t(expected_ty));
    switch (actual_ty) {
    | None => print_endline("TRAD actual_ty: None")
    | Some(ty) => P.p("TRAD actual_ty: %s\n", HTyp.sexp_of_t(ty))
    };

    P.p("  mode: %s\n", CursorInfo.sexp_of_mode(mode));
    P.p(
      "TRAD nearest zopseq: %s\n",
      CursorInfo.sexp_of_syntactic_context(syntactic_context),
    );
  };
  get_operand_suggestions(ci)
  @ get_operator_suggestions(ci)
  |> List.map(renumber_suggestion_holes(ci.ctx, ci.u_gen))
  |> sort_suggestions
  |> sort_by_prefix(CursorInfo_common.string_and_index_of_cursor_term(term));
};

let get_suggestions_of_ty =
    (ci: CursorInfo.pro, ty: HTyp.t): list(suggestion) =>
  ci
  |> get_suggestions
  |> List.filter((s: suggestion) => HTyp.consistent(s.res_ty, ty));

let get_action =
    ({selection_index, filter_editor, _}: t, ci: CursorInfo.pro)
    : option(Action.t) => {
  let filter_ty = Program.get_ty(filter_editor);
  let suggestions = get_suggestions_of_ty(ci, filter_ty);
  let selection_index = wrap_index(selection_index, suggestions);
  let+ selection = List.nth_opt(suggestions, selection_index);
  selection.action;
};

let get_display_suggestions =
    (
      ci: CursorInfo.pro,
      {selection_index, choice_display_limit, filter_editor, _}: t,
    )
    : list(suggestion) => {
  let filter_ty = Program.get_ty(filter_editor);
  let suggestions = get_suggestions_of_ty(ci, filter_ty);
  let selection_index = wrap_index(selection_index, suggestions);
  suggestions
  |> ListUtil.rotate_n(selection_index)
  |> ListUtil.trim(choice_display_limit);
};
