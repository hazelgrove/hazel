open OptUtil.Syntax;
open Sexplib.Std;

type t = {
  active: bool,
  selection_index: int,
  hover_index: option(int),
  choice_display_limit: int,
  filter_editor: Editor.typ,
};

[@deriving sexp]
type update =
  | Toggle
  | Turn_on
  | Turn_off
  | Set_type_editor(UHTyp.t)
  | Reset
  | Increment_selection_index
  | Decrement_selection_index
  | Set_hover_index(option(int));

[@deriving sexp]
type suggestion = SuggestionsExp.suggestion;

let init = {
  active: false,
  selection_index: 0,
  hover_index: None,
  choice_display_limit: 6,
  filter_editor: Editor.mk_typ_editor(OpSeq.wrap(UHTyp.Hole)),
};

let put_filter_editor = (assistant_model, filter_editor) => {
  ...assistant_model,
  filter_editor,
};

let update_filter_editor = (a: Action.t, new_editor, assistant_model: t): t => {
  let edit_state =
    new_editor
    |> Editor.get_edit_state
    |> Editor.EditState_Typ.perform_edit_action(a);
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
    put_filter_editor(model, Editor.mk_typ_editor(uty))
  | Set_hover_index(n) => {...model, hover_index: n}
  };

let wrap_index = (index: int, xs: list('a)): int =>
  IntUtil.wrap(index, List.length(xs));

let mog = (n: int, target: string): option((string, int)) => {
  let* m = StringUtil.matched_group_opt(n, target);
  let+ i = StringUtil.group_beginning_opt(n);
  (m, i);
};

let submatches_and_offsets =
    (pre: string, suf: string, target: string)
    : (option((string, int)), option((string, int))) => {
  let pre = StringUtil.escape_regexp_special_chars(pre);
  let suf = StringUtil.escape_regexp_special_chars(suf);
  switch (pre, suf) {
  | ("", "") => (None, None)
  | ("", _) =>
    let rs = "\\(" ++ suf ++ "\\)";
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    (mog(1, target), None);
  | (_, "") =>
    let rs = "\\(" ++ pre ++ "\\)";
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    (mog(1, target), None);
  | _ =>
    let pre' = "\\(" ++ pre ++ "\\)";
    let suf' = "\\(" ++ suf ++ "\\)";
    let both = "\\(" ++ pre' ++ ".*" ++ suf' ++ "\\)";
    let rs = both ++ "\\|" ++ pre' ++ "\\|" ++ suf';
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    switch (mog(1, target)) {
    | Some(_) =>
      switch (mog(2, target), mog(3, target)) {
      | (Some(p0), Some(p1)) => (Some(p0), Some(p1))
      | _ => (None, None)
      }
    | None =>
      switch (mog(4, target), mog(5, target)) {
      | (Some(p), _) => (Some(p), None)
      | (_, Some(p)) => (None, Some(p))

      | _ => (None, None)
      }
    };
  };
};

let is_filter_match = (pre: string, suf: string, target: string): bool =>
  switch (submatches_and_offsets(pre, suf, target)) {
  | (None, None) => false
  | _ => true
  };

let compare_suggestions_by_text_match =
    (
      before_caret: string,
      after_caret: string,
      a1: suggestion,
      a2: suggestion,
    ) => {
  let s1 = a1.result_text;
  let s2 = a2.result_text;
  let m1 = submatches_and_offsets(before_caret, after_caret, s1);
  let m2 = submatches_and_offsets(before_caret, after_caret, s2);
  switch (m1, m2) {
  | ((Some(_), _), (None, _))
  | ((Some(_), Some(_)), (Some(_), None))
  | ((None, Some(_)), (None, None)) => (-1)

  | ((None, _), (Some(_), _))
  | ((Some(_), None), (Some(_), Some(_)))
  | ((None, None), (None, Some(_))) => 1

  // matches earlier in the string should show up first
  | ((Some(i0), _), (Some(i1), _)) when i0 < i1 => (-1)
  | ((_, Some(i0)), (_, Some(i1))) when i0 < i1 => (-1)
  | ((Some(i0), _), (Some(i1), _)) when i0 > i1 => 1
  | ((_, Some(i0)), (_, Some(i1))) when i0 > i1 => 1

  | _ => String.compare(s1, s2)
  };
};

let sort_by_prefix =
    ((prefix: string, index: int), suggestions: list(suggestion))
    : list(suggestion) => {
  let (before_caret, after_caret) = StringUtil.split_string(index, prefix);
  let matches =
    List.filter(
      (s: suggestion) => {
        is_filter_match(before_caret, after_caret, s.result_text)
      },
      suggestions,
    );
  let matches =
    List.sort(
      compare_suggestions_by_text_match(before_caret, after_caret),
      matches,
    );
  let nonmatches =
    List.filter(
      (s: suggestion) =>
        !is_filter_match(before_caret, after_caret, s.result_text),
      suggestions,
    );
  matches @ nonmatches;
};

let get_operand_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => SuggestionsExpOperand.mk(ci)
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

let get_suggestions =
    ({cursor_term, ctx, _} as ci: CursorInfo.t, ~u_gen: MetaVarGen.t)
    : list(suggestion) => {
  get_operand_suggestions(ci)
  |> List.map(renumber_suggestion_holes(ctx, u_gen))
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

let get_suggestions_of_ty' =
    (
      {filter_editor, selection_index, _}: t,
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
    )
    : (list(Suggestion.t(UHExp.t)), int) => {
  let filter_ty = Editor.get_ty(filter_editor);
  let suggestions = get_suggestions_of_ty(~u_gen, ci, filter_ty);
  let selection_index = wrap_index(selection_index, suggestions);
  (suggestions, selection_index);
};

let get_display_suggestions =
    (
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t=0,
      {choice_display_limit, _} as model: t,
    )
    : list(suggestion) => {
  let (suggestions, selection_index) =
    get_suggestions_of_ty'(model, ci, ~u_gen);
  suggestions
  |> ListUtil.rotate_n(selection_index)
  |> ListUtil.trim(choice_display_limit);
};

let num_suggestions = (ci: CursorInfo.t, model: t) =>
  List.length(get_display_suggestions(ci, model));

let get_indicated_suggestion =
    ({hover_index, _} as model: t, ci: CursorInfo.t, ~u_gen: MetaVarGen.t) => {
  let (suggestions, selection_index) =
    get_suggestions_of_ty'(model, ci, ~u_gen);
  let index =
    switch (hover_index) {
    | None => selection_index
    | Some(hover_index) =>
      wrap_index(selection_index + hover_index, suggestions)
    };
  List.nth_opt(suggestions, index);
};

let get_action =
    (~u_gen: MetaVarGen.t=0, assistant_model: t, ci: CursorInfo.t)
    : option(Action.t) => {
  let+ selection = get_indicated_suggestion(assistant_model, ci, ~u_gen);
  selection.action;
};

let set_hover_index = (hover_index: option(int), model: t): t => {
  ...model,
  hover_index,
};

let is_active_suggestion_index = (model: t, i: int) =>
  switch (model.hover_index) {
  | None => i == 0 // select first by default
  | Some(hover_index) => hover_index == i
  };

let is_hovering = (model: t) =>
  switch (model.hover_index) {
  | None => false
  | Some(_) => true
  };
