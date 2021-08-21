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
type suggestion = Assistant_Exp.suggestion;

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

let matched_group_opt = (n: int, s: string): option(string) =>
  try(Some(Str.matched_group(n, s))) {
  | Not_found => None
  | Invalid_argument(s) =>
    print_endline("invalid_arg: matched_group_opt:");
    print_endline(s);
    None;
  };

let group_beginning_opt = (n: int): option(int) =>
  try(Some(Str.group_beginning(n))) {
  | Not_found => None
  | Invalid_argument(s) =>
    print_endline("invalid_arg: group_beginning_opt:");
    print_endline(s);
    None;
  };
let mog = (n: int, target: string): option((string, int)) => {
  let* m = matched_group_opt(n, target);
  let+ i = group_beginning_opt(n);
  (m, i);
};
let submatches_and_offsets =
    (pre: string, suf: string, target: string): list((string, int)) => {
  // let r = Str.regexp(".*((pre).*(suf))|(pre)|(suf).*")
  // TODO: sanitize. (escape chars which are regexpy)
  // need to sanitize: $^\.*+?[]
  let g3 = ".*\\(" ++ pre ++ "\\).*";
  let g4 = ".*\\(" ++ suf ++ "\\).*";
  let g0g1g2 = ".*\\(" ++ g3 ++ ".*" ++ g4 ++ "\\).*";
  let rs = "^" ++ g0g1g2 ++ "\\|" ++ g3 ++ "\\|" ++ g4 ++ "$";
  let a_ = Str.string_match(Str.regexp(rs), target, 0);
  Printf.printf("is match: %b\n", a_);
  //print_endline("submatches_and_offsets");
  //print_endline(rs);
  //print_endline(target);
  switch (mog(1, target)) {
  | Some(_) =>
    switch (mog(2, target), mog(3, target)) {
    | (Some(p0), Some(p1)) =>
      //print_endline("111111");
      [p0, p1]
    | _ =>
      //print_endline("222222");
      []
    }
  | None =>
    switch (mog(4, target), mog(5, target)) {
    | (Some(p), _)
    | (_, Some(p)) =>
      //print_endline("333333");
      [p]
    | _ =>
      //print_endline("444444");
      []
    }
  };
};

let is_filter_match = (pre: string, suf: string, target: string): bool =>
  switch (submatches_and_offsets(pre, suf, target)) {
  | [_]
  | [_, _] => true
  | _ => false
  };

let sort_by_prefix =
    ((prefix: string, index: int), suggestions: list(suggestion))
    : list(suggestion) => {
  let (before_caret, after_caret) = StringUtil.split_string(index, prefix);
  let matches =
    List.filter(
      (s: suggestion) => {
        //TODO(andrew): replace with is_filter_match
        is_filter_match(
          before_caret,
          after_caret,
          s.result_text,
        )
      },
      suggestions,
    );
  let compare = (a1: suggestion, a2: suggestion) =>
    String.compare(a1.result_text, a2.result_text);
  // NOTE: sort gooduns if they are nontrivial matches
  let matches = List.sort(compare, matches);
  //let matches = before_caret == "" ? matches : List.sort(compare, matches);
  let nonmatches =
    List.filter(
      (s: suggestion) =>
        //TODO(andrew): replace with is_filter_match
        !is_filter_match(before_caret, after_caret, s.result_text),
      suggestions,
    );
  matches @ nonmatches;
};

let get_operand_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | ExpOperand(_) => Assistant_Exp.operand_suggestions(ci)
  | _ => []
  };

let get_operator_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  switch (ci.cursor_term) {
  | ExpOperator(_) => Assistant_Exp.operator_suggestions(ci)
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

/*
 let get_suggestion =
     (model: t, ci: CursorInfo.t, ~u_gen: MetaVarGen.t)
     : option(Suggestion.t(UHExp.t)) => {
   let (suggestions, selection_index) =
     get_suggestions_of_ty'(model, ci, ~u_gen);
   List.nth_opt(suggestions, selection_index);
 };
 */

let get_display_suggestions =
    (
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      {choice_display_limit, _} as model: t,
    )
    : list(suggestion) => {
  let (suggestions, selection_index) =
    get_suggestions_of_ty'(model, ci, ~u_gen);
  suggestions
  |> ListUtil.rotate_n(selection_index)
  |> ListUtil.trim(choice_display_limit);
};

let get_indicated_suggestion =
    ({hover_index, _} as model: t, ci: CursorInfo.t, ~u_gen: MetaVarGen.t) => {
  let (suggestions, selection_index) =
    get_suggestions_of_ty'(model, ci, ~u_gen);
  let index =
    switch (hover_index) {
    | None => selection_index
    | Some(hover_index) => selection_index + hover_index
    };
  List.nth_opt(suggestions, index);
};

let get_action =
    (assistant_model: t, ci: CursorInfo.t, ~u_gen: MetaVarGen.t)
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
