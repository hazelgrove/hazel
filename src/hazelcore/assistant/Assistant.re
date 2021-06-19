open OptUtil.Syntax;
open Assistant_common;
open Assistant_Exp;

let raise_search_matches =
    (prefix: string, actions: list(assistant_action))
    : list(assistant_action) => {
  let gooduns =
    List.filter(
      ({text, _}) => StringUtil.match_prefix(prefix, text),
      actions,
    );
  // NOTE: sort gooduns if they are nontrivial matches
  let gooduns =
    prefix == ""
      ? gooduns
      : List.sort((a1, a2) => String.compare(a1.text, a2.text), gooduns);
  let baduns =
    List.filter(
      ({text, _}) => !StringUtil.match_prefix(prefix, text),
      actions,
    );
  gooduns @ baduns;
};

let compute_actions =
    ({term, _} as cursor: cursor_info_pro): list(assistant_action) => {
  compute_operand_actions(cursor)
  @ compute_operator_actions(cursor)
  |> raise_search_matches(term_to_str(term));
};

let get_action_index = (assistant_selection: option(int), actions): int => {
  let num_actions = List.length(actions);
  switch (assistant_selection) {
  | None => 0
  | Some(i) =>
    let z = num_actions == 0 ? 0 : i mod num_actions;
    z + (z < 0 ? num_actions : 0);
  };
};

let get_actions_of_ty = (cursor, filter_ty) =>
  cursor
  |> compute_actions
  |> List.filter(a => HTyp.consistent(a.res_ty, filter_ty));

let select_action =
    (
      assistant_selection: option(int),
      u_gen: MetaVarGen.t,
      cursor_info: CursorInfo.t,
    )
    : option(Action.t) => {
  let+ cursor = promote_cursor_info(cursor_info, u_gen);
  let actions = compute_actions(cursor);
  let selected_index = get_action_index(assistant_selection, actions);
  List.nth(actions, selected_index).action;
};
