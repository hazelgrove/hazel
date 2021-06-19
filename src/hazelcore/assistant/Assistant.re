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

let wrap_index = (index: option(int), xs): int => {
  let length = List.length(xs);
  switch (index) {
  | None => 0
  | Some(i) =>
    let z = length == 0 ? 0 : i mod length;
    z + (z < 0 ? length : 0);
  };
};

let get_operand_actions = ({term, _} as ci): list(assistant_action) =>
  switch (term) {
  | Exp(_) => Assistant_Exp.operand_actions(ci)
  | _ => []
  };

let get_operator_actions = ({term, _} as ci: cursor_info_pro) =>
  switch (term) {
  | ExpOp(_) => Assistant_Exp.operator_actions(ci)
  | _ => []
  };

let get_actions = ({term, _} as ci: cursor_info_pro): list(assistant_action) => {
  get_operand_actions(ci)
  @ get_operator_actions(ci)
  |> raise_search_matches(term_to_str(term));
};

let get_actions_of_ty = (cursor, ty) =>
  cursor |> get_actions |> List.filter(a => HTyp.consistent(a.res_ty, ty));

let select_action =
    (
      assistant_selection: option(int),
      u_gen: MetaVarGen.t,
      cursor_info: CursorInfo.t,
    )
    : option(Action.t) => {
  //TODO(andrew): properly handle empty actions case
  let+ cursor = promote_cursor_info(cursor_info, u_gen);
  let actions = get_actions(cursor);
  let selected_index = wrap_index(assistant_selection, actions);
  List.nth(actions, selected_index).action;
};
