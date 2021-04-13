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
  // BUG(andrew): if list rotated, filter looks weird
  // BUG(andrew): move to next hole should reset scroll position
  print_endline("COMPUTE ACTIONS:");
  print_endline(
    Sexplib.Sexp.to_string_hum(
      CursorInfo.sexp_of_syntactic_context(cursor.syntactic_context),
    ),
  );
  compute_var_actions(cursor)
  @ compute_app_actions(cursor)
  @ compute_lit_actions(cursor)
  |> raise_search_matches(term_to_str(term));
};
