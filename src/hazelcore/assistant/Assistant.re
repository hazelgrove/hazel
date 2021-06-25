//open OptUtil.Syntax;
open Assistant_common;
open Assistant_Exp;

let sort_by_prefix =
    (prefix: string, actions: list(assistant_action))
    : list(assistant_action) => {
  let matches =
    List.filter(a => StringUtil.match_prefix(prefix, a.text), actions);
  let compare = (a1, a2) => String.compare(a1.text, a2.text);
  // NOTE: sort gooduns if they are nontrivial matches
  let matches = prefix == "" ? matches : List.sort(compare, matches);
  let nonmatches =
    List.filter(a => !StringUtil.match_prefix(prefix, a.text), actions);
  matches @ nonmatches;
};

let get_operand_actions = (ci: cursor_info_pro): list(assistant_action) =>
  switch (ci.term) {
  | Exp(_) => Assistant_Exp.operand_actions(ci)
  | _ => []
  };

let get_operator_actions = (ci: cursor_info_pro): list(assistant_action) =>
  switch (ci.term) {
  | ExpOp(_) => Assistant_Exp.operator_actions(ci)
  | _ => []
  };

let sort_by_delta =
    (action_list: list(assistant_action)): list(assistant_action) => {
  let compare = (a1: assistant_action, a2: assistant_action) =>
    Int.compare(a2.delta_errors, a1.delta_errors);
  List.sort(compare, action_list);
};

let get_actions =
    (
      {term, syntactic_context, mode, expected_ty, actual_ty, _} as ci: cursor_info_pro,
    )
    : list(assistant_action) => {
  if (true) {
    print_endline("ASSISTANT DEBUG:");
    P.p("  expected_ty: %s\n", HTyp.sexp_of_t(expected_ty));
    switch (actual_ty) {
    | None => print_endline("  actual_ty: None")
    | Some(ty) => P.p("actual_ty: %s\n", HTyp.sexp_of_t(ty))
    };
    P.p("  mode: %s\n", sexp_of_mode(mode));
    P.p(
      "  syntactic_context: %s\n",
      CursorInfo.sexp_of_syntactic_context(syntactic_context),
    );
    P.p("  term: %s\n", CursorInfo.sexp_of_cursor_term(term));
    switch (syntactic_context) {
    | NoSeq => print_endline("the goggles")
    | ExpSeq(_ty, seq, _err) =>
      let path = seq |> ZExp.mk_ZOpSeq |> CursorPath_Exp.of_zopseq;
      print_endline("path:");
      print_endline(Sexplib.Sexp.to_string_hum(CursorPath.sexp_of_t(path)));
    };
  };
  virtual_actions(ci)
  @ get_operand_actions(ci)
  @ get_operator_actions(ci)
  |> sort_by_delta
  |> sort_by_prefix(term_to_str(term));
};

let get_actions_of_ty =
    (ci: cursor_info_pro, ty: HTyp.t): list(assistant_action) =>
  ci |> get_actions |> List.filter(a => HTyp.consistent(a.res_ty, ty));
