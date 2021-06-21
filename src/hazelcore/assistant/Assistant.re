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

let get_actions = ({term, _} as ci: cursor_info_pro): list(assistant_action) => {
  virtual_actions(ci)
  @ get_operand_actions(ci)
  @ get_operator_actions(ci)
  |> sort_by_prefix(term_to_str(term));
};

let get_actions_of_ty =
    (ci: cursor_info_pro, ty: HTyp.t): list(assistant_action) =>
  ci |> get_actions |> List.filter(a => HTyp.consistent(a.res_ty, ty));
