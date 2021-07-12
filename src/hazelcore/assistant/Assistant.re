//open OptUtil.Syntax;
open Assistant_Exp;

let sort_by_prefix =
    ((prefix: string, index: int), actions: list(assistant_action))
    : list(assistant_action) => {
  let (prefix, _) = StringUtil.split_string(index, prefix);
  let matches =
    List.filter(
      a => StringUtil.match_prefix(prefix, a.result_text),
      actions,
    );
  let compare = (a1, a2) => String.compare(a1.result_text, a2.result_text);
  // NOTE: sort gooduns if they are nontrivial matches
  let matches = prefix == "" ? matches : List.sort(compare, matches);
  let nonmatches =
    List.filter(
      a => !StringUtil.match_prefix(prefix, a.result_text),
      actions,
    );
  matches @ nonmatches;
};

let get_operand_actions = (ci: CursorInfo.pro): list(assistant_action) =>
  switch (ci.term) {
  | Exp(_) => Assistant_Exp.operand_actions(ci)
  | _ => []
  };

let get_operator_actions = (ci: CursorInfo.pro): list(assistant_action) =>
  switch (ci.term) {
  | ExpOp(_) => Assistant_Exp.operator_actions(ci)
  | _ => []
  };

let sort_actions =
    (action_list: list(assistant_action)): list(assistant_action) => {
  let compare = (a1: assistant_action, a2: assistant_action) =>
    Int.compare(a2.delta_errors + a2.score, a1.delta_errors + a1.score);
  List.sort(compare, action_list);
};

let renumber_holes_action =
    (ctx, u_gen, {action, result, _} as a: assistant_action)
    : assistant_action => {
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
  {...a, result, action};
};

let get_actions =
    (
      {term, syntactic_context, mode, expected_ty, actual_ty, opParent, _} as ci: CursorInfo.pro,
    )
    : list(assistant_action) => {
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
    /*
     P.p("  term: %s\n", CursorInfo.sexp_of_cursor_term(term));
     switch (syntactic_context) {
     | NoSeq => print_endline("the goggles do noseq")
     | ExpSeq(_ty, seq, _err) =>
       let path = seq |> ZExp.mk_ZOpSeq |> CursorPath_Exp.of_zopseq;
       print_endline("path:");
       print_endline(Sexplib.Sexp.to_string_hum(CursorPath.sexp_of_t(path)));
     };
     */
  };
  get_operand_actions(ci)
  @ get_operator_actions(ci)
  |> List.map(renumber_holes_action(ci.ctx, ci.u_gen))
  // TODO(andrew): consider using init u_gen extracted from current expr?
  // but then might have overlap after... maybe better to do in Replace action itself
  |> sort_actions
  |> sort_by_prefix(CursorInfo_common.string_and_index_of_cursor_term(term));
};

/*
 RANKING NOTES:
 error delta
 idiomacy points
 type specificity:
   for analytic, concrete types over hole
   for synthetic: none?
  */

let get_actions_of_ty =
    (ci: CursorInfo.pro, ty: HTyp.t): list(assistant_action) =>
  ci |> get_actions |> List.filter(a => HTyp.consistent(a.res_ty, ty));
