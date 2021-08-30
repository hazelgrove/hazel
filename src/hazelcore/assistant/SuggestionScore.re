open OptUtil.Syntax;

let hole_not_empty = (hi: CursorPath.hole_info) =>
  switch (hi.sort) {
  | ExpHole(_, TypeErr | VarErr)
  | PatHole(_, TypeErr | VarErr) => true
  | _ => false
  };

let err_holes = (ze: ZExp.t): list(CursorPath.hole_info) =>
  CursorPath_Exp.holes(ZExp.erase(ze), [], [])
  |> List.filter(hole_not_empty);

let type_specificity_score =
    (expected_ty: HTyp.t, res_ty: HTyp.t, actual_ty: HTyp.t) =>
  switch (HTyp.compare(res_ty, actual_ty)) {
  | _ when expected_ty == Hole => 0
  | _ when !HTyp.consistent(expected_ty, res_ty) => 0
  | Incomparable
  | Equal => 0
  | GT => 1
  | LT => (-1)
  };

let idiomaticity_score_parent =
    (action: Action.t, enclosing_zoperand: CursorInfo.enclosing_zoperand): int => {
  switch (action, enclosing_zoperand) {
  | (ReplaceOperand(operand, None), Some(parent_operand)) =>
    switch (parent_operand) {
    | CaseZE(_) =>
      switch (operand) {
      | Case(_) => (-2)
      | Lam(_) => (-3)
      | Inj(_) => (-1)
      | IntLit(_)
      | BoolLit(_)
      | FloatLit(_)
      | ListNil(_) => (-1)
      | _ => 0
      }
    /* parenthesized apps */
    | ParenthesizedZ((
        [],
        ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), (E, A(Space, _))))),
        [],
      )) =>
      /* lits/inj don't really matter as the type will never match */
      switch (operand) {
      | Case(_) => (-3)
      | Lam(_) => (-2)
      | Parenthesized(_) => (-1)
      | _ => 0
      }
    | _ => 0
    }
  | _ => 0
  };
};

let submatches_and_offsets =
    (pre: string, suf: string, target: string)
    : (option((string, int)), option((string, int))) => {
  let mog = (n: int): option((string, int)) => {
    let* m = StringUtil.matched_group_opt(n, target);
    let+ i = StringUtil.group_beginning_opt(n);
    (m, i);
  };
  let pre = StringUtil.escape_regexp_special_chars(pre);
  let suf = StringUtil.escape_regexp_special_chars(suf);
  switch (pre, suf) {
  | ("", "") => (None, None)
  | ("", _) =>
    let rs = "\\(" ++ suf ++ "\\)";
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    (mog(1), None);
  | (_, "") =>
    let rs = "\\(" ++ pre ++ "\\)";
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    (mog(1), None);
  | _ =>
    let pre' = "\\(" ++ pre ++ "\\)";
    let suf' = "\\(" ++ suf ++ "\\)";
    let both = "\\(" ++ pre' ++ ".*" ++ suf' ++ "\\)";
    let rs = both ++ "\\|" ++ pre' ++ "\\|" ++ suf';
    let _ = StringUtil.search_forward_opt(Str.regexp(rs), target);
    switch (mog(1)) {
    | Some(_) =>
      switch (mog(2), mog(3)) {
      | (Some(p0), Some(p1)) => (Some(p0), Some(p1))
      | _ => (None, None)
      }
    | None =>
      switch (mog(4), mog(5)) {
      | (Some(p), _) => (Some(p), None)
      | (_, Some(p)) => (None, Some(p))

      | _ => (None, None)
      }
    };
  };
};

/* Returns a float between 0.00 and 1.00. First decimal place represents
   match overlap, second decimal place how close match is to beginning */
let text_match_score = (ci: CursorInfo.t, result_text: string): float => {
  let (str, index) =
    CursorInfo_common.string_and_index_of_cursor_term(ci.cursor_term);
  let (before_caret, after_caret) = StringUtil.split_string(index, str);
  let result_length = String.length(result_text);
  let (total_match_length, imm) =
    switch (submatches_and_offsets(before_caret, after_caret, result_text)) {
    | (None, None) => (0, result_length)
    | (None, Some((s, i)))
    | (Some((s, i)), None) => (String.length(s), i)
    | (Some((s1, i)), Some((s2, _))) => (String.length(s1 ++ s2), i)
    };
  let length_ratio =
    result_length == 0
      ? 0. : float_of_int(total_match_length) /. float_of_int(result_length);
  let length_rounded = Float.round(10. *. length_ratio) /. 10.;
  let immediacy_ratio =
    result_length == 0
      ? 0. : 1.0 -. float_of_int(imm) /. float_of_int(result_length);
  let immediacy_ratio_rounded = Float.round(10. *. immediacy_ratio) /. 10.;
  length_rounded +. 0.1 *. immediacy_ratio_rounded;
};

let opseq_report =
    (action: Action.t, {ctx, enclosing_zopseq, _}: CursorInfo.t) => {
  let* (opseq_expected_ty, old_zexp) =
    switch (enclosing_zopseq) {
    | ExpSeq(zopseq, expected_ty) =>
      Some((HTyp.relax(expected_ty), ZExp.ZBlock.wrap'(zopseq)))
    | _ =>
      print_endline("Warning: opseq_report: no zopseq provided");
      None;
    };
  let+ (actual_ty, new_zexp) =
    switch (
      Action_Exp.syn_perform(
        ctx,
        action,
        (old_zexp, opseq_expected_ty, MetaVarGen.init),
      )
    ) {
    | Failed
    | CursorEscaped(_) =>
      print_endline("Warning: opseq_report: syn_perform failure");
      None;
    | Succeeded((new_zexp, new_type, _)) => Some((new_type, new_zexp))
    };
  let context_consistent_after =
    HTyp.consistent(opseq_expected_ty, actual_ty);
  let internal_errors_before = old_zexp |> err_holes |> List.length;
  let internal_errors_after = new_zexp |> err_holes |> List.length;
  (context_consistent_after, internal_errors_before, internal_errors_after);
};

let check_suggestion =
    (
      action: Action.t,
      res_ty: HTyp.t,
      result_text: string,
      {enclosing_zoperand, expected_ty, actual_ty, _} as ci: CursorInfo.t,
    )
    : option(Suggestion.score) => {
  /*Printf.printf(
      "action: %s\n",
      Sexplib.Sexp.to_string_hum(Action.sexp_of_t(action)),
    );*/
  let+ (
    context_consistent_after,
    internal_errors_before,
    internal_errors_after,
  ) =
    opseq_report(action, ci);
  let context_errors =
    switch (context_consistent_after) {
    | false => (-1)
    | true => 0
    };
  let internal_errors = internal_errors_before - internal_errors_after;
  let delta_errors = internal_errors + context_errors;
  let idiomaticity = idiomaticity_score_parent(action, enclosing_zoperand);

  let type_specificity =
    type_specificity_score(expected_ty, res_ty, HTyp.relax(actual_ty));
  let text_match = text_match_score(ci, result_text);
  Suggestion.{idiomaticity, type_specificity, delta_errors, text_match};
};
