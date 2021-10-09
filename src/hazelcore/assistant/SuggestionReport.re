open OptUtil.Syntax;
open Sexplib.Std;

[@deriving sexp]
type score_exp = {
  idiomaticity: float,
  type_specificity: float,
  delta_errors: float,
  syntax_conserved: float,
};

[@deriving sexp]
type report_exp_operand = {
  operand: UHExp.operand,
  ty: HTyp.t,
  score: score_exp,
  show_text: string,
};

let score_params_exp = (score: score_exp) => [
  (score.delta_errors, 1.),
  (score.idiomaticity, 1.),
  (score.type_specificity, 1.),
  (score.syntax_conserved, 1.5),
];

let type_specificity_score =
    (expected_ty: HTyp.t, result_ty: HTyp.t, actual_ty: HTyp.t): float =>
  switch (HTyp.compare(result_ty, actual_ty)) {
  | _ when expected_ty == Hole => 0.
  | _ when !HTyp.consistent(expected_ty, result_ty) => 0.
  | Incomparable
  | Equal => 0.
  | GT => 1.
  | LT => (-1.)
  };

let hole_not_empty = (hi: CursorPath.hole_info) =>
  switch (hi.sort) {
  | ExpHole(_, TypeErr | VarErr)
  | PatHole(_, TypeErr | VarErr) => true
  | _ => false
  };

let err_holes = (ze: ZExp.t): list(CursorPath.hole_info) =>
  CursorPath_Exp.holes(ZExp.erase(ze), [], [])
  |> List.filter(hole_not_empty);

let error_score =
    (action: Action.t, {ctx, enclosing_zopseq, _}: CursorInfo.t): float => {
  switch (enclosing_zopseq) {
  | ExpSeq(zopseq, expected_ty) =>
    let (opseq_expected_ty, old_zexp) = (
      HTyp.relax(expected_ty),
      ZExp.ZBlock.wrap'(zopseq),
    );
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
      0.;
    | Succeeded((new_zexp, new_type, _)) =>
      let (actual_ty, new_zexp) = (new_type, new_zexp);
      let context_consistent_after =
        HTyp.consistent(opseq_expected_ty, actual_ty);
      let internal_errors_before = old_zexp |> err_holes |> List.length;
      let internal_errors_after = new_zexp |> err_holes |> List.length;
      let context_errors =
        switch (context_consistent_after) {
        | false => (-1)
        | true => 0
        };
      let internal_errors = internal_errors_before - internal_errors_after;
      float_of_int(internal_errors + context_errors);
    };
  | _ =>
    print_endline("Warning: opseq_report: no zopseq provided");
    0.;
  };
};

let operand_has_function_type = (operand: UHExp.operand, ctx: Contexts.t) =>
  switch (Statics_Exp.syn_operand(ctx, operand)) {
  | Some(Arrow(_, _)) => true
  | _ => false
  };

let idiomaticity_score_internal =
    (enclosing_operand: UHExp.operand, ctx: Contexts.t): float =>
  switch (enclosing_operand) {
  /* weird case scrutinees */
  | Case(_, [ExpLine(OpSeq(_, S(scrut, _)))], _) =>
    switch (scrut) {
    | Inj(_)
    | IntLit(_)
    | BoolLit(_)
    | FloatLit(_)
    | ListNil(_) => (-1.)
    | Case(_) => (-2.)
    | _ when operand_has_function_type(scrut, ctx) => (-2.)
    | _ => 0.
    }
  /* weird function expressions in (parenthesized) apps */
  | Parenthesized([ExpLine(OpSeq(_, S(f_exp, A(Space, _))))]) =>
    switch (f_exp) {
    | Case(_) => (-2.)
    | Lam(_) => (-1.)
    | _ => 0.
    }
  | _ => 0.
  };

let idiomaticity_score_context =
    (
      operand: UHExp.operand,
      enclosing_zoperand: CursorInfo.enclosing_zoperand,
      ctx: Contexts.t,
    )
    : float =>
  switch (enclosing_zoperand) {
  /* weird case scrutinees */
  | Some(CaseZE(_)) =>
    switch (operand) {
    | Inj(_)
    | IntLit(_)
    | BoolLit(_)
    | FloatLit(_)
    | ListNil(_) => (-1.)
    | Case(_) => (-2.)
    | _ when operand_has_function_type(operand, ctx) => (-2.)
    | _ => 0.
    }
  /* weird function expressions in (parenthesized) apps */
  | Some(
      ParenthesizedZ((
        [],
        ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), (E, A(Space, _))))),
        [],
      )),
    ) =>
    switch (operand) {
    | Case(_) => (-2.)
    | Lam(_) => (-1.)
    | _ => 0.
    }
  | _ => 0.
  };

let idiomaticity_score =
    (
      operand: UHExp.operand,
      enclosing_zoperand: CursorInfo.enclosing_zoperand,
      ctx: Contexts.t,
    )
    : float =>
  idiomaticity_score_context(operand, enclosing_zoperand, ctx)
  +. idiomaticity_score_internal(operand, ctx);

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
let syntax_conserved_score = (~cursor_term, ~result_str: string): float => {
  let term_str = CursorInfo_common.string_of_cursor_term(cursor_term);
  let term_idx = CursorInfo_common.index_of_cursor_term(cursor_term);
  let (before_caret, after_caret) =
    StringUtil.split_string(term_idx, term_str);
  let cursor_text_length = String.length(term_str);
  let result_length = String.length(result_str);
  let (total_match_length, imm) =
    switch (submatches_and_offsets(before_caret, after_caret, result_str)) {
    | (None, None) => (0, result_length)
    | (None, Some((s, i)))
    | (Some((s, i)), None) => (String.length(s), i)
    | (Some((s1, i)), Some((s2, _))) => (String.length(s1 ++ s2), i)
    };
  let length_ratio =
    result_length == 0
      ? 0.
      : float_of_int(total_match_length) /. float_of_int(cursor_text_length);
  let length_rounded = Float.round(10. *. length_ratio) /. 10.;
  let immediacy_ratio =
    result_length == 0
      ? 0. : 1.0 -. float_of_int(imm) /. float_of_int(result_length);
  let immediacy_ratio_rounded = Float.round(10. *. immediacy_ratio) /. 10.;
  cursor_text_length == 0
    ? 0. : length_rounded +. 0.1 *. immediacy_ratio_rounded;
};

let mk_exp_operand_score =
    (
      action: Action.t,
      ~operand: UHExp.operand,
      result_ty: HTyp.t,
      result_str: string,
      {enclosing_zoperand, expected_ty, actual_ty, cursor_term, ctx, _} as ci: CursorInfo.t,
    )
    : score_exp => {
  idiomaticity: idiomaticity_score(operand, enclosing_zoperand, ctx),
  type_specificity:
    type_specificity_score(expected_ty, result_ty, HTyp.relax(actual_ty)),
  delta_errors: error_score(action, ci),
  syntax_conserved: syntax_conserved_score(~cursor_term, ~result_str),
};

let mk_exp_operand_report =
    (action: Action.t, operand: UHExp.operand, ci: CursorInfo.t)
    : report_exp_operand => {
  let ty = HTyp.relax(Statics_Exp.syn_operand(ci.ctx, operand));
  let show_text = UHExp.string_of_operand(operand);
  let score = mk_exp_operand_score(action, ~operand, ty, show_text, ci);
  {operand, ty, show_text, score};
};
