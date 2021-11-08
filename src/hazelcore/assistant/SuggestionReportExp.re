//open OptUtil.Syntax;
open Sexplib.Std;

[@deriving sexp]
type scores = {
  idiomaticity: float,
  type_specificity: float,
  delta_errors: float,
  syntax_conserved: float,
};

[@deriving sexp]
type operand_report = {
  result_ty: HTyp.t,
  show_text: string,
  scores,
};

let scores_params = (score: scores) => [
  (score.delta_errors, 1.1),
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

let string_dist = StringUtil.levenshtein_dist(~case_sensitive=false);

let syntax_conserved_score = (search: string, result: string): float => {
  let dist = string_dist(search, result) |> fst |> float_of_int;
  let len_larger =
    float_of_int(max(String.length(search), String.length(result)));
  len_larger == 0. ? 0. : (len_larger -. dist) /. len_larger;
};

let syntax_conserved_overlay = (search: string, result: string): string =>
  snd(string_dist(search, result));

let err_holes = (ze: ZExp.t): list(CursorPath.hole_info) =>
  CursorPath_Exp.holes(ZExp.erase(ze), [], [])
  |> List.filter(CursorPath.hole_not_empty);

let update_enclosing_opseq =
    (
      old_zexp: ZExp.t,
      opseq_expected_ty: HTyp.t,
      ctx: Contexts.t,
      action: Action.t,
    )
    : option((ZExp.t, HTyp.t)) =>
  switch (
    Action_Exp.syn_perform(
      ctx,
      action,
      (old_zexp, opseq_expected_ty, MetaVarGen.init),
    )
  ) {
  | Failed
  | CursorEscaped(_) =>
    print_endline("Warning: update_enclosing_opseq: syn_perform failure");
    None;
  | Succeeded((new_zexp, new_type, _)) => Some((new_zexp, new_type))
  };

let update_opseq_report =
    (
      action: Action.t,
      ctx: Contexts.t,
      enclosing_zopseq: CursorInfo.enclosing_zopseq,
    ) => {
  switch (enclosing_zopseq) {
  | ExpSeq(zopseq, expected_ty) =>
    let opseq_expected_ty = HTyp.relax(expected_ty);
    let old_zexp = ZExp.ZBlock.wrap'(zopseq);
    switch (update_enclosing_opseq(old_zexp, opseq_expected_ty, ctx, action)) {
    | None => None
    | Some((new_zexp, new_type)) =>
      Some((new_zexp, old_zexp, new_type, opseq_expected_ty))
    };
  | _ => None
  };
};

let error_score =
    (action: Action.t, {ctx, enclosing_zopseq, _}: CursorInfo.t): float =>
  switch (update_opseq_report(action, ctx, enclosing_zopseq)) {
  | None => 0.
  | Some((new_zexp, old_zexp, new_type, opseq_expected_ty)) =>
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

let mk_operand_score =
    (
      ~action: Action.t,
      ~operand: UHExp.operand,
      ~result_ty: HTyp.t,
      ~show_text: string,
      {enclosing_zoperand, expected_ty, actual_ty, cursor_term, ctx, _} as ci: CursorInfo.t,
    )
    : scores => {
  idiomaticity: idiomaticity_score(operand, enclosing_zoperand, ctx),
  type_specificity:
    type_specificity_score(expected_ty, result_ty, HTyp.relax(actual_ty)),
  delta_errors: error_score(action, ci),
  syntax_conserved:
    syntax_conserved_score(
      CursorInfo_common.string_of_cursor_term(cursor_term),
      show_text,
    ),
};

let mk_operand_report =
    (action: Action.t, operand: UHExp.operand, ci: CursorInfo.t)
    : operand_report => {
  let result_ty = HTyp.relax(Statics_Exp.syn_operand(ci.ctx, operand));
  let show_text = UHExp.string_of_operand(operand);
  let scores =
    mk_operand_score(~action, ~operand, ~result_ty, ~show_text, ci);
  {result_ty, show_text, scores};
};
