open OptUtil.Syntax;

[@deriving sexp]
type suggestion = Suggestion.t(UHExp.t);

let string_of_operand = (text: string): UHExp.operand =>
  // NOTE: should be replaced when parser is ready
  switch (TextShape.of_text(text)) {
  | IntLit(s) => UHExp.intlit(s)
  | FloatLit(s) => UHExp.floatlit(s)
  | BoolLit(s) => UHExp.boollit(s)
  | ExpandingKeyword(Let) => UHExp.var("let")
  | ExpandingKeyword(Case) => UHExp.var("case")
  | Underscore => UHExp.var("_")
  | Var(s) => UHExp.var(s)
  | InvalidTextShape(s) => UHExp.InvalidText(0, s)
  };

let rec operand_of_string = (operand: UHExp.operand): string => {
  // NOTE: should be replaced with proper to_string when parser is ready
  // right now it special-cases case and binary apps
  switch (operand) {
  | InvalidText(_, s)
  | Var(_, _, s)
  | IntLit(_, s)
  | FloatLit(_, s) => s
  | BoolLit(_, b) => string_of_bool(b)
  | Inj(_, side, _) => "inj" ++ InjSide.to_string(side) ++ ""
  | Lam(_) => "\\"
  | Case(_, [ExpLine(OpSeq(_, S(operandA, _)))], _) =>
    "case " ++ operand_of_string(operandA)
  | Case(_, _, _) => "case"
  | Parenthesized([
      ExpLine(OpSeq(_, S(operandA, A(Space, S(operandB, _))))),
    ]) =>
    operand_of_string(operandA) ++ " " ++ operand_of_string(operandB)
  | Parenthesized([ExpLine(OpSeq(_, S(operandA, _)))]) =>
    operand_of_string(operandA)
  | ListNil(_)
  | Parenthesized(_)
  | EmptyHole(_)
  | ApPalette(_) => ""
  };
};

let operator_of_ty =
    (l: HTyp.t, r: HTyp.t, out: HTyp.t): list(Operators_Exp.t) =>
  List.concat([
    HTyp.consistent_all([l, r, out, HTyp.Bool])
      ? Operators_Exp.[And, Or] : [],
    HTyp.consistent_all([l, r, out, HTyp.Int])
      ? Operators_Exp.[Plus, Minus, Times, Divide] : [],
    HTyp.consistent_all([l, r, out, HTyp.Float])
      ? Operators_Exp.[FPlus, FMinus, FTimes, FDivide] : [],
    HTyp.consistent_all([l, r, HTyp.Int]) && HTyp.consistent(out, HTyp.Bool)
      ? Operators_Exp.[LessThan, GreaterThan, Equals] : [],
    HTyp.consistent_all([l, r, HTyp.Float])
    && HTyp.consistent(out, HTyp.Bool)
      ? Operators_Exp.[FLessThan, FGreaterThan, FEquals] : [],
  ]);

/* SYNTAX CONSTRUCTION:
 *   Make syntax without proper hole numbering, then
 *   renumber holes at assistant boundary */

let hole_operand = UHExp.EmptyHole(0);
let hole_exp = UHExp.Block.wrap(hole_operand);
let hole_pat = UHPat.EmptyHole(0) |> OpSeq.wrap;
let lambda_operand = UHExp.lam(hole_pat, hole_exp);
let rule = UHExp.Rule(hole_pat, hole_exp);
let mk_case = scrut => UHExp.case(scrut, [rule]);
let case_operand = mk_case(hole_exp);
let mk_inj = side => UHExp.inj(side, hole_exp);
let ap_seq = (operand: UHExp.operand, seq: UHExp.seq): UHExp.seq =>
  Seq.S(operand, A(Space, seq));
let mk_ap = (f_name: string, seq: UHExp.seq): UHExp.t =>
  ap_seq(UHExp.var(f_name), seq) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
let mk_bin_seq = (operand1, operator, operand2) =>
  Seq.seq_op_seq(Seq.wrap(operand1), operator, Seq.wrap(operand2));

let rec mk_ap_iter_seq =
        (f_ty: HTyp.t, hole_ty: HTyp.t): option((HTyp.t, UHExp.seq)) => {
  switch (f_ty) {
  | Arrow(_, out_ty) when HTyp.consistent(out_ty, hole_ty) =>
    Some((out_ty, Seq.wrap(hole_operand)))
  | Arrow(_, out_ty') =>
    let* (out_ty, affix) = mk_ap_iter_seq(out_ty', hole_ty);
    Some((out_ty, ap_seq(hole_operand, affix)));
  | _ => None
  };
};

let mk_ap_iter =
    ({expected_ty, _}: CursorInfo.t, f: Var.t, f_ty: HTyp.t)
    : option((HTyp.t, UHExp.t)) => {
  let+ (output_ty, holes_seq) = mk_ap_iter_seq(f_ty, expected_ty);
  (output_ty, mk_ap(f, holes_seq));
};

let mk_suggestion =
    (~category, ~result_text, ~action, ~result, ~res_ty): suggestion => {
  category,
  result_text,
  action,
  result,
  res_ty,
  score: Suggestion.blank_score,
};

/* returns a blank score if there is an error */
let mk_operand_suggestion' =
    (
      ~ci: CursorInfo.t,
      ~category: Suggestion.category,
      ~result_text: string,
      ~operand: UHExp.operand,
      ~result,
      ~action: Action.t,
    )
    : suggestion => {
  let res_ty =
    switch (Statics_Exp.syn_operand(ci.ctx, operand)) {
    | None => HTyp.Hole
    | Some(ty) => ty
    };
  let score: Suggestion.score =
    switch (SuggestionScore.check_suggestion(action, res_ty, ci)) {
    | None => Suggestion.blank_score
    | Some(res) => res
    };
  {
    ...mk_suggestion(~category, ~result_text, ~action, ~result, ~res_ty),
    score,
  };
};

let mk_operand_suggestion =
    (~operand, ~result=UHExp.Block.wrap(operand), ~category, ci) =>
  mk_operand_suggestion'(
    ~ci,
    ~category,
    ~operand,
    ~result_text=operand_of_string(operand),
    ~action=ReplaceAtCursor(operand, None),
    ~result,
  );

let mk_lit_suggestion = mk_operand_suggestion(~category=InsertLit);

// INTROS  -----------------------------------------------------------------

let mk_bool_lit_suggestion = (ci: CursorInfo.t, b: bool): suggestion =>
  mk_lit_suggestion(~operand=UHExp.boollit(b), ci);

let mk_int_lit_suggestion = (ci: CursorInfo.t, s: string): suggestion =>
  mk_lit_suggestion(~operand=UHExp.intlit(s), ci);

let mk_float_lit_suggestion = (ci: CursorInfo.t, s: string): suggestion =>
  mk_lit_suggestion(~operand=UHExp.floatlit(s), ci);

let mk_nil_list_suggestion = (ci: CursorInfo.t): suggestion =>
  mk_lit_suggestion(~operand=UHExp.listnil(), ci);

let mk_var_suggestion = (ci: CursorInfo.t, (s: string, _)): suggestion =>
  mk_operand_suggestion(~category=InsertVar, ~operand=UHExp.var(s), ci);

let mk_empty_hole_suggestion = (ci: CursorInfo.t): suggestion =>
  mk_operand_suggestion(~category=Delete, ~operand=hole_operand, ci);

let mk_inj_suggestion = (ci: CursorInfo.t, side: InjSide.t): suggestion =>
  mk_operand_suggestion(
    ~category=InsertConstructor,
    ~operand=mk_inj(side),
    ci,
  );

let mk_case_suggestion = (ci: CursorInfo.t): suggestion =>
  mk_operand_suggestion(~category=InsertElim, ~operand=case_operand, ci);

let mk_lambda_suggestion = (ci: CursorInfo.t): suggestion =>
  mk_operand_suggestion(
    ~category=InsertConstructor,
    ~operand=lambda_operand,
    ci,
  );

let mk_intro_suggestions = (ci: CursorInfo.t): list(suggestion) => [
  mk_empty_hole_suggestion(ci),
  mk_bool_lit_suggestion(ci, true),
  mk_bool_lit_suggestion(ci, false),
  mk_nil_list_suggestion(ci),
  mk_inj_suggestion(ci, L),
  mk_inj_suggestion(ci, R),
  mk_lambda_suggestion(ci),
];

let intro_suggestions =
    ({expected_ty, _} as ci: CursorInfo.t): list(suggestion) =>
  ci
  |> mk_intro_suggestions
  |> List.filter((a: suggestion) => HTyp.consistent(a.res_ty, expected_ty));

//----------------------------------------------------------------------------

let var_suggestions =
    ({ctx, expected_ty, _} as ci: CursorInfo.t): list(suggestion) =>
  expected_ty
  |> Assistant_common.extract_vars(ctx)
  |> List.map(mk_var_suggestion(ci));

//----------------------------------------------------------------------------

let mk_app_suggestion =
    (ci: CursorInfo.t, (name: string, f_ty: HTyp.t)): suggestion => {
  let (res_ty, e) =
    mk_ap_iter(ci, name, f_ty)
    |> OptUtil.get(_ => failwith("mk_app_suggestion"));
  mk_suggestion(
    ~category=InsertApp,
    ~result_text=name,
    ~action=ReplaceAtCursor(UHExp.Parenthesized(e), None),
    ~res_ty,
    ~result=e,
  );
};

let app_suggestions =
    ({ctx, expected_ty, _} as ci: CursorInfo.t): list(suggestion) => {
  expected_ty
  |> Assistant_common.fun_vars(ctx)
  |> List.map(mk_app_suggestion(ci));
};

let get_guy_from = (pos: CursorPosition.t, operand) => {
  // TODO: ??????????????????????????????????
  switch (pos, operand_of_string(operand)) {
  | (_, "") => operand
  | (OnText(i), guy) =>
    let (_pre, suf) = StringUtil.split_string(i, guy);
    string_of_operand(suf);
  | _ => operand
  };
};

let mk_wrap_case_suggestion =
    ({cursor_term, _} as ci: CursorInfo.t): suggestion => {
  let operand =
    switch (cursor_term) {
    | ExpOperand(pos, operand) =>
      let guy = get_guy_from(pos, operand);
      guy |> UHExp.Block.wrap |> mk_case;
    | _ => failwith("mk_wrap_case_suggestion impossible")
    };
  mk_operand_suggestion(~category=Wrap, ~operand, ci);
};

let elim_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  app_suggestions(ci) @ [mk_wrap_case_suggestion(ci)];

//----------------------------------------------------------------------------

let mk_operand_wrap_suggestion = (~ci: CursorInfo.t, ~category, ~operand) =>
  mk_operand_suggestion'(
    ~ci,
    ~category,
    ~operand,
    ~result_text=operand_of_string(operand),
    ~action=ReplaceAtCursor(operand, None),
  );

let mk_wrap_suggestion =
    ({cursor_term, _} as ci: CursorInfo.t, (name: string, _)) => {
  //TODO(andrew): considering splicing into opseq context
  let result =
    switch (cursor_term) {
    | ExpOperand(pos, operand) =>
      // TODO: ??????????????????????????????????
      //print_endline("666 mk_wrap_suggestion");
      let guy = get_guy_from(pos, operand);
      mk_ap(name, S(guy, E));
    | _ => failwith("mk_basic_wrap_suggestion impossible")
    };
  mk_operand_suggestion(
    ~category=Wrap,
    ~operand=UHExp.Parenthesized(result),
    ~result,
    ci,
  );
};

// TODO(andrew): label replacement suggestions seperately!! for non-empty hole case, categorization MATTERS
// ie we want to know why this is being suggested
// TODO: mode toggle for favoring simplifying versus complexifying suggestions?
// TODO: for simple/complex biasing... maybe closer to root is complex-biased, getting simpler as descends?

let wrap_suggestions =
    ({ctx, expected_ty, actual_ty, cursor_term, _} as ci: CursorInfo.t) => {
  // TODO(andrew): non-unary wraps
  switch (actual_ty, cursor_term) {
  | (_, ExpOperand(_, EmptyHole(_))) => []
  // NOTE: wrapping empty holes redundant to ap
  | (None, _) =>
    // hack, maybe, so we get wrappings for caret case
    let actual_ty = HTyp.Hole;
    Assistant_common.fun_vars(ctx, expected_ty)
    |> List.filter(((_, f_ty)) =>
         HTyp.consistent(f_ty, HTyp.Arrow(actual_ty, expected_ty))
       )
    |> List.map(mk_wrap_suggestion(ci));
  | (Some(actual_ty), _) =>
    Assistant_common.fun_vars(ctx, expected_ty)
    |> List.filter(((_, f_ty)) =>
         HTyp.consistent(f_ty, HTyp.Arrow(actual_ty, expected_ty))
       )
    |> List.map(mk_wrap_suggestion(ci))
  };
};

let str_float_to_int = s =>
  s |> float_of_string |> Float.to_int |> string_of_int;
let str_int_to_float = s =>
  s |> int_of_string |> Float.of_int |> string_of_float;

let int_float_suggestions =
    ({cursor_term, expected_ty, _} as ci: CursorInfo.t): list(suggestion) => {
  (
    switch (cursor_term) {
    | ExpOperand(_, IntLit(_, s)) when s != "0" => [
        mk_float_lit_suggestion(ci, s ++ "."),
        mk_int_lit_suggestion(ci, "0"),
      ]
    | ExpOperand(_, IntLit(_, s)) when s == "0" => [
        mk_float_lit_suggestion(ci, str_int_to_float(s)),
      ]
    | ExpOperand(_, FloatLit(_, s)) when float_of_string(s) != 0.0 =>
      s |> float_of_string |> Float.is_integer
        ? [
          mk_int_lit_suggestion(ci, str_float_to_int(s)),
          mk_float_lit_suggestion(ci, "0."),
        ]
        : [mk_int_lit_suggestion(ci, "0")]
    | ExpOperand(_, FloatLit(_, s)) when float_of_string(s) == 0.0 => [
        mk_int_lit_suggestion(ci, "0"),
      ]
    | _ => [
        mk_float_lit_suggestion(ci, "0."),
        mk_int_lit_suggestion(ci, "0"),
      ]
    }
  )
  |> List.filter((a: suggestion) => HTyp.consistent(a.res_ty, expected_ty));
};

let operand_suggestions = (ci: CursorInfo.t): list(suggestion) =>
  int_float_suggestions(ci)
  @ wrap_suggestions(ci)
  @ intro_suggestions(ci)
  @ var_suggestions(ci)
  @ elim_suggestions(ci);

let mk_replace_operator_suggestion =
    (
      seq_ty: HTyp.t,
      zseq: ZExp.zseq,
      ctx: Contexts.t,
      new_operator: Operators_Exp.t,
    )
    : suggestion => {
  /* only support binary operators */
  /* TODO(andrew): bug: resets cursor position */
  let fix_holes_local = (ctx: Contexts.t, exp: UHExp.t): UHExp.t =>
    exp
    |> Statics_Exp.syn_fix_holes(ctx, MetaVarGen.init)
    |> (((x, _, _)) => x);
  let new_seq =
    switch (ZExp.erase_zseq(zseq)) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("mk_replace_operator_suggestion impossible case")
    };
  let new_opseq = new_seq |> UHExp.mk_OpSeq;
  let ZOpSeq(_, new_zseq) = ZExp.place_before_opseq(new_opseq);
  mk_suggestion(
    ~category=ReplaceOperator,
    ~result_text=Operators_Exp.to_string(new_operator),
    ~action=ReplaceOpSeqAroundCursor(new_zseq),
    ~res_ty=seq_ty,
    ~result=UHExp.Block.wrap'(new_opseq) |> fix_holes_local(ctx),
  );
};

let actual_ty_operand = (~ctx, operand) =>
  switch (
    Statics_Exp.syn_operand(
      ctx,
      UHExp.set_err_status_operand(NotInHole, operand),
    )
  ) {
  | None => HTyp.Hole
  | Some(ty) => ty
  };

let replace_operator_suggestions =
    (ctx: Contexts.t, seq_ty: HTyp.t, zseq: ZExp.zseq, _err: ErrStatus.t) => {
  /* only supports binary operators */
  switch (ZExp.erase_zseq(zseq)) {
  | S(operand1, A(_operator, S(operand2, E))) =>
    let in1_ty = actual_ty_operand(~ctx, operand1);
    let in2_ty = actual_ty_operand(~ctx, operand2);
    operator_of_ty(in1_ty, in2_ty, seq_ty)
    |> List.map(mk_replace_operator_suggestion(seq_ty, zseq, ctx));
  | _ => []
  };
};

let operator_suggestions =
    ({syntactic_context, ctx, _}: CursorInfo.t): list(suggestion) =>
  switch (syntactic_context) {
  | ExpSeq(seq_ty, zseq, err) =>
    replace_operator_suggestions(ctx, seq_ty, zseq, err)
  | _ => []
  };
