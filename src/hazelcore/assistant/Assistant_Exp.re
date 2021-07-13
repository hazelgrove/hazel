open OptUtil.Syntax;

[@deriving sexp]
type suggestion = Suggestion.t(UHExp.t);

let string_of_operand = (text: string): UHExp.operand =>
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
  switch (operand) {
  | InvalidText(_, s)
  | Var(_, _, s)
  | IntLit(_, s)
  | FloatLit(_, s) => s
  | BoolLit(_, b) => string_of_bool(b)
  | Inj(_, side, _) => "inj" ++ InjSide.to_string(side) ++ ""
  | Lam(_) => "\\"
  | Case(_, _, _) => "case"
  | Parenthesized([ExpLine(OpSeq(_, S(operandA, _)))]) =>
    operand_of_string(operandA) // HACK
  | ListNil(_)
  | Parenthesized(_)
  | EmptyHole(_)
  | ApPalette(_) => ""
  };
};

let operator_of_ty =
    (l: HTyp.t, r: HTyp.t, out: HTyp.t): list(Operators_Exp.t) =>
  //TODO(andrew): Add Comma, Cons, Space ops (requires a bit more deconstruction)
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

let hole_operand = UHExp.new_EmptyHole(0) |> fst;
let hole_exp = UHExp.Block.wrap(hole_operand);
let hole_pat = UHPat.new_EmptyHole(0) |> fst |> OpSeq.wrap;
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
    ({expected_ty, _}: CursorInfo.pro, f: Var.t, f_ty: HTyp.t)
    : option((HTyp.t, UHExp.t)) => {
  let+ (output_ty, holes_seq) = mk_ap_iter_seq(f_ty, expected_ty);
  (output_ty, mk_ap(f, holes_seq));
};

type context_status = {
  ty: HTyp.t,
  errors: list(CursorPath.hole_info),
};

type suggestion_report = {
  before: context_status,
  after: context_status,
  delta_errors: int,
};

let hole_not_empty = (hi: CursorPath.hole_info) =>
  switch (hi.sort) {
  | ExpHole(_, TypeErr | VarErr)
  | PatHole(_, TypeErr | VarErr) => true
  | _ => false
  };

let err_holes = ze =>
  CursorPath_Exp.holes(ZExp.erase(ze), [], [])
  |> List.filter(hole_not_empty);

let _idomaticity_score = (operand: UHExp.operand): int => {
  // this doesn't work if the cursor is already on the case scrut or fexpr in app
  // assume aps are parenthesized for now
  UHExp.(
    switch (operand) {
    | Case(_, [ExpLine(OpSeq(_, S(operand, E)))], _) =>
      switch (operand) {
      | Case(_) => (-3)
      | Lam(_) => (-3) // idea: addn cursorinfo case type check for non-enum types?
      | Inj(_) => (-1)
      | IntLit(_)
      | BoolLit(_)
      | FloatLit(_)
      | ListNil(_) => (-1)
      | _ => 0
      }
    | Parenthesized([
        ExpLine(OpSeq(_, S(operand, A(Operators_Exp.Space, _)))),
      ]) =>
      // lits/inj don't really matter as the type will never match
      switch (operand) {
      | Case(_) => (-3)
      | Lam(_) => (-2)
      | Parenthesized(_) => (-1)
      | _ => 0
      // lits/inj don't really matter as the type will never match
      }
    | _ => 0
    }
  );
};

let idomaticity_score_parent =
    (operand: UHExp.operand, opParent: CursorInfo.opParent): int => {
  switch (opParent) {
  | None => 0
  | Some(parent_operand) =>
    switch (parent_operand) {
    | CaseZE(_) =>
      switch (operand) {
      | Case(_) => (-2)
      | Lam(_) => (-3) // idea: addn cursorinfo case type check for non-enum types?
      | Inj(_) => (-1)
      | IntLit(_)
      | BoolLit(_)
      | FloatLit(_)
      | ListNil(_) => (-1)
      | _ => 0
      }
    // as this only handles operands, it only handles parenthesized apps
    | ParenthesizedZ((
        [],
        ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), (E, A(Space, _))))),
        [],
      )) =>
      // lits/inj don't really matter as the type will never match
      switch (operand) {
      | Case(_) => (-3)
      | Lam(_) => (-2)
      | Parenthesized(_) => (-1)
      | _ => 0
      }
    | _ => 0
    }
  };
};

let check_suggestion =
    (
      action: Action.t,
      res_ty: HTyp.t,
      {ctx, syntactic_context, opParent, expected_ty, _}: CursorInfo.pro,
    )
    : option((int, int)) => {
  let* (opseq_expected_ty, old_zexp, context_consistent_before) =
    switch (syntactic_context) {
    | ExpSeq(expected_ty, zseq, err) =>
      Some((
        expected_ty,
        zseq |> ZExp.mk_ZOpSeq |> ZExp.ZBlock.wrap',
        switch (err) {
        | NotInHole => true
        | _ => false
        },
      ))
    | _ => None
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
    | CursorEscaped(_) => None
    | Succeeded((new_zexp, new_type, _)) => Some((new_type, new_zexp))
    };
  let context_consistent_after =
    HTyp.consistent(opseq_expected_ty, actual_ty);
  let err_paths_before = err_holes(old_zexp);
  let err_paths_after = err_holes(new_zexp);
  let internal_errors =
    List.length(err_paths_before) - List.length(err_paths_after);
  let context_errors =
    switch (context_consistent_before, context_consistent_after) {
    | (true, false) => (-1)
    | (false, true) => 1
    | _ => 0
    };
  let delta_errors = internal_errors + context_errors;
  let score =
    switch (action) {
    | ReplaceAtCursor(operand, None) =>
      idomaticity_score_parent(operand, opParent)
    | _ => 0 // TODO (log)
    };
  // TODO: 'if res_ty is less specific then expected_ty, -1'
  let type_specificity_score =
    switch (expected_ty) {
    | Hole => 0
    | _ =>
      switch (res_ty) {
      | Hole => (-1)
      | _ => 0
      }
    };
  if (false) {
    print_endline("START check_suggestion");
    Printf.printf(
      "context_consistent before: %b\n",
      context_consistent_before,
    );
    Printf.printf("context_consistent after: %b\n", context_consistent_after);
    /*
     P.p("old_zexp: %s\n", ZExp.sexp_of_t(old_zexp));
     P.p("new_zexp AFTER: %s\n", ZExp.sexp_of_t(new_zexp));
     P.p(
       "err_paths_before %s\n",
       CursorPath.sexp_of_hole_list(err_paths_before),
     );
     P.p(
       "err_paths_after: %s\n",
       CursorPath.sexp_of_hole_list(err_paths_after),
     );
     */
    Printf.printf("internal_errors: %d\n", internal_errors);
    print_endline("END check_suggestion");
  };
  (delta_errors, score + type_specificity_score);
};

let mk_suggestion =
    (~category, ~result_text, ~action, ~result, ~res_ty): suggestion => {
  category,
  result_text,
  action,
  result,
  res_ty,
  delta_errors: 0,
  score: 0,
};

let mk_operand_suggestion' =
    (
      ~ci: CursorInfo.pro,
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
  let (delta_errors, score) =
    switch (check_suggestion(action, res_ty, ci)) {
    | None => (0, 0) //TODO(andrew): do properly or at least log
    | Some(res) => res
    };
  {
    ...mk_suggestion(~category, ~result_text, ~action, ~result, ~res_ty),
    delta_errors,
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

let mk_bool_lit_suggestion = (ci: CursorInfo.pro, b: bool): suggestion =>
  mk_lit_suggestion(~operand=UHExp.boollit(b), ci);

let mk_int_lit_suggestion = (ci: CursorInfo.pro, s: string): suggestion =>
  mk_lit_suggestion(~operand=UHExp.intlit(s), ci);

let mk_float_lit_suggestion = (ci: CursorInfo.pro, s: string): suggestion =>
  mk_lit_suggestion(~operand=UHExp.floatlit(s), ci);

let mk_nil_list_suggestion = (ci: CursorInfo.pro): suggestion =>
  mk_lit_suggestion(~operand=UHExp.listnil(), ci);

let mk_var_suggestion = (ci: CursorInfo.pro, (s: string, _)): suggestion =>
  mk_operand_suggestion(~category=InsertVar, ~operand=UHExp.var(s), ci);

let mk_empty_hole_suggestion = (ci: CursorInfo.pro): suggestion =>
  mk_operand_suggestion(~category=Delete, ~operand=hole_operand, ci);

let mk_inj_suggestion = (ci: CursorInfo.pro, side: InjSide.t): suggestion =>
  mk_operand_suggestion(
    ~category=InsertConstructor,
    ~operand=mk_inj(side),
    ci,
  );

let mk_case_suggestion = (ci: CursorInfo.pro): suggestion =>
  mk_operand_suggestion(~category=InsertElim, ~operand=case_operand, ci);

let mk_lambda_suggestion = (ci: CursorInfo.pro): suggestion =>
  mk_operand_suggestion(
    ~category=InsertConstructor,
    ~operand=lambda_operand,
    ci,
  );

let mk_intro_suggestions = (ci: CursorInfo.pro): list(suggestion) => [
  mk_empty_hole_suggestion(ci),
  mk_bool_lit_suggestion(ci, true),
  mk_bool_lit_suggestion(ci, false),
  mk_nil_list_suggestion(ci),
  mk_inj_suggestion(ci, L),
  mk_inj_suggestion(ci, R),
  mk_lambda_suggestion(ci),
];

let intro_suggestions =
    ({expected_ty, _} as ci: CursorInfo.pro): list(suggestion) =>
  ci
  |> mk_intro_suggestions
  |> List.filter((a: suggestion) => HTyp.consistent(a.res_ty, expected_ty));

//----------------------------------------------------------------------------

let var_suggestions =
    ({ctx, expected_ty, _} as ci: CursorInfo.pro): list(suggestion) =>
  expected_ty
  |> Assistant_common.extract_vars(ctx)
  |> List.map(mk_var_suggestion(ci));

//----------------------------------------------------------------------------

let mk_app_suggestion =
    (ci: CursorInfo.pro, (name: string, f_ty: HTyp.t)): suggestion => {
  let (res_ty, e) =
    mk_ap_iter(ci, name, f_ty)
    |> OptUtil.get(_ => failwith("mk_app_suggestion"));
  mk_suggestion(
    ~category=InsertApp,
    ~result_text=name,
    //TODO(andrew): this should probably actually be an opseq-level action
    ~action=ReplaceAtCursor(UHExp.Parenthesized(e), None),
    ~res_ty,
    ~result=e,
  );
};

let app_suggestions =
    ({ctx, expected_ty, _} as ci: CursorInfo.pro): list(suggestion) => {
  expected_ty
  |> Assistant_common.fun_vars(ctx)
  |> List.map(mk_app_suggestion(ci));
};

let get_guy_from = (pos: CursorPosition.t, operand) => {
  switch (pos, operand_of_string(operand)) {
  | (_, "") => operand
  | (OnText(i), guy) =>
    let (_pre, suf) = StringUtil.split_string(i, guy);
    string_of_operand(suf);
  | _ => operand
  };
};

let mk_wrap_case_suggestion = ({term, _} as ci: CursorInfo.pro): suggestion => {
  let operand =
    switch (term) {
    | Exp(pos, operand) =>
      let guy = get_guy_from(pos, operand);
      guy |> UHExp.Block.wrap |> mk_case;
    | _ => failwith("mk_wrap_case_suggestion impossible")
    };
  mk_operand_suggestion(~category=Wrap, ~operand, ci);
};

let elim_suggestions = (ci: CursorInfo.pro): list(suggestion) =>
  app_suggestions(ci) @ [mk_wrap_case_suggestion(ci)];

//----------------------------------------------------------------------------

let mk_operand_wrap_suggestion = (~ci: CursorInfo.pro, ~category, ~operand) =>
  mk_operand_suggestion'(
    ~ci,
    ~category,
    ~operand,
    ~result_text=operand_of_string(operand),
    ~action=ReplaceAtCursor(operand, None),
  );

let mk_wrap_suggestion =
    ({term, _} as ci: CursorInfo.pro, (name: string, _)) => {
  //TODO(andrew): considering splicing into opseq context
  let result =
    switch (term) {
    | Exp(pos, operand) =>
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
//open Sexplib.Std;
let wrap_suggestions =
    ({ctx, expected_ty, actual_ty, term, _} as ci: CursorInfo.pro) => {
  // TODO(andrew): decide if want to limit options for synthetic mode
  // TODO(andrew): non-unary wraps
  //print_endline("666 wrap_suggestions");
  //P.p("actual_ty: %s\n", sexp_of_option(HTyp.sexp_of_t, actual_ty));
  switch (actual_ty, term) {
  //| (None, _)
  | (_, Exp(_, EmptyHole(_))) => []
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
    //print_endline("666 wrap_suggestions inner");
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

let virtual_suggestions =
    ({term, expected_ty, _} as ci: CursorInfo.pro): list(suggestion) => {
  (
    switch (term) {
    | Exp(_, IntLit(_, s)) when s != "0" => [
        //mk_int_lit_suggestion(ci, s),
        mk_float_lit_suggestion(ci, s ++ "."),
        mk_int_lit_suggestion(ci, "0"),
      ]
    | Exp(_, IntLit(_, s)) when s == "0" => [
        //mk_int_lit_suggestion(ci, "0"),
        mk_float_lit_suggestion(ci, str_int_to_float(s)),
      ]
    | Exp(_, FloatLit(_, s)) when float_of_string(s) != 0.0 =>
      s |> float_of_string |> Float.is_integer
        ? [
          //mk_float_lit_suggestion(ci, s),
          mk_int_lit_suggestion(ci, str_float_to_int(s)),
          mk_float_lit_suggestion(ci, "0."),
        ]
        : [
          //mk_float_lit_suggestion(ci, s),
          mk_int_lit_suggestion(ci, "0"),
        ]
    | Exp(_, FloatLit(_, s)) when float_of_string(s) == 0.0 => [
        //mk_float_lit_suggestion(ci, "0."),
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

let operand_suggestions = (ci: CursorInfo.pro): list(suggestion) =>
  virtual_suggestions(ci)
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
  // TODO(andrew): this is hardcoded for binops, and resets cursor pos. rewrite!
  let fix_holes_local = (ctx: Contexts.t, exp: UHExp.t): UHExp.t =>
    exp
    |> Statics_Exp.syn_fix_holes(ctx, MetaVarGen.init)
    |> (((x, _, _)) => x);
  let new_seq =
    switch (ZExp.erase_zseq(zseq)) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("TODO(andrew) mk_replace_operator_suggestion")
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
  | None =>
    print_endline("TODO(andrew): WARNING actual_ty_operand");
    HTyp.Hole;
  | Some(ty) => ty
  };

let replace_operator_suggestions =
    (ctx: Contexts.t, seq_ty: HTyp.t, zseq: ZExp.zseq, _err: ErrStatus.t) => {
  //TODO(andrew): unhardcode from binops
  switch (ZExp.erase_zseq(zseq)) {
  | S(operand1, A(_operator, S(operand2, E))) =>
    let in1_ty = actual_ty_operand(~ctx, operand1);
    let in2_ty = actual_ty_operand(~ctx, operand2);
    operator_of_ty(in1_ty, in2_ty, seq_ty)
    |> List.map(mk_replace_operator_suggestion(seq_ty, zseq, ctx));
  | _ =>
    print_endline("TODO(andrew): WARNING replace_operator_suggestions");
    [];
  };
};

/*
 case 1v1:
 given opseq: prefix Zoperand| <o1>s1 <o2>s2 ... <on>sn
 suggest:
 prefix Zoperand| (f <o1>s1) <o2>s2 ... <on>sn
 prefix Zoperand| (f <o1>s1 <o2>s2) ... <on>sn
 prefix Zoperand| (f <o1>s1 <o2>s2 ... <on>sn)
 iff the types work
 case 1v2: above, but splice in without parens if possible

 case 2v1:
 given opseq: preseq Zoperator| s1<o1> s2<o2> ... sn
 suggest:
 preseq Zoperator| (f s1)<o1> s2<o2> ... sn
 preseq Zoperator| (f s1<o1> s2)<o2> ... sn
 preseq Zoperator| (f s1<o1> s2<o2> ... sn)


 possible helpers:
 1. predicate to tell if parens are necessary
   when pointed at a parensthesized operand in an opseq

 2. function that when, pointed at an operator, gives
    the subprefix and subsuffic which are that operator's operands
  */
let _mk_seq_wrap_suggestion =
    (
      seq_ty: HTyp.t,
      zseq: ZExp.zseq,
      ctx: Contexts.t,
      new_operator: Operators_Exp.t,
    )
    : suggestion => {
  // TODO(andrew): this is hardcoded for binops, and resets cursor pos. rewrite!
  let fix_holes_local = (ctx: Contexts.t, exp: UHExp.t): UHExp.t =>
    exp
    |> Statics_Exp.syn_fix_holes(ctx, MetaVarGen.init)
    |> (((x, _, _)) => x);
  let new_seq =
    switch (ZExp.erase_zseq(zseq)) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("TODO(andrew) mk_replace_operator_suggestion")
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

let operator_suggestions =
    ({syntactic_context, ctx, _}: CursorInfo.pro): list(suggestion) =>
  switch (syntactic_context) {
  | ExpSeq(seq_ty, zseq, err) =>
    replace_operator_suggestions(ctx, seq_ty, zseq, err)
  | _ => []
  };
