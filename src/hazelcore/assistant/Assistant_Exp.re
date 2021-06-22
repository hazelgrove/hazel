open OptUtil.Syntax;
open Assistant_common;

type assistant_action_categories =
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertConstructor
  | InsertElim
  | Wrap
  | ReplaceOperator
  | Delete;

type assistant_action = {
  category: assistant_action_categories,
  text: string,
  action: Action.t,
  result: UHExp.t,
  res_ty: HTyp.t,
  delta_errors: int,
  //context_consistent: bool,
};

type exp_seq = Seq.t(UHExp.operand, Operators_Exp.t);
type exp_zseq =
  ZSeq.t(
    UHExp.operand,
    Operators_Exp.t,
    ZExp.zoperand,
    (CursorPosition.t, Operators_Exp.t),
  );

let lit_to_string = (operand: UHExp.operand): string => {
  switch (operand) {
  | ListNil(_) => "[]"
  | InvalidText(_, s)
  | Var(_, _, s)
  | IntLit(_, s)
  | FloatLit(_, s) => s
  | BoolLit(_, b) => string_of_bool(b)
  | Inj(_, side, _) => "inj" ++ InjSide.to_string(side) ++ ""
  | Lam(_) => "\\"
  | Case(_, _, _) => "case"
  | Parenthesized(_) => "()"
  | EmptyHole(_)
  | ApPalette(_) => ""
  };
};

let exp_operator_of_ty =
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

let wrap = UHExp.Block.wrap;
let mk_hole = u_gen => u_gen |> UHExp.new_EmptyHole |> fst;
let mk_hole_wrap = u_gen => u_gen |> mk_hole |> wrap;
let mk_pat_hole_wrap = u_gen =>
  UHPat.new_EmptyHole(u_gen + 1) |> fst |> OpSeq.wrap;
let mk_rule = u_gen =>
  UHExp.Rule(mk_pat_hole_wrap(u_gen), mk_hole_wrap(u_gen + 1));
let mk_case_wrap = (u_gen, scrut) => UHExp.case(scrut, [mk_rule(u_gen)]);
let mk_case = u_gen => mk_case_wrap(u_gen, mk_hole_wrap(u_gen + 2));
let mk_lambda = u_gen =>
  UHExp.lam(mk_pat_hole_wrap(u_gen), mk_hole_wrap(u_gen + 1));
let mk_inj = (side, u_gen) => UHExp.inj(side, mk_hole_wrap(u_gen));
let ap_seq = (operand: UHExp.operand, seq: exp_seq): exp_seq =>
  Seq.S(operand, A(Space, seq));
let mk_ap = (f_name: string, seq: exp_seq): UHExp.t =>
  ap_seq(UHExp.var(f_name), seq) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
let mk_bin_seq = (operand1, operator, operand2) =>
  Seq.seq_op_seq(Seq.wrap(operand1), operator, Seq.wrap(operand2));

let fix_holes_local =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, exp: UHExp.t): UHExp.t =>
  exp
  |> Statics_Exp.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes=true)
  |> (((x, _, _)) => x);

let mk_ap_and_fix =
    (f_name: string, seq: exp_seq, ctx: Contexts.t, u_gen: MetaVarGen.t)
    : UHExp.t =>
  mk_ap(f_name, seq) |> fix_holes_local(ctx, u_gen);

let rec mk_ap_iter_seq =
        (f_ty: HTyp.t, hole_ty: HTyp.t): option((HTyp.t, exp_seq)) => {
  switch (f_ty) {
  | Arrow(_, out_ty) =>
    if (HTyp.consistent(out_ty, hole_ty)) {
      Some((out_ty, S(EmptyHole(0), E)));
    } else {
      let* (res_ty, affix) = mk_ap_iter_seq(out_ty, hole_ty);
      Some((res_ty, ap_seq(UHExp.EmptyHole(0), affix)));
    }
  | _ => None
  };
};

let mk_ap_iter =
    ({expected_ty, ctx, u_gen, _}: cursor_info_pro, f: Var.t, f_ty: HTyp.t)
    : option((HTyp.t, UHExp.t)) => {
  let+ (output_ty, holes_seq) = mk_ap_iter_seq(f_ty, expected_ty);
  let exp = mk_ap_and_fix(f, holes_seq, ctx, u_gen);
  (output_ty, exp);
};

type context_status = {
  ty: HTyp.t,
  errors: list(CursorPath.hole_info),
};

type action_report = {
  before: context_status,
  after: context_status,
  //context_consistent: bool,
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

let check_action =
    (action: Action.t, {ctx, u_gen, syntactic_context, _}: cursor_info_pro)
    : option(int) => {
  let* (expected_ty, old_zexp, context_consistent_before) =
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
      Action_Exp.syn_perform(ctx, action, (old_zexp, expected_ty, u_gen))
    ) {
    | Failed
    | CursorEscaped(_) => None
    | Succeeded((new_zexp, new_type, _)) => Some((new_type, new_zexp))
    };
  let context_consistent_after = HTyp.consistent(expected_ty, actual_ty);
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
  if (false) {
    print_endline("START check_action");
    Printf.printf(
      "context_consistent before: %b\n",
      context_consistent_before,
    );
    Printf.printf("context_consistent after: %b\n", context_consistent_after);
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
    Printf.printf("internal_errors: %d\n", internal_errors);
    print_endline("END check_action");
  };
  /*Some({
      before: {
        ty: expected_ty,
        errors: err_paths_before,
      },
      after: {
        ty: actual_ty,
        errors: err_paths_after,
      },
      delta_errors
    });*/
  delta_errors;
};

let mk_action =
    (~category, ~text, ~action, ~result, ~res_ty): assistant_action => {
  category,
  text,
  action,
  result,
  res_ty,
  delta_errors: 0,
  //context_consistent: true,
};

let mk_operand_action' =
    (
      ~ci: cursor_info_pro,
      ~category: assistant_action_categories,
      ~text: string,
      ~operand: UHExp.operand,
      ~result,
      ~action: Action.t,
    )
    : assistant_action => {
  let res_ty =
    switch (Statics_Exp.syn_operand(ci.ctx, operand)) {
    | None => HTyp.Hole
    | Some(ty) => ty
    };
  let delta_errors =
    switch (check_action(action, ci)) {
    | None => 0 //TODO(andrew): unfuck
    | Some(delta_errors) => delta_errors
    };
  {category, action, text, result, res_ty, delta_errors};
};

let mk_operand_action = (~ci: cursor_info_pro, ~category, ~operand) =>
  mk_operand_action'(
    ~ci,
    ~category,
    ~operand,
    ~text=lit_to_string(operand),
    ~action=ReplaceAtCursor(operand),
    ~result=wrap(operand),
  );

let mk_operand_action_res =
    (~ci: cursor_info_pro, ~category, ~operand, ~result) =>
  mk_operand_action'(
    ~ci,
    ~category,
    ~operand,
    ~text=lit_to_string(operand),
    ~action=ReplaceAtCursor(operand),
    ~result,
  );

let mk_lit_action = (~ci, ~operand) =>
  mk_operand_action(~ci, ~category=InsertLit, ~operand);

// INTROS  -----------------------------------------------------------------

let mk_bool_lit_action = (ci: cursor_info_pro, b: bool): assistant_action =>
  mk_lit_action(~ci, ~operand=UHExp.boollit(b));

let mk_int_lit_action = (ci: cursor_info_pro, s: string): assistant_action =>
  mk_lit_action(~ci, ~operand=UHExp.intlit(s));

let mk_float_lit_action = (ci: cursor_info_pro, s: string): assistant_action =>
  mk_lit_action(~ci, ~operand=UHExp.floatlit(s));

let mk_nil_list_action = (ci: cursor_info_pro): assistant_action =>
  mk_lit_action(~ci, ~operand=UHExp.listnil());

let mk_var_action = (ci: cursor_info_pro, (s: string, _)): assistant_action =>
  mk_operand_action(~ci, ~category=InsertVar, ~operand=UHExp.var(s));

let mk_empty_hole_action =
    ({u_gen, _} as ci: cursor_info_pro): assistant_action => {
  mk_operand_action(~ci, ~category=Delete, ~operand=UHExp.EmptyHole(u_gen));
};

let mk_inj_action =
    ({u_gen, _} as ci: cursor_info_pro, side: InjSide.t): assistant_action =>
  mk_operand_action(
    ~ci,
    ~category=InsertConstructor,
    ~operand=mk_inj(side, u_gen),
  );

let mk_case_action = ({u_gen, _} as ci: cursor_info_pro): assistant_action =>
  mk_operand_action(~ci, ~category=InsertElim, ~operand=mk_case(u_gen));

let mk_lambda_action = ({u_gen, _} as ci: cursor_info_pro): assistant_action =>
  mk_operand_action(
    ~ci,
    ~category=InsertConstructor,
    ~operand=mk_lambda(u_gen),
  );

let mk_intro_actions = (ci: cursor_info_pro): list(assistant_action) => [
  mk_empty_hole_action(ci),
  mk_bool_lit_action(ci, true),
  mk_bool_lit_action(ci, false),
  mk_nil_list_action(ci),
  mk_inj_action(ci, L),
  mk_inj_action(ci, R),
  mk_lambda_action(ci),
];

let intro_actions =
    ({expected_ty, _} as ci: cursor_info_pro): list(assistant_action) =>
  ci
  |> mk_intro_actions
  |> List.filter(a => HTyp.consistent(a.res_ty, expected_ty));

//----------------------------------------------------------------------------

let var_actions =
    ({ctx, expected_ty, _} as ci: cursor_info_pro): list(assistant_action) =>
  expected_ty |> extract_vars(ctx) |> List.map(mk_var_action(ci));

//----------------------------------------------------------------------------

let mk_app_action =
    (ci: cursor_info_pro, (name: string, f_ty: HTyp.t)): assistant_action => {
  let (res_ty, e) =
    mk_ap_iter(ci, name, f_ty) |> OptUtil.get(_ => failwith("mk_app_action"));
  mk_action(
    ~category=InsertApp,
    ~text=name,
    //TODO(andrew): this should probably actually be an opseq-level action
    ~action=ReplaceAtCursor(UHExp.Parenthesized(e)),
    ~res_ty,
    ~result=e,
  );
};

let app_actions =
    ({ctx, expected_ty, _} as ci: cursor_info_pro): list(assistant_action) => {
  expected_ty |> fun_vars(ctx) |> List.map(mk_app_action(ci));
};

let mk_wrap_case_action =
    ({term, u_gen, _} as ci: cursor_info_pro): assistant_action => {
  let operand =
    switch (term) {
    | Exp(_, operand) => operand |> wrap |> mk_case_wrap(u_gen)
    | _ => failwith("mk_wrap_case_action impossible")
    };
  mk_operand_action(~ci, ~category=Wrap, ~operand);
};

let elim_actions = (ci: cursor_info_pro): list(assistant_action) =>
  app_actions(ci) @ [mk_wrap_case_action(ci)];

//----------------------------------------------------------------------------

let mk_operand_wrap_action = (~ci: cursor_info_pro, ~category, ~operand) =>
  mk_operand_action'(
    ~ci,
    ~category,
    ~operand,
    ~text=lit_to_string(operand),
    ~action=ReplaceAtCursor(operand),
  );
let mk_wrap_action =
    ({term, ctx, u_gen, _} as ci: cursor_info_pro, (name: string, _)) => {
  //TODO(andrew): considering splicing into opseq context
  let result =
    switch (term) {
    | Exp(_, operand) => mk_ap_and_fix(name, S(operand, E), ctx, u_gen)
    | _ => failwith("mk_basic_wrap_action impossible")
    };
  mk_operand_action_res(
    ~ci,
    ~category=Wrap,
    ~operand=UHExp.Parenthesized(result),
    ~result,
  );
};

// TODO(andrew): label replacement actions seperately!! for non-empty hole case, categorization MATTERS
// ie we want to know why this is being suggested
// TODO: mode toggle for favoring simplifying versus complexifying actions?
// TODO: for simple/complex biasing... maybe closer to root is complex-biased, getting simpler as descends?

let wrap_actions =
    ({ctx, expected_ty, actual_ty, term, _} as ci: cursor_info_pro) => {
  // TODO(andrew): decide if want to limit options for synthetic mode
  // TODO(andrew): non-unary wraps
  switch (actual_ty, term) {
  | (None, _)
  | (_, Exp(_, EmptyHole(_))) => []
  // NOTE: wrapping empty holes redundant to ap
  | (Some(actual_ty), _) =>
    fun_vars(ctx, expected_ty)
    |> List.filter(((_, f_ty)) =>
         HTyp.consistent(f_ty, HTyp.Arrow(actual_ty, expected_ty))
       )
    |> List.map(mk_wrap_action(ci))
  };
};

let operand_actions = (ci: cursor_info_pro): list(assistant_action) =>
  wrap_actions(ci) @ intro_actions(ci) @ var_actions(ci) @ elim_actions(ci);

let mk_replace_operator_action =
    (
      seq_ty: HTyp.t,
      zseq: exp_zseq,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      new_operator: Operators_Exp.t,
    )
    : assistant_action => {
  // TODO(andrew): this is hardcoded for binops, and resets cursor pos. rewrite!
  let new_seq =
    switch (ZExp.erase_zseq(zseq)) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("TODO(andrew) mk_replace_operator_action")
    };
  let new_opseq = new_seq |> UHExp.mk_OpSeq;
  let ZOpSeq(_, new_zseq) = ZExp.place_before_opseq(new_opseq);
  mk_action(
    ~category=ReplaceOperator,
    ~text=Operators_Exp.to_string(new_operator),
    ~action=ReplaceOpSeqAroundCursor(new_zseq),
    ~res_ty=seq_ty,
    ~result=UHExp.Block.wrap'(new_opseq) |> fix_holes_local(ctx, u_gen),
  );
};

let replace_operator_actions =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      seq_ty: HTyp.t,
      zseq: exp_zseq,
      _err: ErrStatus.t,
    ) => {
  let ty_ignoring_err = operand =>
    switch (
      Statics_Exp.syn_operand(
        ctx,
        UHExp.set_err_status_operand(NotInHole, operand),
      )
    ) {
    | None =>
      print_endline("TODO(andrew): WARNING 1 replace_operator_actions");
      HTyp.Hole;
    | Some(ty) => ty
    };
  switch (ZExp.erase_zseq(zseq)) {
  | S(operand1, A(_operator, S(operand2, E))) =>
    let in1_ty = ty_ignoring_err(operand1);
    let in2_ty = ty_ignoring_err(operand2);
    exp_operator_of_ty(in1_ty, in2_ty, seq_ty)
    |> List.map(mk_replace_operator_action(seq_ty, zseq, ctx, u_gen));
  | _ =>
    print_endline("TODO(andrew): WARNING 2 replace_operator_actions");
    [];
  };
};

let operator_actions =
    ({syntactic_context, ctx, u_gen, _}: cursor_info_pro)
    : list(assistant_action) =>
  switch (syntactic_context) {
  | ExpSeq(seq_ty, zseq, err) =>
    replace_operator_actions(ctx, u_gen, seq_ty, zseq, err)
  | _ => []
  };

let str_float_to_int = s =>
  s |> float_of_string |> Float.to_int |> string_of_int;
let str_int_to_float = s =>
  s |> int_of_string |> Float.of_int |> string_of_float;

let virtual_actions =
    ({term, expected_ty, _} as ci: cursor_info_pro): list(assistant_action) => {
  (
    switch (term) {
    | Exp(_, IntLit(_, s)) when s != "0" => [
        mk_int_lit_action(ci, s),
        mk_float_lit_action(ci, s ++ "."),
        mk_int_lit_action(ci, "0"),
      ]
    | Exp(_, IntLit(_, s)) when s == "0" => [
        mk_int_lit_action(ci, "0"),
        mk_float_lit_action(ci, str_int_to_float(s)),
      ]
    | Exp(_, FloatLit(_, s)) when float_of_string(s) != 0.0 =>
      s |> float_of_string |> Float.is_integer
        ? [
          mk_float_lit_action(ci, s),
          mk_int_lit_action(ci, str_float_to_int(s)),
          mk_float_lit_action(ci, "0."),
        ]
        : [mk_float_lit_action(ci, s), mk_int_lit_action(ci, "0.")]
    | Exp(_, FloatLit(_, s)) when float_of_string(s) == 0.0 => [
        mk_float_lit_action(ci, "0."),
        mk_int_lit_action(ci, "0"),
      ]
    | _ => [mk_float_lit_action(ci, "0."), mk_int_lit_action(ci, "0")]
    }
  )
  |> List.filter(a => HTyp.consistent(a.res_ty, expected_ty));
};
