open OptUtil.Syntax;
open Assistant_common;

type assistant_action_categories =
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertConstructor
  | InsertElim
  | Wrap
  | ReplaceOperator;

type assistant_action = {
  category: assistant_action_categories,
  text: string,
  action: Action.t,
  result: UHExp.t,
  res_ty: HTyp.t,
};

type exp_seq = Seq.t(UHExp.operand, Operators_Exp.t);
type exp_zseq =
  ZSeq.t(
    UHExp.operand,
    Operators_Exp.t,
    ZExp.zoperand,
    (CursorPosition.t, Operators_Exp.t),
  );

let wrap = UHExp.Block.wrap;
let mk_hole = u_gen => u_gen |> UHExp.new_EmptyHole |> fst;
let mk_case = u_gen => {
  let rule =
    UHExp.Rule(
      UHPat.new_EmptyHole(u_gen + 1) |> fst |> OpSeq.wrap,
      wrap(mk_hole(u_gen + 2)),
    );
  UHExp.case(wrap(mk_hole(u_gen)), [rule]);
};
let wrap_space = (operand: UHExp.operand, seq: exp_seq) =>
  Seq.S(operand, A(Operators_Exp.Space, seq));
let mk_ap = (f_name: string, seq: exp_seq): UHExp.t =>
  wrap_space(UHExp.var(f_name), seq) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
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

let rec mk_ap_seq_holes =
        (f_ty: HTyp.t, hole_ty: HTyp.t)
        : option((HTyp.t, Seq.t(UHExp.operand, UHExp.operator))) => {
  switch (f_ty) {
  | Arrow(_, output_ty) =>
    if (HTyp.consistent(output_ty, hole_ty)) {
      Some((output_ty, S(EmptyHole(0), E)));
    } else {
      let+ (res_ty, affix) = mk_ap_seq_holes(output_ty, hole_ty);
      (res_ty, wrap_space(UHExp.EmptyHole(0), affix));
    }
  | _ => None
  };
};

let mk_ap_inner =
    ({expected_ty, ctx, u_gen, _}: cursor_info_pro, f: Var.t, f_ty: HTyp.t)
    : option((HTyp.t, UHExp.t)) => {
  let+ (res_ty, inner_seq) = mk_ap_seq_holes(f_ty, expected_ty);
  let exp = mk_ap_and_fix(f, inner_seq, ctx, u_gen);
  (res_ty, exp);
};

let mk_var_action =
    (_: cursor_info_pro, (name: string, ty: HTyp.t)): assistant_action => {
  let operand = UHExp.var(name);
  {
    category: InsertVar,
    action: ReplaceAtCursor(operand),
    text: name,
    result: wrap(operand),
    res_ty: ty,
  };
};

let mk_app_action =
    (ci: cursor_info_pro, (name: string, f_ty: HTyp.t)): assistant_action => {
  //TODO(andrew): splice maybe?
  let (res_ty, e) =
    mk_ap_inner(ci, name, f_ty)
    |> OptUtil.get(_ => failwith("mk_app_action"));
  {
    category: InsertApp,
    text: name,
    action: ReplaceAtCursor(UHExp.Parenthesized(e)),
    res_ty,
    result: e,
  };
};

let mk_bool_lit_action = (b: bool): assistant_action => {
  let operand = UHExp.boollit(b);
  {
    category: InsertLit,
    text: string_of_bool(b),
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.Bool,
    result: wrap(operand),
  };
};

let mk_nil_list_action: assistant_action = {
  let operand = UHExp.listnil();
  {
    category: InsertLit,
    text: "[]",
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.List(Hole),
    result: wrap(operand),
  };
};

let mk_inj_action =
    ({u_gen, _}: cursor_info_pro, side: InjSide.t): assistant_action => {
  let operand = UHExp.inj(side, wrap(mk_hole(u_gen)));
  // TODO(andrew): reconcile with visual presentation ie brackets
  {
    category: InsertConstructor,
    text: "inj" ++ InjSide.to_string(side) ++ "",
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.Sum(Hole, Hole),
    result: wrap(operand),
  };
};

let mk_case_action = ({u_gen, _}: cursor_info_pro): assistant_action => {
  let operand = mk_case(u_gen);
  {
    category: InsertElim,
    text: "case",
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.Hole,
    result: wrap(operand),
  };
};

let intro_actions = (ci: cursor_info_pro): list(assistant_action) => [
  mk_bool_lit_action(true),
  mk_bool_lit_action(false),
  mk_nil_list_action,
  mk_inj_action(ci, L),
  mk_inj_action(ci, R),
];

let compute_intro_actions =
    ({expected_ty, _} as ci: cursor_info_pro): list(assistant_action) =>
  ci
  |> intro_actions
  |> List.filter(a => HTyp.consistent(a.res_ty, expected_ty));

let compute_var_actions =
    ({ctx, expected_ty, _} as ci: cursor_info_pro): list(assistant_action) =>
  //TODO(andrew): resolve issue with fn position in apps
  expected_ty |> extract_vars(ctx) |> List.map(mk_var_action(ci));

let compute_app_actions =
    ({ctx, expected_ty, _} as ci: cursor_info_pro): list(assistant_action) => {
  //TODO(andrew): Do I want to special-case synthetic here?
  expected_ty |> fun_vars(ctx) |> List.map(mk_app_action(ci));
};

let compute_elim_actions = (ci: cursor_info_pro): list(assistant_action) =>
  compute_var_actions(ci) @ compute_app_actions(ci) @ [mk_case_action(ci)];

let mk_basic_wrap_action =
    ({expected_ty, term, ctx, u_gen, _}: cursor_info_pro, (name: string, _)) => {
  //TODO(andrew): considering splicing into opseq context
  let exp =
    switch (term) {
    | Exp(_, operand) => mk_ap_and_fix(name, S(operand, E), ctx, u_gen)
    | _ => failwith("mk_basic_wrap_action unimplemented TODO(andrew)")
    };
  {
    category: Wrap,
    text: name,
    action: ReplaceAtCursor(UHExp.Parenthesized(exp)),
    res_ty: expected_ty,
    result: exp,
  };
};

let compute_wrap_actions =
    ({ctx, expected_ty, actual_ty, _} as ci: cursor_info_pro) => {
  //TODO(andrew): decide if want to limit options for synthetic mode
  //TODO(andrew): non-unary wraps
  switch (actual_ty) {
  | None => []
  | Some(actual_ty) =>
    fun_vars(ctx, expected_ty)
    |> List.filter(((_, f_ty)) =>
         HTyp.consistent(f_ty, HTyp.Arrow(actual_ty, expected_ty))
       )
    |> List.map(mk_basic_wrap_action(ci))
  };
};

let operand_actions = (ci: cursor_info_pro): list(assistant_action) =>
  compute_wrap_actions(ci)
  @ compute_elim_actions(ci)
  @ compute_intro_actions(ci);

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
  {
    category: ReplaceOperator,
    text: Operators_Exp.to_string(new_operator),
    action: ReplaceOpSeqAroundCursor(new_zseq),
    res_ty: seq_ty,
    result: UHExp.Block.wrap'(new_opseq) |> fix_holes_local(ctx, u_gen),
  };
};

let compute_replace_operator_actions =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, seq_ty: HTyp.t, zseq: exp_zseq) => {
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
  | ExpSeq(seq_ty, zseq) =>
    compute_replace_operator_actions(ctx, u_gen, seq_ty, zseq)
  | _ => []
  };
