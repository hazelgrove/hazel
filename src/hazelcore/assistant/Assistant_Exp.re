open OptUtil.Syntax;
open Assistant_common;

type assistant_action_categories =
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertConstructor
  | Wrap
  | ReplaceOperator;

type assistant_action = {
  category: assistant_action_categories,
  text: string,
  action: Action.t,
  result: UHExp.t,
  res_ty: HTyp.t,
};

let wrap = operand =>
  Seq.mk(operand, []) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';

let mk_hole = u_gen => fst(UHExp.new_EmptyHole(u_gen));

let mk_var_uhexp = name => wrap(UHExp.var(name));

let mk_bool_lit_uhexp = b => wrap(UHExp.boollit(b));

let mk_list_nil_uhexp = wrap(UHExp.listnil());

let wrap_space = (operator, seq) =>
  Seq.S(operator, A(Operators_Exp.Space, seq));

let mk_bin_seq = (operand1, operator, operand2) =>
  Seq.seq_op_seq(Seq.wrap(operand1), operator, Seq.wrap(operand2));

let rec mk_ap_seq_holes =
        (f_ty: HTyp.t, hole_ty: HTyp.t)
        : option(Seq.t(UHExp.operand, UHExp.operator)) => {
  switch (f_ty) {
  | Arrow(_, output_ty) =>
    if (HTyp.consistent(output_ty, hole_ty)) {
      Some(S(EmptyHole(0), E));
    } else {
      let+ affix = mk_ap_seq_holes(output_ty, hole_ty);
      wrap_space(UHExp.EmptyHole(0), affix);
    }
  | _ => None
  };
};

let mk_ap =
    ({expected_ty, ctx, u_gen, _}: cursor_info_pro, f: Var.t, f_ty: HTyp.t)
    : option(UHExp.t) => {
  let+ inner_seq = mk_ap_seq_holes(f_ty, expected_ty);
  wrap_space(UHExp.var(f), inner_seq)
  |> UHExp.mk_OpSeq
  |> UHExp.Block.wrap'
  |> Statics_Exp.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes=true)
  |> (((x, _, _)) => x);
};

let mk_var_action =
    (_: cursor_info_pro, name: string, ty: HTyp.t): assistant_action => {
  let e = mk_var_uhexp(name);
  {
    category: InsertVar,
    action: ReplaceAtCursor(UHExp.var(name)),
    text: name,
    result: e,
    res_ty: ty,
  };
};

let mk_app_action =
    (cursor: cursor_info_pro, name: string, ty: HTyp.t): assistant_action => {
  let e =
    mk_ap(cursor, name, ty) |> OptUtil.get(_ => failwith("mk_app_action"));
  {
    category: InsertApp,
    text: name,
    action: ReplaceAtCursor(UHExp.Parenthesized(e)),
    res_ty: ty,
    result: e,
  };
};

let mk_bool_lit_action = (b: bool): assistant_action => {
  {
    category: InsertLit,
    text: string_of_bool(b),
    action: ReplaceAtCursor(UHExp.boollit(b)),
    res_ty: HTyp.Bool,
    result: mk_bool_lit_uhexp(b),
  };
};

let mk_nil_list_action: assistant_action = {
  {
    category: InsertLit,
    text: "[]",
    action: ReplaceAtCursor(UHExp.listnil()),
    res_ty: HTyp.List(Hole),
    result: mk_list_nil_uhexp,
  };
};

let mk_inj_action =
    ({u_gen, _}: cursor_info_pro, side: InjSide.t): assistant_action => {
  let operand = UHExp.inj(side, wrap(mk_hole(u_gen)));
  let uhexp = Seq.mk(operand, []) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
  {
    category: InsertConstructor,
    text: "inj" ++ InjSide.to_string(side) ++ "",
    // TODO: reconcile visual presentation ie brackets
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.Sum(Hole, Hole),
    result: uhexp,
  };
};

let mk_case_action = ({u_gen, _}: cursor_info_pro): assistant_action => {
  let rule =
    UHExp.Rule(
      UHPat.new_EmptyHole(u_gen + 1)
      |> (((o, _)) => o |> (op => Seq.mk(op, []) |> UHPat.mk_OpSeq)),
      wrap(mk_hole(u_gen + 2)),
    );
  let operand = UHExp.case(wrap(mk_hole(u_gen)), [rule]);
  let uhexp = Seq.mk(operand, []) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
  {
    category: InsertConstructor, // TODO(andrew): new category
    text: "case",
    action: ReplaceAtCursor(operand),
    res_ty: HTyp.Hole,
    result: uhexp,
  };
};

let compute_gen_actions = (cursor: cursor_info_pro) => {
  [mk_case_action(cursor)];
};

let compute_ctor_actions =
    ({expected_ty, mode, _} as cursor: cursor_info_pro) => {
  switch (mode) {
  // TODO(andrew): factor this more cleanly
  | Synthetic => [
      mk_bool_lit_action(true),
      mk_bool_lit_action(false),
      mk_nil_list_action,
      mk_inj_action(cursor, L),
      mk_inj_action(cursor, R),
    ]
  | Analytic =>
    switch (expected_ty) {
    | Bool => [mk_bool_lit_action(true), mk_bool_lit_action(false)]
    | List(_) => [mk_nil_list_action]
    | Sum(_) => [mk_inj_action(cursor, L), mk_inj_action(cursor, R)]
    | _ => []
    }
  | UnknownMode => []
  };
};

let compute_var_actions =
    ({ctx, expected_ty, mode, _} as cursor: cursor_info_pro) => {
  print_endline("computer var actions");
  print_endline(Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(expected_ty)));
  print_endline(
    Sexplib.Sexp.to_string_hum(Assistant_common.sexp_of_mode(mode)),
  );
  // TODO(andrew): this looks a bit weird for replacing
  // fn names in applications, since that's synthetic pos
  // for these, prob good to uprank things that have types consistant
  // with the current actual type (and show that things that will result
  // in type errors, will)
  switch (mode) {
  | Synthetic =>
    extract_vars(ctx, Hole)
    |> List.map(((name, var_ty)) => mk_var_action(cursor, name, var_ty))
  | Analytic =>
    extract_vars(ctx, expected_ty)
    |> List.map(((name, var_ty)) => mk_var_action(cursor, name, var_ty))
  | UnknownMode => []
  };
};

let compute_app_actions =
    ({ctx, expected_ty, mode, _} as cursor: cursor_info_pro) => {
  switch (mode) {
  | Synthetic
  | UnknownMode => []
  | Analytic =>
    fun_vars(ctx, expected_ty)
    |> List.map(((name, f_ty)) => mk_app_action(cursor, name, f_ty))
  };
};

let mk_basic_wrap_action = (cursor, name, _f_ty) => {
  let operand =
    switch (cursor.term) {
    | Exp(_, operand) =>
      wrap_space(UHExp.var(name), S(operand, E))
      |> UHExp.mk_OpSeq
      |> UHExp.Block.wrap'
      |> (x => UHExp.Parenthesized(x))
    | _ => failwith("mk_basic_wrap_action unimplemented TODO(andrew)")
    };
  {
    category: Wrap,
    text: name,
    action: ReplaceAtCursor(operand),
    res_ty: cursor.expected_ty,
    result: wrap(operand),
  };
};

let compute_basic_wrap_actions =
    ({ctx, expected_ty, actual_ty, mode, _} as cursor: cursor_info_pro) => {
  switch (mode) {
  | Synthetic
  | UnknownMode => []
  | Analytic =>
    switch (actual_ty) {
    | None => []
    | Some(actual_ty) =>
      fun_vars(ctx, expected_ty)
      |> List.filter(((_, f_ty)) =>
           HTyp.consistent(f_ty, HTyp.Arrow(actual_ty, expected_ty))
         )
      |> List.map(((name, f_ty)) =>
           mk_basic_wrap_action(cursor, name, f_ty)
         )
    }
  };
};

let compute_operand_actions = ({term, _} as cursor): list(assistant_action) =>
  switch (term) {
  | Exp(_) =>
    List.map(
      f => f(cursor),
      [
        compute_basic_wrap_actions,
        compute_var_actions,
        compute_app_actions,
        compute_ctor_actions,
        compute_gen_actions,
      ],
    )
    |> List.concat
  | _ => []
  };

let exp_operator_of_ty =
    (inl: HTyp.t, inr: HTyp.t, out: HTyp.t): list(Operators_Exp.t) => {
  Operators_Exp.
    // TODO(andrew): Add Comma, Cons, Space ops (requires a bit more deconstruction)
    (
      List.concat([
        HTyp.consistent_all([inl, inr, out, HTyp.Bool]) ? [And, Or] : [],
        HTyp.consistent_all([inl, inr, out, HTyp.Int])
          ? [Plus, Minus, Times, Divide] : [],
        HTyp.consistent_all([inl, inr, out, HTyp.Float])
          ? [FPlus, FMinus, FTimes, FDivide] : [],
        HTyp.consistent_all([inl, inr, HTyp.Int])
        && HTyp.consistent(out, HTyp.Bool)
          ? [LessThan, GreaterThan, Equals] : [],
        HTyp.consistent_all([inl, inr, HTyp.Float])
        && HTyp.consistent(out, HTyp.Bool)
          ? [FLessThan, FGreaterThan, FEquals] : [],
      ])
    );
};

let mk_replace_operator_action =
    (seq_ty, zseq, new_operator): assistant_action => {
  // TODO(andrew): this is hardcoded for binops, and resets cursor pos. rewrite!
  let init_seq = zseq |> ZExp.erase_zseq;
  //let init_skel = init_seq |> UHExp.associate;
  let new_seq =
    switch (init_seq) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("TODO andrew...")
    };
  let ZOpSeq(_, new_zseq) =
    new_seq |> UHExp.mk_OpSeq |> ZExp.place_before_opseq;
  let uhexp =
    new_zseq |> ZExp.erase_zseq |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
  {
    category: ReplaceOperator,
    text: Operators_Exp.to_string(new_operator),
    action: ReplaceOpSeqAroundCursor(new_zseq),
    res_ty: seq_ty,
    result: uhexp,
  };
};

let compute_replace_operator_actions = ({ctx, _}, seq_ty, zseq) => {
  print_endline("computing operator replace actions");
  let init_seq = zseq |> ZExp.erase_zseq;
  let* (in1_ty, in2_ty) =
    switch (init_seq) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      let* in1_ty =
        Statics_Exp.syn_operand(
          ctx,
          UHExp.set_err_status_operand(NotInHole, operand1),
        );
      let* in2_ty =
        Statics_Exp.syn_operand(
          ctx,
          UHExp.set_err_status_operand(NotInHole, operand2),
        );
      Some((in1_ty, in2_ty));
    | _ => None
    };
  let ops = exp_operator_of_ty(in1_ty, in2_ty, seq_ty);
  Some(List.map(mk_replace_operator_action(seq_ty, zseq), ops));
};

let compute_operator_actions = ({term, syntactic_context, _} as cursor) =>
  switch (term, syntactic_context) {
  | (ExpOp(_), ExpSeq(seq_ty, zseq)) =>
    OptUtil.get(
      _ => failwith("whatever, gawd!!!"),
      compute_replace_operator_actions(cursor, seq_ty, zseq),
    )
  | _ => []
  };
