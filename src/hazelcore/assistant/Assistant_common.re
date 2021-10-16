open OptUtil.Syntax;

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((_, ty)) => HTyp.consistent(ty, typ));
};

/**
   * Filter the variables that are functions that have the correct resulting type
   */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let rec compatible_funs = right_ty =>
    if (HTyp.consistent(right_ty, typ)) {
      true;
    } else {
      switch (right_ty) {
      | Arrow(_, right_ty) => compatible_funs(right_ty)
      | _ => false
      };
    };
  let can_extract = ((_, ty: HTyp.t)) => {
    switch (ty) {
    | Arrow(_, t2) => compatible_funs(t2)
    | _ => false
    };
  };
  ctx |> Contexts.gamma |> VarMap.filter(can_extract);
};

/**
 * Gets the type in string format.
 * Return string
 */
let type_to_str = (ty: HTyp.t) => {
  switch (ty) {
  | Hole => "a"
  | Int => "an Integer"
  | Float => "a Float"
  | Bool => "a Boolean"
  | Arrow(_, _) => "a Function"
  | Sum(_, _) => "a Sum"
  | Prod(_) => "a Product"
  | List(_) => "a List"
  };
};

let vars_of_type_matching_str = (ctx: Contexts.t, typ: HTyp.t, str: string) => {
  ctx
  |> Contexts.gamma
  |> VarMap.filter(((name, ty)) =>
       switch (StringUtil.search_forward_opt(Str.regexp(str), name)) {
       | None => false
       | Some(_) => HTyp.consistent(ty, typ)
       }
     );
};

/* SYNTAX CONSTRUCTION HELPERS:
 * These assemble syntax without proper hole numbering.
 * Remember to re-number holes at assistant boundary. */

let hole_operand = UHExp.EmptyHole(0);

let hole_exp = UHExp.Block.wrap(hole_operand);

let lambda_operand = UHExp.lam(OpSeq.wrap(UHPat.EmptyHole(0)), hole_exp);

let mk_inj = (side, operand) => UHExp.inj(side, UHExp.Block.wrap(operand));

let mk_case = scrut =>
  UHExp.case(
    UHExp.Block.wrap(scrut),
    [UHExp.Rule(OpSeq.wrap(UHPat.EmptyHole(0)), hole_exp)],
  );

let seq_to_uhexp = seq => seq |> UHExp.mk_OpSeq |> UHExp.Block.wrap';

let mk_ap_seq = (operand: UHExp.operand, seq: UHExp.seq): UHExp.seq =>
  Seq.S(operand, A(Space, seq));

let mk_ap_uhexp = (f_exp: UHExp.operand, seq: UHExp.seq): UHExp.t =>
  seq |> mk_ap_seq(f_exp) |> seq_to_uhexp;

let mk_bin_ap_uhexp = (f_exp: UHExp.operand, a_exp: UHExp.operand) =>
  mk_ap_uhexp(f_exp, S(a_exp, E));

let mk_bin_seq = (operand1, operator, operand2) =>
  Seq.seq_op_seq(Seq.wrap(operand1), operator, Seq.wrap(operand2));

let rec mk_n_seq = (op: Operators_Exp.t, n: int) =>
  switch (n) {
  | 0
  | 1 => failwith("mk_n_opseq_operand: n must be 2 or more")
  | 2 => Seq.S(hole_operand, A(op, S(hole_operand, E)))
  | _ => Seq.S(hole_operand, A(op, mk_n_seq(op, n - 1)))
  };

let mk_ap_iter =
    (expected_ty: HTyp.t, f_name: Var.t, f_ty: HTyp.t): option(UHExp.t) => {
  let rec mk_hole_seq = (f_ty: HTyp.t) =>
    switch (f_ty) {
    | Arrow(_, out_ty) when HTyp.consistent(out_ty, expected_ty) =>
      Some(Seq.wrap(hole_operand))
    | Arrow(_, out_ty) =>
      let* affix = mk_hole_seq(out_ty);
      Some(mk_ap_seq(hole_operand, affix));
    | _ => None
    };
  let+ holes_seq = mk_hole_seq(f_ty);
  mk_ap_uhexp(UHExp.var(f_name), holes_seq);
};

/* Make op independent of ctx and u_gen, for comparison purposes */
let normalize_operand = (op: UHExp.operand) => {
  let (op, _, _) =
    Statics_Exp.syn_fix_holes_operand(
      Contexts.empty,
      0,
      ~renumber_empty_holes=true,
      op,
    );
  op;
};

let equals_operand = (op1: UHExp.operand, op2: UHExp.operand) =>
  normalize_operand(op1) == normalize_operand(op2);
