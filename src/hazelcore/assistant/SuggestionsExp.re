open OptUtil.Syntax;

[@deriving sexp]
type suggestion = Suggestion.t(UHExp.t);

[@deriving sexp]
type generator = Suggestion.generator(UHExp.t);

/* SYNTAX CONSTRUCTION HELPERS:
 * These assemble syntax without proper hole numbering.
 * Remember to re-number holes at assistant boundary. */

let hole_operand = UHExp.EmptyHole(0);
let hole_exp = UHExp.Block.wrap(hole_operand);
let hole_pat = UHPat.EmptyHole(0) |> OpSeq.wrap;
let lambda_operand = UHExp.lam(hole_pat, hole_exp);
let rule = UHExp.Rule(hole_pat, hole_exp);
let mk_case = scrut => UHExp.case(scrut, [rule]);
let case_operand = mk_case(hole_exp);
let mk_inj = side => UHExp.inj(side, hole_exp);

let seq_into_uhexp = seq => seq |> UHExp.mk_OpSeq |> UHExp.Block.wrap';

let ap_seq = (operand: UHExp.operand, seq: UHExp.seq): UHExp.seq =>
  Seq.S(operand, A(Space, seq));

let mk_ap_uhexp = (f_exp: UHExp.operand, seq: UHExp.seq): UHExp.t =>
  seq |> ap_seq(f_exp) |> seq_into_uhexp;

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
      Some(ap_seq(hole_operand, affix));
    | _ => None
    };
  let+ holes_seq = mk_hole_seq(f_ty);
  mk_ap_uhexp(UHExp.var(f_name), holes_seq);
};
