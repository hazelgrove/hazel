[@deriving sexp]
type suggestion = Suggestion.t(UHExp.t);
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
let ap_seq = (operand: UHExp.operand, seq: UHExp.seq): UHExp.seq =>
  Seq.S(operand, A(Space, seq));
let mk_ap = (f_name: string, seq: UHExp.seq): UHExp.t =>
  ap_seq(UHExp.var(f_name), seq) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';
let mk_bin_seq = (operand1, operator, operand2) =>
  Seq.seq_op_seq(Seq.wrap(operand1), operator, Seq.wrap(operand2));
