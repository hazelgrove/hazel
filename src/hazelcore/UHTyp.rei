[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type sum_body_operator = Operators_SumBody.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | FiniteSum(option(sum_body))
  | ElidedSum(sum_body_operand)
  | Parenthesized(t)
  | List(t)
and sum_body = OpSeq.t(sum_body_operand, sum_body_operator)
and sum_body_operand =
  | ConstTag(UHTag.t)
  | ArgTag(UHTag.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

[@deriving sexp]
type sum_body_skel = OpSeq.skel(sum_body_operator);
[@deriving sexp]
type sum_body_seq = OpSeq.seq(sum_body_operand, sum_body_operator);

let get_prod_elements: skel => list(skel);
let get_sum_body_elements: sum_body_skel => list(sum_body_skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);
let associate_sum_body: sum_body_seq => Skel.t(Operators_SumBody.t);

let mk_OpSeq: seq => opseq;
let mk_OpSeq_sum_body: sum_body_seq => sum_body;

let contract: HTyp.t => t;

let expand: t => HTyp.t;

let is_complete: t => bool;

let fix_holes: (t, MetaVarGen.t) => (t, MetaVarGen.t);
let fix_holes_sum_body: (sum_body, MetaVarGen.t) => (sum_body, MetaVarGen.t);
