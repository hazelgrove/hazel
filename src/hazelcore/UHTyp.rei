[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type sumtyp_operator = Operators_SumTyp.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | Sum(sumtyp)
  | Parenthesized(t)
  | List(t)
and sumtyp = OpSeq.t(sumtyp_operand, sumtyp_operator)
and sumtyp_operand =
  | ConstTag(UHTag.t)
  | ArgTag(UHTag.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

[@deriving sexp]
type skel_sumtyp = OpSeq.skel(sumtyp_operator);
[@deriving sexp]
type seq_sumtyp = OpSeq.seq(sumtyp_operand, sumtyp_operator);

let get_prod_elements: skel => list(skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);
let associate_sumtyp: seq_sumtyp => Skel.t(Operators_SumTyp.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let contract: HTyp.t => t;

let expand: t => HTyp.t;

let is_complete: t => bool;
