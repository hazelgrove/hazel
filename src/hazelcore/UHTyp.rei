[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type sumbody_operator = Operators_SumBody.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | Sum(option(sumbody))
  | Parenthesized(t)
  | List(t)
and sumbody = OpSeq.t(sumbody_operand, sumbody_operator)
and sumbody_operand =
  | ConstTag(UHTag.t)
  | ArgTag(UHTag.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

[@deriving sexp]
type sumbody_skel = OpSeq.skel(sumbody_operator);
[@deriving sexp]
type sumbody_seq = OpSeq.seq(sumbody_operand, sumbody_operator);

let get_prod_elements: skel => list(skel);
let get_sumbody_elements: sumbody_skel => list(sumbody_skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);
let associate_sumbody: sumbody_seq => Skel.t(Operators_SumBody.t);

let mk_OpSeq: seq => opseq;
let mk_OpSeq_sumbody: sumbody_seq => sumbody;

let contract: HTyp.t => t;

let expand: t => HTyp.t;

let is_complete: t => bool;

let is_empty_sumbody_operand: sumbody_operand => bool;
