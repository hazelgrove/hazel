[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | Parenthesized(t)
  | List(t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type annotated_skel = AnnotatedSkel.t(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

let get_prod_elements: skel => list(skel);

let get_annotated_prod_elements: annotated_skel => list(annotated_skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let contract: HTyp.t => t;

let expand: t => HTyp.t;

let is_complete: t => bool;
