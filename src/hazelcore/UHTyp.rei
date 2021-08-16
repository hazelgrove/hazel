[@deriving (sexp, show)]
type operator = Operators_Typ.t;

[@deriving (sexp, show)]
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

[@deriving (sexp, show)]
type skel = OpSeq.skel(operator);
[@deriving (sexp, show)]
type seq = OpSeq.seq(operand, operator);

let get_prod_elements: skel => list(skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let contract: HTyp.t => t;

let expand: t => HTyp.t;

let is_complete: t => bool;
