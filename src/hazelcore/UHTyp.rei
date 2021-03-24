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
  | TyVar(VarErrStatus.t, TyId.t)
  | Parenthesized(t)
  | List(t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

let get_prod_elements: skel => list(skel);

let unwrap_parentheses: operand => t;

let associate: seq => Skel.t(Operators_Typ.t);

let mk_OpSeq: OpSeq.seq(operand, operator) => OpSeq.t(operand, operator);

let contract: HTyp.t => t;

/* "expand" is handled by Elaborator_Typ.syn/ana */

let is_complete: t => bool;

let to_string_exn: operand => string;
