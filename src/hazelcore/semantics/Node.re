module Typ = {
  [@deriving sexp]
  type t =
    | Operand(UHTyp.operand)
    | Operator(UHTyp.operator);
};

module Pat = {
  [@deriving sexp]
  type t =
    | Operand(UHPat.operand)
    | Operator(UHPat.operator);
};

module Exp = {
  [@deriving sexp]
  type t =
    | Line(UHExp.line)
    | Rule(UHExp.rule)
    | Operand(UHExp.operand)
    | Operator(UHExp.operator);
};

/**
 * Nodes upon which the cursor can be placed.
 * Note that there are neither block- nor
 * opseq-level cursors, only cursors on their
 * constituents.
 */
[@deriving sexp]
type t =
  | Typ(Typ.t)
  | Pat(Pat.t)
  | Exp(Exp.t);
