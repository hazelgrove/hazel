module UnIntOp: {
  [@deriving sexp]
  type t =
    | Negate;
  let of_op: UHPat.unop => option((t, HTyp.t));

  let to_op: t => UHPat.unop;
};

module UnFloatOp: {
  [@deriving sexp]
  type t =
    | FNegate;
  let of_op: UHPat.unop => option((t, HTyp.t));

  let to_op: t => UHPat.unop;
};

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | UnIntOp(UnIntOp.t, t)
  | UnFloatOp(UnFloatOp.t, t)
  | Inj(InjSide.t, t)
  | ListNil
  | Cons(t, t)
  | Pair(t, t)
  | Triv
  | Ap(t, t);

let mk_tuple: list(t) => t;

/**
 * Whether dp contains the variable x outside of a hole.
 */
let binds_var: (Var.t, t) => bool;
