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
  | StringLit(
      UnescapedString.t,
      list(UnescapedString.valid_seq),
      list(UnescapedString.invalid_seq),
    )
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
