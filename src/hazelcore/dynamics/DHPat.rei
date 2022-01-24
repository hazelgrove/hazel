[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, HoleClosureId.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleClosureId.t, t)
  | Wild
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, HoleClosureId.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, HoleClosureId.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
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
