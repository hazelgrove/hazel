[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | StringLit(string)
  | Inj(InjSide.t, t)
  | ListLit(Typ.t, list(t))
  | Cons(t, t)
  | Pair(t, t)
  | Triv
  | Ap(t, t);

let mk_tuple: list(t) => t;

/**
 * Whether dp contains the variable x outside of a hole.
 */
let binds_var: (Var.t, t) => bool;
