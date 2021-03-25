// open Sexplib.Std;

[@deriving sexp]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentSumType(HTyp.t, MetaVar.t)
  | InconsistentTag(Tag.t, MetaVar.t);