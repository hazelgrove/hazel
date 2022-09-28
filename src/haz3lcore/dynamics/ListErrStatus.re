open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(Typ.t), MetaVar.t);
