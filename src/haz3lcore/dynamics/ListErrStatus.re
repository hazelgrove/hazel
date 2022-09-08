open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(HTyp.t), MetaVar.t);
