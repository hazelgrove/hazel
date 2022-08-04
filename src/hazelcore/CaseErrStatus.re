open Sexplib.Std;

/* TODO: (eric) make it possible to report simultaneous InconsistentBranches and NotExhaustive? */
[@deriving sexp]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(HTyp.t), MetaVar.t)
  | NotExhaustive(MetaVar.t);
