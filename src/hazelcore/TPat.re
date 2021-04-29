open Sexplib.Std;

module VarPatErrStatus = {
  [@deriving sexp]
  type t =
    | Keyword(ExpandingKeyword.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(option(VarPatErrStatus.t), TyId.t);

let is_complete =
  fun
  | EmptyHole
  | TyVar(Some(_), _) => false
  | TyVar(None, _) => true;
