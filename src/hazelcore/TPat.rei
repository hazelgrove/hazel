module VarPatErrStatus: {
  [@deriving sexp]
  type t =
    | Keyword(ExpandingKeyword.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(option(VarPatErrStatus.t), TyId.t);

let is_complete: t => bool;

let tyvar_of_tyid: TyId.t => t;

let binds_tyvar: (TyId.t, t) => bool;
