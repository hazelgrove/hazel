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

let tyvar_of_tyid = tyid =>
  TyVar(
    tyid
    |> TyId.to_string
    |> ExpandingKeyword.mk
    |> Option.map(kw => VarPatErrStatus.Keyword(kw)),
    tyid,
  );

let binds_tyvar = tyid =>
  fun
  | EmptyHole
  | TyVar(Some(_), _) => false
  | TyVar(None, id') => TyId.eq(tyid, id');
