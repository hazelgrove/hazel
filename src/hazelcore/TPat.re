/* type variable pattern errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | Reserved
    | InvalidName;
};

/* type variable pattern hole status */
module Status = {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, TyVar.Name.t);

// let is_complete =
//   fun
//   | EmptyHole
//   | TyVar(Some(_), _) => false
//   | TyVar(None, _) => true;

// let tyvar_of_tyid = tyid => {
//   let error_option =
//     tyid
//     |> TyId.to_string
//     |> ExpandingKeyword.mk
//     |> Option.map(kw => VarPatErrStatus.Keyword(kw));
//   let error_option =
//     switch (error_option) {
//     | None =>
//       TyId.to_builtin_type(tyid)
//       |> Option.map(e => VarPatErrStatus.BuiltInType(e))
//     | Some(e) => Some(e)
//     };
//   TyVar(error_option, tyid);
// };

// let binds_tyvar = tyid =>
//   fun
//   | EmptyHole
//   | TyVar(Some(_), _) => false
//   | TyVar(None, id') => TyId.eq(tyid, id');
