/* type variable pattern errors */
module HoleReason = {
  // [@deriving sexp]
  type t =
    | Reserved
    | InvalidName;
};

/* type variable pattern hole status */
module Status = {
  // [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

// [@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, TyVar.Name.t);

// let is_complete =
//   fun
//   | EmptyHole
//   | TyVar(Some(_), _) => false
//   | TyVar(None, _) => true;

let tyvar_of_tyid = name => {
  let status =
    name
    |> TyVar.Name.to_string
    |> ExpandingKeyword.mk
    |> Option.fold(
         ~some=_ => Status.InHole(Reserved, u),
         ~none=
           TyVar.Name.to_builtin_type(name)
           |> Option.map(e => VarPatErrStatus.BuiltInType(e)),
       );
  TyVar(status, name);
};

let binds_tyvar = (name: TyVar.Name.t): (t => bool) =>
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, name') => TyVar.Name.equal(name, name');
