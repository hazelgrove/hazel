[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(TPatErrStatus.t, TyVar.t)
  | InvalidText(MetaVar.t, string);

let of_string: string => t;
let invalid_of_string: (IDGen.t, string) => (t, IDGen.t);

let is_complete: t => bool;
let binds_tyvar: (TyVar.t, t) => bool;
let tyvar_name: t => option(TyVar.t);
