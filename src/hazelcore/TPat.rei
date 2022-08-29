[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(TPatErrStatus.t, TyVar.t)
  | InvalidText(MetaVar.t, string);

let of_string: string => t;
let invalid_of_string: (IDGen.t, string) => (t, IDGen.t);

let is_complete: t => bool;
let binds_tyvar: (string, t) => bool;
let tyvar_name: t => option(string);
let to_string: t => string;
