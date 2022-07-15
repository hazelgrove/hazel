/* type variable pattern errors */
module HoleReason: {
  [@deriving sexp]
  type t =
    | ReservedKeyword
    | BuiltinType
    | InvalidText;
};

/* type variable pattern hole status */
module ErrStatus: {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(ErrStatus.t, string);

let of_string: string => t;
let invalid_of_string: (IDGen.t, string) => (t, IDGen.t);

let is_complete: t => bool;
let binds_tyvar: (string, t) => bool;
let tyvar_name: t => option(string);
