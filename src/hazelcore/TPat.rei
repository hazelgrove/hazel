/* type variable pattern errors */
module HoleReason: {
  [@deriving sexp]
  type t =
    | ReservedKeyword
    | BuiltinType
    | InvalidName;
};

/* type variable pattern hole status */
module Status: {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, string);

let is_complete: t => bool;
let of_string: string => t;
let binds_tyvar: (string, t) => bool;
let tyvar_name: t => option(string);
