module HoleReason: {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

module Status: {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

/* type variable names */
module Name: {
  [@deriving sexp]
  type t = string;

  let equal: (t, t) => bool;
};

[@deriving sexp]
type t = (Index.t, Name.t);

let equal: (t, t) => bool;
