module HoleReason: {
  // [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

module Status: {
  // [@deriving sexp]
  type t =
    | NotInHole(Index.t)
    | InHole(HoleReason.t, MetaVar.t);
};

/* type variable names */
module Name: {
  // [@deriving sexp]
  type t = string;

  let of_string: string => t;
  let to_string: t => string;
  let length: t => int;
  let equal: (t, t) => bool;
  let valid: string => bool;
  let reserved: string => bool;
};
