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
    | NotInHole(Index.t)
    | InHole(HoleReason.t, MetaVar.t);
};

let valid_name: string => bool;
let reserved_word: string => bool;
