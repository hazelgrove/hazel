/* type variable errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

/* type variable hole status */
module Status = {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

/* type variable names */
module Name = {
  open Sexplib.Std;

  [@deriving sexp]
  type t = string;

  let equal: (t, t) => bool = String.equal;
};

[@deriving sexp]
type t = (Index.t, Name.t);

let equal: (t, t) => bool = (==);
