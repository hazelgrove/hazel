open Sexplib.Std;

[@deriving sexp]
type t =
  | Tag(string)
  | TagHole(MetaVar.t);

let compare = compare;

let eq = (t1: t, t2: t): bool => t1 == t2;
