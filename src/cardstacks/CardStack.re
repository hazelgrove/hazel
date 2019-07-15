open Sexplib.Std;

[@deriving sexp]
type t = list(Card.t);
