open Sexplib.Std;

[@deriving sexp]
type t = int;

let init = 0;

let next = x => {
  let name = "t" ++ string_of_int(x);
  (name, x + 1);
};
