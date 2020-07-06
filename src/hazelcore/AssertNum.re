open Sexplib.Std;

[@deriving sexp]
type t = int;
let eq = (x: t, y: t) => x === y;

let generate = () => Random.int(100000000);

let print = (x: t) => print_endline(Sexplib.Sexp.to_string(sexp_of_t(x)));
