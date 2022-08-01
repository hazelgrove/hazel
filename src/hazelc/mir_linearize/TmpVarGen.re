open Sexplib.Std;

let prefix = Ident.v("t");
let delim = Ident.v("_");

[@deriving sexp]
type t = int;

let init = 0;

let next = t_gen => {
  let name = Ident.concat(prefix, Ident.v(string_of_int(t_gen)));
  (name, t_gen + 1);
};

let next_named = (x, t_gen) => {
  let (suffix, t_gen) = next(t_gen);
  let name = Ident.concat(Ident.concat(x, delim), suffix);
  (name, t_gen);
};
