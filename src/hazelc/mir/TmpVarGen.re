open Sexplib.Std;

[@deriving sexp]
type t = int;

let init = 0;

let next = t_gen => {
  let name = "t" ++ string_of_int(t_gen);
  (name, t_gen + 1);
};

let next_named = (x, t_gen) => {
  let (prefix, t_gen) = next(t_gen);
  (x ++ "_" ++ prefix, t_gen);
};
