open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

let equal = String.equal;

let length = String.length;

let is_fun = equal("fun");

let is_wild = equal("_");

let split = (pos, name) => {
  let left_var = String.sub(name, 0, pos);
  let right_var = String.sub(name, pos, String.length(name) - pos);
  (left_var, right_var);
};

/* Used for VarBstMap */
let compare = (x: t, y: t) => compare(x, y);

let next_name = (x: t) => x ++ "'";

let rec free_name = (x: t, bound: list(t)) =>
  if (List.mem(x, bound)) {
    free_name(next_name(x), bound);
  } else {
    x;
  };
