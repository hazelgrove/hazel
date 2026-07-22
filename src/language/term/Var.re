open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

let equal = String.equal;

/* Required by Map.Make(Var) / Set.Make(Var) */
let compare = (x: t, y: t) => compare(x, y);

let next_name = (x: t) => x ++ "'";

let rec free_name = (x: t, bound: list(t)) =>
  if (List.mem(x, bound)) {
    free_name(next_name(x), bound);
  } else {
    x;
  };
