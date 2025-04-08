open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

/* Used for VarBstMap */
let compare = (x: t, y: t) => compare(x, y);

let rec free_name = (x: t, bound: list(t)) =>
  if (List.mem(x, bound)) {
    free_name(x ++ "'", bound);
  } else {
    x;
  };
