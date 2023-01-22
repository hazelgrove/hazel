open Sexplib.Std;

include Lang.Sort;

[@deriving (show({with_path: false}), sexp, yojson)]
type sort = t;
// None represents "unsorted" sort used for unrecognized tokens
[@deriving (show({with_path: false}), sexp, yojson)]
type o = option(sort);

let compare = (l: o, r: o): int =>
  switch (l, r) {
  | (None, None) => 0
  | (None, Some(_)) => 1
  | (Some(_), None) => (-1)
  | (Some(l), Some(r)) => compare(l, r) // user-specified
  };

let lca = (l: o, r: o): o =>
  switch (compare(l, r)) {
  | c when c < 0 => l
  | c when c > 0 => r
  | _ => l
  };

// expected sort bound
module Ana = {
  type t = {
    sort: o,
    // whether kid should have exactly sort
    // or anything prec-lower-bounded by sort
    strict: bool,
  };
  let mk = (~strict=false, ~sort=?, ()) => {sort, strict};
};
