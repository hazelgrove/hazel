open Sexplib.Std;

include Lang.Sort;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type sort = t;
// None represents "unsorted" sort used for unrecognized tokens
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type o = option(sort);

let lca = (l: o, r: o): o =>
  switch (compare_o(l, r)) {
  | c when c < 0 => l
  | c when c > 0 => r
  | _ => l
  };

let root_o = Some(root);

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
