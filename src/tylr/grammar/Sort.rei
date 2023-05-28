[@deriving (show({with_path: false}), sexp, yojson)]
type t;

let root: t;
let compare: (t, t) => int;
let lca: (t, t) => t;

let to_string: t => string;
// hack to do sort-specific stuff in web
// todo: unhack
let of_string: string => t;

module Map: Map.S with type key = t;

/** old Sort.re
open Sexplib.Std;

include Lang.Sort;

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type sort = t;
// None represents "unsorted" sort used for unrnecognized tokens
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type o = option(sort);

let consistent = (l: o, r: o) =>
  switch (l, r) {
  | (None, _)
  | (_, None) => true
  | (Some(l), Some(r)) => l == r
  };

let eq = (l: o, r: o) => l == r;

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
  let mk = (~strict=false, ~sort=None, ()) => {sort, strict};
};

let leq = (l, r) => lca(l, r) == l;
let geq = (l, r) => lca(l, r) == r;
*/;
