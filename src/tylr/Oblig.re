[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Missing_term // convex grout
  | Missing_tile // ghost tile
  | Inconsistent_term // prefix/postfix grout
  | Extra_term; // infix grout

// low to high severity
let all = [Missing_term, Missing_tile, Inconsistent_term, Extra_term];
let severity = o => Option.get(List.find_index((==)(o), all));

module Ord = {
  type nonrec t = t;
  let compare = (l, r) => Int.compare(severity(l), severity(r));
};
module Map = Map.Make(Ord);

module Delta = {
  include Map;
  type t = Map.t(int);
  let find = (o, map) =>
    Option.value(find_opt(o, map), ~default=0);

  let compare = (l, r) =>
    List.fold_right(
      (o, c) => c != 0 ? c : Int.compare(find(o, l), find(o, r)),
      all,
      0,
    );
};
