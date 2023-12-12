[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Missing_term // convex grout
  | Missing_tile // ghost tile
  | Incon_term // pre/postfix grout
  | Extra_term; // infix grout

// low to high severity
let all = [Missing_term, Missing_tile, Incon_term, Extra_term];
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

  let pick = (xs: list(('x, t))): option('x) =>
    xs
    |> List.sort(((_, l), (_, r)) => compare(l, r))
    |> ListUtil.hd_opt
    |> Option.map(fst)
};

module Result = {
  type t('a) = ('a, Delta.t);

  let map = (f, (a, d)) => (f(a), d);
  let bind = ((a, d), f) => {
    let (b, d') = f(a);
    (b, Delta.add([d, d']));
  };

  module Syntax = {
    let return = a => (a, Delta.zero);
    let (let+) = Fun.flip(map);
    let (let*) = bind;
  };
};
