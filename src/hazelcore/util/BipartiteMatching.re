open Sexplib.Std;

include IntMap;

[@deriving sexp]
type v =
  | Unmatched
  | Matched(int);

[@deriving sexp]
type nonrec t = t(v);

[@deriving sexp]
type binding = (int, v);

let match = (u: int, v: int): (t => t) => add(u, Matched(v));

let unmatch = (u: int): (t => t) => add(u, Unmatched);

let init = (partU: IntSet.t): t =>
  IntSet.fold(u => add(u, Unmatched), partU, empty);

let unmatched_sources = (matching: t): IntSet.t =>
  IntSet.empty
  |> fold((u, v, us) => v == Unmatched ? us |> IntSet.add(u) : us, matching);

let matched_sources = (matching: t): IntSet.t =>
  IntSet.empty
  |> fold((u, v, us) => v == Unmatched ? us : us |> IntSet.add(u), matching);

let matched_targets = (matching: t): IntSet.t =>
  matching
  |> bindings
  |> List.filter_map(
       fun
       | (_, Unmatched) => None
       | (_, Matched(v)) => Some(v),
     )
  |> IntSet.of_list;

let unmatched_targets = (targets: IntSet.t, matching: t): IntSet.t =>
  IntSet.diff(targets, matching |> matched_targets);

let find_sources = (v: int, matching: t): IntSet.t =>
  matching
  |> bindings
  |> List.filter_map(
       fun
       | (u, Matched(v')) when v' == v => Some(u)
       | (_, Matched(_))
       | (_, Unmatched) => None,
     )
  |> IntSet.of_list;

let is_perfect = (matching: t): bool =>
  matching |> for_all(_ => (!=)(Unmatched));
