open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {probe_ids: MerkleSet.t(Id.t)};

let empty: t = {probe_ids: MerkleSet.empty};

let union = (a: t, b: t): t => {
  probe_ids: MerkleSet.union(a.probe_ids, b.probe_ids),
};

let union_all = (xs: list(t)): t => List.fold_left(union, empty, xs);

let equal = (a: t, b: t): bool => MerkleSet.equal(a.probe_ids, b.probe_ids);

/* Inject this id into the witness if it's in the user's probe-targets map.
 * Non-probed ids are not tracked here — adding/removing a probe is the only
 * change that doesn't already show up in the elaboration. */
let add_self = (~is_probed: bool, id: Id.t, t: t): t =>
  is_probed
    ? {probe_ids: MerkleSet.union(MerkleSet.singleton(id), t.probe_ids)} : t;
